{-# LANGUAGE TemplateHaskell, TupleSections #-}
-- | Defines functions for simultaneous interpretation of ExternalLibrary and
-- ExternalLibraryCall.
module Language.Drasil.Code.ExtLibImport (ExtLibState(..), auxMods, defs,
  imports, modExports, steps, genExternalLibraryCall) where

import Language.Drasil (HasSpace(typ), getActorName)

import Language.Drasil.Chunk.Code (CodeVarChunk, CodeFuncChunk, codeName,
  ccObjVar)
import Language.Drasil.Chunk.Parameter (ParameterChunk)
import Language.Drasil.Chunk.NamedArgument (NamedArgument)
import Language.Drasil.CodeExpr (CodeExpr, ($&&), applyWithNamedArgs,
  msgWithNamedArgs, new, newWithNamedArgs, sy)
import Language.Drasil.Mod (Class, StateVariable, Func(..), Mod, Name,
  Description, packmodRequires, classDef, classImplements, FuncStmt(..),
  funcDefParams, ctorDef)
import Language.Drasil.Code.ExternalLibrary (ExternalLibrary, Step(..),
  FunctionInterface(..), Result(..), Argument(..), ArgumentInfo(..),
  Parameter(..), ClassInfo(..), MethodInfo(..), FuncType(..))
import Language.Drasil.Code.ExternalLibraryCall (ExternalLibraryCall,
  StepGroupFill(..), StepFill(..), FunctionIntFill(..), ArgumentFill(..),
  ParameterFill(..), ClassInfoFill(..), MethodInfoFill(..))

import Control.Lens (makeLenses, (^.), over)
import Control.Monad (zipWithM)
import Control.Monad.State (State, execState, get, modify)
import Data.List (nub, partition)
import Data.List.NonEmpty (NonEmpty(..), (!!), toList)
import Data.Maybe (isJust)
import Prelude hiding ((!!))

-- | State object used during interpretation of an 'ExternalLibrary' and
-- 'ExternalLibraryCall'.
data ExtLibState = ELS {
  -- | Additional modules that must be generated to use the library.
  _auxMods :: [Mod],
  -- | The defining statements for variables that must be pre-defined before
  -- being passed as arguments in an external library call.
  _defs :: [FuncStmt],
  -- | The names of variables for which a defining statement has already been
  -- generated.
  _defined :: [Name],
  -- | The statements corresponding to the external library use case.
  _steps :: [FuncStmt],
  -- | The imports required to use the external library.
  _imports :: [String],
  -- | An association list between library method/function names and the external
  -- library module that exports them.
  _modExports :: [(Name, Name)]
}
makeLenses ''ExtLibState

-- | Initialize an empty 'ExtLibState'.
initELS :: ExtLibState
initELS = ELS {
  _auxMods = [],
  _defs = [],
  _defined = [],
  _steps = [],
  _imports = [],
  _modExports = []
}

-- State Modifiers

-- | Adds a module definition to an ExtLibState.
addMod :: Mod -> ExtLibState -> ExtLibState
addMod m = over auxMods (m:)

-- | Adds a defining statement for the given 'CodeVarChunk' and 'CodeExpr' to the
-- 'ExtLibState' and adds the 'CodeVarChunk''s name to the defined field of the
-- state, but only if it was not already in the defined field.
addDef :: CodeExpr -> CodeVarChunk -> ExtLibState -> ExtLibState
addDef e c s = if n `elem` (s ^. defined)
               then s
               else over defs (++ [FDecDef c e]) (addDefined n s)
  where n = codeName c

-- | Adds a defining statement for a local function, represented by the given
-- 'CodeFuncChunk', 'ParameterChunk's, and 'FuncStmt's, to the 'ExtLibState', and adds
-- the function's name to the defined field, but only if it was not already in
-- the defined field.
addFuncDef :: CodeFuncChunk -> [ParameterChunk] -> [FuncStmt] -> ExtLibState ->
  ExtLibState
addFuncDef c ps b s = if n `elem` (s ^. defined) then s else over defs
  (++ [FFuncDef c ps b]) (addDefined n s)
  where n = codeName c

-- | Adds to the 'ExtLibState' statements for initializing fields, represented by
-- the list of 'CodeVarChunk', of a record, represented by the 'CodeVarChunk', with
-- values, represented by the list of 'CodeExpr'.
addFieldAsgs :: CodeVarChunk -> [CodeVarChunk] -> [CodeExpr] -> ExtLibState ->
  ExtLibState
addFieldAsgs o cs es = over defs (++ zipWith FAsg (map (ccObjVar o) cs) es)

-- | Adds a name to the defined field of 'ExtLibState'.
addDefined :: Name -> ExtLibState -> ExtLibState
addDefined n = over defined (n:)

-- | Adds a list of imports to the 'ExtLibState'.
addImports :: [String] -> ExtLibState -> ExtLibState
addImports is = over imports (\l -> nub $ l ++ is)

-- | Adds to the 'ExtLibState' an association between a library function/method and
-- the library's module that exports it.
addModExport :: (Name, Name) -> ExtLibState -> ExtLibState
addModExport e = over modExports (e:)

-- | Adds a list of statements for an external library use case to the 'ExtLibState'.
addSteps :: [FuncStmt] -> ExtLibState -> ExtLibState
addSteps fs = over steps (++fs)

-- | Resets fields of 'ExtLibState' that are local to each module.
refreshLocal :: ExtLibState -> ExtLibState
refreshLocal s = s {_defs = [], _defined = [], _imports = []}

-- | Returns fields of 'ExtLibState' that are local to each module to a previous state.
returnLocal :: ExtLibState -> ExtLibState -> ExtLibState
returnLocal oldS newS = newS {_defs = oldS ^. defs,
                              _defined = oldS ^. defined,
                              _imports = oldS ^. imports}

-- Generators

-- | Interprets an 'ExternalLibrary' and 'ExternalLibraryCall' and returns the
-- resulting 'ExtLibState'.
genExternalLibraryCall :: ExternalLibrary -> ExternalLibraryCall ->
  ExtLibState
genExternalLibraryCall el elc = execState (genExtLibCall el elc) initELS

-- | Interprets a list of 'StepGroups' and 'StepGroupFills', adding the resulting
-- statements to the 'ExtLibState'.
genExtLibCall :: ExternalLibrary -> ExternalLibraryCall ->
  State ExtLibState ()
genExtLibCall [] [] = return ()
genExtLibCall (sg:el) (SGF n sgf:elc) = let s = sg!!n in
  if length s /= length sgf then error stepNumberMismatch else do
    fs <- zipWithM genStep s sgf
    modify (addSteps fs)
    genExtLibCall el elc
genExtLibCall _ _ = error stepNumberMismatch

-- | Interprets a 'Step' and 'StepFill', resulting in a 'FuncStmt' that performs the step.
genStep :: Step -> StepFill -> State ExtLibState FuncStmt
genStep (Call fi) (CallF fif) = genFI fi fif
genStep (Loop fis f ss) (LoopF fifs ccList sfs) = do
  es <- zipWithM genFIVal (toList fis) (toList fifs)
  fs <- zipWithM genStep (toList ss) (toList sfs)
  return $ FWhile (foldl1 ($&&) es $&& f ccList) fs
genStep (Statement f) (StatementF ccList exList) = return $ f ccList exList
genStep _ _ = error stepTypeMismatch

-- | Interprets a 'FunctionInterface' and 'FunctionIntFill', resulting in a 'CodeExpr'
-- representing a call to the library. Imports required for the call are added
-- to the 'ExtLibState', and the called function/method is added to the library
-- export association list in the 'ExtLibState'.
genFIVal :: FunctionInterface -> FunctionIntFill -> State ExtLibState CodeExpr
genFIVal (FI (r:|rs) ft f as _) (FIF afs) = do
  args <- genArguments as afs
  let isNamed = isJust . fst
      (nas, ars) = partition isNamed args
  modify (addImports rs . addModExport (codeName f, r))
  return $ getCallFunc ft f (map snd ars) (map (\(n, e) ->
    maybe (error "defective isNamed") (,e) n) nas)
  where getCallFunc Function = applyWithNamedArgs
        getCallFunc (Method o) = msgWithNamedArgs o
        getCallFunc Constructor = newWithNamedArgs

-- | Interprets a 'FunctionInterface' and 'FunctionIntFill', resulting in a 'FuncStmt'
-- for the function/method call.
genFI :: FunctionInterface -> FunctionIntFill -> State ExtLibState FuncStmt
genFI fi@(FI _ _ _ _ r) fif = do
  fiEx <- genFIVal fi fif
  return $ maybeGenAssg r fiEx

-- | Interprets a list of 'Argument' and list of 'ArgumentFill', returning the 'CodeExpr'
-- for each argument and the 'NamedArgument' chunk for arguments that are named.
genArguments :: [Argument] -> [ArgumentFill] ->
  State ExtLibState [(Maybe NamedArgument, CodeExpr)]
genArguments (Arg n (LockedArg e):as) afs = fmap ((n,e):) (genArguments as afs)
genArguments as (UserDefinedArgF n e:afs) = fmap ((n,e):) (genArguments as afs)
genArguments (Arg n (Basic _ Nothing):as) (BasicF e:afs) = fmap ((n,e):)
  (genArguments as afs)
genArguments (Arg n (Basic _ (Just v)):as) (BasicF e:afs) = do
  modify (addDef e v)
  fmap ((n, sy v):) (genArguments as afs)
genArguments (Arg n (Fn c ps s):as) (FnF pfs sf:afs) = do
  let prms = genParameters ps pfs
  st <- genStep s sf
  modify (addFuncDef c prms [st])
  fmap ((n, sy c):) (genArguments as afs)
genArguments (Arg n (Class rs desc o ctor ci):as) (ClassF svs cif:afs) = do
  (c, is) <- genClassInfo o ctor an desc svs ci cif
  modify (addMod (packmodRequires an desc (rs ++ is) [c] []))
  fmap ((n, sy o):) (genArguments as afs)
  where an = getActorName (o ^. typ)
genArguments (Arg n (Record (rq:|rqs) rn r fs):as) (RecordF es:afs) =
  if length fs /= length es then error recordFieldsMismatch else do
    modify (addFieldAsgs r fs es . addDef (new rn []) r .
      addModExport (codeName rn, rq) . addImports rqs)
    fmap ((n, sy r):) (genArguments as afs)
genArguments [] [] = return []
genArguments _ _ = error argumentMismatch

-- | Interprets a 'ClassInfo' and 'ClassInfoFill'. These are required when a
-- 'customObjArg' is needed for an external library call, so the 'CodeVarChunk'
-- parameter represents the object of the class. The 'CodeFuncChunk' represents
-- the class's constructor. Other parameters are the name, description, and
-- state variables for the class.
genClassInfo :: CodeVarChunk -> CodeFuncChunk -> Name -> Description ->
  [StateVariable] -> ClassInfo -> ClassInfoFill ->
  State ExtLibState (Class, [String])
genClassInfo o c n desc svs ci cif = let (mis, mifs, f) = genCI ci cif in
  if length mis /= length mifs then error methodInfoNumberMismatch else do
    ms <- zipWithM (genMethodInfo o c) mis mifs
    modify (if any isConstructor mis then id else addDef (new c []) o)
    return (f desc svs (map fst ms), concatMap snd ms)
  where genCI (Regular mis') (RegularF mifs') = (mis', mifs', classDef n)
        genCI (Implements intn mis') (ImplementsF mifs') = (mis', mifs',
          classImplements n intn)
        genCI _ _ = error classInfoMismatch

-- | Interprets a 'MethodInfo' and 'MethodInfoFill'. These are required when a
-- 'customObjArg' is needed for an external library call, so the 'CodeVarChunk'
-- parameter represents the object of the class. The 'CodeFuncChunk' represents
-- the class's constructor.
genMethodInfo :: CodeVarChunk -> CodeFuncChunk -> MethodInfo ->
  MethodInfoFill -> State ExtLibState (Func, [String])
genMethodInfo o c (CI desc ps ss) (CIF pfs is sfs) = do
  let prms = genParameters ps pfs
  (fs, newS) <- withLocalState $ zipWithM genStep ss sfs
  modify (addDef (new c (map sy prms)) o)
  return (ctorDef (codeName c) desc prms is (newS ^. defs ++ fs),
    newS ^. imports)
genMethodInfo _ _ (MI m desc ps rDesc ss) (MIF pfs sfs) = do
  let prms = genParameters ps pfs
  (fs, newS) <- withLocalState (zipWithM genStep (toList ss) (toList sfs))
  return (funcDefParams (codeName m) desc prms (m ^. typ) rDesc (
    newS ^. defs ++ fs), newS ^. imports)
genMethodInfo _ _ _ _ = error methodInfoMismatch

-- | Interprets a list of 'Parameter' and a list of 'ParameterFill', resulting in
-- 'ParameterChunk's.
genParameters :: [Parameter] -> [ParameterFill] -> [ParameterChunk]
genParameters (LockedParam c:ps) pfs = c : genParameters ps pfs
genParameters ps (UserDefined c:pfs) = c : genParameters ps pfs
genParameters (NameableParam _:ps) (NameableParamF c:pfs) = c :
  genParameters ps pfs
genParameters [] [] = []
genParameters _ _ = error paramMismatch

-- | Interprets a 'Result', which determines which 'FuncStmt' constructor should be
-- used for a defining statement. If no result, the statement is just the value
-- for the function call. If result is assigned, the statement is an
-- assignment. If the result is returned, the statement is a return statement.
maybeGenAssg :: Maybe Result -> (CodeExpr -> FuncStmt)
maybeGenAssg Nothing = FVal
maybeGenAssg (Just (Assign c)) = FDecDef c
maybeGenAssg (Just Return)  = FRet

-- Helpers

-- | Run a stateful value with refreshed local state, and return the resulting value and modified state. Reverts back to the original state before returning.
withLocalState :: State ExtLibState a -> State ExtLibState (a, ExtLibState)
withLocalState st = do
  s <- get
  modify refreshLocal
  st' <- st
  newS <- get
  modify (returnLocal s)
  return (st', newS)

-- | Predicate that is true only if then MethodInfo is a constructor.
isConstructor :: MethodInfo -> Bool
isConstructor CI{} = True
isConstructor _    = False

-- Error messages
-- | Various error messages.
elAndElc, stepNumberMismatch, stepTypeMismatch, argumentMismatch,
  paramMismatch, recordFieldsMismatch, ciAndCif, classInfoMismatch,
  methodInfoNumberMismatch, methodInfoMismatch :: String
elAndElc = "ExternalLibrary and ExternalLibraryCall have different "
stepNumberMismatch = elAndElc ++ "number of steps"
stepTypeMismatch = elAndElc ++ "order of steps"
argumentMismatch = "FunctionInterface and FunctionIntFill have different number or types of arguments"
paramMismatch = "Parameters mismatched with ParameterFills"
recordFieldsMismatch = "Different number of record fields than field values"
ciAndCif = "ClassInfo and ClassInfoFill have different "
classInfoMismatch = ciAndCif ++ "class types"
methodInfoNumberMismatch = ciAndCif ++ "number of MethodInfos/MethodInfoFills"
methodInfoMismatch = "MethodInfo and MethodInfoFill have different method types"
