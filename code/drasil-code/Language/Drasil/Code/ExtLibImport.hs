{-# LANGUAGE TemplateHaskell, TupleSections #-}
module Language.Drasil.Code.ExtLibImport (ExtLibState(..), auxMods, defs, 
  imports, modExports, steps, genExternalLibraryCall) where

import Language.Drasil

import Language.Drasil.Chunk.Code (CodeVarChunk, CodeFuncChunk, codeName, 
  ccObjVar)
import Language.Drasil.CodeExpr (new, newWithNamedArgs, msgWithNamedArgs)
import Language.Drasil.Mod (Class, StateVariable, Func(..), Mod, Name, 
  packmodRequires, classDef, classImplements, FuncStmt(..), funcDef, ctorDef)
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

data ExtLibState = ELS {
  _auxMods :: [Mod],
  _defs :: [FuncStmt],
  _defined :: [Name],
  _imports :: [String],
  _modExports :: [(String, String)],
  _steps :: [FuncStmt]
}
makeLenses ''ExtLibState

initELS :: ExtLibState
initELS = ELS {
  _auxMods = [],
  _defs = [],
  _defined = [],
  _imports = [],
  _modExports = [],
  _steps = []
}

-- State Modifiers

addMod :: Mod -> ExtLibState -> ExtLibState
addMod m = over auxMods (m:)

addDef :: Expr -> CodeVarChunk -> ExtLibState -> ExtLibState
addDef e c s = if n `elem` (s ^. defined) then s else over defs (++ [FDecDef c 
  e]) (addDefined n s)
  where n = codeName c

addFieldAsgs :: CodeVarChunk -> [CodeVarChunk] -> [Expr] -> ExtLibState -> 
  ExtLibState
addFieldAsgs o cs es = over defs (++ zipWith FAsg (map (ccObjVar o) cs) es)

addDefined :: String -> ExtLibState -> ExtLibState
addDefined n = over defined (n:)

addImports :: [String] -> ExtLibState -> ExtLibState
addImports is = over imports (\l -> nub $ l ++ is)

addModExport :: (String, String) -> ExtLibState -> ExtLibState
addModExport e = over modExports (e:)

addSteps :: [FuncStmt] -> ExtLibState -> ExtLibState
addSteps fs = over steps (++fs)

refreshLocal :: ExtLibState -> ExtLibState
refreshLocal s = s {_defs = [], _defined = [], _imports = []}

returnLocal :: ExtLibState -> ExtLibState -> ExtLibState
returnLocal oldS newS = newS {_defs = oldS ^. defs, 
                              _defined = oldS ^. defined, 
                              _imports = oldS ^. imports}

-- Generators

genExternalLibraryCall :: ExternalLibrary -> ExternalLibraryCall -> 
  ExtLibState
genExternalLibraryCall el elc = execState (genExtLibCall el elc) initELS

genExtLibCall :: ExternalLibrary -> ExternalLibraryCall ->
  State ExtLibState ()
genExtLibCall [] [] = return ()
genExtLibCall (sg:el) (SGF n sgf:elc) = let s = sg!!n in 
  if length s /= length sgf then error stepNumberMismatch else do
    fs <- zipWithM genStep s sgf
    modify (addSteps fs)
    genExtLibCall el elc
genExtLibCall _ _ = error stepNumberMismatch

genStep :: Step -> StepFill -> State ExtLibState FuncStmt
genStep (Call fi) (CallF fif) = genFI fi fif
genStep (Loop fis f ss) (LoopF fifs ccList sfs) = do
  es <- zipWithM genFIVal (toList fis) (toList fifs)
  fs <- zipWithM genStep (toList ss) (toList sfs)
  return $ FWhile (foldl1 ($&&) es $&& f ccList) fs
genStep (Statement f) (StatementF ccList exList) = return $ f ccList exList
genStep _ _ = error stepTypeMismatch

genFIVal :: FunctionInterface -> FunctionIntFill -> State ExtLibState Expr
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

genFI :: FunctionInterface -> FunctionIntFill -> State ExtLibState FuncStmt
genFI fi@(FI _ _ _ _ r) fif = do
  fiEx <- genFIVal fi fif
  return $ maybeGenAssg r fiEx 

genArguments :: [Argument] -> [ArgumentFill] -> 
  State ExtLibState [(Maybe NamedArgument, Expr)]
genArguments (Arg n (LockedArg e):as) afs = fmap ((n,e):) (genArguments as afs)
genArguments as (UserDefinedArgF n e:afs) = fmap ((n,e):) (genArguments as afs)
genArguments (Arg n (Basic _ Nothing):as) (BasicF e:afs) = fmap ((n,e):) 
  (genArguments as afs)
genArguments (Arg n (Basic _ (Just v)):as) (BasicF e:afs) = do
  modify (addDef e v)
  fmap ((n,sy v):) (genArguments as afs)
-- FIXME: funcexpr needs to be defined, a function-valued expression
-- Uncomment the below when funcexpr is added
genArguments (Arg n (Fn c _ _{-ps s-}):as) (FnF _ _{-pfs sf-}:afs) = -- do
  -- let prms = genParameters ps pfs
  -- st <- genStep s sf
  -- modify (addDef (funcexpr prms st) c)
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
  
genClassInfo :: CodeVarChunk -> CodeFuncChunk -> String -> String -> 
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
  return (funcDef (codeName m) desc prms (m ^. typ) rDesc (newS ^. defs ++ fs),
    newS ^. imports)
genMethodInfo _ _ _ _ = error methodInfoMismatch

genParameters :: [Parameter] -> [ParameterFill] -> [CodeVarChunk]
genParameters (LockedParam c:ps) pfs = c : genParameters ps pfs
genParameters ps (UserDefined c:pfs) = c : genParameters ps pfs
genParameters (NameableParam _:ps) (NameableParamF c:pfs) = c : 
  genParameters ps pfs
genParameters [] [] = []
genParameters _ _ = error paramMismatch

maybeGenAssg :: Maybe Result -> (Expr -> FuncStmt)
maybeGenAssg Nothing = FVal
maybeGenAssg (Just (Assign c)) = FDecDef c
maybeGenAssg (Just Return)  = FRet

-- Helpers

withLocalState :: State ExtLibState a -> State ExtLibState (a, ExtLibState)
withLocalState st = do
  s <- get
  modify refreshLocal
  st' <- st
  newS <- get
  modify (returnLocal s)
  return (st', newS)

isConstructor :: MethodInfo -> Bool
isConstructor CI {} = True
isConstructor _ = False

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