module Language.Drasil.Code.Imperative.GenerateGOOL (ClassType(..),
  genModuleWithImports, genModuleWithImportsProc, genModule, genModuleProc,
  genDoxConfig, genReadMe, primaryClass, auxClass, fApp, fAppProc, ctorCall,
  fAppInOut, fAppInOutProc
) where

import Data.Bifunctor (second)
import qualified Data.Map as Map (lookup)
import Data.Maybe (catMaybes)
import Control.Monad.State (get, modify)
import Control.Lens ((^.))

import Language.Drasil hiding (List)
import Language.Drasil.Code.Imperative.DrasilState (GenState, DrasilState(..))
import Language.Drasil.Code.Imperative.GOOL.ClassInterface (AuxiliarySym(..))
import Language.Drasil.Code.Imperative.ReadMe.Import (ReadMeInfo(..))
import Language.Drasil.Choices (Comments(..), AuxFile(..))
import Language.Drasil.CodeSpec (HasOldCodeSpec(..))
import Language.Drasil.Mod (Name, Description, Import)

import Drasil.GOOL (VSType, SVariable, SValue, MSStatement, SMethod,
  CSStateVar, SClass, NamedArgs, SharedProg, OOProg, TypeElim(..),
  ValueSym(..), Argument(..), ValueExpression(..), OOValueExpression(..),
  FuncAppStatement(..), OOFuncAppStatement(..), ClassSym(..), CodeType(..),
  GOOLState)
import qualified Drasil.GOOL as OO (SFile, FileSym(..), ModuleSym(..))

import Drasil.GProc (ProcProg)
import qualified Drasil.GProc as Proc (SFile, FileSym(..), ModuleSym(..))

-- | Defines a GOOL module. If the user chose 'CommentMod', the module will have
-- Doxygen comments. If the user did not choose 'CommentMod' but did choose
-- 'CommentFunc', a module-level Doxygen comment is still created, though it only
-- documents the file name, because without this Doxygen will not find the
-- function-level comments in the file.
genModuleWithImports :: (OOProg r) => Name -> Description -> [Import] ->
  [GenState (Maybe (SMethod r))] -> [GenState (Maybe (SClass r))] ->
  GenState (OO.SFile r)
genModuleWithImports n desc is maybeMs maybeCs = do
  g <- get
  modify (\s -> s { currentModule = n })
  let as = map name (codeSpec g ^. authorsO )
  cs <- sequence maybeCs
  ms <- sequence maybeMs
  let commMod | CommentMod `elem` commented g                   = OO.docMod desc
                  as (date g)
              | CommentFunc `elem` commented g && not (null ms) = OO.docMod "" []
                  ""
              | otherwise                                       = id
  return $ commMod $ OO.fileDoc $ OO.buildModule n is (catMaybes ms) (catMaybes cs)

-- | Generates a module for when imports do not need to be explicitly stated.
genModule :: (OOProg r) => Name -> Description ->
  [GenState (Maybe (SMethod r))] -> [GenState (Maybe (SClass r))] ->
  GenState (OO.SFile r)
genModule n desc = genModuleWithImports n desc []

-- | Generates a Doxygen configuration file if the user has comments enabled.
genDoxConfig :: (AuxiliarySym r) => GOOLState -> GenState (Maybe (r (Auxiliary r)))
genDoxConfig s = do
  g <- get
  let n = codeSpec g ^. pNameO
      cms = commented g
      v = doxOutput g
  return $ if not (null cms) then Just (doxConfig n s v) else Nothing

-- | Generates a README file.
genReadMe :: (AuxiliarySym r) => ReadMeInfo -> GenState (Maybe (r (Auxiliary r)))
genReadMe rmi = do
  g <- get
  let n = codeSpec g ^. pNameO
  return $ getReadMe (auxiliaries g) rmi {caseName = n}

-- | Helper for generating a README file.
getReadMe :: (AuxiliarySym r) => [AuxFile] -> ReadMeInfo -> Maybe (r (Auxiliary r))
getReadMe auxl rmi = if ReadME `elem` auxl then Just (readMe rmi) else Nothing

data ClassType = Primary | Auxiliary

-- | Generates a primary or auxiliary class with the given name, description,
-- state variables, and methods. The 'Maybe' 'Name' parameter is the name of the
-- interface the class implements, if applicable.
mkClass :: (OOProg r) => ClassType -> Name -> Maybe Name -> Description ->
  [CSStateVar r] -> GenState [SMethod r] -> GenState [SMethod r] ->
  GenState (SClass r)
mkClass s n l desc vs cstrs mths = do
  g <- get
  modify (\ds -> ds {currentClass = n})
  cs <- cstrs
  ms <- mths
  modify (\ds -> ds {currentClass = ""})
  let getFunc Primary = getFunc' l
      getFunc Auxiliary = extraClass n Nothing
      getFunc' Nothing = buildClass Nothing
      getFunc' (Just intfc) = implementingClass n [intfc]
      c = getFunc s vs cs ms
  return $ if CommentClass `elem` commented g
    then docClass desc c
    else c

-- | Generates a primary class.
primaryClass :: (OOProg r) => Name -> Maybe Name -> Description ->
  [CSStateVar r] -> GenState [SMethod r] -> GenState [SMethod r] ->
  GenState (SClass r)
primaryClass = mkClass Primary

-- | Generates an auxiliary class (for when a module contains multiple classes).
auxClass :: (OOProg r) => Name -> Maybe Name -> Description ->
  [CSStateVar r] -> GenState [SMethod r] -> GenState [SMethod r] ->
  GenState (SClass r)
auxClass = mkClass Auxiliary

-- | Converts lists or objects to pointer arguments, since we use pointerParam
-- for list or object-type parameters.
mkArg :: (SharedProg r) => SValue r -> SValue r
mkArg v = do
  vl <- v
  let mkArg' (List _) = pointerArg
      mkArg' (Object _) = pointerArg
      mkArg' _ = id
  mkArg' (getType $ valueType vl) (return vl)

-- | Gets the current module and calls mkArg on the arguments.
-- Called by more specific function call generators ('fApp' and 'ctorCall').
fCall :: (SharedProg r) => (Name -> [SValue r] -> NamedArgs r -> SValue r) ->
  [SValue r] -> NamedArgs r -> GenState (SValue r)
fCall f vl ns = do
  g <- get
  let cm = currentModule g
      args = map mkArg vl
      nargs = map (second mkArg) ns
  return $ f cm args nargs

-- | Function call generator.
-- The first parameter (@m@) is the module where the function is defined.
-- If @m@ is not the current module, use GOOL's function for calling functions from
--   external modules.
-- If @m@ is the current module and the function is in export map, use GOOL's basic
--   function for function applications.
-- If @m@ is the current module and function is not exported, use GOOL's function for
--   calling a method on self. This assumes all private methods are dynamic,
--   which is true for this generator.
fApp :: (OOProg r) => Name -> Name -> VSType r -> [SValue r] ->
  NamedArgs r -> GenState (SValue r)
fApp m s t vl ns = do
  g <- get
  fCall (\cm args nargs ->
    if m /= cm then extFuncAppMixedArgs m s t args nargs else
      if Map.lookup s (eMap g) == Just cm then funcAppMixedArgs s t args nargs
      else selfFuncAppMixedArgs s t args nargs) vl ns

-- | Logic similar to 'fApp', but the self case is not required here
-- (because constructor will never be private). Calls 'newObjMixedArgs'.
ctorCall :: (OOProg r) => Name -> VSType r -> [SValue r] -> NamedArgs r
  -> GenState (SValue r)
ctorCall m t = fCall (\cm args nargs -> if m /= cm then
  extNewObjMixedArgs m t args nargs else newObjMixedArgs t args nargs)

-- | Logic similar to 'fApp', but for In/Out calls.
fAppInOut :: (OOProg r) => Name -> Name -> [SValue r] ->
  [SVariable r] -> [SVariable r] -> GenState (MSStatement r)
fAppInOut m n ins outs both = do
  g <- get
  let cm = currentModule g
  return $ if m /= cm then extInOutCall m n ins outs both else if Map.lookup n
    (eMap g) == Just cm then inOutCall n ins outs both else
    selfInOutCall n ins outs both

-- Procedural Versions --

-- | Defines a GOOL module. If the user chose 'CommentMod', the module will have
-- Doxygen comments. If the user did not choose 'CommentMod' but did choose
-- 'CommentFunc', a module-level Doxygen comment is still created, though it only
-- documents the file name, because without this Doxygen will not find the
-- function-level comments in the file.
genModuleWithImportsProc :: (ProcProg r) => Name -> Description -> [Import] ->
  [GenState (Maybe (SMethod r))] -> GenState (Proc.SFile r)
genModuleWithImportsProc n desc is maybeMs = do
  g <- get
  modify (\s -> s { currentModule = n })
  let as = map name (codeSpec g ^. authorsO )
  ms <- sequence maybeMs
  let commMod | CommentMod `elem` commented g                   = Proc.docMod desc
                  as (date g)
              | CommentFunc `elem` commented g && not (null ms) = Proc.docMod "" []
                  ""
              | otherwise                                       = id
  return $ commMod $ Proc.fileDoc $ Proc.buildModule n is (catMaybes ms)

-- | Generates a module for when imports do not need to be explicitly stated.
genModuleProc :: (ProcProg r) => Name -> Description ->
  [GenState (Maybe (SMethod r))] -> GenState (Proc.SFile r)
genModuleProc n desc = genModuleWithImportsProc n desc []

-- | Function call generator.
-- The first parameter (@m@) is the module where the function is defined.
-- If @m@ is not the current module, use GOOL's function for calling functions from
--   external modules.
-- If @m@ is the current module and the function is in export map, use GOOL's basic
--   function for function applications.
-- If @m@ is the current module and function is not exported, use GOOL's function for
--   calling a method on self. This assumes all private methods are dynamic,
--   which is true for this generator.
fAppProc :: (SharedProg r) => Name -> Name -> VSType r -> [SValue r] ->
  NamedArgs r -> GenState (SValue r)
fAppProc m s t vl ns = do
  g <- get
  fCall (\cm args nargs ->
    if m /= cm then extFuncAppMixedArgs m s t args nargs else
      if Map.lookup s (eMap g) == Just cm then funcAppMixedArgs s t args nargs
      else error "fAppProc: Procedural languages do not support method calls.") vl ns

-- | Logic similar to 'fApp', but for In/Out calls.
fAppInOutProc :: (SharedProg r) => Name -> Name -> [SValue r] ->
  [SVariable r] -> [SVariable r] -> GenState (MSStatement r)
fAppInOutProc m n ins outs both = do
  g <- get
  let cm = currentModule g
  return $ if m /= cm then extInOutCall m n ins outs both else if Map.lookup n
    (eMap g) == Just cm then inOutCall n ins outs both
    else error "fAppInOutProc: Procedural languages do not support method calls."
