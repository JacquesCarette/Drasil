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

import Drasil.FileHandling (FileLayout)
import Drasil.GProc (ProcProg)
import qualified Drasil.GProc as Proc (FileSym(..), ModuleSym(..))
import Language.Drasil hiding (List)
import Language.Drasil.Code.Imperative.DrasilState (GenState, DrasilState(..),
  getDoxOutput, getSoftwareDossierFiles, HasChoices(..))
import Language.Drasil.SoftwareDossier.SoftwareDossierSym (SoftwareDossierSym(..),
  SoftwareDossierState)
import Language.Drasil.Code.Imperative.README.Core (ReadMeInfo(..))
import Language.Drasil.Choices (Comments(..), SoftwareDossierFile(..))
import Language.Drasil.Mod (Name, Description, Import)
import Drasil.Metadata (watermark)
import Drasil.System (HasSystemMeta(..))
import Drasil.SRS (HasSmithEtAlSRS(..))

import Drasil.GOOL (SVariable, SValue, Class, CSStateVar, NamedArgs, OOProg, CS,
  FS, MS, VS, TypeData, ValueSym(..), Argument(..), ValueExpression(..),
  InternalValueExp, OOValueExpression(..), SelfSym(..), VariableValue(..),
  FuncAppStatement(..), OOFuncAppStatement(..), ClassSym(..), CodeType(..),
  TypeElim(..), objMethodCallMixedArgs)
import qualified Drasil.GOOL as OO (FileSym(..), ModuleSym(..))

-- | Defines a GOOL module. If the user chose 'CommentMod', the module will have
-- Doxygen comments. If the user did not choose 'CommentMod' but did choose
-- 'CommentFunc', a module-level Doxygen comment is still created, though it only
-- documents the file name, because without this Doxygen will not find the
-- function-level comments in the file.
genModuleWithImports
  :: (OOProg r vis stmt mthd stvr attch prg file mod bod block)
  => Name
  -> Description
  -> [Import]
  -> [GenState (Maybe (MS (r mthd)))]
  -> [GenState (Maybe (CS (r Class)))]
  -> GenState (FS (r file))
genModuleWithImports n desc is maybeMs maybeCs = do
  g <- get
  modify (\s -> s { currentModule = n })
  let as = map fullName (g ^. authors)
  cs <- sequence maybeCs
  ms <- sequence maybeMs
  let commMod | CommentMod `elem` g ^. commented                   = OO.docMod desc watermark as (g ^. date)
              | CommentFunc `elem` g ^. commented && not (null ms) = OO.docMod "" watermark [] ""
              | otherwise                                          = id
  return $ commMod $ OO.fileDoc $ OO.buildModule n is (catMaybes ms) (catMaybes cs)

-- | Generates a module for when imports do not need to be explicitly stated.
genModule
  :: (OOProg r vis stmt mthd stvr attch prg file mod bod block)
  => Name
  -> Description
  -> [GenState (Maybe (MS (r mthd)))]
  -> [GenState (Maybe (CS (r Class)))]
  -> GenState (FS (r file))
genModule n desc = genModuleWithImports n desc []

-- | Generates a Doxygen configuration file if the user has comments enabled.
genDoxConfig :: (SoftwareDossierSym r) => SoftwareDossierState ->
  GenState (Maybe (r FileLayout))
genDoxConfig s = do
  g <- get
  let n = g ^. programName
      cms = g ^. commented
      v = getDoxOutput g
  return $ if not (null cms) then doxConfig n s v else Nothing

-- | Generates a README file.
genReadMe :: (SoftwareDossierSym r) => ReadMeInfo -> GenState (Maybe (r FileLayout))
genReadMe rmi = do
  g <- get
  let n = g ^. programName
  return $ getReadMe (getSoftwareDossierFiles g) rmi {caseName = n}

-- | Helper for generating a README file.
getReadMe :: (SoftwareDossierSym r) => [SoftwareDossierFile] -> ReadMeInfo -> Maybe (r FileLayout)
getReadMe auxl rmi = if ReadME `elem` auxl then Just (readMe rmi) else Nothing

data ClassType = Primary | Auxiliary

-- | Generates a primary or auxiliary class with the given name, description,
-- state variables, and methods. The 'Maybe' 'Name' parameter is the name of the
-- interface the class implements, if applicable.
mkClass :: (ClassSym r vis mthd stvr attch) => ClassType -> Name -> Maybe Name ->
  Description -> [CSStateVar r stvr] -> GenState [MS (r mthd)] ->
    GenState [MS (r mthd)] -> GenState (CS (r Class))
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
  return $ if CommentClass `elem` g ^. commented
    then docClass desc c
    else c

-- | Generates a primary class.
primaryClass :: (ClassSym r vis mthd stvr attch) => Name -> Maybe Name -> Description ->
  [CSStateVar r stvr] -> GenState [MS (r mthd)] -> GenState [MS (r mthd)] ->
  GenState (CS (r Class))
primaryClass = mkClass Primary

-- | Generates an auxiliary class (for when a module contains multiple classes).
auxClass :: (ClassSym r vis mthd stvr attch) => Name -> Maybe Name -> Description ->
  [CSStateVar r stvr] -> GenState [MS (r mthd)] -> GenState [MS (r mthd)] ->
  GenState (CS (r Class))
auxClass = mkClass Auxiliary

-- | Converts lists or objects to pointer arguments, since we use pointerParam
-- for list or object-type parameters.
mkArg :: (Argument r, TypeElim r) => SValue r -> SValue r
mkArg v = do
  vl <- v
  let mkArg' (List _) = pointerArg
      mkArg' (Object _) = pointerArg
      mkArg' _ = id
  mkArg' (getCodeType $ valueType vl) (return vl)

-- | Gets the current module and calls mkArg on the arguments.
-- Called by more specific function call generators ('fApp' and 'ctorCall').
fCall :: (Argument r, TypeElim r) => (Name -> [SValue r] -> NamedArgs r ->
  SValue r) -> [SValue r] -> NamedArgs r -> GenState (SValue r)
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
fApp
  ::
    ( Argument r
    , VariableValue r
    , SelfSym r
    , InternalValueExp r
    , ValueExpression r
    , TypeElim r
    )
  => Name -> Name -> VS (r TypeData) -> [SValue r] -> NamedArgs r -> GenState (SValue r)
fApp m s t vl ns = do
  g <- get
  fCall (\cm args nargs ->
    if m /= cm then extFuncAppMixedArgs m s t args nargs else
      if Map.lookup s (eMap g) == Just cm then funcAppMixedArgs s t args nargs
      else objMethodCallMixedArgs t (valueOf self) s args nargs) vl ns

-- | Logic similar to 'fApp', but the self case is not required here
-- (because constructor will never be private). Calls 'newObjMixedArgs'.
ctorCall
  :: (Argument r, OOValueExpression r, TypeElim r)
  => Name -> VS (r TypeData) -> [SValue r] -> NamedArgs r -> GenState (SValue r)
ctorCall m t = fCall (\cm args nargs -> if m /= cm then
  extNewObjMixedArgs m t args nargs else newObjMixedArgs t args nargs)

-- | Logic similar to 'fApp', but for In/Out calls.
fAppInOut :: (OOFuncAppStatement r stmt) => Name -> Name -> [SValue r] ->
  [SVariable r] -> [SVariable r] -> GenState (MS (r stmt))
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
genModuleWithImportsProc
  :: (ProcProg r vis stmt mthd prg file mod bod block)
  => Name
  -> Description
  -> [Import]
  -> [GenState (Maybe (MS (r mthd)))]
  -> GenState (FS (r file))
genModuleWithImportsProc n desc is maybeMs = do
  g <- get
  modify (\s -> s { currentModule = n })
  let as = map fullName (g ^. authors)
  ms <- sequence maybeMs
  let commMod | CommentMod `elem` g ^. commented                   = Proc.docMod desc watermark as (g ^. date)
              | CommentFunc `elem` g ^. commented && not (null ms) = Proc.docMod "" watermark [] ""
              | otherwise                                          = id
  return $ commMod $ Proc.fileDoc $ Proc.buildModule n is (catMaybes ms)

-- | Generates a module for when imports do not need to be explicitly stated.
genModuleProc
  :: (ProcProg r vis stmt mthd prg file mod bod block)
  => Name
  -> Description
  -> [GenState (Maybe (MS (r mthd)))]
  -> GenState (FS (r file))
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
fAppProc
  :: (Argument r, TypeElim r, ValueExpression r) => Name
  -> Name
  -> VS (r TypeData)
  -> [SValue r]
  -> NamedArgs r
  -> GenState (SValue r)
fAppProc m s t vl ns = do
  g <- get
  fCall (\cm args nargs ->
    if m /= cm then extFuncAppMixedArgs m s t args nargs else
      if Map.lookup s (eMap g) == Just cm then funcAppMixedArgs s t args nargs
      else error "fAppProc: Procedural languages do not support method calls.") vl ns

-- | Logic similar to 'fApp', but for In/Out calls.
fAppInOutProc :: (FuncAppStatement r stmt) => Name -> Name -> [SValue r] ->
  [SVariable r] -> [SVariable r] -> GenState (MS (r stmt))
fAppInOutProc m n ins outs both = do
  g <- get
  let cm = currentModule g
  return $ if m /= cm then extInOutCall m n ins outs both else if Map.lookup n
    (eMap g) == Just cm then inOutCall n ins outs both
    else error "fAppInOutProc: Procedural languages do not support method calls."
