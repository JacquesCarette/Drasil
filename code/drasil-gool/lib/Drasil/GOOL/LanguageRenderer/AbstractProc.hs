{-# LANGUAGE PostfixOperators #-}

module Drasil.GOOL.LanguageRenderer.AbstractProc (fileDoc, fileFromData,
  buildModule, docMod, listInnerType, arrayElem, funcDecDef, function
) where

import Drasil.GOOL.InterfaceCommon (Label, SMethod, MSBody, MSStatement, SValue,
  SVariable, MSParameter, VSType, VariableElim(variableName, variableType),
  VisibilitySym(..), getType)
import qualified Drasil.GOOL.InterfaceCommon as IC (MethodSym(function),
  List(intToIndex), ParameterSym(param))
import Drasil.GOOL.InterfaceGOOL (SFile, FSModule, FileSym (File),
  ModuleSym(Module), PermanenceSym(static), convTypeOO)
import qualified Drasil.GOOL.RendererClassesCommon as RCC (MethodElim(..),
  BlockCommentSym(..), ValueElim(value), InternalVarElim(variable),
  MethodTypeSym(mType))
import Drasil.GOOL.RendererClassesOO (OORenderSym)
import qualified Drasil.GOOL.RendererClassesOO as RCO (RenderFile(..),
  ModuleElim(..), RenderMod(..), OORenderMethod(intFunc))
import Drasil.GOOL.AST (isSource)
import Drasil.GOOL.Helpers (vibcat, toState, emptyIfEmpty, getInnerType)
import Drasil.GOOL.LanguageRenderer (addExt)
import qualified Drasil.GOOL.LanguageRenderer.CommonPseudoOO as CP (modDoc')
import Drasil.GOOL.LanguageRenderer.Constructors (mkStmtNoEnd, mkStateVar)
import Drasil.GOOL.State (FS, lensFStoGS, lensFStoMS, lensMStoVS, getModuleName,
  setMainMod, currFileType, currMain, addFile, useVarName, currParameters)

import Prelude hiding ((<>))
import Control.Monad.State (get, modify)
import Control.Lens ((^.), over)
import qualified Control.Lens as L (set)
import Control.Lens.Zoom (zoom)
import Text.PrettyPrint.HughesPJ (Doc, render, isEmpty, brackets, (<>))

-- Files --

fileDoc :: (OORenderSym r) => String -> FSModule r -> SFile r
fileDoc ext md = do
  m <- md
  nm <- getModuleName
  let fp = addExt ext nm
  RCO.fileFromData fp (toState m)

fileFromData :: (OORenderSym r) => (FilePath -> r (Module r) -> r (File r)) 
  -> FilePath -> FSModule r -> SFile r
fileFromData f fpath mdl' = do
  -- Add this file to list of files as long as it is not empty
  mdl <- mdl'
  modify (\s -> if isEmpty (RCO.module' mdl) 
    then s
    else over lensFStoGS (addFile (s ^. currFileType) fpath) $ 
      -- If this is the main source file, set it as the main module in the state
      if s ^. currMain && isSource (s ^. currFileType) 
        then over lensFStoGS (setMainMod fpath) s
        else s)
  return $ f fpath mdl

-- Parameters: Module name, Doc for imports, Doc to put at bottom of module,
-- methods
buildModule :: (OORenderSym r) => Label -> FS Doc -> FS Doc -> [SMethod r]
  -> FSModule r
buildModule n imps bot fs = RCO.modFromData n (do
  fns <- mapM (zoom lensFStoMS) fs
  is <- imps
  bt <- bot
  let fnDocs = vibcat (map RCC.method fns ++ [bt])
  return $ emptyIfEmpty fnDocs (vibcat (filter (not . isEmpty) [is, fnDocs])))

docMod :: (OORenderSym r) => String -> String -> [String] -> String -> 
  SFile r -> SFile r
docMod e d a dt fl = RCO.commentedMod fl (RCC.docComment $ CP.modDoc' d a dt . 
  addExt e <$> getModuleName)

listInnerType :: (OORenderSym r) => VSType r -> VSType r
listInnerType t = t >>= (convTypeOO . getInnerType . getType)

arrayElem :: (OORenderSym r) => SValue r -> SVariable r -> SVariable r
arrayElem i' v' = do
  i <- IC.intToIndex i'
  v <- v'
  let vName = variableName v ++ "[" ++ render (RCC.value i) ++ "]"
      vType = listInnerType $ return $ variableType v
      vRender = RCC.variable v <> brackets (RCC.value i)
  mkStateVar vName vType vRender

funcDecDef :: (OORenderSym r) => SVariable r -> [SVariable r] -> MSBody r ->
  MSStatement r
funcDecDef v ps b = do
  vr <- zoom lensMStoVS v
  modify $ useVarName $ variableName vr
  s <- get
  f <- IC.function (variableName vr) private (return $ variableType vr) 
    (map IC.param ps) b
  modify (L.set currParameters (s ^. currParameters))
  mkStmtNoEnd $ RCC.method f

function :: (OORenderSym r) => Label -> r (Visibility r) -> VSType r -> 
  [MSParameter r] -> MSBody r -> SMethod r
function n s t = RCO.intFunc False n s static (RCC.mType t)
