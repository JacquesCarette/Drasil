{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Drasil.GProc.LanguageRenderer.AbstractProc (fileDoc, fileFromData,
  buildModule, docMod, modFromData, innerType, arrayElem, listAppend,
  listAdd, funcDecDef, function
) where

import Drasil.Shared.InterfaceCommon (UnRepr(..), Label, SMethod, MSBody,
  MSStatement, SValue, SVariable, MSParameter, VSType,
  VariableElim(variableName, variableType), VisibilitySym(..), funcApp,
  getCodeType, convType)
import qualified Drasil.Shared.InterfaceCommon as IC
import Drasil.GProc.InterfaceProc (SFile, FSModule, FileSym (File),
  ModuleSym(Module))
import Drasil.Shared.RendererClassesCommon (CommonRenderSym)
import qualified Drasil.Shared.RendererClassesCommon as RCC (MethodElim(..),
  BlockCommentSym(..), ValueElim(value), InternalVarElim(variable),
  MethodTypeSym(mType), ScopeElim(scopeData))
import Drasil.GProc.RendererClassesProc (ProcRenderSym)
import qualified Drasil.GProc.RendererClassesProc as RCP (RenderFile(..),
  ModuleElim(..), RenderMod(..), ProcRenderMethod(intFunc))
import Drasil.Shared.AST (isSource, ScopeData, TypeData)
import Drasil.Shared.Helpers (vibcat, toState, emptyIfEmpty, getInnerType,
  onStateValue)
import Drasil.Shared.LanguageRenderer (addExt)
import qualified Drasil.Shared.LanguageRenderer.CommonPseudoOO as CP (modDoc')
import Drasil.Shared.LanguageRenderer.Constructors (mkStmtNoEnd, mkStateVar)
import Drasil.Shared.State (FS, lensFStoGS, lensFStoMS, lensMStoVS, getModuleName,
  setModuleName, setMainMod, currFileType, currMain, addFile, useVarName,
  currParameters, setVarScope)

import Prelude hiding ((<>))
import Control.Monad.State (get, modify)
import Control.Lens ((^.), over)
import qualified Control.Lens as L (set)
import Control.Lens.Zoom (zoom)
import Text.PrettyPrint.HughesPJ (Doc, isEmpty, brackets, (<>))

-- Files --

fileDoc :: (ProcRenderSym r) => String -> FSModule r -> SFile r
fileDoc ext md = do
  m <- md
  nm <- getModuleName
  let fp = addExt ext nm
  RCP.fileFromData fp (toState m)

fileFromData :: (ProcRenderSym r) => (FilePath -> r (Module r) -> r (File r))
  -> FilePath -> FSModule r -> SFile r
fileFromData f fpath mdl' = do
  -- Add this file to list of files as long as it is not empty
  mdl <- mdl'
  modify (\s -> if isEmpty (RCP.module' mdl)
    then s
    else over lensFStoGS (addFile (s ^. currFileType) fpath) $
      -- If this is the main source file, set it as the main module in the state
      if s ^. currMain && isSource (s ^. currFileType)
        then over lensFStoGS (setMainMod fpath) s
        else s)
  return $ f fpath mdl

-- Parameters: Module name, Doc for imports, Doc to put at bottom of module,
-- methods
buildModule :: (ProcRenderSym r) => Label -> FS Doc -> FS Doc -> [SMethod r]
  -> FSModule r
buildModule n imps bot fs = RCP.modFromData n (do
  fns <- mapM (zoom lensFStoMS) fs
  is <- imps
  bt <- bot
  let fnDocs = vibcat (map RCC.method fns ++ [bt])
  return $ emptyIfEmpty fnDocs (vibcat (filter (not . isEmpty) [is, fnDocs])))

docMod :: (ProcRenderSym r) => String -> String -> String -> [String] -> String ->
  SFile r -> SFile r
docMod e d wm a dt fl = RCP.commentedMod fl (RCC.docComment $ CP.modDoc' d wm a dt .
  addExt e <$> getModuleName)

modFromData :: Label -> (Doc -> r (Module r)) -> FS Doc -> FSModule r
modFromData n f d = modify (setModuleName n) >> onStateValue f d

-- Lists and Arrays --

innerType :: (ProcRenderSym r, UnRepr r TypeData) => VSType r -> VSType r
innerType t = t >>= (convType . getInnerType . getCodeType)

-- | Call to append a value to a list using a function call
listAppend :: (CommonRenderSym r) => String -> SValue r -> SValue r -> SValue r
listAppend fnName list val = funcApp fnName IC.void [list, val]

-- | Call to insert a value into a list as a function call
listAdd :: (CommonRenderSym r) => String -> SValue r -> SValue r -> SValue r -> MSStatement r
listAdd fnName list idx val = IC.valStmt $ funcApp fnName IC.void [list, IC.intToIndex idx, val]

arrayElem :: (ProcRenderSym r, UnRepr r TypeData) => SValue r ->
  SVariable r -> SVariable r
arrayElem i' v' = do
  i <- IC.intToIndex i'
  v <- v'
  let vName = variableName v -- Slight hack; we used to add `++ "[" ++ render (RCC.value i) ++ "]"`
      vType = innerType $ return $ variableType v
      vRender = RCC.variable v <> brackets (RCC.value i)
  mkStateVar vName vType vRender

funcDecDef :: (ProcRenderSym r) => SVariable r -> r ScopeData -> [SVariable r]
  -> MSBody r -> MSStatement r
funcDecDef v scp ps b = do
  vr <- zoom lensMStoVS v
  modify $ useVarName $ variableName vr
  modify $ setVarScope (variableName vr) (RCC.scopeData scp)
  s <- get
  f <- IC.function (variableName vr) private (return $ variableType vr)
    (map IC.param ps) b
  modify (L.set currParameters (s ^. currParameters))
  mkStmtNoEnd $ RCC.method f

function :: (ProcRenderSym r) => Label -> r (Visibility r) -> VSType r ->
  [MSParameter r] -> MSBody r -> SMethod r
function n s t = RCP.intFunc False n s (RCC.mType t)
