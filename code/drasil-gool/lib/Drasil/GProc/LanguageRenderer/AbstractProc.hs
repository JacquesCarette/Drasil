{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Drasil.GProc.LanguageRenderer.AbstractProc (fileDoc, fileFromData,
  buildModule, docMod, modFromData, innerType, arrayElem, listAppend,
  listAdd, funcDecDef, function
) where

import Drasil.Shared.InterfaceCommon (Label, MSBody, SValue, SVariable,
  VariableElim(variableName, variableType), VisibilitySym(..), funcApp,
  getCodeType, convType, StatementSym, ValueExpression, IndexTranslator)
import qualified Drasil.Shared.InterfaceCommon as IC
import Drasil.GProc.InterfaceProc (File, SFile, Module, FSModule)
import qualified Drasil.Shared.RendererClassesCommon as RC
import qualified Drasil.GProc.RendererClassesProc as RP
import Drasil.Shared.AST (isSource, ScopeData, TypeData, ParamData)
import Drasil.Shared.Helpers (vibcat, toState, emptyIfEmpty, getInnerType,
  onStateValue)
import Drasil.Shared.LanguageRenderer (addExt)
import qualified Drasil.Shared.LanguageRenderer.CommonPseudoOO as CP (modDoc')
import Drasil.Shared.LanguageRenderer.Constructors (mkStmtNoEnd, mkStateVar)
import Drasil.Shared.State (MS, VS, FS, lensFStoGS, lensFStoMS, lensMStoVS,
  getModuleName, setModuleName, setMainMod, currFileType, currMain, addFile,
  useVarName, currParameters, setVarScope)

import Prelude hiding ((<>))
import Control.Monad.State (get, modify)
import Control.Lens ((^.), over)
import qualified Control.Lens as L (set)
import Control.Lens.Zoom (zoom)
import Text.PrettyPrint.HughesPJ (Doc, isEmpty, brackets, (<>), render)

-- Files --

fileDoc :: (RP.RenderFile r) => String -> FSModule r -> SFile r
fileDoc ext md = do
  m <- md
  nm <- getModuleName
  let fp = addExt ext nm
  RP.fileFromData fp (toState m)

fileFromData
  :: (RP.ModuleElim r)
  => (FilePath -> r Module -> r File) -> FilePath -> FSModule r -> SFile r
fileFromData f fpath mdl' = do
  -- Add this file to list of files as long as it is not empty
  mdl <- mdl'
  modify (\s -> if isEmpty (RP.module' mdl)
    then s
    else over lensFStoGS (addFile (s ^. currFileType) fpath) $
      -- If this is the main source file, set it as the main module in the state
      if s ^. currMain && isSource (s ^. currFileType)
        then over lensFStoGS (setMainMod fpath) s
        else s)
  return $ f fpath mdl

-- Parameters: Module name, Doc for imports, Doc to put at bottom of module,
-- methods
buildModule
  :: (RC.MethodElim r md, RP.RenderMod r)
  => Label -> FS Doc -> FS Doc -> [MS (r md)] -> FSModule r
buildModule n imps bot fs = RP.modFromData n (do
  fns <- mapM (zoom lensFStoMS) fs
  is <- imps
  bt <- bot
  let fnDocs = vibcat (map RC.method fns ++ [bt])
  return $ emptyIfEmpty fnDocs (vibcat (filter (not . isEmpty) [is, fnDocs])))

docMod
  :: (RP.RenderFile r)
  => String -> String -> String -> [String] -> String -> SFile r -> SFile r
docMod e d wm a dt fl = RP.commentedMod fl
  (RC.docComment $ CP.modDoc' d wm a dt . addExt e <$> getModuleName)

modFromData :: Label -> (Doc -> r Module) -> FS Doc -> FSModule r
modFromData n f d = modify (setModuleName n) >> onStateValue f d

-- Lists and Arrays --

innerType :: (IC.TypeElim r) => VS (r TypeData) -> VS (r TypeData)
innerType t = t >>= (convType . getInnerType . getCodeType)

-- | Call to append a value to a list using a function call
listAppend
  :: (StatementSym r smt, ValueExpression r)
  => String -> SValue r -> SValue r -> MS (r smt)
listAppend fnName list val = IC.valStmt $
  funcApp fnName IC.void [list, val]

-- | Call to insert a value into a list as a function call
listAdd
  :: (IndexTranslator r, StatementSym r smt, ValueExpression r)
  => String -> SValue r -> SValue r -> SValue r -> MS (r smt)
listAdd fnName list idx val = IC.valStmt $
  funcApp fnName IC.void [list, IC.intToIndex idx, val]

arrayElem
  :: (IndexTranslator r, RC.RenderVariable r, IC.TypeElim r, RC.ValueElim r)
  => SValue r -> SValue r -> SVariable r
arrayElem arr' i' = do
  i <- IC.intToIndex i'
  arr <- arr'
  let vName = render $ RC.value arr
      vType = innerType $ return $ IC.valueType arr
      vRender = RC.value arr <> brackets (RC.value i)
  mkStateVar vName vType vRender

funcDecDef :: (RP.ProcRenderSym r vis smt md) => SVariable r -> r ScopeData ->
  [SVariable r] -> MSBody r -> MS (r smt)
funcDecDef v scp ps b = do
  vr <- zoom lensMStoVS v
  modify $ useVarName $ variableName vr
  modify $ setVarScope (variableName vr) (RC.scopeData scp)
  s <- get
  f <- IC.function (variableName vr) private (return $ variableType vr)
    (map IC.param ps) b
  modify (L.set currParameters (s ^. currParameters))
  mkStmtNoEnd $ RC.method f

function :: (RP.ProcRenderMethod r vis md) => Label -> r vis -> VS (r TypeData) ->
  [MS (r ParamData)] -> MSBody r -> MS (r md)
function n s t = RP.intFunc False n s (RC.mType t)
