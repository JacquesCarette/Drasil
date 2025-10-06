{-# LANGUAGE PostfixOperators #-}

module Drasil.GProc.LanguageRenderer.AbstractProc (fileDoc, fileFromData,
  buildModule, docMod, modFromData, listInnerType, arrayElem, funcDecDef,
  function
) where

import Drasil.Shared.InterfaceCommon (Label,
  VariableElim(variableName, variableType),
  VisibilitySym(..), getType, convType, ScopeSym(Scope), MethodSym(Method),
  VariableSym(Variable), TypeSym(Type), ValueSym(Value),
  StatementSym(Statement), ParameterSym(Parameter), BodySym(Body))
import qualified Drasil.Shared.InterfaceCommon as IC (MethodSym(function),
  List(intToIndex), ParameterSym(param))
import Drasil.GProc.InterfaceProc (FileSym (File),
  ModuleSym(Module))
import qualified Drasil.Shared.RendererClassesCommon as RCC (MethodElim(..),
  BlockCommentSym(..), ValueElim(value), InternalVarElim(variable),
  MethodTypeSym(mType), ScopeElim(scopeData))
import Drasil.GProc.RendererClassesProc (ProcRenderSym)
import qualified Drasil.GProc.RendererClassesProc as RCP (RenderFile(..),
  ModuleElim(..), RenderMod(..), ProcRenderMethod(intFunc))
import Drasil.Shared.AST (isSource)
import Drasil.Shared.Helpers (vibcat, toState, emptyIfEmpty, getInnerType,
  onStateValue)
import Drasil.Shared.LanguageRenderer (addExt)
import qualified Drasil.Shared.LanguageRenderer.CommonPseudoOO as CP (modDoc')
import Drasil.Shared.LanguageRenderer.Constructors (mkStmtNoEnd, mkStateVar)

import Prelude hiding ((<>))
import Control.Monad.State (get, modify)
import Control.Lens ((^.), over)
import qualified Control.Lens as L (set)
import Control.Lens.Zoom (zoom)
import Text.PrettyPrint.HughesPJ (Doc, render, isEmpty, brackets, (<>))

-- Files --

fileDoc :: (ProcRenderSym r) => String -> Module r -> File r
fileDoc ext md = undefined {-do
  m <- md
  nm <- getModuleName
  let fp = addExt ext nm
  RCP.fileFromData fp (toState m)-}

fileFromData :: (ProcRenderSym r) => (FilePath -> Module r -> File r)
  -> FilePath -> Module r -> File r
fileFromData f fpath mdl' = undefined{-do
  -- Add this file to list of files as long as it is not empty
  mdl <- mdl'
  modify (\s -> if isEmpty (RCP.module' mdl) 
    then s
    else over lensFStoGS (addFile (s ^. currFileType) fpath) $ 
      -- If this is the main source file, set it as the main module in the state
      if s ^. currMain && isSource (s ^. currFileType) 
        then over lensFStoGS (setMainMod fpath) s
        else s)
  return $ f fpath mdl-}

-- Parameters: Module name, Doc for imports, Doc to put at bottom of module,
-- methods
buildModule :: (ProcRenderSym r) => Label -> Doc -> Doc -> [Method r]
  -> Module r
buildModule n imps bot fns =
  let fnDocs = vibcat (map RCC.method fns ++ [bot])
      doc    = emptyIfEmpty fnDocs (vibcat (filter (not . isEmpty) [imps, fnDocs]))
  in  RCP.modFromData n doc

docMod :: (ProcRenderSym r) => String -> String -> [String] -> String ->
  File r -> File r
docMod e d a dt fl = undefined{-RCP.commentedMod fl (RCC.docComment $ CP.modDoc' d a dt . 
  addExt e <$> getModuleName)-}

modFromData :: Label -> (Doc -> Module r) -> Doc -> Module r
modFromData n f d = undefined{-modify (setModuleName n) >> onStateValue f d-}

listInnerType :: (ProcRenderSym r) => Type r -> Type r
listInnerType = convType . getInnerType . getType

arrayElem :: (ProcRenderSym r) => Value r -> Variable r -> Variable r
arrayElem i' v =
  let i = IC.intToIndex i'
      vName = variableName v ++ "[" ++ render (RCC.value i) ++ "]"
      vType = listInnerType $ variableType v
      vRender = RCC.variable v <> brackets (RCC.value i)
  in mkStateVar vName vType vRender

funcDecDef :: (ProcRenderSym r) => Variable r -> Scope r -> [Variable r]
  -> Body r -> Statement r
funcDecDef v scp ps b = undefined{-} do
  vr <- zoom lensMStoVS v
  modify $ useVarName $ variableName vr
  modify $ setVarScope (variableName vr) (RCC.scopeData scp)
  s <- get
  f <- IC.function (variableName vr) private (return $ variableType vr)
    (map IC.param ps) b
  modify (L.set currParameters (s ^. currParameters))
  mkStmtNoEnd $ RCC.method f-}

function :: (ProcRenderSym r) => Label -> Visibility r -> Type r ->
  [Parameter r] -> Body r -> Method r
function n s t = RCP.intFunc False n s (RCC.mType t)
