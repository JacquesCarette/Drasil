{-# LANGUAGE FlexibleContexts #-}
-- | Contains common implementations specific to GOOL

module Drasil.GOOL.LanguageRenderer.CommonGOOL (
  constDecDef, classMethodCall, listAppend, listAdd, innerType
) where

import Drasil.Shared.InterfaceCommon (UnRepr(..), TypeElim(..), SVariable,
  SValue, NamedArgs, VariableElim(..), TypeSym(void), IndexTranslator(..),
  getCodeType, StatementSym (valStmt), StatementSym(..))
import Drasil.GOOL.InterfaceGOOL (objMethodCall, convTypeOO)
import Drasil.Shared.RendererClassesCommon (CommonRenderSym, ScopeElim(..),
  RenderValue(..))
import Drasil.GOOL.RendererClassesOO (OORenderSym)
import Drasil.Shared.LanguageRenderer.Constructors (mkStmt)
import Drasil.Shared.LanguageRenderer (dot)
import Drasil.GOOL.Renderers (renderType, renderConstDecDef)
import Drasil.Shared.AST (TypeData, ScopeData)
import Drasil.Shared.State (MS, VS, lensMStoVS, useVarName, setVarScope)
import Drasil.Shared.Helpers (getInnerType)

import Control.Lens.Zoom (zoom)
import Control.Monad.State (modify)

constDecDef :: (CommonRenderSym r TypeData vis smt, UnRepr r TypeData) =>
  SVariable r -> r ScopeData -> SValue r -> MS (r smt)
constDecDef vr' scp v'= do
  vr <- zoom lensMStoVS vr'
  v <- zoom lensMStoVS v'
  modify $ useVarName $ variableName vr
  modify $ setVarScope (variableName vr) (scopeData scp)
  mkStmt (renderConstDecDef vr v)

classMethodCall :: (CommonRenderSym r TypeData vis smt, UnRepr r TypeData) =>
  String -> VS (r TypeData) -> VS (r TypeData) -> [SValue r] ->
  NamedArgs r TypeData -> SValue r
classMethodCall f t cls vs ns = do
  c <- cls
  call Nothing (Just $ renderType c <> dot) f t vs ns

listAppend :: (OORenderSym r tp vis smt) => String -> SValue r -> SValue r -> MS (r smt)
listAppend fnName list val = valStmt $ objMethodCall void list fnName [val]

listAdd :: (OORenderSym r tp vis smt) => String -> SValue r -> SValue r -> SValue r -> MS (r smt)
listAdd fnName list idx val = valStmt $ objMethodCall void list fnName [intToIndex idx, val]

innerType :: (OORenderSym r TypeData vis smt, TypeElim r TypeData) =>
  VS (r TypeData) -> VS (r TypeData)
innerType t = t >>= (convTypeOO . getInnerType . getCodeType)
