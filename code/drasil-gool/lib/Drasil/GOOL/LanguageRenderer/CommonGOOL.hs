{-# LANGUAGE FlexibleContexts #-}
-- | Contains common implementations specific to GOOL

module Drasil.GOOL.LanguageRenderer.CommonGOOL (
  constDecDef, classMethodCall, listAppend
) where

import Drasil.Shared.InterfaceCommon (UnRepr(..), SVariable, SValue, VSType,
  MSStatement, NamedArgs, VariableElim(..), TypeSym(..))
import Drasil.GOOL.InterfaceGOOL (objMethodCall, InternalValueExp(..))
import Drasil.Shared.RendererClassesCommon (CommonRenderSym, ScopeElim(..),
  RenderValue(..))
import Drasil.Shared.LanguageRenderer.Constructors (mkStmt)
import Drasil.Shared.LanguageRenderer (dot)
import Drasil.GOOL.Renderers (renderType, renderConstDecDef)
import Drasil.Shared.AST (TypeData, ScopeData)
import Drasil.Shared.State (lensMStoVS, useVarName, setVarScope)

import Control.Lens.Zoom (zoom)
import Control.Monad.State (modify)

constDecDef :: (CommonRenderSym r, UnRepr r TypeData) => SVariable r ->
  r ScopeData -> SValue r -> MSStatement r
constDecDef vr' scp v'= do
  vr <- zoom lensMStoVS vr'
  v <- zoom lensMStoVS v'
  modify $ useVarName $ variableName vr
  modify $ setVarScope (variableName vr) (scopeData scp)
  mkStmt (renderConstDecDef vr v)

classMethodCall :: (CommonRenderSym r, UnRepr r TypeData) => String ->
  VSType r -> VSType r -> [SValue r] -> NamedArgs r -> SValue r
classMethodCall f t cls vs ns = do
  c <- cls
  call Nothing (Just $ renderType c <> dot) f t vs ns

listAppend :: (InternalValueExp r) => String -> SValue r ->
  SValue r -> SValue r
listAppend fnName list val = objMethodCall void list fnName [val]
