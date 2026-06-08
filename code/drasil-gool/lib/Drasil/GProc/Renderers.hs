{-# LANGUAGE FlexibleContexts #-}
-- | Contains renderer-related functions specific to GOOL

module Drasil.GProc.Renderers (renderType, renderParam) where

import Drasil.Shared.InterfaceCommon (VariableSym(..), VariableElim(..))
import Drasil.Shared.RendererClassesCommon (UnRepr(..), InternalVarElim (..))
import Drasil.GProc.RendererClassesProc (ProcRenderSym)
import Drasil.Shared.CodeType (CodeType(..))
import Drasil.Shared.AST (TypeData(..))

import Text.PrettyPrint.HughesPJ (Doc, (<+>))

renderType :: (UnRepr r TypeData) => r TypeData -> Doc
renderType tp = case cType $ unRepr tp of
    (Object _) -> error "Classes are not supported in procedural languages"
    _ -> typeDoc $ unRepr tp

renderParam :: (ProcRenderSym r, UnRepr r TypeData) => r (Variable r) -> Doc
renderParam v = renderType (variableType v) <+> variable v
