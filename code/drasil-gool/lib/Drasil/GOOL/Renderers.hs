{-# LANGUAGE FlexibleContexts #-}
-- | Contains renderer-related functions specific to GOOL

module Drasil.GOOL.Renderers (renderType, renderParam) where

import Drasil.Shared.InterfaceCommon (VariableSym(..), VariableElim (..))
import Drasil.Shared.RendererClassesCommon (UnRepr(..),
  InternalVarElim(..))
import Drasil.GOOL.RendererClassesOO (OORenderSym)
import Drasil.Shared.AST (TypeData(..))

import Text.PrettyPrint.HughesPJ (Doc, (<+>))

renderType :: (UnRepr r TypeData) => r TypeData -> Doc
renderType = typeDoc . unRepr

renderParam :: (OORenderSym r, UnRepr r TypeData) => r (Variable r) -> Doc
renderParam v = renderType (variableType v) <+> variable v
