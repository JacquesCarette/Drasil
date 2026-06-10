{-# LANGUAGE FlexibleContexts #-}
-- | Contains renderer-related functions specific to GOOL

module Drasil.GProc.Renderers (
  renderType, renderParam, renderListDec, renderConstDecDef
) where

import Drasil.Shared.InterfaceCommon (UnRepr(..), VariableSym(..),
  VariableElim(..), ValueSym(..))
import Drasil.Shared.RendererClassesCommon (CommonRenderSym, InternalVarElim(..),
  ValueElim(..))
import Drasil.GProc.RendererClassesProc (ProcRenderSym)
import Drasil.Shared.LanguageRenderer (new', constDec')
import Drasil.Shared.CodeType (CodeType(..))
import Drasil.Shared.AST (TypeData(..))

import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJ (Doc, (<+>), (<>), space, equals, parens)

renderType :: (UnRepr r TypeData) => r TypeData -> Doc
renderType tp = case cType $ unRepr tp of
    (Object _) -> error "Classes are not supported in procedural languages"
    _ -> typeDoc $ unRepr tp

renderParam :: (ProcRenderSym r, UnRepr r TypeData) => r (Variable r) -> Doc
renderParam v = renderType (variableType v) <+> variable v

renderListDec :: (CommonRenderSym r, UnRepr r TypeData) => r (Variable r) ->
  r (Value r) -> Doc
renderListDec v n = space <> equals <+> new' <+> renderType (variableType v)
  <> parens (value n)

renderConstDecDef :: (CommonRenderSym r, UnRepr r TypeData) => r (Variable r) ->
  r (Value r) -> Doc
renderConstDecDef v def = constDec' <+> renderType (variableType v) <+>
  variable v <+> equals <+> value def
