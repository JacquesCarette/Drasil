{-# LANGUAGE FlexibleContexts #-}
-- | Contains renderer-related functions specific to GOOL

module Drasil.GOOL.Renderers (
  renderType, renderParam, renderMethod, renderListDec, renderConstDecDef
) where

import Drasil.FileHandling.Legacy (indent)

import Drasil.Shared.InterfaceCommon (VariableSym(..), VariableElim (..),
  ValueSym(..), VisibilitySym(..), ParameterSym(..), BodySym(..))
import Drasil.GOOL.InterfaceGOOL (AttachmentSym(..))
import Drasil.Shared.RendererClassesCommon (CommonRenderSym, UnRepr(..),
  InternalVarElim(..), VisibilityElim(..), ValueElim(..))
import qualified Drasil.Shared.RendererClassesCommon as RC (BodyElim(..))
import Drasil.GOOL.RendererClassesOO (OORenderSym, PermElim(..))
import Drasil.Shared.LanguageRenderer (parameterList, new', constDec')
import Drasil.Shared.AST (TypeData(..))

import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJ (Doc, (<+>), (<>), vcat, text, lbrace, rbrace,
  parens, space, equals)

renderType :: (UnRepr r TypeData) => r TypeData -> Doc
renderType = typeDoc . unRepr

renderParam :: (OORenderSym r, UnRepr r TypeData) => r (Variable r) -> Doc
renderParam v = renderType (variableType v) <+> variable v

renderMethod :: (OORenderSym r, UnRepr r TypeData) => String ->
  r (Visibility r) -> r (Attachment r) -> r TypeData -> [r (Parameter r)] ->
  r (Body r) -> Doc
renderMethod n s p t ps b = vcat [
  visibility s <+> perm p <+> renderType t <+> text n <>
    (parens (parameterList ps) <+> lbrace),
  indent (RC.body b),
  rbrace]

renderListDec :: (CommonRenderSym r, UnRepr r TypeData) => r (Variable r) ->
  r (Value r) -> Doc
renderListDec v n = space <> equals <+> new' <+> renderType (variableType v)
  <> parens (value n)

renderConstDecDef :: (CommonRenderSym r, UnRepr r TypeData) => r (Variable r) ->
  r (Value r) -> Doc
renderConstDecDef v def = constDec' <+> renderType (variableType v) <+>
  variable v <+> equals <+> value def
