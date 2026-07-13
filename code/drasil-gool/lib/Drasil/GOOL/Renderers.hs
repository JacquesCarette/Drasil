{-# LANGUAGE FlexibleContexts #-}
-- | Contains renderer-related functions specific to GOOL

module Drasil.GOOL.Renderers (
  renderType, renderParam, renderMethod, renderListDec, renderConstDecDef
) where

import Drasil.FileHandling.Legacy (indent)

import Drasil.Shared.InterfaceCommon (UnRepr(..), VariableSym(..),
  VariableElim(..), ValueSym(..), BodySym(..))
import Drasil.GOOL.InterfaceGOOL (AttachmentSym(..))
import Drasil.Shared.RendererClassesCommon (InternalVarElim(..),
  VisibilityElim(..), ValueElim(..), ParamElim)
import qualified Drasil.Shared.RendererClassesCommon as RC (BodyElim(..))
import Drasil.GOOL.RendererClassesOO (PermElim(..))
import Drasil.Shared.LanguageRenderer (parameterList, new', constDec')
import Drasil.Shared.AST (TypeData(..), ParamData)

import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJ (Doc, (<+>), (<>), vcat, text, lbrace, rbrace,
  parens, space, equals)

renderType :: (UnRepr r TypeData) => r TypeData -> Doc
renderType = typeDoc . unRepr

renderParam
  :: (InternalVarElim r, UnRepr r TypeData, VariableElim r)
  => r (Variable r) -> Doc
renderParam v = renderType (variableType v) <+> variable v

renderMethod
  :: ( RC.BodyElim r
     , ParamElim r
     , PermElim r
     , UnRepr r TypeData
     , VisibilityElim r vis
     )
  => String
  -> r vis
  -> r (Attachment r)
  -> r TypeData
  -> [r ParamData]
  -> r (Body r)
  -> Doc
renderMethod n s p t ps b = vcat [
  visibility s <+> perm p <+> renderType t <+> text n <>
    (parens (parameterList ps) <+> lbrace),
  indent (RC.body b),
  rbrace]

renderListDec
  :: (UnRepr r TypeData, ValueElim r, VariableElim r)
  => r (Variable r) -> r (Value r) -> Doc
renderListDec v n = space <> equals <+> new' <+> renderType (variableType v)
  <> parens (value n)

renderConstDecDef
  :: (InternalVarElim r, UnRepr r TypeData, ValueElim r, VariableElim r)
  =>  r (Variable r) -> r (Value r) -> Doc
renderConstDecDef v def = constDec' <+> renderType (variableType v) <+>
  variable v <+> equals <+> value def
