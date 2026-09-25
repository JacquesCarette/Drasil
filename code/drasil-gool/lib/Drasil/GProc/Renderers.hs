-- | Contains renderer-related functions specific to GOOL
module Drasil.GProc.Renderers (
  renderType, renderParam, renderListDec, renderConstDecDef
) where

import Drasil.Shared.InterfaceCommon (UnRepr(..), VariableElim(..))
import Drasil.Shared.RendererClassesCommon (InternalVarElim(..), ValueElim(..))
import Drasil.Shared.LanguageRenderer (new', constDec')
import Drasil.Shared.CodeType (CodeType(..))
import Drasil.Shared.AST (TypeData(..))

import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJ (Doc, (<+>), (<>), space, equals, parens)

renderType :: (UnRepr r TypeData) => r TypeData -> Doc
renderType tp = case cType $ unRepr tp of
    (Object _) -> error "Classes are not supported in procedural languages"
    _ -> typeDoc $ unRepr tp

renderParam
  :: (InternalVarElim r var, UnRepr r TypeData, VariableElim r TypeData var)
  => r var -> Doc
renderParam v = renderType (variableType v) <+> variable v

renderListDec
  :: (UnRepr r TypeData, ValueElim r val, VariableElim r TypeData var)
  => r var -> r val -> Doc
renderListDec v n = space <> equals <+> new' <+> renderType (variableType v)
  <> parens (value n)

renderConstDecDef
  ::
    ( InternalVarElim r var
    , UnRepr r TypeData
    , ValueElim r val
    , VariableElim r TypeData var
    )
  => r var -> r val -> Doc
renderConstDecDef v def = constDec' <+> renderType (variableType v) <+>
  variable v <+> equals <+> value def
