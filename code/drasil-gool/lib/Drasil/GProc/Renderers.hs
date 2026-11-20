-- | Contains renderer-related functions specific to GOOL
module Drasil.GProc.Renderers (
  renderType
) where

import Drasil.Shared.InterfaceCommon (UnRepr(..))
import Drasil.Shared.CodeType (CodeType(..))
import Drasil.Shared.AST (TypeData(..))

import Text.PrettyPrint.HughesPJ (Doc)

renderType :: (UnRepr r TypeData) => r TypeData -> Doc
renderType tp = case cType $ unRepr tp of
    (Object _) -> error "Classes are not supported in procedural languages"
    _ -> typeDoc $ unRepr tp
