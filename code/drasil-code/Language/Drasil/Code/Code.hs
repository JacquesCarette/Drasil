-- | Defines the 'Code' data type
module Language.Drasil.Code.Code (
    Code(..),
    spaceToCodeType
    ) where

import qualified Language.Drasil as S (Space(..))

import GOOL.Drasil (CodeType(..))

import Text.PrettyPrint.HughesPJ (Doc)

-- | Represents the generated code as a list of file names and rendered code pairs
newtype Code = Code { unCode :: [(FilePath, Doc)]}

spaceToCodeType :: S.Space -> CodeType
spaceToCodeType S.Integer       = Integer
spaceToCodeType S.Natural       = Integer
spaceToCodeType S.Radians       = Double
spaceToCodeType S.Real          = Double
spaceToCodeType S.Rational      = Double
spaceToCodeType S.Boolean       = Boolean
spaceToCodeType S.Char          = Char
spaceToCodeType S.String        = String
spaceToCodeType (S.Vect s)      = List (spaceToCodeType s)
spaceToCodeType (S.DiscreteI _) = List (spaceToCodeType S.Integer)
spaceToCodeType (S.DiscreteD _) = List (spaceToCodeType S.Rational)
spaceToCodeType (S.DiscreteS _) = List (spaceToCodeType S.String)
