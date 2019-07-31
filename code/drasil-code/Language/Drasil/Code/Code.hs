-- | Defines the 'Code' data type
module Language.Drasil.Code.Code (
    Code(..),
    CodeType(..),
    spaceToCodeType
    ) where

import qualified Language.Drasil as S (Space(..))

import Text.PrettyPrint.HughesPJ (Doc)

-- | Represents the generated code as a list of file names and rendered code pairs
newtype Code = Code { unCode :: [(FilePath, Doc)]}

data CodeType = Boolean
              | Integer
              | Float
              | Char
              | String
              | File
              | List CodeType
              | Iterator CodeType
              | Object String
              | Enum String
              | Void deriving Eq

spaceToCodeType :: S.Space -> CodeType
spaceToCodeType S.Integer       = Integer
spaceToCodeType S.Natural       = Integer
spaceToCodeType S.Radians       = Float
spaceToCodeType S.Real          = Float
spaceToCodeType S.Rational      = Float
spaceToCodeType S.Boolean       = Boolean
spaceToCodeType S.Char          = Char
spaceToCodeType S.String        = String
spaceToCodeType (S.Vect s)      = List (spaceToCodeType s)
spaceToCodeType (S.DiscreteI _) = List (spaceToCodeType S.Integer)
spaceToCodeType (S.DiscreteD _) = List (spaceToCodeType S.Rational)
spaceToCodeType (S.DiscreteS _) = List (spaceToCodeType S.String)
