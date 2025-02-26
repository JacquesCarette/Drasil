-- | Defines the 'Code' data type.
module Language.Drasil.Code.Code (
    Code(..),
    spaceToCodeType
    ) where

import qualified Language.Drasil as S (Space(..))

import Drasil.GOOL (CodeType(..))

import Text.PrettyPrint.HughesPJ (Doc)
import Data.List.NonEmpty (toList)

-- | Represents the generated code as a list of file names and rendered code pairs.
newtype Code = Code { unCode :: [(FilePath, Doc)]}

-- | Default mapping between 'Space' and 'CodeType'.
spaceToCodeType :: S.Space -> [CodeType]
spaceToCodeType S.Integer        = [Integer]
spaceToCodeType S.Natural        = [Integer]
spaceToCodeType S.Real           = [Double, Float]
spaceToCodeType S.Rational       = [Double, Float]
spaceToCodeType S.Boolean        = [Boolean]
spaceToCodeType S.Char           = [Char]
spaceToCodeType S.String         = [String]
spaceToCodeType (S.ClifS _ _ s)  = map List (spaceToCodeType s)
spaceToCodeType (S.Matrix _ _ s) = map (List . List) (spaceToCodeType s)
spaceToCodeType (S.Set s)        = map List (spaceToCodeType s)
spaceToCodeType (S.Array s)      = map Array (spaceToCodeType s)
spaceToCodeType (S.Actor s)      = [Object s]
spaceToCodeType S.Void           = [Void]
spaceToCodeType (S.Function i t) = [Func is ts | is <- ins, ts <- trgs]
    where trgs = spaceToCodeType t
          ins  = map spaceToCodeType (toList i)
