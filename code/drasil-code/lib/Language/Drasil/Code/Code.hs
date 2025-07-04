-- | Defines the 'Code' data type.
module Language.Drasil.Code.Code (
    Code(..),
    spaceToCodeType
    ) where

import qualified Language.Drasil as S (Space(..), ClifKind(..), Dimension(..))

import Drasil.GOOL (CodeType(..))

import Text.PrettyPrint.HughesPJ (Doc)
import Data.List.NonEmpty (toList)

-- | Represents the generated code as a list of file names and rendered code pairs.
newtype Code = Code { unCode :: [(FilePath, Doc)]}

-- | Default mapping between 'Space' and 'CodeType'.
-- TODO: For now, ClifS is rendered like Vect (i.e., as a list).
-- This does not support full GA structure (e.g., blades, bivectors, matrices).
-- Matrix <-> ClifS representation is unclear and deferred.
spaceToCodeType :: S.Space -> [CodeType]
spaceToCodeType S.Integer        = [Integer]
spaceToCodeType S.Natural        = [Integer]
spaceToCodeType S.Real           = [Double, Float]
spaceToCodeType S.Rational       = [Double, Float]
spaceToCodeType S.Boolean        = [Boolean]
spaceToCodeType S.Char           = [Char]
spaceToCodeType S.String         = [String]
spaceToCodeType (S.ClifS _ s)       = map List (spaceToCodeType s)
-- This is just something I could think of after Dr Carette's comment
-- spaceToCodeType (S.ClifS _ kind s) = case kind of
--     S.Scalar      -> spaceToCodeType s
--     S.Vector      -> map List (spaceToCodeType s)
--     S.Bivector    -> map List (spaceToCodeType s) -- TODO: Consider a more specific structure
--     S.Multivector -> map (List . List) (spaceToCodeType s) -- TODO: This is a simplification; full GA structure not supported
spaceToCodeType (S.Matrix _ _ s) = map (List . List) (spaceToCodeType s)
spaceToCodeType (S.Set s)        = map List (spaceToCodeType s)
spaceToCodeType (S.Array s)      = map Array (spaceToCodeType s)
spaceToCodeType (S.Actor s)      = [Object s]
spaceToCodeType S.Void           = [Void]
spaceToCodeType (S.Function i t) = [Func is ts | is <- ins, ts <- trgs]
    where trgs = spaceToCodeType t
          ins  = map spaceToCodeType (toList i)
