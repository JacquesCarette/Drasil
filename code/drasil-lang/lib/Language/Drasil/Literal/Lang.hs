module Language.Drasil.Literal.Lang (Literal(..)) where

import Drasil.Database (HasChunkRefs(..))
import Language.Drasil.Space (Space (..))
import Language.Drasil.WellTyped (Typed(..), TypingContext, TypeError,
  typeCheckByInfer)

data Literal where
    Int      :: Integer -> Literal
    Str      :: String -> Literal
    Dbl      :: Double -> Literal
    ExactDbl :: Integer -> Literal
    Perc     :: Integer -> Integer -> Literal
    deriving Eq

-- | A 'Literal' is a constant value, so it never refers to any chunk.
instance HasChunkRefs Literal where
  chunkRefs _ = mempty
  {-# INLINABLE chunkRefs #-}

instance Typed Literal Space where
  infer :: TypingContext Space -> Literal -> Either TypeError Space
  infer _ (Int _)      = pure Integer
  infer _ (Str _)      = pure String
  infer _ (Dbl _)      = pure Real
  infer _ (ExactDbl _) = pure Real
  infer _ (Perc _ _)   = pure Real

  check :: TypingContext Space -> Literal -> Space -> Either TypeError Space
  check = typeCheckByInfer
