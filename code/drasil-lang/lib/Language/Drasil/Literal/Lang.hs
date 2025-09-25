{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Drasil.Literal.Lang where

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

instance Typed Literal Space where
  infer :: TypingContext Space -> Literal -> Either TypeError Space
  infer _ (Int _)      = pure Integer
  infer _ (Str _)      = pure String
  infer _ (Dbl _)      = pure Real
  infer _ (ExactDbl _) = pure Real
  infer _ (Perc _ _)   = pure Real

  check :: TypingContext Space -> Literal -> Space -> Either TypeError Space
  check = typeCheckByInfer
