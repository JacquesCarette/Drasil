{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Drasil.Literal.Lang where
import Language.Drasil.WellTyped
import Language.Drasil.Space (Space(..))

data Literal where
    Int      :: Integer -> Literal
    Str      :: String -> Literal
    Dbl      :: Double -> Literal
    ExactDbl :: Integer -> Literal
    Perc     :: Integer -> Integer -> Literal
    deriving Eq

{- TODO: When typing the Expression language, this will be usable
instance Eq (Literal a) where
    (Int l)      == (Int r)      =  l == r
    (Str l)      == (Str r)      =  l == r
    (Dbl l)      == (Dbl r)      =  l == r
    (ExactDbl l) == (ExactDbl r) =  l == r
    (Perc l1 l2) == (Perc r1 r2) =  l1 == r1 && l2 == r2
    _            == _            =  False
-}

instance Typed Literal Space where
  infer :: TypingContext Space -> Literal -> Either Space TypeError
  infer _ (Int _) = Left Integer
  infer _ (Str _) = Left String
  infer _ (Dbl _) = Left Real
  infer _ (ExactDbl _) = Left Real
  infer _ (Perc _ _) = Left Real
