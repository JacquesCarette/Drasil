module Drasil.Shared.Classes (Pair(..)) where

class Pair p where
  pfst :: p x y a -> x a
  psnd :: p x y b -> y b
  pair :: x a -> y a -> p x y a