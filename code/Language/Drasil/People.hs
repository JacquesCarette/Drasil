module Language.Drasil.People (Person, person, HasName, name, twoNames) where

import Language.Drasil.Spec (Sentence(S),(+:+))

data Person = Person { _first :: String, _last :: String, _name :: Sentence}

person :: String -> String -> Person
person f l = Person f l (S $ f ++ " " ++ l)

class HasName p where
  name :: p -> Sentence

instance HasName Person where
  name (Person _ _ n) = n

twoNames :: (HasName p1, HasName p2) => p1 -> p2 -> Sentence
twoNames a1 a2 = (name a1) +:+ S "and" +:+ (name a2)
