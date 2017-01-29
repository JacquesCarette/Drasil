module Language.Drasil.People (Person, person, HasName, name) where

import Language.Drasil.Spec (Sentence(S))

data Person = Person { _first :: String, _last :: String, _name :: Sentence}

person :: String -> String -> Person
person f l = Person f l (S $ f ++ " " ++ l)

class HasName p where
  name :: p -> Sentence

instance HasName Person where
  name (Person _ _ n) = n
