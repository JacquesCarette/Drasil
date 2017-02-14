module Language.Drasil.People (People, Person, person, HasName, name, twoNames, manyNames) where

import Language.Drasil.Spec (Sentence(S,(:+:)),(+:+))

data Person = Person { _first :: String, _last :: String, _name :: Sentence}
type People = [Person]


person :: String -> String -> Person
person f l = Person f l (S $ f ++ " " ++ l)

class HasName p where
  name :: p -> Sentence

instance HasName Person where
  name (Person _ _ n) = n

twoNames :: (HasName p1, HasName p2) => p1 -> p2 -> Sentence
twoNames a1 a2 = (name a1) +:+ S "and" +:+ (name a2)

-- this is a weirder recursion, so it's ok to do it explicitly
-- make it work for short lists too, but it shouldn't be used that way!
manyNames :: (HasName p) => [p] -> Sentence
manyNames [] = S ""
manyNames [x] = name x
manyNames [x,y] = (name x) :+: (S ", and ") :+: (name y)
manyNames (x : y : rest) = (name x) :+: (S ", ") :+: (manyNames (y : rest))
