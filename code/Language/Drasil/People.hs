module Language.Drasil.People (People, Person, person, HasName, name, manyNames) where

import Language.Drasil.Spec (Sentence(S,(:+:)),(+:+), sC)

data Person = Person { _first :: String, _last :: String, _name :: Sentence}
type People = [Person]


person :: String -> String -> Person
person f l = Person f l (S $ f ++ " " ++ l)

class HasName p where
  name :: p -> Sentence

instance HasName Person where
  name (Person _ _ n) = n

-- this is a weirder recursion, so it's ok to do it explicitly
-- make it work for short lists too, but it shouldn't be used that way!
manyNames :: (HasName p) => [p] -> Sentence
manyNames [x,y] = (name x) +:+ (S "and") +:+ (name y)
manyNames names = nameList names
  where nameList [] = S ""
        nameList [x] = name x
        nameList [x,y] = (name x) `sC` (S "and") +:+ (name y)
        nameList (x : y : rest) = (name x) `sC` (nameList (y : rest))
