module Language.Drasil.People 
  ( People, Person
  , person, person', personWM, personWM', mononym
  , HasName
  , name, manyNames) where

import Language.Drasil.Spec (Sentence(S, EmptyS),(+:+), sC)
import Data.List

data Person = Person { _given :: String, _surname :: String, 
                       _middle :: [String], _convention :: Conv}
type People = [Person]

data Conv = Western
          | Eastern
          | Mono

person :: String -> String -> Person
person f l = Person f l [] Western

person' :: String -> String -> Person
person' g s = Person g s [] Eastern

personWM :: String -> [String] -> String -> Person
personWM f ms l = Person f l ms Western

personWM' :: String -> [String] -> String -> Person
personWM' g ms s = Person g s ms Eastern

mononym :: String -> Person
mononym n = Person "NFN" n [] Mono

class HasName p where
  name :: p -> Sentence

instance HasName Person where
  name (Person _ n _ Mono) = S n
  name (Person f l ms Western) = S $ concat (intersperse " " $ [f] ++ ms ++ [l])
  name (Person g s ms Eastern) = S $ concat (intersperse " " $ [s] ++ ms ++ [g])

-- this is a weirder recursion, so it's ok to do it explicitly
-- make it work for short lists too, but it shouldn't be used that way!
manyNames :: (HasName p) => [p] -> Sentence
manyNames [x,y] = (name x) +:+ (S "and") +:+ (name y)
manyNames names = nameList names
  where nameList [] = EmptyS
        nameList [x] = name x
        nameList [x,y] = (name x) `sC` (S "and") +:+ (name y)
        nameList (x : y : rest) = (name x) `sC` (nameList (y : rest))
