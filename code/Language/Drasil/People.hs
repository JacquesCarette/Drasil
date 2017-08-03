module Language.Drasil.People 
  ( People, Person(..)
  , person, person', personWM, personWM', mononym
  , HasName
  , name, manyNames
  , Conv(..) --This is needed to unwrap names for the bibliography
  , lstName, initial
  , rendPersLFM, rendPersLFM', rendPersLFM''
  ) where

import Language.Drasil.Spec (Sentence(S, EmptyS),(+:+), sC)
import Data.List

-- | A person can have a given name, middle name(s), and surname, as well
-- as the naming convention they use.
data Person = Person { _given :: String, _surname :: String, 
                       _middle :: [String], _convention :: Conv}
-- ^ Western style conventions are given name followed
-- by middle names, followed by surname.
-- Eastern style conventions are surname followed by middle names, 
-- followed by given name.
-- Mononyms are for those people who have only one name (ex. Madonna)

type People = [Person]

-- | Naming conventions.
data Conv = Western
          | Eastern
          | Mono

-- | Constructor for a person using Western naming conventions. 
-- Used for a person with only a given name and surname.
-- Arguments are in the order: given name, surname.
person :: String -> String -> Person
person f l = Person f l [] Western

-- | Constructor for a person using Eastern naming conventions. 
-- Used for a person with only a given name and surname.
-- Arguments are in the order: surname, given name
person' :: String -> String -> Person
person' s g = Person g s [] Eastern

-- | Constructor for a person using Western naming conventions.
-- Similar to the 'person' constructor,
-- except the middle argument is a list of middle names.
personWM :: String -> [String] -> String -> Person
personWM f ms l = Person f l ms Western

-- | Constructor for a person using Eastern naming conventions.
-- Similar to the 'person\'' constructor,
-- except the middle argument is a list of middle names.
personWM' :: String -> [String] -> String -> Person
personWM' g ms s = Person g s ms Eastern

-- | Constructor for a person with a mononym (only one name).
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
-- | Used for rendering lists of names (one or more) to Sentences.
manyNames :: (HasName p) => [p] -> Sentence
manyNames [x,y] = (name x) +:+ (S "and") +:+ (name y)
manyNames names = nameList names
  where nameList [] = EmptyS
        nameList [x] = name x
        nameList [x,y] = (name x) `sC` (S "and") +:+ (name y)
        nameList (x : y : rest) = (name x) `sC` (nameList (y : rest))

lstName :: Person -> String
lstName (Person {_surname = l}) = l

-- LFM is Last, First Middle
rendPersLFM :: Person -> String
rendPersLFM (Person {_surname = n, _convention = Mono}) = n
rendPersLFM (Person {_given = f, _surname = l, _middle = ms}) =
  isInitial l ++ ", " ++ unwords (isInitial f: map isInitial ms)

-- LFM' is Last, F. M.
rendPersLFM' :: Person -> String
rendPersLFM' (Person {_surname = n, _convention = Mono}) = n
rendPersLFM' (Person {_given = f, _surname = l, _middle = ms}) =
  isInitial l ++ ", " ++ (unwords . map initial) (f:ms)

-- LFM'' is Last, First M.
rendPersLFM'' :: Person -> String
rendPersLFM'' (Person {_surname = n, _convention = Mono}) = n
rendPersLFM'' (Person {_given = f, _surname = l, _middle = ms}) =
  isInitial l ++ ", " ++ unwords (isInitial f:(map initial ms))

initial :: String -> String
initial = (\xs -> head xs : ".")

isInitial :: String -> String
isInitial [x]  = [x,'.']
isInitial name = name