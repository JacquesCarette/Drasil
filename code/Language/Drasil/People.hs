module Language.Drasil.People 
  ( People, Person(..)
  , person, person', personWM, personWM', mononym
  , HasName
  , name, manyNames, nameStr
  , Conv(..) --This is needed to unwrap names for the bibliography
  , lstName, initial, dotInitial
  , rendPersLFM, rendPersLFM', rendPersLFM''
  ) where

import Language.Drasil.Spec (Sentence(S, EmptyS, (:+:)),(+:+), sC)

-- | A person can have a given name, middle name(s), and surname, as well
-- as the naming convention they use.
data Person = Person { _given :: Sentence, _surname :: Sentence, 
                       _middle :: [Sentence], _convention :: Conv}
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
person f l = Person (S f) (S l) [] Western

-- | Constructor for a person using Eastern naming conventions. 
-- Used for a person with only a given name and surname.
-- Arguments are in the order: surname, given name
person' :: String -> String -> Person
person' s g = Person (S g) (S s) [] Eastern

-- | Constructor for a person using Western naming conventions.
-- Similar to the 'person' constructor,
-- except the middle argument is a list of middle names.
personWM :: String -> [String] -> String -> Person
personWM f ms l = Person (S f) (S l) (map S ms) Western

-- | Constructor for a person using Eastern naming conventions.
-- Similar to the 'person\'' constructor,
-- except the middle argument is a list of middle names.
personWM' :: String -> [String] -> String -> Person
personWM' g ms s = Person (S g) (S s) (map S ms) Eastern

-- | Constructor for a person with a mononym (only one name).
mononym :: String -> Person
mononym n = Person (S "NFN") (S n) [] Mono

class HasName p where
  nameStr :: p -> Sentence

instance HasName Person where
  nameStr (Person _ n _ Mono) =  dotInitial n
  nameStr (Person f l ms Western) = foldr (+:+) EmptyS (
    [dotInitial f] ++ map dotInitial ms ++ [dotInitial l])
  nameStr (Person g s ms Eastern) = foldr (+:+) EmptyS (
    [dotInitial s] ++ map dotInitial ms ++ [dotInitial g])

name :: (HasName n) => n -> Sentence
name = nameStr
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

lstName :: Person -> Sentence
lstName (Person {_surname = l}) = l

-- LFM is Last, First Middle
rendPersLFM :: Person -> Sentence
rendPersLFM (Person {_surname = n, _convention = Mono}) = n
rendPersLFM (Person {_given = f, _surname = l, _middle = ms}) =
  (dotInitial l) `sC` (dotInitial f) +:+ foldr (+:+) EmptyS (map dotInitial ms)

-- LFM' is Last, F. M.
rendPersLFM' :: Person -> Sentence
rendPersLFM' (Person {_surname = n, _convention = Mono}) = n
rendPersLFM' (Person {_given = f, _surname = l, _middle = ms}) =
  (dotInitial l) `sC` foldr (+:+) EmptyS (map (initial) (f:ms))

-- LFM'' is Last, First M.
rendPersLFM'' :: Person -> Sentence
rendPersLFM'' (Person {_surname = n, _convention = Mono}) = n
rendPersLFM'' (Person {_given = f, _surname = l, _middle = ms}) =
  (dotInitial l) `sC` foldr1 (+:+) (dotInitial f : (map (initial) ms))

initial :: Sentence -> Sentence
initial (EmptyS :+: b) = initial b
initial (a :+: _) = initial a
initial (S s) = S $ (head s : ".")
initial _ = error "Cannot get initials for this name"

-- | dotInitial will add a . after a name which is an 'initial', aka a single letter.
dotInitial :: Sentence -> Sentence
dotInitial (EmptyS :+: b) = dotInitial b
dotInitial (a :+: _) = dotInitial a
dotInitial (S [x])   = S [x,'.']
dotInitial nm = nm
