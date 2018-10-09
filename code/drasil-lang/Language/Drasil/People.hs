module Language.Drasil.People 
  ( People, Person
  , person, person', personWM, personWM', mononym
  , HasName
  , name, manyNames, nameStr
  , Conv(..) --This is needed to unwrap names for the bibliography
  , lstName
  , rendPersLFM, rendPersLFM', rendPersLFM''
  , comparePeople --For sorting references
  ) where

-- | A person can have a given name, middle name(s), and surname, as well
-- as the naming convention they use.
data Person = Person { _given :: String
                     , _surname :: String
                     , _middle :: [String]
                     , _convention :: Conv
                     } deriving (Eq)
type People = [Person]

-- | Naming conventions.
data Conv = Western
          | Eastern
          | Mono deriving (Eq)

-- ^ Western style conventions are given name followed
-- by middle names, followed by surname.
-- Eastern style conventions are surname followed by middle names, 
-- followed by given name.
-- Mononyms are for those people who have only one name (ex. Madonna)

comparePeople :: [Person] -> [Person] -> Ordering
comparePeople [] [] = EQ
comparePeople _  [] = GT -- this makes sure that if the authors are the same 
comparePeople []  _ = LT -- up to a point, the citation with more goes last
comparePeople (Person f1 l1 _ _:xs) (Person f2 l2 _ _:ys)
  | l1 /= l2  = l1 `compare` l2
  | f1 /= f2  = f1 `compare` f2
  | otherwise = comparePeople xs ys

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
  nameStr :: p -> String

instance HasName Person where
  nameStr (Person _ n _ Mono) =  dotInitial n
  nameStr (Person f l ms Western) = foldr nameSep "" (
    [dotInitial f] ++ map dotInitial ms ++ [dotInitial l])
  nameStr (Person g s ms Eastern) = foldr nameSep "" (
    [dotInitial s] ++ map dotInitial ms ++ [dotInitial g])

name :: (HasName n) => n -> String
name = nameStr
-- this is a weirder recursion, so it's ok to do it explicitly
-- make it work for short lists too, but it shouldn't be used that way!
-- | Used for rendering lists of names (one or more).
manyNames :: (HasName p) => [p] -> String
manyNames [x,y] = name x ++ " and " ++ name y
manyNames names = nameList names
  where nameList [] = ""
        nameList [x] = name x
        nameList [x,y] = name x ++ ", and " ++ name y
        nameList (x : y : rest) = name x ++ ", " ++ nameList (y : rest)

lstName :: Person -> String
lstName Person {_surname = l} = l

-- LFM is Last, First Middle
rendPersLFM :: Person -> String
rendPersLFM Person {_surname = n, _convention = Mono} = n
rendPersLFM Person {_given = f, _surname = l, _middle = ms} =
  dotInitial l ++ ", " ++ dotInitial f `nameSep`
  foldr (nameSep . dotInitial) "" ms

-- LFM' is Last, F. M.
rendPersLFM' :: Person -> String
rendPersLFM' Person {_surname = n, _convention = Mono} = n
rendPersLFM' Person {_given = f, _surname = l, _middle = ms} =
  dotInitial l ++ ", " ++ foldr (nameSep . initial) "" (f:ms)

-- LFM'' is Last, First M.
rendPersLFM'' :: Person -> String
rendPersLFM'' Person {_surname = n, _convention = Mono} = n
rendPersLFM'' Person {_given = f, _surname = l, _middle = ms} =
  dotInitial l ++ ", " ++ foldr1 nameSep (dotInitial f : map initial ms)

initial :: String -> String
initial []    = [] -- is this right?
initial (x:_) = [x , '.']

-- | dotInitial will add a . after a name which is an 'initial', aka a single letter.
dotInitial :: String -> String
dotInitial [x] = [x,'.']
dotInitial nm  = nm

nameSep :: String -> String -> String
"" `nameSep` b = b
a `nameSep` "" = a
a `nameSep` b = a ++ " " ++ b
