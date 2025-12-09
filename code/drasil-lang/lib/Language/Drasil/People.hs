-- | Defines types and functions to encode people, names, and naming convention.
-- Used for referencing and authorship of work.
module Language.Drasil.People (
    -- * Class
    HasName
    -- * Types
  , Person, People, Conv(..) --This is needed to unwrap names for the bibliography
    -- * Constructors
  , person, person', personWM, personWM', mononym
    -- * Accessors
  , name, nameStr
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
-- | People is a synonymn for many 'Person's.
type People = [Person]

-- | Naming conventions.
data Conv = Western -- ^ Western style conventions are given name followed
                    -- by middle names, followed by surname.
          | Eastern -- ^ Eastern style conventions are surname followed by middle names,
                    -- followed by given name.
          | Mono  -- ^ Mononyms are for those people who have only one name (ex. Madonna).
          deriving (Eq)

-- | Orderes different groups of 'Person's. If two lists are the same up to a point, the citation with more 'Person's will go last.
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
-- Arguments are in the order: surname, given name.
person' :: String -> String -> Person
person' s g = Person g s [] Eastern

-- | Constructor for a person using Western naming conventions.
-- Similar to the 'person' constructor,
-- except the middle argument is a list of middle names.
personWM :: String -> [String] -> String -> Person
personWM f ms l = Person f l ms Western

-- | Constructor for a person using Eastern naming conventions.
-- Similar to the 'person'' constructor,
-- except the middle argument is a list of middle names.
personWM' :: String -> [String] -> String -> Person
personWM' g ms s = Person g s ms Eastern

-- | Constructor for a person with a mononym (only one name).
mononym :: String -> Person
mononym n = Person "NFN" n [] Mono

-- | Members of this class must have a name.
class HasName p where
  -- | Provides the ability to hold a name.
  nameStr :: p -> String

-- | Gets the name of a 'Person'. Adds a dot after any initials.
instance HasName Person where
  nameStr (Person _ n _ Mono) =  dotInitial n
  nameStr (Person f l ms Western) = foldr nameSep "" (
    [dotInitial f] ++ map dotInitial ms ++ [dotInitial l])
  nameStr (Person g s ms Eastern) = foldr nameSep "" (
    [dotInitial s] ++ map dotInitial ms ++ [dotInitial g])

-- | Gets the name of a 'Person'. Adds a dot after any initials.
name :: (HasName n) => n -> String
name = nameStr

-- | Gets the last name of a 'Person'.
lstName :: Person -> String
lstName Person {_surname = l} = l

-- | Gets a 'Person'\'s name in the form: Last, First Middle.
rendPersLFM :: Person -> String
rendPersLFM Person {_surname = n, _convention = Mono} = n
rendPersLFM Person {_given = f, _surname = l, _middle = ms} =
  dotInitial l `orderSep` dotInitial f `nameSep`
  foldr (nameSep . dotInitial) "" ms

-- | Gets a 'Person'\'s name in the form: Last, F. M.
rendPersLFM' :: Person -> String
rendPersLFM' Person {_surname = n, _convention = Mono} = n
rendPersLFM' Person {_given = f, _surname = l, _middle = ms} =
  dotInitial l `orderSep` foldr (nameSep . initial) "" (f:ms)

-- | Gets a 'Person'\'s name in the form: Last, First M.
rendPersLFM'' :: Person -> String
rendPersLFM'' Person {_surname = n, _convention = Mono} = n
rendPersLFM'' Person {_given = f, _surname = l, _middle = ms} =
  dotInitial l `orderSep` foldr1 nameSep (dotInitial f : map initial ms)

-- | Finds an initial and appends a period after it.
initial :: String -> String
initial []    = [] -- is this right?
initial (x:_) = [x , '.']

-- | Adds a . after a name which is an initial, aka a single letter.
dotInitial :: String -> String
dotInitial [x] = [x,'.']
dotInitial nm  = nm

-- | Helper that joins two strings (second and third arguments) together with another string (first argument).
joiner :: String -> String -> String -> String
joiner _ a "" = a
joiner _ "" b = b
joiner j a b = a ++ j ++ b

-- | Joins strings with a comma in between.
orderSep :: String -> String -> String
orderSep = joiner ", "

-- | Joins strings with a space in between.
nameSep :: String -> String -> String
nameSep = joiner " "
