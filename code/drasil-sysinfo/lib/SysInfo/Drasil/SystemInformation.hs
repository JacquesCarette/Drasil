{-# LANGUAGE GADTs, TemplateHaskell, RankNTypes #-}
-- | Define types and functions related to creating a system information database.
module SysInfo.Drasil.SystemInformation (
  -- * System Information
  -- ** Types
  SystemInformation(..), Block(..),
  -- ** Lenses
  instModels, datadefs, configFiles, inputs,
  defSequence, constraints, constants, sysinfodb, usedinfodb,
  -- ** Lookup Functions
  citeDB, citationsFromBibMap,
  -- * Reference Database
  -- ** Types
  ReferenceDB, RefMap,
  -- ** Constructors
  rdb, simpleMap,
  -- ** Lenses
  citationDB, conceptDB,
  ) where

import Language.Drasil
import Theory.Drasil
import Database.Drasil (ChunkDB)

import Control.Lens ((^.), makeLenses)
import Data.Function (on)
import Data.List (find, groupBy, sortBy)
import qualified Data.Map as Map

-- | Data structure for holding all of the requisite information about a system
-- to be used in artifact generation.
data SystemInformation where
--FIXME:
--There should be a way to remove redundant "Quantity" constraint.
-- I'm thinking for getting concepts that are also quantities, we could
-- use a lookup of some sort from their internal (Drasil) ids.
 SI :: (CommonIdea a, Idea a, Idea b, HasName c,
  Quantity e, Eq e, MayHaveUnit e, Quantity f, MayHaveUnit f, Concept f, Eq f,
  Quantity h, MayHaveUnit h, Quantity i, MayHaveUnit i,
  HasUID j, Constrained j) => 
  { _sys         :: a
  , _kind        :: b
  , _authors     :: [c]
  , _purpose     :: d
  , _quants      :: [e]
  , _concepts    :: [f]
  , _instModels  :: [InstanceModel]
  , _datadefs    :: [DataDefinition]
  , _configFiles :: [String]
  , _inputs      :: [h]
  , _outputs     :: [i]
  , _defSequence :: [Block SimpleQDef]
  , _constraints :: [j] --TODO: Add SymbolMap OR enough info to gen SymbolMap
  , _constants   :: [ConstQDef]
  , _sysinfodb   :: ChunkDB
  , _usedinfodb  :: ChunkDB
  , refdb        :: ReferenceDB
  } -> SystemInformation

-- | for listing 'QDefinition's in 'SystemInformation'.
data Block a = Coupled a a [a] | Parallel a [a]

-- | Helper for extracting a bibliography from the system information.
citeDB :: SystemInformation -> BibRef
citeDB si = citationsFromBibMap (_citationDB (refdb si))

-- | Create sorted citations from a bibliography.
citationsFromBibMap :: BibMap -> [Citation]
citationsFromBibMap bm = sortBy compareAuthYearTitle citations
  where citations :: [Citation]
        citations = map fst (Map.elems bm)

-- | Orders two authors. If given two of the exact same authors, year, and title, returns an error.
compareAuthYearTitle :: (HasFields c) => c -> c -> Ordering
compareAuthYearTitle c1 c2
  | cp /= EQ = cp
  | y1 /= y2 = y1 `compare` y2
  | t1 /= t2 = t1 `compare` t2
  | otherwise = error "Couldn't sort authors"
  where
    cp = comparePeople (getAuthor c1) (getAuthor c2)
    y1 = getYear c1
    y2 = getYear c2
    t1 = getTitle c1
    t2 = getTitle c2

-- | Helper that gets the author name from something that has a field.
getAuthor :: (HasFields c) => c -> People
getAuthor c = maybe (error "No author found") (\(Author x) -> x) (find isAuthor (c ^. getFields))
  where isAuthor :: CiteField -> Bool
        isAuthor (Author _) = True
        isAuthor _          = False

-- | Helper that gets the year published from something that has a field.
getYear :: (HasFields c) => c -> Int
getYear c = maybe (error "No year found") (\(Year x) -> x) (find isYear (c ^. getFields))
  where isYear :: CiteField -> Bool
        isYear (Year _) = True
        isYear _        = False

-- | Helper that gets the title of the article from something that has a field.
getTitle :: (HasFields c) => c -> String
getTitle c = maybe (error "No title found") (\(Title x) -> x) (find isTitle (c ^. getFields))
  where isTitle :: CiteField -> Bool
        isTitle (Title _) = True
        isTitle _         = False

-- | Database for maintaining references.
-- The Int is that reference's number.
-- Maintains access to both num and chunk for easy reference swapping
-- between number and shortname/refname when necessary (or use of number
-- if no shortname exists).
type RefMap a = Map.Map UID (a, Int)

-- | Citation Database (bibliography information).
type BibMap = RefMap Citation
-- | ConceptInstance Database.
type ConceptMap = RefMap ConceptInstance


-- | Database for internal references. Contains citations and referrable concepts.
data ReferenceDB = RDB -- organized in order of appearance in SmithEtAl template
  { _citationDB :: BibMap
  , _conceptDB :: ConceptMap
  }

makeLenses ''ReferenceDB

-- | Smart constructor for creating a reference database from a bibliography and concept instances.
rdb :: BibRef -> [ConceptInstance] -> ReferenceDB
rdb citations con = RDB (bibMap citations) (conceptMap con)

-- | Constructor that makes a 'RefMap' from things that have a 'UID'.
simpleMap :: HasUID a => [a] -> RefMap a
simpleMap xs = Map.fromList $ zip (map (^. uid) xs) (zip xs [1..])

-- | Constructs a citation database from citations (sorted).
bibMap :: [Citation] -> BibMap
bibMap cs = Map.fromList $ zip (map (^. uid) scs) (zip scs [1..])
  where scs :: [Citation]
        scs = sortBy compareAuthYearTitle cs
        -- Sorting is necessary if using elems to pull all the citations
        -- (as it sorts them and would change the order).
        -- We can always change the sorting to whatever makes most sense

-- | Check if the 'UID's of two 'ConceptInstance's are the same.
conGrp :: ConceptInstance -> ConceptInstance -> Bool
conGrp a b = cdl a == cdl b where
  cdl :: ConceptInstance -> UID
  cdl = sDom . cdom

-- | Constructs a 'ConceptInstance' database from 'ConceptInstance's.
conceptMap :: [ConceptInstance] -> ConceptMap
conceptMap cs = Map.fromList $ zip (map (^. uid) (concat grp)) $ concatMap
  (\x -> zip x [1..]) grp
  where grp :: [[ConceptInstance]]
        grp = groupBy conGrp $ sortBy uidSort cs

-- | Compare two things by their 'UID's.
uidSort :: HasUID c => c -> c -> Ordering
uidSort = compare `on` (^. uid)

makeLenses ''SystemInformation