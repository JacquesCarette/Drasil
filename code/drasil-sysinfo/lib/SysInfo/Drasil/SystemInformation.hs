{-# LANGUAGE GADTs, TemplateHaskell, RankNTypes #-}
-- | Define types and functions related to creating a system information database.

-- Changes to SystemInformation should be reflected in the 'Creating Your Project 
-- in Drasil' tutorial found on the wiki:
-- https://github.com/JacquesCarette/Drasil/wiki/Creating-Your-Project-in-Drasil

module SysInfo.Drasil.SystemInformation (
  -- * System Information
  -- ** Types
  SystemInformation(..), Block(..),
  -- ** Lenses
  HasSystemInformation(..),
  -- ** Lookup Functions
  citeDB, citationsFromBibMap,
  -- * Reference Database
  -- ** Types
  ReferenceDB, RefMap, Purpose, Background, Scope, Motivation,
  -- ** Constructors
  rdb, simpleMap,
  -- ** Lenses
  citationDB, conceptDB,
  ) where

import Language.Drasil
import Theory.Drasil
import Database.Drasil (ChunkDB)

import Control.Lens ((^.), makeLenses, makeClassy)
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map


-- | Data structure for holding all of the requisite information about a system
-- to be used in artifact generation.
data SystemInformation where
--FIXME:
--There should be a way to remove redundant "Quantity" constraint.
-- I'm thinking for getting concepts that are also quantities, we could
-- use a lookup of some sort from their internal (Drasil) ids.
 SI :: (CommonIdea a, Idea a, Idea b,
  Quantity e, Eq e, MayHaveUnit e, Quantity f, MayHaveUnit f, Concept f, Eq f,
  Quantity h, MayHaveUnit h, Quantity i, MayHaveUnit i,
  HasUID j, Constrained j) => 
  { _sys         :: a
  , _kind        :: b
  , _authors     :: People
  , _purpose     :: Purpose
  , _background  :: Background
  , _scope       :: Scope
  , _motivation  :: Motivation
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


-- | Project Example purpose.
type Purpose = [Sentence]
-- | Project Example background information, used in the 'What' section of README.
type Background = [Sentence]
-- | Project Example scope.
type Scope = [Sentence]
-- | Project Example motivation.
type Motivation = [Sentence]

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
  | cp /= EQ  = cp
  | y1 /= y2  = y1 `compare` y2
  | otherwise = t1 `compare` t2
  where
    (a1, y1, t1) = getAuthorYearTitle c1
    (a2, y2, t2) = getAuthorYearTitle c2

    cp = comparePeople a1 a2

-- | Search for the Author, Year, and Title of a Citation-like data type, and
-- error out if it doesn't have them.
getAuthorYearTitle :: HasFields c => c -> (People, Int, String)
getAuthorYearTitle c = (a, y, t)
  where
    fs = c ^. getFields

    justAuthor (Author x) = Just x
    justAuthor _          = Nothing

    as = mapMaybe justAuthor fs
    a = if not (null as) then head as else error "No author found"

    justYear (Year x) = Just x
    justYear _        = Nothing

    ys = mapMaybe justYear fs
    y = if not (null ys) then head ys else error "No year found"

    justTitle (Title x) = Just x
    justTitle _         = Nothing

    ts = mapMaybe justTitle fs
    t = if not (null ts) then head ts else error "No title found"

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
makeClassy ''SystemInformation

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