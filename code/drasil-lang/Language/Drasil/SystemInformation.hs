{-# LANGUAGE GADTs, TemplateHaskell #-}
module Language.Drasil.SystemInformation(SystemInformation(..), Block(..), citeDB,
  ReferenceDB, citationsFromBibMap, citationDB,
  assumptionsFromDB, rdb, RefMap, simpleMap,
  conceptDB, assumpDB
  ) where

import Language.Drasil.Chunk.AssumpChunk as A (AssumpChunk)
import Language.Drasil.Chunk.Citation (BibRef, Citation)
import Language.Drasil.Chunk.Concept (ConceptInstance)
import Language.Drasil.Chunk.Concept.Core (sDom)
import Language.Drasil.Chunk.Eq (QDefinition)
import Language.Drasil.Chunk.UnitDefn (MayHaveUnit)
import Language.Drasil.ChunkDB (ChunkDB)
import Language.Drasil.Classes.Core (HasUID(uid))
import Language.Drasil.Classes (CommonIdea, Concept, ConceptDomain(cdom), Constrained, 
  Idea, IsUnit, Quantity)
import Language.Drasil.Classes.Citations (HasFields(getFields))
import Language.Drasil.Data.Citation(CiteField(Author, Title, Year))
import Language.Drasil.People (HasName, People, comparePeople)
import Language.Drasil.UID (UID)

import Control.Lens ((^.), makeLenses)
import Data.Function (on)
import Data.List (concatMap, find, groupBy, sortBy)
import qualified Data.Map as Map

import Language.Drasil.Chunk.DataDefinition (DataDefinition)

-- | Data structure for holding all of the requisite information about a system
-- to be used in artefact generation
data SystemInformation where
--FIXME:
--There should be a way to remove redundant "Quantity" constraint.
-- I'm thinking for getting concepts that are also quantities, we could
-- use a lookup of some sort from their internal (Drasil) ids.
 SI :: (CommonIdea a, Idea a, Idea b, HasName c, IsUnit d,
  Quantity e, Eq e, MayHaveUnit e, Quantity f, MayHaveUnit f, Concept f, Eq f,
  Quantity h, MayHaveUnit h, Quantity i, MayHaveUnit i,
  HasUID j, Constrained j) => 
  { _sys :: a
  , _kind :: b
  , _authors :: [c]
  , _units :: [d]
  , _quants :: [e]
  , _concepts :: [f]
  , _definitions :: [QDefinition] --FIXME: will be removed upon migration to use of [DataDefinition] below
  , _datadefs :: [DataDefinition]
  , _inputs :: [h]
  , _outputs :: [i]
  , _defSequence :: [Block QDefinition]
  , _constraints :: [j] --TODO: Add SymbolMap OR enough info to gen SymbolMap
  , _constants :: [QDefinition]
  , _sysinfodb :: ChunkDB
  , _usedinfodb :: ChunkDB
  , refdb :: ReferenceDB
  } -> SystemInformation
  
-- | for listing QDefs in SystemInformation
data Block a = Coupled a a [a] | Parallel a [a]

-- | Helper for extracting bibliography
citeDB :: SystemInformation -> BibRef
citeDB si = citationsFromBibMap (_citationDB (refdb si))

citationsFromBibMap :: BibMap -> [Citation]
citationsFromBibMap bm = sortBy compareAuthYearTitle citations
  where citations :: [Citation]
        citations = map fst (Map.elems bm)

compareAuthYearTitle :: (HasFields c) => c -> c -> Ordering
compareAuthYearTitle c1 c2 =
  if cp /= EQ then cp
  else if y1 /= y2 then y1 `compare` y2
  else if t1 /= t2 then t1 `compare` t2
  else error "Couldn't sort authors"
  where
    cp = comparePeople (getAuthor c1) (getAuthor c2)
    y1 = getYear c1
    y2 = getYear c2
    t1 = getTitle c1
    t2 = getTitle c2

getAuthor :: (HasFields c) => c -> People
getAuthor c = maybe (error "No author found") (\(Author x) -> x) (find isAuthor (c ^. getFields))
  where isAuthor :: CiteField -> Bool
        isAuthor (Author _) = True
        isAuthor _          = False

getYear :: (HasFields c) => c -> Int
getYear c = maybe (error "No year found") (\(Year x) -> x) (find isYear (c ^. getFields))
  where isYear :: CiteField -> Bool
        isYear (Year _) = True
        isYear _        = False

getTitle :: (HasFields c) => c -> String
getTitle c = maybe (error "No title found") (\(Title x) -> x) (find isTitle (c ^. getFields))
  where isTitle :: CiteField -> Bool
        isTitle (Title _) = True
        isTitle _         = False

-- | Database for maintaining references.
-- The Int is that reference's number.
-- Maintains access to both num and chunk for easy reference swapping
-- between number and shortname/refname when necessary (or use of number
-- if no shortname exists)
type RefMap a = Map.Map UID (a, Int)

-- | Assumption Database
type AssumpMap = RefMap AssumpChunk
-- | Citation Database (bibliography information)
type BibMap = RefMap Citation
-- | ConceptInstance Database
type ConceptMap = RefMap ConceptInstance


-- | Database for internal references.
data ReferenceDB = RDB -- organized in order of appearance in SmithEtAl template
  { _assumpDB :: AssumpMap
  , _citationDB :: BibMap
  , _conceptDB :: ConceptMap
  }

makeLenses ''ReferenceDB

rdb :: [AssumpChunk] -> BibRef -> [ConceptInstance] -> ReferenceDB
rdb assumps citations con = RDB (simpleMap assumps) (bibMap citations) (conceptMap con)

simpleMap :: HasUID a => [a] -> RefMap a
simpleMap xs = Map.fromList $ zip (map (^. uid) xs) (zip xs [1..])

bibMap :: [Citation] -> BibMap
bibMap cs = Map.fromList $ zip (map (^. uid) scs) (zip scs [1..])
  where scs :: [Citation]
        scs = sortBy compareAuthYearTitle cs
        -- Sorting is necessary if using elems to pull all the citations
        -- (as it sorts them and would change the order).
        -- We can always change the sorting to whatever makes most sense

conGrp :: ConceptInstance -> ConceptInstance -> Bool
conGrp a b = cdl a == cdl b where
  cdl :: ConceptInstance -> UID
  cdl = sDom . cdom

conceptMap :: [ConceptInstance] -> ConceptMap
conceptMap cs = Map.fromList $ zip (map (^. uid) (concat grp)) $ concatMap
  (\x -> zip x [1..]) grp
  where grp :: [[ConceptInstance]]
        grp = groupBy conGrp $ sortBy uidSort cs

uidSort :: HasUID c => c -> c -> Ordering
uidSort = compare `on` (^. uid)

assumptionsFromDB :: AssumpMap -> [AssumpChunk]
assumptionsFromDB am = dropNums $ sortBy (compare `on` snd) assumptions
  where assumptions = Map.elems am
        dropNums = map fst

