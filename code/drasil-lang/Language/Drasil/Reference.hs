{-# Language TemplateHaskell #-}
module Language.Drasil.Reference(makeRef2, makeRef2S, makeCite,
  makeCiteS, ReferenceDB, citationsFromBibMap, citationRefTable, assumpRefTable,
  assumptionsFromDB, rdb, RefBy(..), Referable(..), RefMap, simpleMap,
  HasConceptRefs(conceptRefTable),
  assumpDB, AssumpMap, assumpLookup, HasAssumpRefs) where

import Control.Lens ((^.), Simple, Lens, makeLenses)
import Data.Function (on)
import Data.List (concatMap, find, groupBy, sortBy)
import qualified Data.Map as Map

import Language.Drasil.Chunk.AssumpChunk as A (AssumpChunk)
import Language.Drasil.Chunk.Citation as Ci (BibRef, citeID, Citation)
import Language.Drasil.Chunk.Concept (ConceptInstance)
import Language.Drasil.Chunk.DataDefinition (DataDefinition)
import Language.Drasil.Chunk.GenDefn (GenDefn)
import Language.Drasil.Chunk.InstanceModel (InstanceModel)
import Language.Drasil.Chunk.Theory (TheoryModel)
import Language.Drasil.Classes.Core (HasUID(uid), HasRefAddress(getRefAdd),
  HasShortName(shortname))
import Language.Drasil.Classes.Citations (HasFields(getFields))
import Language.Drasil.Classes (ConceptDomain(cdom), abrv)
import Language.Drasil.Data.Citation(CiteField(Author, Title, Year))
import Language.Drasil.Document (Section(Section))
import Language.Drasil.Document.Core (LabelledContent(..), RawContent(..))
import Language.Drasil.Label.Type (LblType(RP,Citation), IRefProg,
  prepend, name, raw, (+::+), defer)
import Language.Drasil.People (People, comparePeople)
import Language.Drasil.RefProg (Reference(Reference))
import Language.Drasil.Sentence (Sentence(Ref))
import Language.Drasil.UID (UID)

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

data RefBy = ByName
           | ByNum -- If applicable

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

assumpLookup :: HasUID c => c -> AssumpMap -> (AssumpChunk, Int)
assumpLookup a m = getS $ Map.lookup (a ^. uid) m
  where getS (Just x) = x
        getS Nothing = error $ "Assumption: " ++ (a ^. uid) ++
          " referencing information not found in Assumption Map"

-- Classes and instances --
class HasAssumpRefs s where
  assumpRefTable :: Simple Lens s AssumpMap
class HasCitationRefs s where
  citationRefTable :: Simple Lens s BibMap
class HasConceptRefs s where
  conceptRefTable :: Simple Lens s ConceptMap

instance HasAssumpRefs   ReferenceDB where assumpRefTable = assumpDB
instance HasCitationRefs ReferenceDB where citationRefTable = citationDB
instance HasConceptRefs  ReferenceDB where conceptRefTable = conceptDB

class HasUID s => Referable s where
  refAdd    :: s -> String  -- The referencing address (what we're linking to).
                            -- Only visible in the source (tex/html).
  renderRef :: s -> LblType -- alternate

instance Referable AssumpChunk where
  refAdd    x = getRefAdd x
  renderRef l = RP (prepend $ abrv l) (refAdd l)

instance Referable Section where
  refAdd    (Section _ _ lb ) = getRefAdd lb
  renderRef (Section _ _ lb)  = RP (raw "Section: " +::+ name) (getRefAdd lb)

instance Referable Citation where
  refAdd    c = c ^. citeID -- citeID should be unique.
  renderRef c = Citation $ refAdd c

instance Referable TheoryModel where
  refAdd    t = getRefAdd t
  renderRef l = RP (prepend $ abrv l) (getRefAdd l)

instance Referable GenDefn where
  refAdd    g = getRefAdd g
  renderRef l = RP (prepend $ abrv l) (getRefAdd l)

instance Referable DataDefinition where
  refAdd    d = getRefAdd d
  renderRef l = RP (prepend $ abrv l) (getRefAdd l)

instance Referable InstanceModel where
  refAdd    i = getRefAdd i
  renderRef l = RP (prepend $ abrv l) (getRefAdd l)

instance Referable ConceptInstance where
  refAdd l    = l ^. uid
  renderRef l = RP ((defer $ sDom $ cdom l) +::+ raw ": " +::+ name) (l ^. uid)

instance Referable LabelledContent where
  refAdd     (LblC lb _) = getRefAdd lb
  renderRef  (LblC lb c) = RP (refLabelledCon c) (getRefAdd lb)

refLabelledCon :: RawContent -> IRefProg
refLabelledCon (Table _ _ _ _)       = raw "Table:" +::+ name 
refLabelledCon (Figure _ _ _)        = raw "Fig:" +::+ name
refLabelledCon (Graph _ _ _ _)       = raw "Fig:" +::+ name
refLabelledCon (Defini _ _)          = raw "Def:" +::+ name
refLabelledCon (Assumption _ _)      = raw "Assump:" +::+ name
refLabelledCon (EqnBlock _)          = raw "EqnB:" +::+ name
refLabelledCon (Enumeration _)       = raw "Lst:" +::+ name 
refLabelledCon (Paragraph _)         = error "Shouldn't reference paragraphs"
refLabelledCon (Bib _)               = error $ 
    "Bibliography list of references cannot be referenced. " ++
    "You must reference the Section or an individual citation."

uidSort :: HasUID c => c -> c -> Ordering
uidSort = compare `on` (^. uid)

sDom :: [UID] -> UID
sDom [d] = d
sDom d = error $ "Expected ConceptDomain to have a single domain, found " ++
  show (length d) ++ " instead."

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

citationsFromBibMap :: BibMap -> [Citation]
citationsFromBibMap bm = sortBy compareAuthYearTitle citations
  where citations :: [Citation]
        citations = map fst (Map.elems bm)

assumptionsFromDB :: AssumpMap -> [AssumpChunk]
assumptionsFromDB am = dropNums $ sortBy (compare `on` snd) assumptions
  where assumptions = Map.elems am
        dropNums = map fst

makeRef2 :: (Referable l, HasShortName l) => l -> Reference
makeRef2 l = Reference (l ^. uid) (renderRef l) (shortname l)

makeRef2S :: (Referable l, HasShortName l) => l -> Sentence
makeRef2S = Ref . makeRef2

-- Here we don't use the Lenses as constraints, we really do want a Citation.
makeCite :: Citation -> Reference
makeCite l = Reference (l ^. uid) (renderRef l) (shortname l)

makeCiteS :: Citation -> Sentence
makeCiteS = Ref . makeCite

