{-# Language TemplateHaskell #-}
module Language.Drasil.Reference(makeRef, makeRefS, makeRef2S, makeCite,
  ReferenceDB, citationsFromBibMap, citationRefTable, assumpRefTable,
  assumptionsFromDB, rdb, RefBy(..), Referable(..), RefMap, simpleMap,
  assumpDB, AssumpMap, assumpLookup, HasAssumpRefs) where

import Control.Lens ((^.), Simple, Lens, makeLenses)
import Data.Function (on)
import Data.List (concatMap, find, groupBy, sortBy)
import qualified Data.Map as Map

import Language.Drasil.Chunk.AssumpChunk as A (AssumpChunk)
import Language.Drasil.Chunk.Citation as Ci (BibRef, citeID, Citation)
import Language.Drasil.Data.Citation(CiteField(Author, Title, Year))
import Language.Drasil.Chunk.Concept (ConceptInstance)
import Language.Drasil.Chunk.DataDefinition (DataDefinition)
import Language.Drasil.Chunk.GenDefn (GenDefn)
import Language.Drasil.Chunk.InstanceModel (InstanceModel)
import Language.Drasil.Chunk.Theory (TheoryModel)
import Language.Drasil.Classes (ConceptDomain(cdom), HasUID(uid), HasLabel(getLabel),
  HasRefAddress(getRefAdd), HasShortName(shortname), HasFields(getFields), CommonIdea(abrv))
import Language.Drasil.Document (Section(Section))
import Language.Drasil.Document.Core (RawContent(..), LabelledContent(..))
import Language.Drasil.Label.Core (Label(..))
import Language.Drasil.Label.Type (getAdd)
import Language.Drasil.Label (getDefName, getReqName)
import Language.Drasil.People (People, comparePeople)
import Language.Drasil.RefProg (RefProg(RP, Citation), Reference2(Reference2), (+::+), name,
  prepend, raw, IRefProg)
import Language.Drasil.RefProg as RP (defer)  -- FIXME: Remove prefix once SN.defer is no longer needed.
import Language.Drasil.RefTypes (RefType(..), DType(..), Reference(Reference))
import Language.Drasil.ShortName ( ShortName, getStringSN, shortname', concatSN)
import Language.Drasil.ShortName as SN (defer)
import Language.Drasil.Sentence (Sentence((:+:), S, Ref, Ref2))
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
rdb assumps citations con = RDB
  (simpleMap assumps)
  (bibMap citations)
  (conceptMap con)

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
  cdl x = sDom $ x ^. cdom

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

conceptLookup :: HasUID c => c -> ConceptMap -> (ConceptInstance, Int)
conceptLookup c = maybe (error $ "ConceptInstance: " ++ (c ^. uid) ++
          " referencing information not found in Concept Map") id .
          Map.lookup (c ^. uid)

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


--FIXME: "class (HasLabel s) => Referable s where" instead?
class Referable s where
  refAdd    :: s -> String  -- The plaintext referencing address (what we're linking to).
                            -- Should be string with no spaces/special chars.
                            -- Only visible in the source (tex/html).
  rType     :: s -> RefType -- The reference type (referencing namespace?)
  renderRef :: s -> RefProg -- A program to render a shortname

instance Referable AssumpChunk where
  refAdd    x = getAdd ((x ^. getLabel) ^. getRefAdd)
  rType     _ = Assump
  renderRef l = RP $ prepend $ abrv l

instance Referable Section where
  refAdd    (Section _ _ lb ) = getAdd (lb ^. getRefAdd)
  rType     _                 = Sect
  renderRef _                 = RP $ raw "Section: " +::+ name

instance Referable Citation where
  refAdd    c = citeID c -- citeID should be unique.
  rType     _ = Cite
  renderRef _ = Citation -- $ prepend $ abrv l

instance Referable TheoryModel where
  refAdd    t = getAdd ((t ^. getLabel) ^. getRefAdd)
  rType     _ = Def TM
  renderRef l = RP $ prepend $ abrv l

instance Referable GenDefn where
  refAdd    g = getAdd ((g ^. getLabel) ^. getRefAdd)
  rType     _ = Def General
  renderRef l = RP $ prepend $ abrv l

instance Referable DataDefinition where
  refAdd    d = getAdd ((d ^. getLabel) ^. getRefAdd)
  rType     _ = Def DD
  renderRef l = RP $ prepend $ abrv l

instance Referable InstanceModel where
  refAdd    i = getAdd ((i ^. getLabel) ^. getRefAdd)
  rType     _ = Def Instance
  renderRef l = RP $ prepend $ abrv l

instance Referable ConceptInstance where
  refAdd    i = i ^. uid
  rType     _ = error "makeRef, makeRefS, and rType are deprecated for ConceptInstance. Use the makeRef2S, renderRef instead."
  renderRef l = RP $ (RP.defer $ sDom $ l ^. cdom) +::+ raw ": " +::+ name

--Should refer to an object WITH a variable.
--Can be removed once sections have labels.
instance Referable Label where
  refAdd    lb@(Lbl _ _ _ _) = getAdd (lb ^. getRefAdd)
  rType     (Lbl _ _ _ x)    = x --FIXME: is a hack; see #971
  renderRef _                = RP name -- FIXME

instance Referable LabelledContent where
  refAdd     (LblC lb _) = getAdd (lb ^. getRefAdd)
  rType      (LblC _ c)  = temp c
  renderRef  (LblC _ c)  = RP $ refLabelledCon c

refLabelledCon :: RawContent -> IRefProg
refLabelledCon (Table _ _ _ _)       = raw "Table:" +::+ name 
refLabelledCon (Figure _ _ _)        = raw "Fig:" +::+ name
refLabelledCon (Graph _ _ _ _)       = raw "Fig:" +::+ name
refLabelledCon (Defini _ _)          = raw "Def:" +::+ name
refLabelledCon (Assumption _ _ _)    = raw "Assump:" +::+ name
refLabelledCon (EqnBlock _)          = raw "EqnB:" +::+ name
refLabelledCon (Enumeration _)       = raw "Lst:" +::+ name 
refLabelledCon (Paragraph _)         = error "Shouldn't reference paragraphs"
refLabelledCon (Bib _)               = error $ 
    "Bibliography list of references cannot be referenced. " ++
    "You must reference the Section or an individual citation."

temp :: RawContent -> RefType
temp (Table _ _ _ _)       = Tab
temp (Figure _ _ _)        = Fig
temp (Graph _ _ _ _)       = Fig
temp (Defini x _)          = Def x
temp (Assumption _ _ _)    = Assump -- hard-code, will disappear
temp (EqnBlock _)          = EqnB
temp (Enumeration _)       = Lst 
temp (Paragraph _)         = error "Shouldn't reference paragraphs"
temp (Bib _)               = error $
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
getTitle c = getStr $ maybe (error "No title found") (\(Title x) -> x) (find isTitle (c ^. getFields))
  where isTitle :: CiteField -> Bool
        isTitle (Title _) = True
        isTitle _         = False
        getStr :: Sentence -> String
        getStr (S s) = s
        getStr ((:+:) s1 s2) = getStr s1 ++ getStr s2
        getStr _ = error "Term is not a string"

citationsFromBibMap :: BibMap -> [Citation]
citationsFromBibMap bm = sortBy compareAuthYearTitle citations
  where citations :: [Citation]
        citations = map fst (Map.elems bm)

assumptionsFromDB :: AssumpMap -> [AssumpChunk]
assumptionsFromDB am = dropNums $ sortBy (compare `on` snd) assumptions
  where assumptions = Map.elems am
        dropNums = map fst

makeRef2S :: (Referable l, HasShortName l) => l -> Sentence
makeRef2S l = Ref2 $ Reference2 (renderRef l) (refAdd l) (l ^. shortname)

-- Here we don't use the Lenses as constraints, we really do want a Citation.
makeCite :: Citation -> Sentence
makeCite l = Ref2 $ Reference2 Citation (refAdd l) (l ^. shortname)

-- | Create References to a given 'LayoutObj'
-- This should not be exported to the end-user, but should be usable
-- within the recipe (we want to force reference creation to check if the given
-- item exists in our database of referable objects.
--FIXME: completely shift to being `HasLabel` since customref checks for 
--  `HasShortName` and `Referable`?
makeRef :: (HasShortName l, Referable l) => l -> Reference
makeRef r = customRef r (r ^. shortname)

makeRefS :: (HasShortName l, Referable l) => l -> Sentence
makeRefS = Ref . makeRef

--FIXME: should be removed from Examples once sections have labels
-- | Create a reference with a customized 'ShortName'
customRef :: (HasShortName l, Referable l) => l -> ShortName -> Reference
customRef r n = Reference (fixupRType $ rType r) (refAdd r) (getAcc' (rType r) n)
  where 
    getAcc' :: RefType -> ShortName -> ShortName
    getAcc' (Def dtp) sn = shortname' $ (getDefName dtp) ++ " " ++ (getStringSN sn)
    getAcc' LCh       sn = shortname' $ "LC: " ++ (getStringSN sn)
    getAcc' UnCh      sn = shortname' $ "UC: " ++ (getStringSN sn)
    getAcc' Assump    sn = shortname' $ "A: " ++ (getStringSN sn)
    getAcc' (Req rq)  sn = shortname' $ (getReqName rq)  ++ " " ++ (getStringSN sn)
    getAcc' (DeferredCC u) s = concatSN (SN.defer u) s
    getAcc' _         sn = sn
    fixupRType (DeferredCC _) = Blank  -- FIXME: This is a hack
    fixupRType a = a

-- This works for passing the correct id to the reference generator for Assumptions,
-- Requirements and Likely Changes but I question whether we should use it.
-- Pass it the item to be referenced and the enumerated list of the respective
-- contents for that file. Change rType values to implement.
