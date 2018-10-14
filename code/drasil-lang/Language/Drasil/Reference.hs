{-# Language TemplateHaskell #-}
module Language.Drasil.Reference where

import Language.Drasil.RefTypes (RefType(..), DType(..), ReqType(..))
import Control.Lens ((^.), Simple, Lens, makeLenses)
import Data.Function (on)
import Data.List (concatMap, find, groupBy, partition, sortBy)
import qualified Data.Map as Map

import Language.Drasil.Chunk.AssumpChunk as A (AssumpChunk)
import Language.Drasil.Chunk.Change as Ch (Change(..), ChngType(..))
import Language.Drasil.Chunk.Citation as Ci (BibRef, Citation(citeID), CiteField(Author, Title, Year), HasFields(getFields))
import Language.Drasil.Chunk.Concept (ConceptInstance)
import Language.Drasil.Chunk.GenDefn (GenDefn)
import Language.Drasil.Chunk.Goal as G (Goal)
import Language.Drasil.Chunk.InstanceModel (InstanceModel)
import Language.Drasil.Chunk.ReqChunk as R (ReqChunk(..))
import Language.Drasil.Chunk.PhysSystDesc as PD (PhysSystDesc)
import Language.Drasil.ShortName ( ShortName, getStringSN, shortname', concatSN, defer)
import Language.Drasil.Chunk.Theory (TheoryModel)
import Language.Drasil.Classes (ConceptDomain(cdom), HasUID(uid), HasLabel(getLabel),
  HasRefAddress(getRefAdd), HasShortName(shortname))
import Language.Drasil.Document (Section(Section))
import Language.Drasil.Document.Core (RawContent(..), LabelledContent(..))
import Language.Drasil.People (People, comparePeople)
import Language.Drasil.Spec (Sentence((:+:), Ref, S))
import Language.Drasil.UID (UID)
import Language.Drasil.Chunk.DataDefinition (DataDefinition)
import Language.Drasil.Label.Core (Label(..), getAdd)
import Language.Drasil.Label (getDefName, getReqName)

-- | Database for maintaining references.
-- The Int is that reference's number.
-- Maintains access to both num and chunk for easy reference swapping
-- between number and shortname/refname when necessary (or use of number
-- if no shortname exists)
type RefMap a = Map.Map UID (a, Int)

-- | Physical System Description Database
type PhysSystDescMap = RefMap PhysSystDesc
-- | Goal Statement Database
type GoalMap = RefMap Goal
-- | Assumption Database
type AssumpMap = RefMap AssumpChunk
-- | Requirement (functional/non-functional) Database
type ReqMap = RefMap ReqChunk
-- | Change (likely/unlikely) Database
type ChangeMap = RefMap Change
-- | Citation Database (bibliography information)
type BibMap = RefMap Citation
-- | ConceptInstance Database
type ConceptMap = RefMap ConceptInstance


-- | Database for internal references.
data ReferenceDB = RDB -- organized in order of appearance in SmithEtAl template
  { _physSystDescDB :: PhysSystDescMap
  , _goalDB :: GoalMap
  , _assumpDB :: AssumpMap
  , _reqDB :: ReqMap
  , _changeDB :: ChangeMap
  , _citationDB :: BibMap
  , _conceptDB :: ConceptMap
  }

makeLenses ''ReferenceDB

data RefBy = ByName
           | ByNum -- If applicable

rdb :: [PhysSystDesc] -> [Goal] -> [AssumpChunk] -> [ReqChunk] -> [Change] ->
  BibRef -> [ConceptInstance] -> ReferenceDB
rdb psds goals assumps reqs changes citations con = RDB
  (simpleMap psds)
  (simpleMap goals)
  (simpleMap assumps)
  (reqMap reqs)
  (changeMap changes)
  (bibMap citations)
  (conceptMap con)

simpleMap :: HasUID a => [a] -> RefMap a
simpleMap xs = Map.fromList $ zip (map (^. uid) xs) (zip xs [1..])

reqMap :: [ReqChunk] -> ReqMap
reqMap rs = Map.fromList $ zip (map (^. uid) (frs ++ nfrs)) (zip frs [1..] ++
  zip nfrs [1..])
  where (frs, nfrs)  = partition (isFuncRec . reqType) rs
        isFuncRec FR = True
        isFuncRec _  = False

changeMap :: [Change] -> ChangeMap
changeMap cs = Map.fromList $ zip (map (^. uid) (lcs ++ ulcs))
  (zip lcs [1..] ++ zip ulcs [1..])
  where (lcs, ulcs) = partition (isLikely . chngType) cs
        isLikely Likely = True
        isLikely _ = False

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

psdLookup :: HasUID c => c -> PhysSystDescMap -> (PhysSystDesc, Int)
psdLookup p m = getS $ Map.lookup (p ^. uid) m
  where getS (Just x) = x
        getS Nothing = error $ "No referencing information found for: " ++
          (p ^. uid) ++ " in PhysSystDesc Map"

goalLookup :: HasUID c => c -> GoalMap -> (Goal, Int)
goalLookup g m = getS $ Map.lookup (g ^. uid) m
  where getS (Just x) = x
        getS Nothing = error $ "No referencing information found for: " ++
          (g ^. uid) ++ " in Goal Map"

assumpLookup :: HasUID c => c -> AssumpMap -> (AssumpChunk, Int)
assumpLookup a m = getS $ Map.lookup (a ^. uid) m
  where getS (Just x) = x
        getS Nothing = error $ "Assumption: " ++ (a ^. uid) ++
          " referencing information not found in Assumption Map"

reqLookup :: HasUID c => c -> ReqMap -> (ReqChunk, Int)
reqLookup r m = getS $ Map.lookup (r ^. uid) m
  where getS (Just x) = x
        getS Nothing = error $ "Requirement: " ++ (r ^. uid) ++
          " referencing information not found in Requirement Map"

changeLookup :: HasUID c => c -> ChangeMap -> (Change, Int)
changeLookup c m = getS $ Map.lookup (c ^. uid) m
  where getS (Just x) = x
        getS Nothing = error $ "Change: " ++ (c ^. uid) ++
          " referencing information not found in Change Map"

citeLookup :: HasUID c => c -> BibMap -> (Citation, Int)
citeLookup c m = getS $ Map.lookup (c ^. uid) m
  where getS (Just x) = x
        getS Nothing = error $ "Change: " ++ (c ^. uid) ++
          " referencing information not found in Change Map"

conceptLookup :: HasUID c => c -> ConceptMap -> (ConceptInstance, Int)
conceptLookup c = maybe (error $ "ConceptInstance: " ++ (c ^. uid) ++
          " referencing information not found in Concept Map") id .
          Map.lookup (c ^. uid)

-- Classes and instances --
class HasAssumpRefs s where
  assumpRefTable :: Simple Lens s AssumpMap
class HasReqRefs s where
  reqRefTable :: Simple Lens s ReqMap
class HasChangeRefs s where
  changeRefTable :: Simple Lens s ChangeMap
class HasCitationRefs s where
  citationRefTable :: Simple Lens s BibMap
class HasGoalRefs s where
  goalRefTable :: Simple Lens s GoalMap
class HasPSDRefs s where
  psdRefTable :: Simple Lens s PhysSystDescMap
class HasConceptRefs s where
  conceptRefTable :: Simple Lens s ConceptMap

instance HasGoalRefs ReferenceDB where goalRefTable = goalDB
instance HasPSDRefs      ReferenceDB where psdRefTable = physSystDescDB
instance HasAssumpRefs   ReferenceDB where assumpRefTable = assumpDB
instance HasReqRefs      ReferenceDB where reqRefTable = reqDB
instance HasChangeRefs   ReferenceDB where changeRefTable = changeDB
instance HasCitationRefs ReferenceDB where citationRefTable = citationDB
instance HasConceptRefs  ReferenceDB where conceptRefTable = conceptDB


--FIXME: "class (HasLabel s) => Referable s where" instead?
class Referable s where
  refAdd  :: s -> String  -- The plaintext referencing address (what we're linking to).
                          -- Should be string with no spaces/special chars.
                          -- Only visible in the source (tex/html).
  rType   :: s -> RefType -- The reference type (referencing namespace?)

instance Referable Goal where
  refAdd g = getAdd ((g ^. getLabel) ^. getRefAdd)
  rType _ = Goal

instance Referable PhysSystDesc where
  refAdd p = getAdd ((p ^. getLabel) ^. getRefAdd)
  rType _ = PSD

instance Referable AssumpChunk where
  refAdd  x = getAdd ((x ^. getLabel) ^. getRefAdd)
  rType   _ = Assump

instance Referable ReqChunk where
  refAdd  r               = getAdd ((r ^. getLabel) ^. getRefAdd)
  rType   (RC _ FR _ _)   = Req FR
  rType   (RC _ NFR _ _)  = Req NFR

instance Referable Change where
  refAdd r                   = getAdd ((r ^. getLabel) ^. getRefAdd)
  rType (ChC _ Likely _ _)   = LCh
  rType (ChC _ Unlikely _ _) = UnCh

instance Referable Section where
  refAdd  (Section _ _ lb) = getAdd (lb ^. getRefAdd)
  rType   _                = Sect

instance Referable Citation where
  refAdd c = citeID c -- citeID should be unique.
  rType _  = Cite

instance Referable TheoryModel where
  refAdd  t = getAdd ((t ^. getLabel) ^. getRefAdd)
  rType   _ = Def TM

instance Referable GenDefn where
  refAdd  g = getAdd ((g ^. getLabel) ^. getRefAdd)
  rType   _ = Def General

instance Referable DataDefinition where
  refAdd  d = getAdd ((d ^. getLabel) ^. getRefAdd)
  rType   _ = Def DD

instance Referable InstanceModel where
  refAdd  i = getAdd ((i ^. getLabel) ^. getRefAdd)
  rType   _ = Def Instance

instance Referable ConceptInstance where
  refAdd i = i ^. uid
  rType i  = DeferredCC $ sDom $ i ^. cdom

--Should refer to an object WITH a variable.
--Can be removed once sections have labels.
instance Referable Label where
  refAdd lb@(Lbl _ _ _) = getAdd (lb ^. getRefAdd)
  rType  (Lbl _ _ _)  = error "should not ask the type of a Label" --FIXME: is a hack; see #971

instance Referable LabelledContent where
  refAdd (LblC lb _) = getAdd (lb ^. getRefAdd)
  rType  (LblC _ c)  = temp c

temp :: RawContent -> RefType
temp (Table _ _ _ _)       = Tab
temp (Figure _ _ _)        = Fig
temp (Graph _ _ _ _)       = Fig
temp (Definition x _)      = Def x
temp (Requirement r)       = rType r
temp (Assumption a)        = rType a
temp (Change l)            = rType l
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

-- | Create References to a given 'LayoutObj'
-- This should not be exported to the end-user, but should be usable
-- within the recipe (we want to force reference creation to check if the given
-- item exists in our database of referable objects.
--FIXME: completely shift to being `HasLabel` since customref checks for 
--  `HasShortName` and `Referable`?
makeRef :: (HasShortName l, Referable l) => l -> Sentence
makeRef r = customRef r (r ^. shortname)

--FIXME: needs design (HasShortName, Referable only possible when HasLabel)
mkRefFrmLbl :: (HasLabel l, HasShortName l, Referable l) => l -> Sentence
mkRefFrmLbl r = makeRef r

--FIXME: should be removed from Examples once sections have labels
-- | Create a reference with a customized 'ShortName'
customRef :: (HasShortName l, Referable l) => l -> ShortName -> Sentence
customRef r n = Ref (fixupRType $ rType r) (refAdd r) (getAcc' (rType r) n)
  where 
    getAcc' :: RefType -> ShortName -> ShortName
    getAcc' (Def dtp) sn = shortname' $ (getDefName dtp) ++ " " ++ (getStringSN sn)
    getAcc' (Req rq)  sn = shortname' $ (getReqName rq)  ++ " " ++ (getStringSN sn)
    getAcc' LCh       sn = shortname' $ "LC: " ++ (getStringSN sn)
    getAcc' UnCh      sn = shortname' $ "UC: " ++ (getStringSN sn)
    getAcc' Assump    sn = shortname' $ "A: " ++ (getStringSN sn)
    getAcc' Goal      sn = shortname' $ "GS: " ++ (getStringSN sn)
    getAcc' PSD       sn = shortname' $ "PS: " ++ (getStringSN sn)
    getAcc' (DeferredCC u) s = concatSN (defer u) s
    getAcc' _         sn = sn
    fixupRType (DeferredCC _) = Blank  -- FIXME: This is a hack
    fixupRType a = a

-- This works for passing the correct id to the reference generator for Assumptions,
-- Requirements and Likely Changes but I question whether we should use it.
-- Pass it the item to be referenced and the enumerated list of the respective
-- contents for that file. Change rType values to implement.
