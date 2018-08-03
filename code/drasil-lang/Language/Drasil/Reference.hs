{-# Language TemplateHaskell #-}
module Language.Drasil.Reference where

import Language.Drasil.RefTypes (RefType(..), DType(..), ReqType(..))
import Control.Lens ((^.), Simple, Lens, makeLenses)
import Data.Function (on)
import Data.List (concatMap, find, groupBy, partition, sortBy)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

import Language.Drasil.Chunk.AssumpChunk as A (AssumpChunk)
import Language.Drasil.Chunk.Change as Ch (Change(..), ChngType(..))
import Language.Drasil.Chunk.Citation as Ci (BibRef, Citation(citeID), CiteField(Author, Title, Year), HasFields(getFields))
import Language.Drasil.Chunk.Concept (ConceptInstance)
import Language.Drasil.Chunk.Eq (QDefinition)
import Language.Drasil.Chunk.GenDefn (GenDefn)
import Language.Drasil.Chunk.Goal as G (Goal)
import Language.Drasil.Chunk.InstanceModel (InstanceModel)
import Language.Drasil.Chunk.ReqChunk as R (ReqChunk(..))
import Language.Drasil.Chunk.PhysSystDesc as PD (PhysSystDesc)
import Language.Drasil.Chunk.ShortName (HasShortName(shortname), ShortName, getStringSN, shortname')
import Language.Drasil.Chunk.Theory (TheoryModel)
import Language.Drasil.Classes (ConceptDomain(cdom), HasUID(uid), HasLabel(getLabel), HasRefAddress(getRefAdd))
import Language.Drasil.Document (Section(Section), getDefName, repUnd, setSN)
import Language.Drasil.Document.Core (RawContent(..), LabelledContent(..))
import Language.Drasil.People (People, comparePeople)
import Language.Drasil.Spec (Sentence((:+:), Ref, S))
import Language.Drasil.UID (UID)
import Language.Drasil.Chunk.DataDefinition (DataDefinition)
import Language.Drasil.Label.Core (Label(..), getAdd)

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
  cdl x = sDom $ x ^. cdom where
    sDom [d] = d
    sDom d = error $ "Expected ConceptDomain for: " ++ (x ^. uid) ++
                     " to have a single domain, found " ++ show (length d) ++
                     " instead."

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
  refAdd g = "GS:" ++ (getAdd ((g ^. getLabel) ^. getRefAdd))
  rType _ = Goal

instance Referable PhysSystDesc where
  refAdd p = "PS:" ++ (getAdd ((p ^. getLabel) ^. getRefAdd))
  rType _ = PSD

instance Referable AssumpChunk where
  refAdd  x = getAdd ((x ^. getLabel) ^. getRefAdd)
  rType   _ = Assump

instance Referable ReqChunk where
  refAdd  r@(RC _ rt _ _) = show rt ++ ":" ++ (getAdd ((r ^. getLabel) ^. getRefAdd))
  rType   (RC _ FR _ _)   = Req FR
  rType   (RC _ NFR _ _)  = Req NFR

instance Referable Change where
  refAdd r@(ChC _ rt _ _)    = show rt ++ ":" ++ (getAdd ((r ^. getLabel) ^. getRefAdd))
  rType (ChC _ Likely _ _)   = LC
  rType (ChC _ Unlikely _ _) = UC

instance Referable Section where
  refAdd  (Section _ _ lb) = "Sec:" ++ (getAdd (lb ^. getRefAdd))
  rType   _               = Sect

instance Referable Citation where
  refAdd c = concatMap repUnd $ citeID c -- citeID should be unique.
  rType _ = Cite

instance Referable TheoryModel where
  refAdd  t = "T:" ++ (getAdd ((t ^. getLabel) ^. getRefAdd))
  rType   _ = Def TM

instance Referable GenDefn where
  refAdd  g = "GD:" ++ (getAdd ((g ^. getLabel) ^. getRefAdd))
  rType   _ = Def General

instance Referable QDefinition where -- FIXME: This could lead to trouble; need
                                     -- to ensure sanity checking when building
                                     -- Refs. Double-check QDef is a DD before allowing
                                     -- FIXME: QDefinition should no longer be referable
                                     -- after its Label is removed.
  refAdd  d = "DD:" ++ (getAdd ((d ^. getLabel) ^. getRefAdd))
  rType   _ = Def DD

instance Referable DataDefinition where
  refAdd  d = "DD:" ++ (getAdd ((d ^. getLabel) ^. getRefAdd))
  rType   _ = Def DD

instance Referable InstanceModel where
  refAdd  i = "IM:" ++ (getAdd ((i ^. getLabel) ^. getRefAdd))
  rType   _ = Def Instance

instance Referable ConceptInstance where
  refAdd i = i ^. uid
  rType _ = Def Instance --note: not actually used

--FIXME: assumes reference to a direct Label is for a section
--Should refer to an object WITH a variable.
--Can be removed once sections have labels.
instance Referable Label where
  refAdd lb@(Lbl _ _ _ x) = getAcc x ++ (getAdd (lb ^. getRefAdd))
    where
      getAcc :: RefType -> String
      getAcc Sect   = "Sec:"
      getAcc Assump = ""
      getAcc LC     = "LC:"
      getAcc UC     = "UC:"
      getAcc Goal   = "GS:"
      getAcc PSD   = "PS:"
      getAcc x = show x ++ ":"
  rType  (Lbl _ _ _ x) = x

instance Referable LabelledContent where
  refAdd (LblC lb c) = temp' (getAdd (lb ^. getRefAdd)) c 
  rType  (LblC _ c)  = temp c

temp' :: String -> RawContent -> String
temp' r (Table _ _ _ _)      = "Table:" ++ r
temp' r (Figure _ _ _)       = "Figure:" ++ r
temp' r (Graph _ _ _ _)      = "Figure:" ++ r
temp' r (EqnBlock _)         = "Equation:" ++ r
temp' _ (Definition d)       = getDefName d --fixme: to be removed
temp' r (Defnt _ _)          = concatMap repUnd r
temp' r (Requirement rc)     = r
temp' r (Assumption ca)      = r
temp' r (Change lcc)         = r
temp' _ (Enumeration _)      = error "Shouldn't reference lists"
temp' _ (Paragraph _)        = error "Shouldn't reference paragraphs"
temp' r (Bib _)              = r

temp :: RawContent -> RefType
temp (Table _ _ _ _)       = Tab
temp (Figure _ _ _)        = Fig
temp (Graph _ _ _ _)       = Fig
temp (Definition _)        = Def DD --fixme: to be removed completely
temp (Defnt x _)           = Def x
temp (Requirement r)       = rType r
temp (Assumption a)        = rType a
temp (Change l)            = rType l
temp (EqnBlock _)          = EqnB
temp (Enumeration _)       = error "Shouldn't reference lists" 
temp (Paragraph _)         = error "Shouldn't reference paragraphs"
temp (Bib _)               = error $
    "Bibliography list of references cannot be referenced. " ++
    "You must reference the Section or an individual citation."
temp _                     =
    error "Attempting to reference unimplemented reference type"

uidSort :: HasUID c => c -> c -> Ordering
uidSort = compare `on` (^. uid)

compareAuthYearTitle :: (HasFields c) => c -> c -> Ordering
compareAuthYearTitle c1 c2
  | comparePeople (getAuthor c1) (getAuthor c2) /= Nothing = fromJust $ comparePeople (getAuthor c1) (getAuthor c2)
  | getYear c1  /= getYear c2  = getYear c1  `compare` getYear c2
  | getTitle c1 /= getTitle c2 = getTitle c1 `compare` getTitle c2
  | otherwise                      = error "Couldn't sort authors"

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
makeRef r = customRef r (shortname' $ concatMap repUnd $ getStringSN (r ^. shortname))

--FIXME: needs design (HasShortName, Referable only possible when HasLabel)
mkRefFrmLbl :: (HasLabel l, HasShortName l, Referable l) => l -> Sentence
mkRefFrmLbl r = makeRef r

--FIXME: should be removed from Examples once sections have labels
midRef :: Label -> Sentence
midRef r = customRef r (r ^. shortname)

-- | Create a reference with a customized 'ShortName'
customRef :: (HasShortName l, Referable l) => l -> ShortName -> Sentence
customRef r n = Ref (rType r) (concatMap repUnd (refAdd r)) (shortname' $ temp (rType r) n)
  where
    temp :: RefType -> ShortName -> String
    temp (Def dtp) s = setSN (getDefName dtp) s
    temp (Req rq) s  = setSN (getReqName rq) s
    temp LC s        = setSN "LC:" s
    temp Assump s    = setSN "A:" s
    temp UC s        = setSN "UC:" s
    temp Goal s      = setSN "GS:" s
    temp PSD s       = setSN "PS:" s
    temp _ s         = setSN "" s

getReqName :: ReqType -> String
getReqName FR  = "FR:"
getReqName NFR = "NFR:"

-- This works for passing the correct id to the reference generator for Assumptions,
-- Requirements and Likely Changes but I question whether we should use it.
-- Pass it the item to be referenced and the enumerated list of the respective
-- contents for that file. Change rType values to implement.
