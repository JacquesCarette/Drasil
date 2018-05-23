{-# Language TemplateHaskell #-}
module Language.Drasil.Reference where

import Language.Drasil.Classes (HasUID(uid))
import Language.Drasil.Chunk.AssumpChunk as A
import Language.Drasil.Chunk.Change as Ch
import Language.Drasil.Chunk.Citation as Ci
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.GenDefn
import Language.Drasil.Chunk.Goal as G
import Language.Drasil.Chunk.InstanceModel
import Language.Drasil.Chunk.PhysSystDesc as PD
import Language.Drasil.Chunk.ReqChunk as R
import Language.Drasil.Chunk.Theory
import Language.Drasil.Document
import Language.Drasil.Spec (Sentence(..))
import Language.Drasil.RefTypes (RefType(..))
import Control.Lens ((^.), Simple, Lens, makeLenses)
import Language.Drasil.Chunk.Attribute.ShortName

import Data.List (partition, sortBy)
import qualified Data.Map as Map
import Data.Function (on)

-- | Database for maintaining references.
-- The Int is that reference's number.
-- Maintains access to both num and chunk for easy reference swapping
-- between number and shortname/refname when necessary (or use of number
-- if no shortname exists)
type RefMap a = Map.Map String (a, Int)

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


-- | Database for internal references.
data ReferenceDB = RDB -- organized in order of appearance in SmithEtAl template
  { _physSystDescDB :: PhysSystDescMap
  , _goalDB :: GoalMap
  , _assumpDB :: AssumpMap
  , _reqDB :: ReqMap
  , _changeDB :: ChangeMap
  , _citationDB :: BibMap
  }

makeLenses ''ReferenceDB

data RefBy = ByName
           | ByNum -- If applicable

rdb :: [PhysSystDesc] -> [Goal] -> [AssumpChunk] -> [ReqChunk] -> [Change] ->
  BibRef -> ReferenceDB
rdb psds goals assumps reqs changes citations = RDB
  (simpleMap psds)
  (simpleMap goals)
  (simpleMap assumps)
  (reqMap reqs)
  (changeMap changes)
  (bibMap citations)

simpleMap :: HasUID a => [a] -> RefMap a
simpleMap xs = Map.fromList $ zip (map (^. uid) xs) (zip xs [1..])

reqMap :: [ReqChunk] -> ReqMap
reqMap rs = Map.fromList $ zip (map (^. uid) (frs ++ nfrs)) ((zip frs [1..]) ++
  (zip nfrs [1..]))
  where (frs, nfrs)  = partition (isFuncRec . reqType) rs
        isFuncRec FR = True
        isFuncRec _  = False

changeMap :: [Change] -> ChangeMap
changeMap cs = Map.fromList $ zip (map (^. uid) (lcs ++ ulcs))
  ((zip lcs [1..]) ++ (zip ulcs [1..]))
  where (lcs, ulcs) = partition (isLikely . chngType) cs
        isLikely Likely = True
        isLikely _ = False

bibMap :: [Citation] -> BibMap
bibMap cs = Map.fromList $ zip (map (^. uid) scs) (zip scs [1..])
  where scs :: [Citation]
        scs = sortBy citeSort cs
        -- Sorting is necessary if using elems to pull all the citations
        -- (as it sorts them and would change the order).
        -- We can always change the sorting to whatever makes most sense

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

instance HasGoalRefs ReferenceDB where goalRefTable = goalDB
instance HasPSDRefs ReferenceDB where psdRefTable = physSystDescDB
instance HasAssumpRefs ReferenceDB where assumpRefTable = assumpDB
instance HasReqRefs ReferenceDB where reqRefTable = reqDB
instance HasChangeRefs ReferenceDB where changeRefTable = changeDB
instance HasCitationRefs ReferenceDB where citationRefTable = citationDB


class Referable s where
  refAdd  :: s -> String  -- The plaintext referencing address (what we're linking to).
                          -- Should be string with no spaces/special chars.
                          -- Only visible in the source (tex/html).
  rType   :: s -> RefType -- The reference type (referencing namespace?)

instance Referable Goal where
  refAdd g = "GS:" ++ g ^. G.refAddr
  rType _ = Goal

instance Referable PhysSystDesc where
  refAdd p = "PS:" ++ p ^. PD.refAddr
  rType _ = PSD

instance Referable AssumpChunk where
  refAdd  x             = "A:" ++ concatMap repUnd (x ^. uid)
  rType   _             = Assump

instance Referable ReqChunk where
  refAdd  r@(RC _ rt _ _ _) = show rt ++ ":" ++ concatMap repUnd (r ^. uid)
  rType   _                 = Req

instance Referable Change where
  refAdd r@(ChC _ rt _ _ _)    = show rt ++ ":" ++ concatMap repUnd (r ^. uid)
  rType (ChC _ Likely _ _ _)   = LC
  rType (ChC _ Unlikely _ _ _) = UC

instance Referable Section where
  refAdd  (Section _ _ r) = "Sec:" ++ r
  rType   _               = Sect

instance Referable Citation where
  refAdd c = concatMap repUnd $ citeID c -- citeID should be unique.
  rType _ = Cite

-- error used below is on purpose. These refNames should be made explicit as necessary
instance Referable TheoryModel where
  refAdd  t = "T:" ++ t^.uid
  rType   _ = Def

instance Referable GenDefn where
  refAdd  g = "GD:" ++ g^.uid
  rType   _ = Def

instance Referable QDefinition where -- FIXME: This could lead to trouble; need
                                     -- to ensure sanity checking when building
                                     -- Refs. Double-check QDef is a DD before allowing
  refAdd  d = "DD:" ++ d^.uid
  rType   _ = Def

instance Referable InstanceModel where
  refAdd  i = "IM:" ++ i^.uid
  rType   _ = Def

instance Referable Contents where
  rType (Table _ _ _ _ _)       = Tab
  rType (Figure _ _ _ _)        = Fig
  rType (Definition (Data _))   = Def
  rType (Definition (Theory _)) = Def
  rType (Definition _)          = Def
  rType (Defnt _ _ _)           = Def
  rType (Requirement r)         = rType r
  rType (Assumption a)          = rType a
  rType (Change l)              = rType l --rType lc
  rType (Graph _ _ _ _ _)       = Fig
  rType (EqnBlock _ _)          = EqnB
  rType _                       =
    error "Attempting to reference unimplemented reference type"
  refAdd (Table _ _ _ _ r)      = "Table:" ++ r
  refAdd (Figure _ _ _ r)       = "Figure:" ++ r
  refAdd (Graph _ _ _ _ r)      = "Figure:" ++ r
  refAdd (EqnBlock _ r)         = "Equation:" ++ r
  refAdd (Definition d)         = getDefName d
  refAdd (Defnt _ _ r)          = r
  refAdd (Requirement rc)       = refAdd rc
  refAdd (Assumption ca)        = refAdd ca
  refAdd (Change lcc)           = refAdd lcc
  refAdd (Enumeration _)        = error "Can't reference lists"
  refAdd (Paragraph _)          = error "Can't reference paragraphs"
  refAdd (Bib _)                = error $
    "Bibliography list of references cannot be referenced. " ++
    "You must reference the Section or an individual citation."

citeSort :: Citation -> Citation -> Ordering
citeSort = compare `on` (^. uid)

citationsFromBibMap :: BibMap -> [Citation]
citationsFromBibMap bm = sortBy citeSort citations
  where citations :: [Citation]
        citations = map (\(x,_) -> x) (Map.elems bm)

assumptionsFromDB :: AssumpMap -> [AssumpChunk]
assumptionsFromDB am = dropNums $ sortBy (compare `on` snd) assumptions
  where assumptions = Map.elems am
        dropNums = map fst

-- | Create References to a given 'LayoutObj'
-- This should not be exported to the end-user, but should be usable
-- within the recipe (we want to force reference creation to check if the given
-- item exists in our database of referable objects.
makeRef :: (HasShortName l, Referable l) => l -> Sentence
makeRef r = customRef r (shortname r)

-- | Create a reference with a custom 'RefName'
customRef :: (Referable l) => l -> Sentence -> Sentence
customRef r n = Ref (rType r) (refAdd r) n

-- This works for passing the correct id to the reference generator for Assumptions,
-- Requirements and Likely Changes but I question whether we should use it.
-- Pass it the item to be referenced and the enumerated list of the respective
-- contents for that file. Change rType values to implement.

acroTest :: Contents -> [Contents] -> Sentence
acroTest ref reflst = makeRef $ find' ref reflst

find' :: Contents -> [Contents] -> Contents
find' _ [] = error "This object does not match any of the enumerated objects provided by the list."
find' itm@(Assumption comp1) (frst@(Assumption comp2):lst)
  | (comp1 ^. uid) == (comp2 ^. uid) = frst
  | otherwise = find' itm lst
find' itm@(Definition (Data comp1)) (frst@(Definition (Data comp2)):lst)
  | (comp1 ^. uid) == (comp2 ^. uid) = frst
  | otherwise = find' itm lst
find' itm@(Definition (Theory comp1)) (frst@(Definition (Theory comp2)):lst)
  | (comp1 ^. uid) == (comp2 ^. uid) = frst
  | otherwise = find' itm lst
find' itm@(Requirement comp1) (frst@(Requirement comp2):lst)
  | (comp1 ^. uid) == (comp2 ^. uid) = frst
  | otherwise = find' itm lst
find' itm@(Change comp1) (frst@(Change comp2):lst)
  | (comp1 ^. uid) == (comp2 ^. uid) = frst
  | otherwise = find' itm lst
find' _ _ = error "Error: Attempting to find unimplemented type"
