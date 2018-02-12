module Language.Drasil.Reference where

import Language.Drasil.Chunk (Chunk, id)
import Language.Drasil.Chunk.AssumpChunk
import Language.Drasil.Chunk.Change
import Language.Drasil.Chunk.ReqChunk
import Language.Drasil.Document
import Language.Drasil.RefHelpers
import Language.Drasil.Spec
import Control.Lens ((^.), Simple, Lens)

import Prelude hiding (id)

import Data.List (partition)
import qualified Data.Map as Map

-- | Create References to a given 'LayoutObj'
makeRef :: (Referable l) => l -> Sentence
makeRef r = Ref (rType r) (refAdd r) (refName r)

-- | Database for internal references.
data ReferenceDB = RDB 
  { assumpDB :: AssumpMap
  , reqDB :: ReqMap
  , changeDB :: ChangeMap 
  }

rdb :: [AssumpChunk] -> [ReqChunk] -> [Change] -> ReferenceDB
rdb assumps reqs changes = RDB (assumpMap assumps) (reqMap reqs) (changeMap changes)

-- | Map for maintaining assumption references. 
-- The Int is that reference's number.
-- Maintains access to both num and chunk for easy reference swapping
-- between number and shortname when necessary (or use of number
-- if no shortname exists)
type AssumpMap = Map.Map String (AssumpChunk, Int)

assumpMap :: [AssumpChunk] -> AssumpMap
assumpMap a = Map.fromList $ zip (map (^. id) a) (zip a [1..])

assumpLookup :: Chunk c => c -> AssumpMap -> (AssumpChunk, Int)
assumpLookup a m = let lookC = Map.lookup (a ^. id) m in
                   getS lookC
  where getS (Just x) = x
        getS Nothing = error $ "Assumption: " ++ (a ^. id) ++ 
          " referencing information not found in Assumption Map"

type ReqMap = Map.Map String (ReqChunk, Int)

reqMap :: [ReqChunk] -> ReqMap
reqMap rs = Map.fromList $ zip (map (^. id) (frs ++ nfrs)) ((zip frs [1..]) ++ 
  (zip nfrs [1..]))
  where (frs, nfrs)  = partition (isFuncRec . reqType) rs
        isFuncRec FR = True
        isFuncRec _  = False
        
reqLookup :: Chunk c => c -> ReqMap -> (ReqChunk, Int)
reqLookup r m = let lookC = Map.lookup (r ^. id) m in
                   getS lookC
  where getS (Just x) = x
        getS Nothing = error $ "Requirement: " ++ (r ^. id) ++ 
          " referencing information not found in Requirement Map"

type ChangeMap = Map.Map String (Change, Int)

changeMap :: [Change] -> ChangeMap
changeMap cs = Map.fromList $ zip (map (^. id) (lcs ++ ulcs)) 
  ((zip lcs [1..]) ++ (zip ulcs [1..]))
  where (lcs, ulcs) = partition (isLikely . chngType) cs
        isLikely Likely = True
        isLikely _ = False
        
changeLookup :: Chunk c => c -> ChangeMap -> (Change, Int)
changeLookup c m = let lookC = Map.lookup (c ^. id) m in
                   getS lookC
  where getS (Just x) = x
        getS Nothing = error $ "Change: " ++ (c ^. id) ++ 
          " referencing information not found in Change Map"

class HasAssumpRefs s where
  assumpRefTable :: Simple Lens s AssumpMap
  
instance HasAssumpRefs ReferenceDB where
  assumpRefTable f (RDB a b c) = fmap (\x -> RDB x b c) (f a)
  
class HasReqRefs s where
  reqRefTable :: Simple Lens s ReqMap
  
instance HasReqRefs ReferenceDB where
  reqRefTable f (RDB a b c) = fmap (\x -> RDB a x c) (f b)
  
class HasChangeRefs s where
  changeRefTable :: Simple Lens s ChangeMap
  
instance HasChangeRefs ReferenceDB where
  changeRefTable f (RDB a b c) = fmap (\x -> RDB a b x) (f c)
  
class Referable s where
  refName :: s -> RefName -- Sentence; The text to be displayed for the link.
  refAdd  :: s -> String  -- The reference address (what we're linking to).
                          -- Should be string with no spaces/special chars.
  rType   :: s -> RefType -- The reference type (referencing namespace?)
  
instance Referable AssumpChunk where
  refName x@(AC _ _ sn _) = sn
  refAdd  x               = "A:" ++ (x ^. id)
  rType   _               = Assump
  
instance Referable ReqChunk where
  refName (RC _ _ _ sn _)   = sn
  refAdd  r@(RC _ rt _ _ _) = show rt ++ ":" ++ (r ^. id)
  rType   _                 = Req
  
instance Referable Change where
  refName (ChC _ _ _ sn _)     = sn
  refAdd r@(ChC _ rt _ _ _)    = show rt ++ ":" ++ (r ^. id)
  rType (ChC _ Likely _ _ _)   = LC
  rType (ChC _ Unlikely _ _ _) = UC
  
instance Referable Section where
  refName (Section t _ _) = t
  refAdd  (Section _ _ r) = "Sec:" ++ r
  rType   _               = Sect

instance Referable Contents where
  refName (Table _ _ _ _ r)     = S "Table:" :+: S r
  refName (Figure _ _ _ r)      = S "Figure:" :+: S r
  refName (Graph _ _ _ _ r)     = S "Figure:" :+: S r
  refName (EqnBlock _ r)        = S "Equation:" :+: S r
  refName (Definition d)        = S $ getDefName d
  refName (Defnt dt _ r)        = S (getDefName dt) :+: S r
  refName (Requirement rc)      = refName rc
  refName (Assumption ca)       = refName ca
  refName (LikelyChange lcc)    = S $ "LC:" ++ repUnd (lcc ^. id)--refName lcc
  refName (UnlikelyChange ucc)  = S $ "UC:" ++ repUnd (ucc ^. id)--refName ucc
  refName (Enumeration _)       = error "Can't reference lists"
  refName (Paragraph _)         = error "Can't reference paragraphs"
  refName (Bib _)               = error "Bib referencing unimplemented"
  rType (Table _ _ _ _ _)       = Tab
  rType (Figure _ _ _ _)        = Fig
  rType (Definition (Data _))   = Def
  rType (Definition (Theory _)) = Def
  rType (Definition _)          = Def
  rType (Defnt _ _ _)           = Def
  rType (Requirement r)         = rType r
  rType (Assumption a)          = rType a
  rType (LikelyChange _)        = LC --rType lc
  rType (UnlikelyChange _)      = UC --rType uc
  rType (Graph _ _ _ _ _)       = Fig
  rType (EqnBlock _ _)          = EqnB
  rType _                       = 
    error "Attempting to reference unimplemented reference type"
  refAdd (Table _ _ _ _ r)      = "Table:" ++ r
  refAdd (Figure _ _ _ r)       = "Figure:" ++ r
  refAdd (Graph _ _ _ _ r)      = "Figure:" ++ r
  refAdd (EqnBlock _ r)         = "Equation:" ++ r
  refAdd (Definition d)         = getDefName d
  refAdd (Defnt dt _ r)         = getDefName dt ++ r
  refAdd (Requirement rc)       = refAdd rc
  refAdd (Assumption ca)        = refAdd ca
  refAdd (LikelyChange lcc)     = "LC:" ++ repUnd (lcc ^. id)--refName lcc
  refAdd (UnlikelyChange ucc)   = "UC:" ++ repUnd (ucc ^. id)--refName ucc
  refAdd (Enumeration _)        = error "Can't reference lists"
  refAdd (Paragraph _)          = error "Can't reference paragraphs"
  refAdd (Bib _)                = error "Bib referencing unimplemented"
  refAdd _ = "Placeholder"

rShow :: Referable a => a -> Sentence   
rShow = S . show . rType

-- | Automatically create the label for a definition
getDefName :: DType -> String
getDefName (Data c)   = "DD:" ++ (repUnd (c ^. id)) -- FIXME: To be removed
getDefName (Theory c) = "T:" ++ (repUnd (c ^. id)) -- FIXME: To be removed
getDefName TM         = "T:"
getDefName DD         = "DD:"
getDefName Instance   = "IM:"
getDefName General    = "GD:"

-- This works for passing the correct id to the reference generator for Assumptions,
-- Requirements and Likely Changes but I question whether we should use it.
-- Pass it the item to be referenced and the enumerated list of the respective
-- contents for that file. Change rType values to implement.

acroTest :: Contents -> [Contents] -> Sentence
acroTest ref reflst = makeRef $ find ref reflst

find :: Contents -> [Contents] -> Contents
find _ [] = error "This object does not match any of the enumerated objects provided by the list."
find itm@(Assumption comp1) (frst@(Assumption comp2):lst)
  | (comp1 ^. id) == (comp2 ^. id) = frst
  | otherwise = find itm lst
find itm@(Definition (Data comp1)) (frst@(Definition (Data comp2)):lst)
  | (comp1 ^. id) == (comp2 ^. id) = frst
  | otherwise = find itm lst
find itm@(Definition (Theory comp1)) (frst@(Definition (Theory comp2)):lst)
  | (comp1 ^. id) == (comp2 ^. id) = frst
  | otherwise = find itm lst
find itm@(Requirement comp1) (frst@(Requirement comp2):lst)
  | (comp1 ^. id) == (comp2 ^. id) = frst
  | otherwise = find itm lst
find itm@(LikelyChange comp1) (frst@(LikelyChange comp2):lst)
  | (comp1 ^. id) == (comp2 ^. id) = frst
  | otherwise = find itm lst
find itm@(UnlikelyChange comp1) (frst@(UnlikelyChange comp2):lst)
  | (comp1 ^. id) == (comp2 ^. id) = frst
  | otherwise = find itm lst
find _ _ = error "Error: Attempting to find unimplemented type"
