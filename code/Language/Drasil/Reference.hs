module Language.Drasil.Reference where

import Language.Drasil.Chunk (Chunk, id)
import Language.Drasil.Chunk.AssumpChunk
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
makeRef r = Ref (rType r) (refName r)

-- | Database for internal references.
data ReferenceDB = RDB {assumpDB :: AssumpMap, reqDB :: ReqMap }

rdb :: AssumpMap -> ReqMap -> ReferenceDB
rdb = RDB

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
  where frs  = fst $ partition (isFuncRec . reqType) rs
        nfrs = snd $ partition (isFuncRec . reqType) rs
        isFuncRec FR = True
        isFuncRec _  = False

class HasAssumpRefs s where
  assumpRefTable :: Simple Lens s AssumpMap
  
instance HasAssumpRefs ReferenceDB where
  assumpRefTable f (RDB a b) = fmap (\x -> RDB x b) (f a)
  
class HasReqRefs s where
  reqRefTable :: Simple Lens s ReqMap
  
instance HasReqRefs ReferenceDB where
  reqRefTable f (RDB a b) = fmap (\x -> RDB a x) (f b)
  
class Referable s where
  refName :: s -> RefName -- Sentence (see Spec for details)
  rType :: s -> RefType
  
instance Referable AssumpChunk where
  refName (AC _ _ sn _) = (S "A:") :+: sn
  rType _   = Assump
  
instance Referable ReqChunk where
  refName (RC _ rt _ sn _) = S (show rt ++ ":") :+: sn
  rType _ = Req
  
instance Referable Section where
  refName (Section _ _ r) = S "Sec:" :+: r
  rType _ = Sect

instance Referable Contents where
  refName (Table _ _ _ _ r)       = S "Table:" :+: r
  refName (Figure _ _ _ r)        = S "Figure:" :+: r
  refName (Graph _ _ _ _ r)       = S "Figure:" :+: r
  refName (EqnBlock _ r)          = S "Equation:" :+: r
  refName (Definition d)          = getDefName d
  refName (Defnt dt _ r)          = getDefName dt +:+ r
  refName (Requirement rc)        = refName rc
  refName (Assumption ca)         = refName ca
  refName (LikelyChange lcc)      = S $ "LC:" ++ repUnd (lcc ^. id)--refName lcc
  refName (UnlikelyChange ucc)    = S $ "UC:" ++ repUnd (ucc ^. id)--refName ucc
  refName (Enumeration _)         = error "Can't reference lists"
  refName (Paragraph _)           = error "Can't reference paragraphs"
  refName (Bib _)                 = error "Bib referencing unimplemented"
  rType (Table _ _ _ _ _)         = Tab
  rType (Figure _ _ _ _)          = Fig
  rType (Definition (Data _))     = Def
  rType (Definition (Theory _))   = Def
  rType (Definition _)            = Def
  rType (Defnt _ _ _)             = Def
  rType (Requirement r)           = rType r
  rType (Assumption a)            = rType a
  rType (LikelyChange _)         = LC --rType lc
  rType (UnlikelyChange _)       = UC --rType uc
  rType (Graph _ _ _ _ _)         = Fig
  rType _                         = 
    error "Attempting to reference unimplemented reference type"

-- | Automatically create the label for a definition
getDefName :: DType -> Sentence
getDefName (Data c)   = S $ "DD:" ++ (repUnd (c ^. id)) -- FIXME: To be removed
getDefName (Theory c) = S $ "T:" ++ (repUnd (c ^. id)) -- FIXME: To be removed
getDefName TM         = S "T:"
getDefName DD         = S "DD:"
getDefName Instance   = S "IM:"
getDefName General    = S "GD:"

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
