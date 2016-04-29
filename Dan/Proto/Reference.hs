{-# OPTIONS -Wall #-}
module Reference where

import LayoutObjs
import Spec
import Data.Char
import Helpers
import Chunk (name)
import Control.Lens

-- Creating References --
makeRef :: LayoutObj -> Spec
makeRef r = Ref (rType r) (getRefName r)

rType :: LayoutObj -> RefType
rType (Table _ _ _ _) = Tab
rType (Figure _ _)    = Fig
rType (Section d _ _) = Sec
rType (Definition _)  = Def
rType _ = error "Attempting to reference unimplemented reference type"

getRefName :: LayoutObj -> Spec
getRefName (Table h d l b)  = S "Table:" :+: simplify l
getRefName (Section d t _)  = writeSec d :+: simplify t
getRefName (Figure l _)     = S "Figure:" :+: simplify l
getRefName (Paragraph c)    = error "Can't reference paragraphs" --yet
getRefName (EqnBlock c)     = error "EqnBlock ref unimplemented"
getRefName (CodeBlock c)    = error "Codeblock ref unimplemented"
getRefName (Definition d)   = getDefName d
getRefName (BulletList b)   = error "BulletList ref unimplemented"
getRefName (NumberedList i) = error "NumberedList ref unimplemented"
getRefName (SimpleList p)   = error "SimpleList ref unimplemented"

simplify :: Spec -> Spec
simplify (s1 :+: s2) = simplify s1 :+: simplify s2
simplify (S s1)      = S (stringSimp s1)
simplify (F _ s)     = S [s]
simplify (Ref _ _)   = error "Attempting to simplify an existing reference"
simplify _           = Empty

stringSimp :: String -> String
stringSimp s = (map head (words s))

repUnd :: String -> String
repUnd s = map (\c -> if c == '_' then '.' else c) s

writeSec :: Int -> Spec
writeSec n
  | n < 0     = error "Illegal section depth. Must be positive."
  | n > 2     = error "Section too deep (Reference.hs)"
  | otherwise = S $ (capitalize $ (concat $ replicate n "sub") ++ "sec:")
  
getDefName :: DType -> Spec
getDefName (Data c)   = S $ "DD:" ++ (repUnd (c ^. name))
getDefName (Theory c) = S $ "T:" ++ stringSimp (repUnd (c ^. name))
  
-- Need to figure out Eq of specs or change ref to take String instead of Spec and use Strings throughout.  
  
-- getRefsTo :: Chunk c => c -> Document -> Spec
-- getRefsTo c (Document _ _ (ls)) = concat $ intersperse (", ") $ 
                                    -- map (findRef c) ls
                                    
-- findRef :: Chunk c => c -> LayoutObj -> [Spec]
-- findRef c x@(Table _ d _ _)  = [checkTable (getRefName x) (getRefName c) d]
-- findRef c x@(Section _ _ ls) = concat map (findSecRef x c ls)
-- findRef c x@(Definition (Data c2)) = [checkChunk x c c2]
-- findRef c x@(Definition (Theory c2)) = [checkChunk x c c2]
-- findRef _ = []

-- checkTable :: Chunk c => Spec -> c -> [[Spec]] -> Spec
-- checkTable r c d = 
