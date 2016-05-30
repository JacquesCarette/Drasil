module Language.Drasil.Reference where

import Language.Drasil.Document
import Language.Drasil.Spec
import Language.Drasil.Printing.Helpers (capitalize)
import Language.Drasil.Chunk (name)
import Control.Lens

-- Creating References --
makeRef :: LayoutObj -> Sentence
makeRef r = Ref (rType r) (getRefName r)

rType :: LayoutObj -> RefType
rType (Table _ _ _ _) = Tab
rType (Figure _ _)    = Fig
rType (Section _ _ _) = Sec
rType (Definition _)  = Def
rType _ = error "Attempting to reference unimplemented reference type"

-- the need for this seems like a hack!
getRefName :: LayoutObj -> Sentence
getRefName (Table _ _ l _)  = S "Table:" :+: inferName l
getRefName (Section d t _)  = writeSec d :+: inferName t
getRefName (Figure l _)     = S "Figure:" :+: inferName l
getRefName (Paragraph _)    = error "Can't reference paragraphs" --yet
getRefName (EqnBlock _)     = error "EqnBlock ref unimplemented"
getRefName (CodeBlock _)    = error "Codeblock ref unimplemented"
getRefName (Definition d)   = getDefName d
getRefName (BulletList _)   = error "BulletList ref unimplemented"
getRefName (NumberedList _) = error "NumberedList ref unimplemented"
getRefName (SimpleList _)   = error "SimpleList ref unimplemented"

-- for now, magic: infer the name of sentences!
inferName :: Sentence -> Sentence
inferName (s1 :+: s2) = inferName s1 :+: inferName s2
inferName (S s1)      = S (firstLetter s1)
inferName (F _ s)     = S [s]
inferName (Ref _ _)   = error "Attempting to infer the name an existing reference"
inferName _           = S "" -- Was Empty.

firstLetter :: String -> String
firstLetter = map head . words

repUnd :: String -> String
repUnd s = map (\c -> if c == '_' then '.' else c) s

writeSec :: Int -> Sentence
writeSec n
  | n < 0     = error "Illegal section depth. Must be positive."
  | n > 2     = error "Section too deep (Reference.hs)"
  | otherwise = S $ (capitalize $ (concat $ replicate n "sub") ++ "sec:")
  
getDefName :: DType -> Sentence
getDefName (Data c)   = S $ "DD:" ++ (repUnd (c ^. name))
getDefName (Theory c) = S $ "T:" ++ firstLetter (repUnd (c ^. name))
  
-- Need to figure out Eq of specs or change ref to take String instead of Sentence and use Strings throughout.  
  
-- getRefsTo :: Chunk c => c -> Document -> Sentence
-- getRefsTo c (Document _ _ (ls)) = concat $ intersperse (", ") $ 
                                    -- map (findRef c) ls
                                    
-- findRef :: Chunk c => c -> LayoutObj -> [Sentence]
-- findRef c x@(Table _ d _ _)  = [checkTable (getRefName x) (getRefName c) d]
-- findRef c x@(Section _ _ ls) = concat map (findSecRef x c ls)
-- findRef c x@(Definition (Data c2)) = [checkChunk x c c2]
-- findRef c x@(Definition (Theory c2)) = [checkChunk x c c2]
-- findRef _ = []

-- checkTable :: Chunk c => Sentence -> c -> [[Sentence]] -> Sentence
-- checkTable r c d = 
