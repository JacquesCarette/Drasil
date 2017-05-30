module Language.Drasil.RefHelpers where

import Language.Drasil.Spec

import Data.Char (isAlphaNum)

-- | for now, magic: infer the name of sentences!
inferName :: Sentence -> Sentence
inferName (s1 :+: s2) = inferName s1 :+: inferName s2
inferName (S s1)      = S $ (firstFourLetters s1)
inferName (F _ s)     = S [s]
inferName (Ref _ _)   = EmptyS
  -- error "Attempting to infer the name an existing reference"
inferName _           = EmptyS -- Was Empty.

-- | helper to get first 4 letters of each word. Used by inferName
firstFourLetters :: String -> String
firstFourLetters s1 = (filter (\x -> not (x `elem` ",.?!")) stringId)
  where stringId = concat (map (take 4) (words s1))


-- | replace underscore in strings with "."
repUnd :: String -> String
repUnd s = map (\c -> if c == '_' then '.' else c) s

-- | helper for filtering out any non-alpha-numeric characters from a string
alphanumOnly :: String -> String
alphanumOnly = filter (isAlphaNum)

{-
writeSec :: Int -> Sentence
writeSec n
  | n < 0     = error "Illegal section depth. Must be positive."
  | n > 3     = error "Section too deep (Reference.hs)"
  | otherwise = S $ (capitalize $ (concat $ replicate n "sub") ++ "sec:")
  
Need to figure out Eq of specs or change ref to take String instead of Sentence and use Strings throughout.  
  
getRefsTo :: Chunk c => c -> Document -> Sentence
getRefsTo c (Document _ _ (ls)) = concat $ intersperse (", ") $ 
                                    map (findRef c) ls
                                    
findRef :: Chunk c => c -> LayoutObj -> [Sentence]
findRef c x@(Table _ d _ _)  = [checkTable (getRefName x) (getRefName c) d]
findRef c x@(Section _ _ ls) = concat map (findSecRef x c ls)
findRef c x@(Definition (Data c2)) = [checkChunk x c c2]
findRef c x@(Definition (Theory c2)) = [checkChunk x c c2]
findRef _ = []

checkTable :: Chunk c => Sentence -> c -> [[Sentence]] -> Sentence
checkTable r c d = -}
