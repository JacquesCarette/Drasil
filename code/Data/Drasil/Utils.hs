module Data.Drasil.Utils
  ( foldle
  , foldlSent
  , mkEnumAbbrevList
  ) where

import Language.Drasil (Sentence(EmptyS, S, (:+:)), (+:+), (+:+.), ItemType(Flat), 
                        NamedIdea, getAcc)
  
-- | fold helper functions applies f to all but the last element, applies g to
-- last element and the accumulator
foldle :: (a -> a -> a) -> (a -> a -> a) -> a -> [a] -> a
foldle _ _ z []     = z
foldle _ g z [x]    = g z x
foldle f g z [x,y]  = g (f z x) y
foldle f g z (x:xs) = foldle f g (f z x) xs

-- | partial function application of foldle for sentences specifically
foldlSent :: [Sentence] -> Sentence
foldlSent = foldle (+:+) (+:+.) EmptyS

-- | concantenates number to abbreviation
-- should not be exported
enumWithAbbrev :: Sentence -> [Sentence]
enumWithAbbrev abbrev = [abbrev :+: (S $ show x) | x <- [(1 :: Integer)..]]

-- | zip helper function enumerates abbreviation and zips it with list of itemtype
mkEnumAbbrevList :: NamedIdea c => c -> [Sentence] -> [(Sentence, ItemType)]
mkEnumAbbrevList title list = zip (enumWithAbbrev $ getAcc title) (map (Flat) list)