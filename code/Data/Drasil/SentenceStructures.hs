module Data.Drasil.SentenceStructures
  ( foldlSent, foldlSent_, foldlSentCol, foldlsC, foldlList
  , sAnd, andIts, andThe, sAre, sIn, sVersus
  , sIs, isThe, sOf, sOr, ofThe, ofThe'
  , ofGiv, ofGiv'
  , toThe, tableShows, figureLabel
  , showingCxnBw, refineChain, foldlSP, foldlSP_, foldlSPCol
  , maybeChanged, maybeExpanded, maybeWOVerb
  , tAndDWAcc, tAndDWSym, tAndDOnly
  , makeConstraint, displayConstr
  ) where

import Language.Drasil
import Data.Drasil.Utils (foldle, foldle1, getS, fmtU)
import Data.Drasil.Concepts.Documentation
import Control.Lens ((^.))

{--** Sentence Folding **--}
-- | partial function application of foldle for sentences specifically
foldlSent :: [Sentence] -> Sentence
foldlSent = foldle (+:+) (+:+.) EmptyS

-- | foldlSent but does not end with period
foldlSent_ :: [Sentence] -> Sentence
foldlSent_ = foldle (+:+) (+:+) EmptyS

-- | foldlSent but ends with colon
foldlSentCol :: [Sentence] -> Sentence
foldlSentCol = foldle (+:+) (+:) EmptyS

-- | fold sentences then turns into content
foldlSP :: [Sentence] -> Contents
foldlSP = (Paragraph . foldlSent)

foldlSP_ :: [Sentence] -> Contents
foldlSP_ = (Paragraph . foldlSent_)

foldlSPCol :: [Sentence] -> Contents
foldlSPCol = (Paragraph . foldlSentCol)

-- | creates a list of elements seperated by commas, including the last element
foldlsC :: [Sentence] -> Sentence
foldlsC []       = EmptyS
foldlsC [x]      = x
foldlsC [x,y]    = x `sC` y
foldlsC (x:y:xs) = foldle sC sC (x `sC` y) xs

-- | creates a list of elements seperated by commas, ending in a "_, and _"
foldlList :: [Sentence] -> Sentence
foldlList []    = EmptyS
foldlList [a,b] = a +:+ S "and" +:+ b
foldlList lst   = foldle1 sC (\a b -> a `sC` S "and" +:+ b) lst


{--** Combinators **--}
sAnd, andIts :: Sentence -> Sentence -> Sentence
sAnd p1 p2 = p1 +:+ S "and" +:+ p2

andIts p1 p2 = p1 +:+ S "and its" +:+ p2

andThe :: Sentence -> Sentence -> Sentence
andThe p1 p2 = p1 +:+ S "and the" +:+ p2

sAre :: Sentence -> Sentence -> Sentence
sAre p1 p2 = p1 +:+ S "are" +:+ p2

sIn :: Sentence -> Sentence -> Sentence
sIn p1 p2 = p1 +:+ S "in" +:+ p2

sIs :: Sentence -> Sentence -> Sentence
sIs p1 p2 = p1 +:+ S "is" +:+ p2

isThe :: Sentence -> Sentence -> Sentence
isThe p1 p2 = p1 +:+ S "is the" +:+ p2

sOf :: Sentence -> Sentence -> Sentence
sOf p1 p2 = p1 +:+ S "of" +:+ p2

sOr :: Sentence -> Sentence -> Sentence
sOr p1 p2 = p1 +:+ S "or" +:+ p2

sVersus :: Sentence -> Sentence -> Sentence
sVersus p1 p2 = p1 +:+ S "versus" +:+ p2

ofThe, ofThe' :: Sentence -> Sentence -> Sentence
ofThe  p1 p2 = S "the" +:+ p1 +:+ S "of the" +:+ p2
ofThe' p1 p2 = S "The" +:+ p1 +:+ S "of the" +:+ p2

ofGiv, ofGiv' :: Sentence -> Sentence -> Sentence
ofGiv  p1 p2 = S "the" +:+ p1 +:+ S "of a given" +:+ p2
ofGiv' p1 p2 = S "The" +:+ p1 +:+ S "of a given" +:+ p2

toThe :: Sentence -> Sentence -> Sentence
toThe p1 p2 = p1 +:+ S "to the" +:+ p2


{--** Miscellaneous **--}
tableShows :: Contents -> Sentence -> Sentence
tableShows ref trailing = (makeRef ref) +:+ S "shows the" +:+ 
  plural dependency +:+ S "of" +:+ trailing

-- | Function that creates (a label for) a figure
--FIXME: Is `figureLabel` defined in the correct file?
figureLabel :: NamedIdea c => [Char] -> c -> Sentence -> [Char]-> Contents
figureLabel num traceyMG contents filePath = Figure (titleize figure +: 
  S num +:+ (showingCxnBw (traceyMG) (contents))) filePath

showingCxnBw :: NamedIdea c => c -> Sentence -> Sentence
showingCxnBw traceyVar contents = titleize traceyVar +:+ S "Showing the" +:+
  titleize' connection +:+ S "Between" +:+ contents

-- | Create a list in the pattern of "The __ are refined to the __".
-- Note: Order matters!
refineChain :: NamedIdea c => [c] -> Sentence
refineChain (x:y:[]) = S "The" +:+ plural x +:+ S "are refined to the" +:+ plural y
refineChain (x:y:xs) = refineChain [x,y] `sC` rc ([y] ++ xs)
refineChain _ = error "refineChain encountered an unexpected empty list"


-- | Helper used by refineChain
rc :: NamedIdea c => [c] -> Sentence
rc (x:y:[]) = S "and the" +:+ (plural x) +:+ S "to the" +:+. 
  (plural y)
rc (x:y:xs) = S "the" +:+ plural x +:+ S "to the" +:+ plural y `sC` rc ([y] ++ xs)
rc _ = error "refineChain helper encountered an unexpected empty list"


-- | helper functions for making likely change statements
likelyFrame :: Sentence -> Sentence -> Sentence -> Sentence
likelyFrame a verb x = foldlSent [S "The", a, S "may be", verb, x]
maybeWOVerb, maybeChanged, maybeExpanded :: Sentence -> Sentence -> Sentence
maybeWOVerb a b = likelyFrame a EmptyS b
maybeChanged a b = likelyFrame a (S "changed") b
maybeExpanded a b = likelyFrame a (S "expanded") b

-- | helpful combinators for making Sentences for Terminologies with Definitions
-- term (acc) - definition
tAndDWAcc :: (Concept s, NamedIdea c) => s -> c -> ItemType
tAndDWAcc tD acc = Flat $ ((at_start tD) :+: sParenDash (short acc)) :+: (tD ^. defn)
-- term (symbol) - definition
tAndDWSym :: (Concept s, SymbolForm a) => s -> a -> ItemType
tAndDWSym tD sym = Flat $ ((at_start tD) :+: sParenDash (getS sym)) :+: (tD ^. defn)
-- term - definition
tAndDOnly :: Concept s => s -> ItemType
tAndDOnly chunk  = Flat $ ((at_start chunk) +:+ S "- ") :+: (chunk ^. defn)

--FIXME:Reduce duplication. Idealy only use displayConstr. Gamephysics uses pi/2 which is not a "number" yet
makeConstraint :: (Constrained s, Quantity s, SymbolForm s) => s -> Sentence -> [Sentence]
makeConstraint s num = [getS s, foldlList $ fmtCP (filter filterP (s ^. constraints)), fmtU num s]
  where filterP (Phys _) = True
        filterP (Sfwr _) = False
        fmtCP = map (\(Phys f) -> E $ f (C s))

displayConstr :: (Constrained s, Quantity s, SymbolForm s, Show a) => s -> a -> Sentence -> [Sentence]
displayConstr s num uncrty = [getS s, foldlList $ fmtCP (filter filterP (s ^. constraints)),
  foldlList $ fmtCS (filter filterS (s ^. constraints)), fmtU (S (show num)) (qs s), uncrty]
  where filterP (Phys _) = True
        filterP (Sfwr _) = False
        filterS (Phys _) = False
        filterS (Sfwr _) = True
        fmtCP = map (\(Phys f) -> E $ f (C s))
        fmtCS = map (\(Sfwr f) -> E $ f (C s))