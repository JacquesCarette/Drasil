module Data.Drasil.SentenceStructures
  ( foldlSent, foldlSent_, foldlSentCol, foldlsC, foldlList
  , sAnd, andIts, andThe, sAre, sIn, sVersus
  , sIs, isThe, sOf, sOr, ofThe, ofThe'
  , ofGiv, ofGiv'
  , toThe, tableShows, figureLabel
  , isExpctdToHv, underConsidertn, showingCxnBw, refineChain
  , foldlSP, foldlSP_, foldlSPCol,foldlOptions
  , maybeChanged, maybeExpanded, maybeWOVerb
  , tAndDWAcc, tAndDWSym, tAndDOnly
  , followA
  , getTandS, getTDS
  , eqN
  , displayConstrntsAsSet
  , fmtPhys, fmtSfwr, typUncr
  , mkTableFromColumns
  , acroA, acroGD, acroGS, acroIM, acroLC, acroPS, acroR, acroT
  ) where

import Language.Drasil
import Data.Drasil.Utils (foldle, foldle1, addPercent)
import Data.Drasil.Concepts.Documentation hiding (constraint)
import Data.Drasil.Concepts.Math (equation)

import Control.Lens ((^.))
import Data.Monoid (mconcat)
import Data.List (intersperse,transpose)

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
foldlSP = Paragraph . foldlSent

foldlSP_ :: [Sentence] -> Contents
foldlSP_ = Paragraph . foldlSent_

foldlSPCol :: [Sentence] -> Contents
foldlSPCol = Paragraph . foldlSentCol

-- | creates a list of elements seperated by commas, including the last element
foldlsC :: [Sentence] -> Sentence
foldlsC = mconcat . intersperse (S ", ")

-- | creates a list of elements seperated by commas, ending in a "_, and _"
foldlList :: [Sentence] -> Sentence
foldlList []    = EmptyS
foldlList [a,b] = a `sAnd` b
foldlList lst   = foldle1 sC (\a b -> a `sC` S "and" +:+ b) lst

-- | creates a list of elements seperated by commas, ending in a "_, or _"
foldlOptions :: [Sentence] -> Sentence
foldlOptions []    = EmptyS
foldlOptions [a,b] = a `sOr` b
foldlOptions lst   = foldle1 sC (\a b -> a `sC` S "or" +:+ b) lst

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

{--Acronyms to be used throughout--}
-- ex. S "as seen in (A1)" -> S "as seen in" +:+ sParen (acroA "1")
acroA, acroGD, acroGS, acroIM, acroLC, acroPS, acroR, 
  acroT :: Int -> Sentence

acroA  numVar = short assumption  :+: S (show numVar)
acroGD numVar = short genDefn     :+: S (show numVar)
acroGS numVar = short goalStmt    :+: S (show numVar)
acroIM numVar = short inModel     :+: S (show numVar)
acroLC numVar = short likelyChg   :+: S (show numVar)
acroPS numVar = short physSyst    :+: S (show numVar)
acroR  numVar = short requirement :+: S (show numVar)
acroT  numVar = short thModel     :+: S (show numVar)


{--** Miscellaneous **--}
tableShows :: Contents -> Sentence -> Sentence
tableShows ref trailing = (makeRef ref) +:+ S "shows the" +:+ 
  plural dependency +:+ S "of" +:+ trailing

-- | Function that creates (a label for) a figure
--FIXME: Is `figureLabel` defined in the correct file?
figureLabel :: NamedIdea c => Int -> c -> Sentence -> [Char]-> String ->Contents
figureLabel num traceyMG contents filePath rn = Figure (titleize figure +: 
  (S (show num)) +:+ (showingCxnBw traceyMG contents)) filePath 100 rn

showingCxnBw :: NamedIdea c => c -> Sentence -> Sentence
showingCxnBw traceyVar contents = titleize traceyVar +:+ S "Showing the" +:+
  titleize' connection +:+ S "Between" +:+ contents

isExpctdToHv :: Sentence -> Sentence -> Sentence
a `isExpctdToHv` b = S "The" +:+ a +:+ S "is expected to have" +:+ b

underConsidertn :: ConceptChunk -> Sentence
underConsidertn chunk = S "The" +:+ (phrase chunk) +:+ 
  S "under consideration is" +:+. (chunk ^. defn)

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
tAndDWAcc :: Concept s => s -> ItemType
tAndDWAcc temp = Flat $ ((at_start temp) :+: sParenDash (short temp) :+: (temp ^. defn)) 
-- term (symbol) - definition
tAndDWSym :: (Concept s, Quantity a) => s -> a -> ItemType
tAndDWSym tD sym = Flat $ ((at_start tD) :+: 
  sParenDash (ch sym)) :+: (tD ^. defn)
-- term - definition
tAndDOnly :: Concept s => s -> ItemType
tAndDOnly chunk  = Flat $ ((at_start chunk) +:+ S "- ") :+: (chunk ^. defn)

followA :: Sentence -> Int -> Sentence
preceding `followA` num = preceding +:+ S "following" +:+ acroA num

-- | Used when you want to say a term followed by its symbol. ex. "...using the Force F in..."
getTandS :: (Quantity a, NamedIdea a) => a -> Sentence
getTandS a = phrase a +:+ ch a

-- | get term, definition, and symbol
getTDS :: (Quantity a, Concept a) => a -> Sentence
getTDS a = phrase a +:+ (a ^. defn) +:+ ch a

--Ideally this would create a reference to the equation too
eqN :: Int -> Sentence
eqN n = phrase equation +:+ sParen (S $ show n)

--Produces a sentence that displays the constraints in a {}.
displayConstrntsAsSet :: Quantity a => a -> [String] -> Sentence
displayConstrntsAsSet sym listOfVals = E $ (sy sym) `isin` (DiscreteS listOfVals)

{-BELOW IS TO BE MOVED TO EXAMPLE/DRASIL/SECTIONS-}

mkTableFromColumns :: [(Sentence, [Sentence])] -> ([Sentence], [[Sentence]])
mkTableFromColumns l = 
  let l' = filter (\(_,b) -> not $ null $ filter (not . isEmpty) b) l in 
  (map fst l', transpose $ map ((map replaceEmptyS) . snd) l')
  where
    isEmpty EmptyS = True
    isEmpty _      = False

none :: Sentence
none = S "--"

found :: Double -> Sentence
found x = (addPercent . realToFrac) (x*100)

typUncr :: (UncertainQuantity c) => c -> Sentence
typUncr x = maybe none found (x ^. uncert)

constraintToExpr :: (Quantity c) => c -> Constraint -> Expr
constraintToExpr c (Range _ ri) = real_interval c ri
constraintToExpr c (EnumeratedReal _ l) = isin (sy c) (DiscreteD l)
constraintToExpr c (EnumeratedStr _ l) = isin (sy c) (DiscreteS l)

--Formatters for the constraints
fmtPhys :: (Constrained c, Quantity c) => c -> Sentence
fmtPhys c = foldlList $ map (E . constraintToExpr c) $ filter isPhysC (c ^. constraints)

fmtSfwr :: (Constrained c, Quantity c) => c -> Sentence
fmtSfwr c = foldlList $ map (E . constraintToExpr c) $ filter isSfwrC (c ^. constraints)

replaceEmptyS :: Sentence -> Sentence
replaceEmptyS EmptyS = none
replaceEmptyS s@_ = s
