{-# Language TypeFamilies #-}
module Utils.Drasil.Misc (addPercent, bulletFlat, bulletNested, checkValidStr,
  chgsStart, displayConstrntsAsSet, enumBullet, enumBulletU, enumSimple,
  enumSimpleU, eqN, eqUnR, eqUnR', eqnWSource, fromReplace, fmtU, follows,
  getTandS, itemRefToSent, makeListRef, makeTMatrix, maybeChanged, maybeExpanded,
  maybeWOVerb, mkEnumAbbrevList, mkTableFromColumns, noRefs, refineChain,
  showingCxnBw, sortBySymbol, sortBySymbolTuple, substitute, tAndDOnly,
  tAndDWAcc, tAndDWSym, typUncr, underConsidertn, unwrap, weave, zipSentList) where

import Language.Drasil
import Utils.Drasil.Fold (FoldType(List), SepType(Comma), foldlList, foldlSent)
import Utils.Drasil.Sentence (sAre, toThe)

import Control.Lens ((^.))

import Data.Decimal (DecimalRaw, realFracToDecimal)
import Data.Function (on)
import Data.List (elem, sortBy, transpose)

-- Sorts a list of HasSymbols by Symbol
sortBySymbol :: HasSymbol a => [a] -> [a]
sortBySymbol = sortBy compareBySymbol

sortBySymbolTuple :: HasSymbol a => [(a, b)] -> [(a, b)]
sortBySymbolTuple = sortBy (compareBySymbol `on` fst)

compareBySymbol :: HasSymbol a => a -> a -> Ordering
compareBySymbol a b = compsy (symbol a Equational) (symbol b Equational)

eqUnR :: Expr -> Reference -> LabelledContent
eqUnR e lbl = llcc lbl $ EqnBlock e

eqUnR' :: Expr -> Contents
eqUnR' e = UlC $ ulcc $ EqnBlock e

--Ideally this would create a reference to the equation too
--Doesn't use equation concept so utils doesn't depend on data
eqN :: Int -> Sentence
eqN n = S "Equation" +:+ sParen (S $ show n)

-- | takes an expression and a referable and outputs as a Sentence "expression (source)"
eqnWSource :: (Referable r, HasShortName r) => Expr -> r -> Sentence
eqnWSource a b = E a +:+ sParen (makeRef2S b)

-- | takes a referable and a HasSymbol and outputs as a Sentence "From source we can replace symbol"
fromReplace :: (Referable r, HasShortName r) => r -> UnitalChunk -> Sentence
fromReplace src c = S "From" +:+ makeRef2S src +:+ S "we can replace" +: ch c

-- | takes a referable and a HasSymbol and outputs as a Sentence "By substituting "
substitute :: (Referable r, HasShortName r, HasSymbol r) => [r] -> Sentence
substitute s = S "By substituting" +:+ foldlList Comma List l `sC` S "this can be written as"
  where l = map (\x -> ch x +:+ sParen (S "from" +:+ makeRef2S x)) s

-- | zip helper function enumerates abbreviation and zips it with list of itemtype
-- s - the number from which the enumeration should start from
-- t - the title of the list
-- l - the list to be enumerated
mkEnumAbbrevList :: Integer -> Sentence -> [Sentence] -> [(Sentence, ItemType)]
mkEnumAbbrevList s t l = zip [t :+: (S $ show x) | x <- [s..]] $ map Flat l

-- | takes a amount and adds a unit to it
-- n - sentenc representing an amount
-- u - unit we want to attach to amount
fmtU :: (MayHaveUnit a) => Sentence -> a -> Sentence
fmtU n u  = n +:+ unwrap (getUnit u)

-- | extracts the typical uncertainty to be displayed from something that has an uncertainty
typUncr :: HasUncertainty c => c -> Sentence
typUncr x = found (uncVal x) (uncPrec x)
  where
    found u Nothing  = addPercent $ u * 100
    found u (Just p) = addPercent (realFracToDecimal (fromIntegral p) (u * 100) :: DecimalRaw Integer)

-- | outputs sentence with % attached to it
addPercent :: Show a => a -> Sentence
addPercent num = S (show num) :+: Percent

-- | appends a sentence to the front of a list of list of sentences
zipSentList :: [[Sentence]] -> [Sentence] -> [[Sentence]] -> [[Sentence]] 
zipSentList acc _ []           = acc
zipSentList acc [] r           = acc ++ map (EmptyS:) r
zipSentList acc (x:xs) (y:ys)  = zipSentList (acc ++ [x:y]) xs ys

-- | makes a traceability matrix from list of row title, list of rows
--   of "checked" columns, and a list of columns.
makeTMatrix :: Eq a => [Sentence] -> [[a]] -> [a] -> [[Sentence]]
makeTMatrix rowName rows cols = zipSentList [] rowName [zipFTable' x cols | x <- rows]
  where
    zipFTable' content = concatMap (\x -> if x `elem` content then [S "X"] else [EmptyS])

-- | Helper for making a table from a columns
mkTableFromColumns :: [(Sentence, [Sentence])] -> ([Sentence], [[Sentence]])
mkTableFromColumns l = 
  let l' = filter (any (not . isEmpty) . snd) l in 
  (map fst l', transpose $ map (map replaceEmptyS . snd) l')
  where
    isEmpty       EmptyS = True
    isEmpty       _      = False
    replaceEmptyS EmptyS = S "--"
    replaceEmptyS s      = s

-- | makes sentences from an item and its reference 
-- a - String title of reference
-- b - Sentence containing the full reference
itemRefToSent :: String -> Sentence -> Sentence
itemRefToSent a b = S a +:+ sParen b

-- | makeListRef takes a list and a reference and generates references to 
--   match the length of the list
-- l - list whos length is to be matched
makeListRef :: [a] -> Section -> [Sentence]
makeListRef l = replicate (length l) . Ref . makeRef2

-- | bulletFlat applies Bullet and Flat to a list.
bulletFlat :: [Sentence] -> ListType
bulletFlat = Bullet . noRefs . map Flat

-- | bulletNested applies Bullets and headers to a Nested ListType.
-- t - Headers of the Nested lists.
-- l - Lists of ListType.
bulletNested :: [Sentence] -> [ListType] -> ListType
bulletNested t l = Bullet . map (\(h,c) -> (Nested h c, Nothing)) $ zip t l

-- | enumBullet apply Enumeration, Bullet and Flat to a list
enumBullet :: Reference -> [Sentence] -> LabelledContent --FIXME: should Enumeration be labelled?
enumBullet lb s = llcc lb $ Enumeration $ bulletFlat s

enumBulletU :: [Sentence] -> Contents --FIXME: should Enumeration be labelled?
enumBulletU s =  UlC $ ulcc $ Enumeration $ bulletFlat s

-- | enumSimple enumerates a list and applies simple and enumeration to it
-- s - start index for the enumeration
-- t - title of the list
-- l - list to be enumerated
enumSimple :: Reference -> Integer -> Sentence -> [Sentence] -> LabelledContent --FIXME: should Enumeration be labelled?
enumSimple lb s t l = llcc lb $ Enumeration $ Simple $ noRefsLT $ mkEnumAbbrevList s t l

enumSimpleU :: Integer -> Sentence -> [Sentence] -> Contents --FIXME: should Enumeration be labelled?
enumSimpleU s t l = UlC $ ulcc $ Enumeration $ Simple $ noRefsLT $ mkEnumAbbrevList s t l

-- | interweaves two lists together [[a,b,c],[d,e,f]] -> [a,d,b,e,c,f]
weave :: [[a]] -> [a]
weave = concat . transpose

-- | get a unit symbol if there is one
unwrap :: Maybe UnitDefn -> Sentence
unwrap (Just a) = Sy $ usymb a
unwrap Nothing  = EmptyS

-- | noRefs converts lists of simple ItemTypes into a lists which may be used
-- in Contents but not directly referable.
noRefs :: [ItemType] -> [(ItemType, Maybe String)]
noRefs a = zip a $ repeat Nothing

-- | noRefsLT converts lists of tuples containing a title and ItemType into
-- a ListTuple which can be used with Contents but not directly referable.
noRefsLT :: [(Sentence, ItemType)] -> [ListTuple]
noRefsLT a = uncurry zip3 (unzip a) $ repeat Nothing

--Doesn't use connection phrase so utils doesn't depend on data
showingCxnBw :: NamedIdea c => c -> Sentence -> Sentence
showingCxnBw traceyVar contents = titleize traceyVar +:+
  S "Showing the Connections Between" +:+ contents

underConsidertn :: ConceptChunk -> Sentence
underConsidertn chunk = S "The" +:+ phrase chunk +:+ 
  S "under consideration is" +:+. (chunk ^. defn)

-- | Create a list in the pattern of "The __ are refined to the __".
-- Note: Order matters!
refineChain :: NamedIdea c => [(c, Section)] -> Sentence
refineChain [x,y] = S "The" +:+ plural (fst x) +:+ sParen (makeRef2S $ snd x) `sAre` S "refined" `toThe` plural (fst y)
refineChain (x:y:xs) = foldlList Comma List (refineChain [x,y] : rc (y : xs))
  where
    rc [a, b]   = [rcSent a b +:+. sParen (makeRef2S $ snd b)]
    rc (a:b:as) =  rcSent a b : rc (b : as)
    rc _        = error "refineChain helper encountered an unexpected empty list"
    rcSent a b  = S "the" +:+ plural (fst a) +:+ sParen (makeRef2S $ snd a) `toThe` plural (fst b)
refineChain _ = error "refineChain encountered an unexpected empty list"

-- | helper functions for making likely change statements
likelyFrame :: Sentence -> Sentence -> Sentence -> Sentence
likelyFrame a verb x = foldlSent [S "The", a, S "may be", verb, x]
maybeWOVerb, maybeChanged, maybeExpanded :: Sentence -> Sentence -> Sentence
maybeWOVerb a   = likelyFrame a EmptyS
maybeChanged a  = likelyFrame a (S "changed")
maybeExpanded a = likelyFrame a (S "expanded")

-- | helpful combinators for making Sentences for Terminologies with Definitions
-- term (acc) - definition
tAndDWAcc :: Concept s => s -> ItemType
tAndDWAcc temp = Flat $ atStart temp +:+ (sParen (short temp) `sDash` (temp ^. defn))
-- term (symbol) - definition
tAndDWSym :: (Concept s, Quantity a) => s -> a -> ItemType
tAndDWSym tD sym = Flat $ atStart tD +:+ (sParen (ch sym) `sDash` (tD ^. defn))
-- term - definition
tAndDOnly :: Concept s => s -> ItemType
tAndDOnly chunk  = Flat $ atStart chunk `sDash` (chunk ^. defn)

follows :: (Referable r, HasShortName r) => Sentence -> r -> Sentence
preceding `follows` ref = preceding +:+ S "following" +:+ makeRef2S ref

-- | Used when you want to say a term followed by its symbol. ex. "...using the Force F in..."
getTandS :: (Quantity a) => a -> Sentence
getTandS a = phrase a +:+ ch a

--Produces a sentence that displays the constraints in a {}.
displayConstrntsAsSet :: Quantity a => a -> [String] -> Sentence
displayConstrntsAsSet sym listOfVals = E $ sy sym `isin` DiscreteS listOfVals

--Produces a common beginning of a likely change of the form "reference - sentence"
chgsStart :: (HasShortName x, Referable x) => x -> Sentence -> Sentence
chgsStart = sDash . makeRef2S

--An either type - Left with error message if invalid char in string, else Right with string
checkValidStr :: String -> String -> Either String String
checkValidStr s [] = Right s
checkValidStr s (x:xs)
  | x `elem` s = Left $ "Invalid character: \'" ++ [x] ++ "\' in string \"" ++ s ++ ['\"']
  | otherwise  = checkValidStr s xs
