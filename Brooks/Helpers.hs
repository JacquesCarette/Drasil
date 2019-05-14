{-# LANGUAGE TupleSections #-}

module Helpers (
    blank,spc,oneTabbed,oneTab,vertical,verticalComma,verticalNewLine,
    angles,doubleQuoted,doubleQuotedText,capitalize,containsAll,
    makeLiteralNameValid,makeVarNameValid,makeClassNameValid,powerSet,
    hmap,himap,hicat,vicat,vibcat,vmap,vimap,vibmap, reduceLibs,
    tripFst, tripSnd, tripThird, liftA4, liftA5, liftA6, liftA7, liftList, 
    lift1List, liftPair, lift3Pair, lift4Pair, liftPairFst, liftTripFst
) where

import Prelude hiding ((<>))
import Control.Monad (filterM)
import Control.Applicative (liftA2, liftA3)
import Data.Char (toUpper)
import Data.String.Utils (replace)
import Data.List (nub,intersperse)
import Text.PrettyPrint.HughesPJ (Doc, vcat, hcat, text, char, doubleQuotes, 
  (<>), ($+$), comma, punctuate, nest)

blank :: Doc
blank = text ""

spc :: Doc
spc = text " "

oneTabbed :: [Doc] -> Doc
oneTabbed = vcat . map oneTab

oneTab :: Doc -> Doc
oneTab = nest 4

vertical :: (a -> Doc) -> [a] -> Doc
vertical f = vcat . map f

verticalComma :: (a -> Doc) -> [a] -> Doc
verticalComma f = vcat . punctuate comma . map f

verticalNewLine :: (a -> Doc) -> [a] -> Doc
verticalNewLine f = vcat . punctuate (blank $+$ blank) . map f

angles :: Doc -> Doc
angles d = char '<' <> d <> char '>'

doubleQuotedText :: String -> Doc
doubleQuotedText = doubleQuotes . text

doubleQuoted :: (a -> String) -> a -> Doc
doubleQuoted labeller = doubleQuotedText . labeller

capitalize :: String -> String
capitalize [] = error "capitalize called on an emptry String"
capitalize (c:cs) = (toUpper c):cs

containsAll :: Eq a => [a] -> [a] -> Bool
containsAll = all . flip elem

makeLiteralNameValid :: String -> String
makeLiteralNameValid s =
    let chars = ["\\","\""]
    in foldl myLiteralNameReplace s chars

makeVarNameValid :: String -> String
makeVarNameValid s =
    let illegalChars = [
            "~","`","-","=","!","@","#","$","%","^","&","*","(",")","+",
            "[","]","\\",";","'",".","/","{","}","|",":","\"","<",">","?"," "]
    in foldl myVarNameReplace s illegalChars

makeClassNameValid :: String -> String
makeClassNameValid = makeVarNameValid

--http://community.livejournal.com/evan_tech/220036.html
powerSet :: Eq a => [a] -> [[a]]
powerSet = filterM (const [True, False]) . nub

hmap :: (a -> Doc) -> [a] -> Doc
hmap f = hcat . map f

himap :: Doc -> (a -> Doc) -> [a] -> Doc
himap c f = hcat . intersperse c . map f

hicat :: Doc -> [Doc] -> Doc
hicat c l = hcat $ intersperse c l

vicat :: Doc -> [Doc] -> Doc
vicat c = vcat . intersperse c

vibcat :: [Doc] -> Doc
vibcat = vicat blank

vmap :: (a -> Doc) -> [a] -> Doc
vmap f = vcat . map f

vimap :: Doc -> (a -> Doc) -> [a] -> Doc
vimap c f = vicat c . map f

vibmap :: (a -> Doc) -> [a] -> Doc
vibmap = vimap blank

reduceLibs :: [String] -> [String] -> [String]
reduceLibs libs modules = nub $ filter (`notElem` modules) libs 

tripFst :: (a, b, c) -> a
tripFst (c, _, _) = c

tripSnd :: (a, b, c) -> b
tripSnd (_, n, _) = n

tripThird :: (a, b, c) -> c
tripThird (_, _, b) = b

liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a1 a2 a3 a4 = liftA3 f a1 a2 a3 <*> a4

liftA5 :: Applicative f => (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
liftA5 f a1 a2 a3 a4 a5 = liftA4 f a1 a2 a3 a4 <*> a5

liftA6 :: Applicative f => (a -> b -> c -> d -> e -> g -> h) -> f a -> f b -> f c -> f d -> f e -> f g -> f h
liftA6 f a1 a2 a3 a4 a5 a6 = liftA5 f a1 a2 a3 a4 a5 <*> a6

liftA7 :: Applicative f => (a -> b -> c -> d -> e -> g -> h -> i) -> f a -> f b -> f c -> f d -> f e -> f g -> f h -> f i
liftA7 f a1 a2 a3 a4 a5 a6 a7 = liftA6 f a1 a2 a3 a4 a5 a6 <*> a7

liftList :: Monad m => ([a] -> b) -> [m a] -> m b
liftList f as = fmap f $ sequence as

lift1List :: (Applicative m, Monad m) => (a -> [b] -> c) -> m a -> [m b] -> m c
lift1List f a as = liftA2 f a (sequence as)

lift4Pair :: (Applicative m, Monad m) => (a -> b -> c -> d -> [(e, f)] -> g) -> m a -> m b -> m c -> m d -> [(m e, m f)] -> m g
lift4Pair f a1 a2 a3 a4 as = liftA5 f a1 a2 a3 a4 (mapM liftPair as)

lift3Pair :: (Applicative m, Monad m) => (a -> b -> c -> [(d, e)] -> f) -> m a -> m b -> m c -> [(m d, m e)] -> m f
lift3Pair f a1 a2 a3 as = liftA4 f a1 a2 a3 (mapM liftPair as)

liftPair :: Applicative f => (f a, f b) -> f (a, b)
liftPair (a, b) = liftA2 (,) a b

liftPairFst :: Functor f => (f a, b) -> f (a, b)
liftPairFst (c, n) = fmap (, n) c

liftTripFst :: Functor f => (f a, b, c) -> f (a, b, c)
liftTripFst (c, n, b) = fmap (flip (flip (,,) n) b) c

--private
myLiteralNameReplace :: String -> String -> String
myLiteralNameReplace l old = replace old ("\\" ++ old) l

myVarNameReplace :: String -> String -> String
myVarNameReplace l old = replace old "_" l
----------