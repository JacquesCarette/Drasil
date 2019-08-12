{-# LANGUAGE TupleSections #-}

module Language.Drasil.Code.Imperative.GOOL.Helpers (blank,verticalComma,
  angles,doubleQuotedText,himap,hicat,vicat,vibcat,vmap,vimap,vibmap, 
  emptyIfEmpty, emptyIfNull, mapPairFst, mapPairSnd, liftA4, liftA5, liftA6, 
  liftA7, liftA8, liftList, lift2Lists, lift1List, liftPair, lift3Pair, 
  lift4Pair, liftPairFst, getInnerType, getNestDegree, convType, checkParams
) where

import qualified Language.Drasil.Code.Code as C (CodeType(..))
import Language.Drasil.Code.Imperative.GOOL.Data (ParamData)
import qualified Language.Drasil.Code.Imperative.GOOL.Symantics as S ( 
  RenderSym(..), StateTypeSym(..), PermanenceSym(dynamic_))

import Prelude hiding ((<>))
import Control.Applicative (liftA2, liftA3)
import Data.List (intersperse, nub)
import Text.PrettyPrint.HughesPJ (Doc, vcat, hcat, text, char, doubleQuotes, 
  (<>), comma, punctuate, empty, isEmpty)

blank :: Doc
blank = text ""

verticalComma :: (a -> Doc) -> [a] -> Doc
verticalComma f = vcat . punctuate comma . map f

angles :: Doc -> Doc
angles d = char '<' <> d <> char '>'

doubleQuotedText :: String -> Doc
doubleQuotedText = doubleQuotes . text

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

emptyIfEmpty :: Doc -> Doc -> Doc
emptyIfEmpty ifDoc elseDoc = if isEmpty ifDoc then empty else elseDoc

emptyIfNull :: [a] -> Doc -> Doc
emptyIfNull lst elseDoc = if null lst then empty else elseDoc

mapPairFst :: (a -> b) -> (a, c) -> (b, c)
mapPairFst f (a, c) = (f a, c)

mapPairSnd :: (a -> b) -> (c, a) -> (c, b)
mapPairSnd f (c, b) = (c, f b)

liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> 
  f d -> f e
liftA4 f a1 a2 a3 a4 = liftA3 f a1 a2 a3 <*> a4

liftA5 :: Applicative f => (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c ->
  f d -> f e -> f g
liftA5 f a1 a2 a3 a4 a5 = liftA4 f a1 a2 a3 a4 <*> a5

liftA6 :: Applicative f => (a -> b -> c -> d -> e -> g -> h) -> f a -> f b -> 
  f c -> f d -> f e -> f g -> f h
liftA6 f a1 a2 a3 a4 a5 a6 = liftA5 f a1 a2 a3 a4 a5 <*> a6

liftA7 :: Applicative f => (a -> b -> c -> d -> e -> g -> h -> i) -> f a -> 
  f b -> f c -> f d -> f e -> f g -> f h -> f i
liftA7 f a1 a2 a3 a4 a5 a6 a7 = liftA6 f a1 a2 a3 a4 a5 a6 <*> a7

liftA8 :: Applicative f => (a -> b -> c -> d -> e -> g -> h -> i -> j) -> 
  f a -> f b -> f c -> f d -> f e -> f g -> f h -> f i -> f j
liftA8 f a1 a2 a3 a4 a5 a6 a7 a8 = liftA7 f a1 a2 a3 a4 a5 a6 a7 <*> a8

liftList :: Monad m => ([a] -> b) -> [m a] -> m b
liftList f as = f <$> sequence as

lift2Lists :: Monad m => ([a] -> [b] -> c) -> [m a] -> [m b] -> m c
lift2Lists f as bs = liftA2 f (sequence as) (sequence bs)

lift1List :: Monad m => (a -> [b] -> c) -> m a -> [m b] -> m c
lift1List f a as = liftA2 f a (sequence as)

lift4Pair :: Monad m => (a -> b -> c -> d -> [(e, f)] -> g) -> m a -> m b -> 
  m c -> m d -> [(m e, m f)] -> m g
lift4Pair f a1 a2 a3 a4 as = liftA5 f a1 a2 a3 a4 (mapM liftPair as)

lift3Pair :: Monad m => (a -> b -> c -> [(d, e)] -> f) -> m a -> m b -> m c -> 
  [(m d, m e)] -> m f
lift3Pair f a1 a2 a3 as = liftA4 f a1 a2 a3 (mapM liftPair as)

liftPair :: Applicative f => (f a, f b) -> f (a, b)
liftPair (a, b) = liftA2 (,) a b

liftPairFst :: Functor f => (f a, b) -> f (a, b)
liftPairFst (c, n) = fmap (, n) c

getInnerType :: C.CodeType -> C.CodeType
getInnerType (C.List innerT) = innerT
getInnerType _ = error "Attempt to extract inner type of list from a non-list type" 

getNestDegree :: Integer -> C.CodeType -> Integer
getNestDegree n (C.List t) = getNestDegree (n+1) t
getNestDegree n _ = n

convType :: (S.RenderSym repr) => C.CodeType -> repr (S.StateType repr)
convType C.Boolean = S.bool
convType C.Integer = S.int
convType C.Float = S.float
convType C.Char = S.char
convType C.String = S.string
convType (C.List t) = S.listType S.dynamic_ (convType t)
convType (C.Iterator t) = S.iterator $ convType t
convType (C.Object n) = S.obj n
convType (C.Enum n) = S.enumType n
convType C.Void = S.void
convType C.File = error "convType: File ?"

checkParams :: String -> [ParamData] -> [ParamData]
checkParams n ps = if length ps == length (nub ps) then ps else error 
  ("Duplicate parameters encountered in function " ++ n ++ ".")