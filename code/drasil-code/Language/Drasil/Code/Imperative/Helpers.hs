{-# LANGUAGE TupleSections #-}

module Language.Drasil.Code.Imperative.Helpers (Pair(..), Terminator (..),
  ScopeTag(..), ModData(..), md, MethodData(..), mthd, StateVarData(..), svd,
  blank,oneTabbed,oneTab,verticalComma, angles,doubleQuotedText,
  himap,hicat,vicat,vibcat,vmap,vimap,vibmap, mapPairFst, mapPairSnd, liftA4, 
  liftA5, liftA6, liftA7, liftA8, liftList, lift2Lists, lift1List, liftPair, 
  lift3Pair, lift4Pair, liftPairFst
) where

import Language.Drasil.Code.Imperative.Symantics (Label)

import Prelude hiding ((<>))
import Control.Applicative (liftA2, liftA3)
import Data.List (intersperse)
import Text.PrettyPrint.HughesPJ (Doc, vcat, hcat, text, char, doubleQuotes, 
  (<>), comma, punctuate, nest)

class Pair p where
  pfst :: p x y a -> x a
  psnd :: p x y b -> y b
  pair :: x a -> y a -> p x y a

data Terminator = Semi | Empty

data ScopeTag = Pub | Priv deriving Eq

data ModData = MD {name :: Label, isMainMod :: Bool, modDoc :: Doc}

md :: Label -> Bool -> Doc -> ModData
md = MD

data MethodData = MthD {isMainMthd :: Bool, getMthdScp :: ScopeTag, 
  mthdDoc :: Doc}

mthd :: Bool -> ScopeTag -> Doc -> MethodData
mthd = MthD 

data StateVarData = SVD {getStVarScp :: ScopeTag, stVarDoc :: Doc, 
  destructSts :: (Doc, Terminator)}

svd :: ScopeTag -> Doc -> (Doc, Terminator) -> StateVarData
svd = SVD

blank :: Doc
blank = text ""

oneTabbed :: [Doc] -> Doc
oneTabbed = vcat . map oneTab

oneTab :: Doc -> Doc
oneTab = nest 4

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