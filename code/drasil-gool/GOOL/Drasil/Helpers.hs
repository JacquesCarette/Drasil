module GOOL.Drasil.Helpers (verticalComma, angles, doubleQuotedText, himap,
  hicat, vicat, vibcat, vmap, vimap, vibmap, emptyIfEmpty, emptyIfNull, 
  mapPairFst, mapPairSnd, toCode, toState, onCodeValue, onStateValue, 
  on2CodeValues, on2StateValues, on3CodeValues, on3StateValues, on4CodeValues, 
  onCodeList, onStateList, on2StateLists, on1CodeValue1List, on1StateValue1List,
  getInnerType, getNestDegree, convType
) where

import Utils.Drasil (blank)

import qualified GOOL.Drasil.CodeType as C (CodeType(..))
import qualified GOOL.Drasil.Symantics as S ( 
  RenderSym(..), TypeSym(..), PermanenceSym(dynamic_))

import Prelude hiding ((<>))
import Control.Applicative (liftA2, liftA3)
import Control.Monad (liftM2, liftM3)
import Control.Monad.State (State)
import Data.List (intersperse)
import Text.PrettyPrint.HughesPJ (Doc, vcat, hcat, text, char, doubleQuotes, 
  (<>), comma, punctuate, empty, isEmpty)

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

toCode :: (Monad repr) => a -> repr a
toCode = return

toState :: a -> State s a
toState = return

onCodeValue :: (Functor repr) => (a -> b) -> repr a -> repr b
onCodeValue = fmap

onStateValue :: (a -> b) -> State s a -> State s b
onStateValue = fmap

on2CodeValues :: (Applicative repr) => (a -> b -> c) -> repr a -> repr b -> 
  repr c
on2CodeValues = liftA2

on2StateValues :: (a -> b -> c) -> State s a -> State s b -> State s c
on2StateValues = liftM2

on3CodeValues :: (Applicative repr) => (a -> b -> c -> d) -> repr a -> repr b 
  -> repr c -> repr d
on3CodeValues = liftA3

on3StateValues :: (a -> b -> c -> d) -> State s a -> State s b -> State s c ->
  State s d
on3StateValues = liftM3

on4CodeValues :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c 
  -> f d -> f e
on4CodeValues f a1 a2 a3 a4 = liftA3 f a1 a2 a3 <*> a4

onCodeList :: Monad m => ([a] -> b) -> [m a] -> m b
onCodeList f as = f <$> sequence as

onStateList :: ([a] -> b) -> [State s a] -> State s b
onStateList f as = f <$> sequence as

on2StateLists :: ([a] -> [b] -> c) -> [State s a] -> [State s b] -> State s c
on2StateLists f as bs = liftM2 f (sequence as) (sequence bs)

on1CodeValue1List :: Monad m => (a -> [b] -> c) -> m a -> [m b] -> m c
on1CodeValue1List f a as = liftA2 f a (sequence as)

on1StateValue1List :: (a -> [b] -> c) -> State s a -> [State s b] -> State s c
on1StateValue1List f a as = liftM2 f a (sequence as)

getInnerType :: C.CodeType -> C.CodeType
getInnerType (C.List innerT) = innerT
getInnerType _ = error "Attempt to extract inner type of list from a non-list type" 

getNestDegree :: Integer -> C.CodeType -> Integer
getNestDegree n (C.List t) = getNestDegree (n+1) t
getNestDegree n _ = n

convType :: (S.RenderSym repr) => C.CodeType -> repr (S.Type repr)
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
