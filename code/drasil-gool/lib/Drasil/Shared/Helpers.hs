module Drasil.Shared.Helpers (angles, doubleQuotedText, hicat, vicat, vibcat, 
  vmap, vimap, emptyIfEmpty, emptyIfNull, toCode, toState, onCodeValue, 
  onStateValue, on2CodeValues, on2StateValues, on3CodeValues, on3StateValues, 
  onCodeList, onStateList, on2StateLists, getInnerType, on2StateWrapped,
  getNestDegree
) where

import Utils.Drasil (blank)

import qualified Drasil.Shared.CodeType as C (CodeType(..))

import Prelude hiding ((<>))
import Control.Applicative (liftA3)
import Control.Monad (liftM2, liftM3)
import Control.Monad.State (State)
import Data.List (intersperse)
import Text.PrettyPrint.HughesPJ (Doc, vcat, hcat, text, char, doubleQuotes, 
  (<>), empty, isEmpty)

angles :: Doc -> Doc
angles d = char '<' <> d <> char '>'

doubleQuotedText :: String -> Doc
doubleQuotedText = doubleQuotes . text

hicat :: Doc -> [Doc] -> Doc
hicat c l = hcat $ intersperse c l

vicat :: Doc -> [Doc] -> Doc
vicat c = vcat . intersperse c . filter (not . isEmpty)

vibcat :: [Doc] -> Doc
vibcat = vicat blank

vmap :: (a -> Doc) -> [a] -> Doc
vmap f = vcat . map f

vimap :: Doc -> (a -> Doc) -> [a] -> Doc
vimap c f = vicat c . map f

emptyIfEmpty :: Doc -> Doc -> Doc
emptyIfEmpty ifDoc elseDoc = if isEmpty ifDoc then empty else elseDoc

emptyIfNull :: [a] -> Doc -> Doc
emptyIfNull lst elseDoc = if null lst then empty else elseDoc

toCode :: (Monad r) => a -> r a
toCode = return

toState :: a -> State s a
toState = return

onCodeValue :: (Functor r) => (a -> b) -> r a -> r b
onCodeValue = fmap

onStateValue :: (a -> b) -> State s a -> State s b
onStateValue = fmap

on2CodeValues :: (Applicative r) => (a -> b -> c) -> r a -> r b -> 
  r c
on2CodeValues = liftA2

on2StateValues :: (a -> b -> c) -> State s a -> State s b -> State s c
on2StateValues = liftM2

on3CodeValues :: (Applicative r) => (a -> b -> c -> d) -> r a -> r b 
  -> r c -> r d
on3CodeValues = liftA3

on3StateValues :: (a -> b -> c -> d) -> State s a -> State s b -> State s c ->
  State s d
on3StateValues = liftM3

onCodeList :: Monad m => ([a] -> b) -> [m a] -> m b
onCodeList f as = f <$> sequence as

onStateList :: ([a] -> b) -> [State s a] -> State s b
onStateList f as = f <$> sequence as

on2StateLists :: ([a] -> [b] -> c) -> [State s a] -> [State s b] -> State s c
on2StateLists f as bs = liftM2 f (sequence as) (sequence bs)

on2StateWrapped :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
on2StateWrapped f a' b' = do 
    a <- a'
    b <- b'
    f a b 

getInnerType :: C.CodeType -> C.CodeType
getInnerType (C.List innerT) = innerT
getInnerType (C.Array innerT) = innerT
getInnerType (C.Set innerT) = innerT
getInnerType _ = error "Attempt to extract inner type from a non-nested type"

getNestDegree :: Integer -> C.CodeType -> Integer
getNestDegree n (C.List t) = getNestDegree (n+1) t
getNestDegree n _ = n
