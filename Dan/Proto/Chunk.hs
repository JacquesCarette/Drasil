{-# OPTIONS -Wall #-} 
module Chunk where

import Control.Lens
import Symbol

class Chunk c where
   name :: Simple Lens c String

-- a concept has a description
class Chunk c => Concept c where
   descr :: Simple Lens c String

-- a quantity is a concept that can be represented graphically
class Concept c => Quantity c where
   symbol :: Simple Lens c Symbol

data VarChunk = VC { vname :: String
                   , vdesc :: String
                   , vsymb :: Symbol}

instance Eq VarChunk where
  c1 == c2 = (c1 ^. name) == (c2 ^. name)

data ConceptChunk = CC String String
instance Chunk ConceptChunk where
  name f (CC a b) = fmap (\x -> CC x b) (f a)
instance Concept ConceptChunk where
  descr f (CC a b) = fmap (\x -> CC a x) (f b)

{-
data FullChunk mode = FC { cname :: String
                         , cdesc :: String
                         , csymb :: String
                         , cequat :: Expr mode
                         , csiu :: Unit mode
                         , cdep :: [c] }
-}

instance Chunk VarChunk where
  name f (VC n d s) = fmap (\x -> VC x d s) (f n)

instance Concept VarChunk where
  descr f (VC n d s) = fmap (\x -> VC n x s) (f d)

instance Quantity VarChunk where
  symbol f (VC n d s) = fmap (\x -> VC n d x) (f s)

{-
instance Chunk (FullChunk mode) mode where
  name   f c = fmap (\x -> c {cname = x}) (f $ cname c)
  descr  f c = fmap (\x -> c {cdesc = x}) (f $ cdesc c)
  symbol f c = fmap (\x -> c {csymb = x}) (f $ csymb c)
-}
