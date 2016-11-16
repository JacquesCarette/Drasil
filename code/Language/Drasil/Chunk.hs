{-# OPTIONS -Wall #-}
module Language.Drasil.Chunk where

import Control.Lens

import Language.Drasil.Symbol
import Language.Drasil.Spec

-------- BEGIN CLASSES --------

data Term = Simple Sentence
          | Verbose Sentence Sentence

class Terminology t where
  getTerm :: t -> Sentence
  
--FIXME: This will need to be decided later (depending on what the Recipe calls for)
instance Terminology Term where
  getTerm (Simple t) = t
  getTerm (Verbose s t) = s 
  
-- BEGIN CHUNK --
-- a chunk has a name
class Chunk c where
  name :: Simple Lens c String
-- END CHUNK --
  
-- BEGIN CONCEPT --
-- a concept has a description
class Chunk c => Concept c where
  descr :: (Terminology t) => Simple Lens c t
-- END CONCEPT --

-- BEGIN QUANTITY --
-- a quantity is a concept that can be represented graphically
class Concept c => Quantity c where
  symbol :: Simple Lens c Symbol
-- END QUANTITY --

-------- BEGIN DATATYPES/INSTANCES --------

-- BEGIN CONCEPTCHUNK --
data ConceptChunk = CC String Term
instance Eq ConceptChunk where
  c1 == c2 = (c1 ^. name) == (c2 ^. name)
instance Chunk ConceptChunk where
  name f (CC a b) = fmap (\x -> CC x b) (f a)
instance Concept ConceptChunk where
  descr f (CC a b) = fmap (\x -> CC a x) (f b)
-- END CONCEPTCHUNK --

-- BEGIN VARCHUNK --
data VarChunk = VC { vname :: String
                   , vdesc :: Term
                   , vsymb :: Symbol}

instance Eq VarChunk where
  c1 == c2 = (c1 ^. name) == (c2 ^. name)

instance Chunk VarChunk where
  name f (VC n d s) = fmap (\x -> VC x d s) (f n)

instance Concept VarChunk where
  descr f (VC n d s) = fmap (\x -> VC n x s) (f d)

instance Quantity VarChunk where
  symbol f (VC n d s) = fmap (\x -> VC n d x) (f s)

-- END VARCHUNK --


--Helper Function(s)--
-- FIXME: USE OF Simple HERE IS TEMPORARY WORK AROUND TO GET THINGS
--  WORKING AGAIN BEFORE NEXT CLEANUP STEP

makeCC :: String -> String -> ConceptChunk
makeCC nam des = CC nam (Simple $ S des)

--Currently only used by RelationChunk and EqChunk
ccWithDescrSent :: String -> Sentence -> ConceptChunk
ccWithDescrSent n d = CC n (Simple d)

-- For when name = descr (will likely become deprecated as the chunks become more descriptive).
nCC :: String -> ConceptChunk 
nCC n = makeCC n n

makeVC :: String -> String -> Symbol -> VarChunk
makeVC nam des sym = VC nam (Simple $ S des) sym

vcFromCC :: ConceptChunk -> Symbol -> VarChunk
vcFromCC cc sym = VC (cc ^. name) (cc ^. descr) sym