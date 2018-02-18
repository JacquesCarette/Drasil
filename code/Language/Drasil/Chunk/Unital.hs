{-# LANGUAGE GADTs, Rank2Types #-}
module Language.Drasil.Chunk.Unital 
  ( UnitalChunk(..)
  , makeUCWDS
  , ucFromCV
  , uc
  , uc'
  , ucs
  , ucs'
  , ucsWS
  ) where

import Control.Lens (Simple, Lens, (^.), set)
import Prelude hiding (id)
import Language.Drasil.Chunk (Chunk(..))
import Language.Drasil.Chunk.NamedIdea (NamedIdea(..),Idea(..))
import Language.Drasil.Chunk.Concept (Concept, dcc, dccWDS,Definition(..),ConceptDomain(..))
import Language.Drasil.Chunk.ConVar (ConVar (..))
import Language.Drasil.Chunk.Quantity (Quantity(..),HasSpace(typ))
import Language.Drasil.Chunk.Unitary (Unitary(..))
import Language.Drasil.Chunk.SymbolForm (Stage,HasSymbol(symbol))
import Language.Drasil.Unit (Unit(..), unitWrapper)
import Language.Drasil.Symbol
import Language.Drasil.Space
import Language.Drasil.Spec (Sentence)

import Language.Drasil.NounPhrase (NP)

-- | UnitalChunks are Unitary
data UnitalChunk where --Named Unital...?
  UC :: (Concept c, Unit u) => c -> (Stage -> Symbol) -> u -> Space -> UnitalChunk
  UCV :: (Unit u) => ConVar -> u -> UnitalChunk
instance Chunk UnitalChunk where
  id = nl id
instance NamedIdea UnitalChunk where
  term = nl term
instance Idea UnitalChunk where
  getA (UC qc _ _ _) = getA qc
  getA (UCV cv _ ) = getA cv
instance Definition UnitalChunk where
  defn = nl defn
instance ConceptDomain UnitalChunk where
  cdom = nl cdom
instance Concept UnitalChunk where
instance HasSpace UnitalChunk where
  typ f (UC named s u t) = fmap (\x -> UC named s u x) (f t)
  typ f ucv@(UCV _ _) = cvl typ f ucv
instance HasSymbol UnitalChunk where
  symbol st (UCV c _ ) = symbol st c
  symbol st (UC _ s _ _) = s st
instance Quantity UnitalChunk where
  getUnit = Just . unit
instance Unitary UnitalChunk where
  unit (UC _ _ u _) = unitWrapper u
  unit (UCV _ u) = unitWrapper u
  
nl :: (forall c. (Concept c) => Simple Lens c a) -> Simple Lens UnitalChunk a
nl l f (UC qc s u t) = fmap (\x -> UC (set l x qc) s u t) (f (qc ^. l))
nl l f (UCV cv u) = fmap (\x -> UCV (set l x cv) u) (f (cv ^. l))

cvl :: (forall c. (Concept c, Quantity c) => 
  Simple Lens c a) -> Simple Lens UnitalChunk a
cvl _ _ (UC _ _ _ _) = error $ "Incorrect use of cvl for UC unital chunks. " ++
  "Should only be used with UCV"
cvl l f (UCV cv u) = fmap (\x -> UCV (set l x cv) u) (f (cv ^. l))

--{BEGIN HELPER FUNCTIONS}--

-- FIXME: Temporarily hacking in the space for UC chunks, these can be fixed
-- with the use of other constructors.

-- | Used to create a UnitalChunk from a 'Concept', 'Symbol', and 'Unit'.
-- Assumes the 'Space' is Rational
uc :: (Concept c, Unit u) => c -> Symbol -> u -> UnitalChunk
uc a b c = UC a (\_ -> b) c Real

ucs' :: (Concept c, Unit u) => c -> Symbol -> u -> Space -> UnitalChunk
ucs' a b c p = UC a (\_ -> b) c p

-- | Same as 'uc', except it builds the Concept portion of the UnitalChunk
-- from a given id, term, and defn. Those are the first three arguments
uc' :: (Unit u) => String -> NP -> String -> Symbol -> u -> UnitalChunk
uc' i t d s u = UC (dcc i t d) (\_ -> s) u Real

-- | Same as 'uc'', but does not assume the 'Space'
ucs :: (Unit u) => String -> NP -> String -> Symbol -> u -> Space -> UnitalChunk
ucs nam trm desc sym un space = UC (dcc nam trm desc) (\_ -> sym) un space

-- ucs With a Sentence for desc
ucsWS :: Unit u => String -> NP -> Sentence -> Symbol -> u -> Space -> UnitalChunk
ucsWS nam trm desc sym un space = 
  UC (dccWDS nam trm desc) (\_ -> sym) un space

--Better names will come later.
-- | Create a UnitalChunk in the same way as 'uc'', but with a 'Sentence' for
-- the definition instead of a String
makeUCWDS :: Unit u => String -> NP -> Sentence -> Symbol -> u -> UnitalChunk
makeUCWDS nam trm desc sym un = UC (dccWDS nam trm desc) (\_ -> sym) un Real

-- | Create a UnitalChunk from a 'ConVar' by supplying the additional 'Unit'
ucFromCV :: Unit u => ConVar -> u -> UnitalChunk
ucFromCV conv un = UCV conv un
