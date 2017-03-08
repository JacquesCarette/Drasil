{-# LANGUAGE GADTs, Rank2Types #-}
module Language.Drasil.Chunk.Unital 
  ( UnitalChunk(..)
  , makeUC
  , makeUCWDS
  , ucFromVC
  , uc
  , uc'
  , Unital(..)) where

import Control.Lens (Simple, Lens, (^.), set)
import Prelude hiding (id)
import Language.Drasil.Chunk (Chunk(..))
import Language.Drasil.Chunk.NamedIdea (NamedIdea(..))
import Language.Drasil.Chunk.Concept (Concept(..), dcc, dccWDS)
import Language.Drasil.Chunk.ConVar
import Language.Drasil.Chunk.SymbolForm (SymbolForm(..), SF(..))
import Language.Drasil.Chunk.Quantity (Quantity(..))
import Language.Drasil.Unit (Unit(..), UnitDefn(..))
import Language.Drasil.Symbol
import Language.Drasil.Space
import Language.Drasil.Spec (Sentence)


class (Concept c, Quantity c) => Unital c where
  unit :: c -> UnitDefn

data UnitalChunk where --Named Unital...?
  UC :: (Concept c, Unit u) => c -> Symbol -> u -> Space -> UnitalChunk
instance Chunk UnitalChunk where
  id = nl id
instance NamedIdea UnitalChunk where
  term = nl term
  getA (UC qc _ _ _) = getA qc
instance Concept UnitalChunk where
  defn = nl defn
instance Quantity UnitalChunk where
  typ f (UC named s u t) = fmap (\x -> UC named s u x) (f t)
  getSymb = Just . SF
  getUnit = Just . unit
instance Unital UnitalChunk where
  unit (UC _ _ u _) = UU u
instance SymbolForm UnitalChunk where
  symbol f (UC n s u t) = fmap (\x -> UC n x u t) (f s)
  
nl :: (forall c. (Concept c) => Simple Lens c a) -> Simple Lens UnitalChunk a
nl l f (UC qc s u t) = fmap (\x -> UC (set l x qc) s u t) (f (qc ^. l))

-- FIXME: Temporarily hacking in the space for UC chunks, these can be fixed
-- with the use of other constructors.

uc :: (Concept c, Unit u) => c -> Symbol -> u -> UnitalChunk
uc a b c = UC a b c Rational

uc' :: (Unit u) => String -> String -> String -> Symbol -> u -> UnitalChunk
uc' i t d s u = UC (dcc i t d) s u Rational

--BEGIN HELPER FUNCTIONS--
--FIXME: Space hack
makeUC :: Unit u => String -> String -> String -> Symbol -> u -> UnitalChunk
makeUC = uc'

--Better names will come later.
makeUCWDS :: Unit u => String -> String -> Sentence -> Symbol -> u -> UnitalChunk
makeUCWDS nam trm desc sym un = UC (dccWDS nam trm desc) sym un Rational


--FIXME: I don't like this (it's partly a relic), it should be a 
-- different data structure which is an instance of Unital that has a convar
ucFromVC :: Unit u => ConVar -> u -> UnitalChunk
ucFromVC conv un = uc conv (conv ^. symbol) un
