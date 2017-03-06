{-# LANGUAGE GADTs, Rank2Types #-}
module Language.Drasil.Chunk.Unital 
  ( UnitalChunk(..)
  , makeUC
  , makeUCWDS
  , ucFromVC
  , NUChunk
  , nu
  , nu'
  , Unital(..)) where

import Control.Lens (Simple, Lens, (^.), set)
import Prelude hiding (id)
import Language.Drasil.Chunk (Chunk(..))
import Language.Drasil.Chunk.NamedIdea (NamedIdea(..), nc)
import Language.Drasil.Chunk.Concept (Concept(..), cv, dcc, dccWDS, ConVar(..))
import Language.Drasil.Chunk.SymbolForm (SymbolForm(..), SF(..))
import Language.Drasil.Chunk.Quantity (Quantity(..))
import Language.Drasil.Unit (Unit(..), UnitDefn(..))
import Language.Drasil.Symbol
import Language.Drasil.Space
import Language.Drasil.Spec (Sentence)


class Quantity c => Unital c where
  unit :: c -> UnitDefn

data NUChunk where --Named Unital...?
  NU :: (NamedIdea c, Unit u) => c -> Symbol -> u -> Space -> NUChunk
instance Chunk NUChunk where
  id = nl id
instance NamedIdea NUChunk where
  term = nl term
  getA (NU qc _ _ _) = getA qc
instance Quantity NUChunk where
  typ f (NU named s u t) = fmap (\x -> NU named s u x) (f t)
  getSymb = Just . SF
  getUnit = Just . unit
instance Unital NUChunk where
  unit (NU _ _ u _) = UU u
instance SymbolForm NUChunk where
  symbol f (NU n s u t) = fmap (\x -> NU n x u t) (f s)
  
nl :: (forall c. (NamedIdea c) => Simple Lens c a) -> Simple Lens NUChunk a
nl l f (NU qc s u t) = fmap (\x -> NU (set l x qc) s u t) (f (qc ^. l))


-- FIXME: Temporarily hacking in the space for NU chunks, these can be fixed
-- with the use of other constructors.

nu :: (NamedIdea c, Unit u) => c -> Symbol -> u -> NUChunk
nu a b c = NU a b c Rational

nu' :: (Unit u) => String -> String -> Symbol -> u -> NUChunk
nu' i t s u = NU (nc i t) s u Rational









--BEGIN HELPER FUNCTIONS--
--FIXME: Space hack
makeUC :: Unit u => String -> String -> String -> Symbol -> u -> UnitalChunk
makeUC nam trm desc sym un = UC (cv (dcc nam trm desc) sym Rational) un

--Better names will come later.
makeUCWDS :: Unit u => String -> String -> Sentence -> Symbol -> u -> UnitalChunk
makeUCWDS nam trm desc sym un = UC (cv (dccWDS nam trm desc) sym Rational) un

ucFromVC :: Unit u => ConVar -> u -> UnitalChunk
ucFromVC conv un = UC conv un

{-
FIXME? Should UC require a Quantity? 
Or should we pull the "Space" out of the constructor and only
require SymbolForm and Concept?
Currently hacked in a need for Quantity as it doesn't break
anything and allows for "typ" to work while more changes are made.
I think we should take the latter approach (pull out "Space")
once we're in a more stable position.
-}
qlens :: (forall c. (Quantity c, SymbolForm c, Concept c) => Simple Lens c a) -> Simple Lens Q a
qlens l f (Q a) = fmap (\x -> Q (set l x a)) (f (a ^. l))

-- these don't get exported
q :: Simple Lens UnitalChunk Q
q f (UC a b) = fmap (\(Q x) -> UC x b) (f (Q a))

u :: Simple Lens UnitalChunk UnitDefn
u f (UC a b) = fmap (\(UU x) -> UC a x) (f (UU b))
--END HELPER FUNCTIONS----

-------- BEGIN DATATYPES/INSTANCES --------

-- BEGIN Q --
data Q where
  Q :: (Quantity c, Concept c, SymbolForm c) => c -> Q

instance Chunk Q where 
  id = qlens id

instance NamedIdea Q where 
  term = qlens term
  getA (Q a) = getA a

instance SymbolForm Q where
  symbol = qlens symbol
  
instance Concept Q where
  defn = qlens defn

instance Quantity Q where
  getSymb   = Just . SF
  getUnit _ = Nothing
  typ = qlens typ


-- END Q ----

-- BEGIN UNITALCHUNK --
data UnitalChunk where
  UC :: (Quantity c, Concept c, SymbolForm c, Unit u) => c -> u -> UnitalChunk

instance Chunk UnitalChunk where
  id = q . id

instance NamedIdea UnitalChunk where
  term = q . term
  getA (UC c _) = getA c

instance SymbolForm UnitalChunk where
  symbol = q . symbol
  
instance Concept UnitalChunk where
  defn = q . defn

--FIXME: Add "typ"
instance Quantity UnitalChunk where
  getSymb uc = getSymb (uc ^. q)
  getUnit uc = Just (uc ^. u)
  typ = q . typ
  
instance Unit UnitalChunk where
  usymb = u . usymb
  
instance Unital UnitalChunk where
  unit (UC _ u) = UU u
  
instance Eq UnitalChunk where
  a == b = (a ^. id) == (b ^. id)
-- END UNITALCHUNK ----
