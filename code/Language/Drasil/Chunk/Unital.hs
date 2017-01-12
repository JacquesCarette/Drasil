{-# LANGUAGE GADTs, Rank2Types #-}
module Language.Drasil.Chunk.Unital (UnitalChunk(..), makeUC, ucFromVC) where

import Control.Lens (Simple, Lens, (^.), set)
import Prelude hiding (id)
import Language.Drasil.Chunk (Chunk(..), NamedIdea(..), SymbolForm(..), 
  ConVar(..), dcc, cv, Concept(..),SF(..))
import Language.Drasil.Chunk.Quantity (Quantity(..))
import Language.Drasil.Unit (Unit(..), UnitDefn(..))
import Language.Drasil.Symbol
import Language.Drasil.Space

--BEGIN HELPER FUNCTIONS--
--FIXME: Space hack
makeUC :: Unit u => String -> String -> String -> Symbol -> u -> UnitalChunk
makeUC nam term desc sym un = UC (cv (dcc nam term desc) sym Rational) un 

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
  unit = u . unit
-- END UNITALCHUNK ----
