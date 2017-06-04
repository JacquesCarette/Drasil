{-# LANGUAGE GADTs, Rank2Types #-}
module Language.Drasil.Chunk.Eq 
  (QDefinition(..), fromEqn, fromEqn', equat, getVC) where

import Control.Lens (Simple, Lens, set, (^.))
import Prelude hiding (id)
import Language.Drasil.Expr (Expr)
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea (NamedIdea, term, getA)
import Language.Drasil.Chunk.SymbolForm (SymbolForm, symbol)
import Language.Drasil.Chunk.Concept
import Language.Drasil.Chunk.ConVar
import Language.Drasil.Chunk.Quantity (Quantity(..))
import Language.Drasil.Chunk.VarChunk (VarChunk, vc)
import Language.Drasil.Chunk.Unital (ucFromCV)
import Language.Drasil.Unit (Unit(..))
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Space

import Language.Drasil.NounPhrase (NP, phrase)

-- BEGIN EQCHUNK --
-- | A QDefinition is a 'Quantity' with a defining equation.
data QDefinition where
  EC :: (SymbolForm c, Quantity c) => c -> Expr -> QDefinition

--Removed named record fields, so we want to not break things for now.
-- | Returns the defining equation of a 'QDefinition'
equat :: QDefinition -> Expr 
equat (EC _ b) = b
  
-- this works because UnitalChunk is a Chunk
instance Chunk QDefinition where
  id = ul . id

instance NamedIdea QDefinition where
  term = ul . term
  getA c = getA $ c ^. ul

instance SymbolForm QDefinition where
  symbol = ul . symbol

instance Quantity QDefinition where
  typ = ul . typ
  getSymb (EC a _) = getSymb a
  getUnit (EC a _) = getUnit a
  -- DO SOMETHING
  
{-instance Unit' QDefinition where
  unit' = ul . unit'-}
-- END EQCHUNK --

-- don't export this
ul :: Simple Lens QDefinition E
ul f (EC a b) = fmap (\(E x) -> EC x b) (f (E a))

-- or this
elens :: (forall c. (SymbolForm c, Quantity c) => Simple Lens c a) 
  -> Simple Lens E a
elens l f (E a) = fmap (\x -> E (set l x a)) (f (a ^. l))

-- and especially not this
data E where
  E :: (SymbolForm c, Quantity c) => c -> E

instance Chunk E where
  id = elens id

instance NamedIdea E where
  term = elens term
  getA (E a) = getA a
  
instance SymbolForm E where 
  symbol = elens symbol
  
instance Quantity E where
  typ = elens typ
  getSymb (E c) = getSymb c
  getUnit (E c) = getUnit c
  
-- useful: to be used for equations with units
--FIXME: Space hack
--TODO: Create a version which doesn't use ConVar, but instead only 
--     NamedIdeas if we decide the "new" unital needs only a named idea

-- | Create a 'QDefinition' with an id, noun phrase (term), symbol,
-- unit, and defining equation.
fromEqn :: Unit u => String -> NP -> Symbol -> u -> Expr -> QDefinition
fromEqn nm desc symb chunk eqn = 
  EC (ucFromCV (cv (ccStSS nm desc (phrase desc)) symb Rational) chunk) eqn

-- and without
--FIXME: Space hack
-- | Same as fromEqn, but has no units.
fromEqn' :: String -> NP -> Symbol -> Expr -> QDefinition
fromEqn' nm desc symb eqn = 
  EC (cv (ccStSS nm desc (phrase desc)) symb Rational) eqn
  
-- | Returns a 'VarChunk' from a 'QDefinition'.
-- Currently only used in example /Modules/ which are being reworked.
getVC :: QDefinition -> VarChunk
getVC qd = vc (qd ^. id) (qd ^. term) (qd ^. symbol) (qd ^. typ)
