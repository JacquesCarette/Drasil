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
import Language.Drasil.Chunk.Quantity (Quantity(..))
import Language.Drasil.Chunk.VarChunk (VarChunk, vc)
import Language.Drasil.Chunk.Unital (ucFromVC)
import Language.Drasil.Unit (Unit(..))
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Spec (Sentence(..))
import Language.Drasil.Space

-- BEGIN EQCHUNK --
data QDefinition where
  EC :: (SymbolForm c, Quantity c) => c -> Expr -> QDefinition

--Removed named record fields, so we want to not break things for now.
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
fromEqn :: Unit u => String -> Sentence -> Symbol -> u -> Expr -> QDefinition
fromEqn nm desc symb chunk eqn = 
  EC (ucFromVC (cv (ccStSS nm desc desc) symb Rational) chunk) eqn

-- and without
--FIXME: Space hack
fromEqn' :: String -> Sentence -> Symbol -> Expr -> QDefinition
fromEqn' nm desc symb eqn = 
  EC (cv (ccStSS nm desc desc) symb Rational) eqn


getVC :: QDefinition -> VarChunk
getVC qd = vc (qd ^. id) (qd ^. term) (qd ^. symbol) (qd ^. typ)
