module Language.Drasil.Chunk.Eq(QDefinition(..), fromEqn, fromEqn') where

import Control.Lens (Simple, Lens)
import Prelude hiding (id)
import Language.Drasil.Expr (Expr)
import Language.Drasil.Chunk
import Language.Drasil.Chunk.Quantity (Quantity(..))
import Language.Drasil.Chunk.Unital (ucFromVC)
import Language.Drasil.Chunk.MUChunk
import Language.Drasil.Unit (Unit(..), Unit'(..))
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Spec (Sentence(..))
import Language.Drasil.Space

-- BEGIN EQCHUNK --
data QDefinition = EC 
  { uc :: MUChunk
  , equat :: Expr
  }

-- this works because UnitalChunk is a Chunk
instance Chunk QDefinition where
  id = ul . id

instance NamedIdea QDefinition where
  term = ul . term

instance SymbolForm QDefinition where
  symbol = ul . symbol

instance Quantity QDefinition where
  getSymb q = getSymb $ uc q
  -- DO SOMETHING
  
instance Unit' QDefinition where
  unit' = ul . unit'
-- END EQCHUNK --

-- don't export this
ul :: Simple Lens QDefinition MUChunk
ul f (EC a b) = fmap (\x -> EC x b) (f a)
  
-- useful: to be used for equations with units
--FIXME: Space hack
fromEqn :: Unit u => String -> Sentence -> Symbol -> u -> Expr -> QDefinition
fromEqn nm desc symb chunk eqn = 
  EC (Has $ ucFromVC (cv (dccWDS nm nm desc) symb Rational) chunk) eqn

-- and without
--FIXME: Space hack
fromEqn' :: String -> Sentence -> Symbol -> Expr -> QDefinition
fromEqn' nm desc symb eqn = 
  EC (HasNot $ cv (dccWDS nm nm desc) symb Rational) eqn
