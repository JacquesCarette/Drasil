module Language.Drasil.Chunk.Eq(QDefinition(..), fromEqn, fromEqn') where

import Control.Lens (Simple, Lens)

import Language.Drasil.Expr (Expr)
import Language.Drasil.Chunk
import Language.Drasil.Chunk.Unital (ucFromVC)
import Language.Drasil.Chunk.MUChunk
import Language.Drasil.Unit (Unit(..), Unit'(..))
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Spec (Sentence(..))

-- BEGIN EQCHUNK --
data QDefinition = EC 
  { uc :: MUChunk
  , equat :: Expr
  }

-- this works because UnitalChunk is a Chunk
instance Chunk QDefinition where
  name = ul . name

instance Concept QDefinition where
  descr = ul . descr

instance Quantity QDefinition where
  symbol = ul . symbol

instance Unit' QDefinition where
  unit' = ul . unit'
-- END EQCHUNK --

-- don't export this
ul :: Simple Lens QDefinition MUChunk
ul f (EC a b) = fmap (\x -> EC x b) (f a)
  
-- useful: to be used for equations with units
fromEqn :: Unit u => String -> Sentence -> Symbol -> u -> Expr -> QDefinition
fromEqn nm desc symb chunk eqn = 
  EC (Has $ ucFromVC (vcFromCC (ccWithDescrSent nm desc) symb) chunk) eqn

-- and without
fromEqn' :: String -> Sentence -> Symbol -> Expr -> QDefinition
fromEqn' nm desc symb eqn = 
  EC (HasNot $ vcFromCC (ccWithDescrSent nm desc) symb) eqn
