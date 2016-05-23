{-# OPTIONS -Wall #-} 
module Language.Drasil.Chunk.Eq(EqChunk(..), fromEqn) where

import Control.Lens (Simple, Lens)

import Language.Drasil.Expr (Expr)
import Language.Drasil.Chunk
import Language.Drasil.Chunk.Unital
import Language.Drasil.Unit (Unit(..))
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Spec (Sentence(..))

-- BEGIN EQCHUNK --
data EqChunk = EC 
  { uc :: UnitalChunk
  , equat :: Expr
  }

-- this works because UnitalChunk is a Chunk
instance Chunk EqChunk where
  name = ul . name

instance Concept EqChunk where
  descr = ul . descr

instance Quantity EqChunk where
  symbol = ul . symbol

instance Unit EqChunk where
  unit = ul . unit
-- END EQCHUNK --

-- don't export this
ul :: Simple Lens EqChunk UnitalChunk
ul f (EC a b) = fmap (\x -> EC x b) (f a)
  
-- useful
fromEqn :: Unit u => String -> Sentence -> Symbol -> u -> Expr -> EqChunk
fromEqn nm desc symb chunk eqn = EC (UC (VC nm desc symb) chunk) eqn
