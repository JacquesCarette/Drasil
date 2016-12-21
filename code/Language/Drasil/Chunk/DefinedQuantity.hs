module Language.Drasil.Chunk.DefinedQuantity (DefinedQuantity(..), dqFromDCC) where

import Control.Lens (Simple, Lens, (^.))
import Prelude hiding (id)
import Language.Drasil.Chunk
import Language.Drasil.Symbol

data DefinedQuantity = 
  DQ { dt :: DefinedTerm
     , sy :: Symbol }

instance Eq DefinedQuantity where
  c1 == c2 = (c1 ^. id) == (c2 ^. id)
instance Chunk DefinedQuantity where
  id = dql . id
instance NamedIdea DefinedQuantity where
  term = dql . term
instance Concept DefinedQuantity where
  defn = dql . defn
instance SymbolForm DefinedQuantity where
  symbol f (DQ a b) = fmap (\x -> DQ a x) (f b)

dqFromDCC :: DefinedTerm -> Symbol -> DefinedQuantity
dqFromDCC c s = DQ c s

-- don't export the dql
dql :: Simple Lens DefinedQuantity DefinedTerm
dql f (DQ a b) = fmap (\x -> DQ x b) (f a)