{-# LANGUAGE GADTs, Rank2Types #-}
module Language.Drasil.Chunk.MDefinedConcept where

import Control.Lens (Simple, Lens, (^.), set)
import Prelude hiding (id)
import Language.Drasil.Chunk

data MDefinedConcept where 
  DefinedT :: (NamedIdea h, ConceptDefinition h) => h -> MDefinedConcept
  SimpleT :: NamedIdea c => c -> MDefinedConcept
  
instance Chunk MDefinedConcept where
  id = dclens id
  
instance NamedIdea MDefinedConcept where
  term = dclens term
  
-- Unlike MUChunk where we want a "unitless" (ie. blank) unit for those
-- quantities that should be unitless, we want something anytime
-- cdefn is used. For terms with only simple definitions, we should let the 
-- recipe decide what to do (ie. reuse the simple def'n, or leave it blank)
instance ConceptDefinition' MDefinedConcept where
  cdefn' f (DefinedT h) = fmap (DefinedT . maybe h (\t -> set cdefn t h)) (f $ Just $ h ^. cdefn)
  cdefn' f (SimpleT h) = fmap (SimpleT . maybe h (\_ -> h)) (f $ Nothing)

-- utilities which should not be exported
dclens :: (forall c. NamedIdea c => Simple Lens c a) -> Simple Lens MDefinedConcept a
dclens l f (DefinedT a) = fmap (\x -> DefinedT (set l x a)) (f (a ^. l))
dclens l f (SimpleT a) = fmap (\x -> SimpleT (set l x a)) (f (a ^. l))

