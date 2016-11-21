{-# LANGUAGE GADTs, Rank2Types #-}
module Language.Drasil.Chunk.MDefinedConcept where

import Control.Lens (Simple, Lens, (^.), set)

import Language.Drasil.Chunk

data MDefinedConcept where 
  DefinedT :: (Concept h, ConceptDefinition h) => h -> MDefinedConcept
  SimpleT :: Concept c => c -> MDefinedConcept
  
instance Chunk MDefinedConcept where
  name = dclens name
  
instance Concept MDefinedConcept where
  descr = dclens descr
  
-- Unlike MUChunk where we want a "unitless" (ie. blank) unit for those
-- quantities that should be unitless, we want something anytime
-- cdefn is used. For terms with only simple definitions, we should let the 
-- recipe decide what to do (ie. reuse the simple def'n, or leave it blank)
instance ConceptDefinition' MDefinedConcept where
  cdefn' f (DefinedT h) = fmap (DefinedT . maybe h (\t -> set cdefn t h)) (f $ Just $ h ^. cdefn)
  cdefn' f (SimpleT h) = fmap (SimpleT . maybe h (\_ -> h)) (f $ Nothing)

-- utilities which should not be exported
dclens :: (forall c. Concept c => Simple Lens c a) -> Simple Lens MDefinedConcept a
dclens l f (DefinedT a) = fmap (\x -> DefinedT (set l x a)) (f (a ^. l))
dclens l f (SimpleT a) = fmap (\x -> SimpleT (set l x a)) (f (a ^. l))

