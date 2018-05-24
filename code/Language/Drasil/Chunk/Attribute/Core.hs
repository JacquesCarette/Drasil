module Language.Drasil.Chunk.Attribute.Core
  ( Attributes, Attribute(..)
  ) where

import Language.Drasil.Chunk.Attribute.Derivation (Derivation)

-- | Attributes are just a list of 'Attribute'
type Attributes = [Attribute]

-- | An attribute can be a rationale, a reference to the source (we used) to find
-- this knowledge, or a derivation to show how we arrived 
-- at a given model/definition/etc.
data Attribute =
    ShortName String
  -- | SourceRef Sentence -- Source to reference for this knowledge chunk
                       -- FIXME: Allow URLs/Citations here
  | D Derivation -- Makes sense for now 
        --(derivations are just document sections at the moment), 
        -- but we may need to create a new representation for it in the future.
        -- To collapse Attributes into QDefinitions, can't use Contents
