module Language.Drasil.Chunk.Attribute.Core
  ( Attributes, Attribute(..)
  ) where


-- | Attributes are just a list of 'Attribute'
type Attributes = [Attribute]

-- | An attribute can be a rationale, a reference to the source (we used) to find
-- this knowledge, or a derivation to show how we arrived 
-- at a given model/definition/etc.
data Attribute =
    ShortName String
  -- | SourceRef Sentence -- Source to reference for this knowledge chunk
                       -- FIXME: Allow URLs/Citations here
  | Uses [String] -- Which chunks does this one rely on?
