module Language.Drasil.Chunk.Attribute.Derivation where

import Language.Drasil.Spec (Sentence)
import Language.Drasil.Expr (Expr)

-- | Necessary for removing the dependency on the "Contents" type. 
data DerWrapper = DE Expr
                | DS Sentence

-- Derivations are an ordered list of sentences and expressions.
-- They are rendered in order as paragraphs and equation blocks to display
-- the derivation.
type Derivation = [DerWrapper]

de :: Expr -> DerWrapper
de = DE

ds :: Sentence -> DerWrapper
ds = DS
