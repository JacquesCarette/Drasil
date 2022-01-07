-- | Stages used for displaying symbols.
module Language.Drasil.Stages(
   -- * Type
   Stage(..)
) where

-- FIXME: More fine-grained stages.
-- | Stages correspond to what we're trying to look up. They range from abstract
-- to concrete. Equational stages are more theoretical and oriented towards abstract design
-- while the Implementation stages are more oriented towards detailed design.                  
data Stage = Equational -- AKA Theoretical / Abstract-design
           | Implementation -- AKA Implementation / Detailed-design

{- Note: Keep stages separate from StagedSymbols for lookup purposes, as we may
   have documents which look up both stages of a symbol and show them 
   side-by-side or one after another. -}

-- | For better error messages.
instance Show Stage where
  show Equational     = "Theoretical stage"
  show Implementation = "Implementation Stage"

