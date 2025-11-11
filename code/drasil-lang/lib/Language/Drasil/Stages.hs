-- | Stages of the development process.
module Language.Drasil.Stages(
   -- * Type
   Stage(..)
) where

-- | Stages are what part of the development process we are in.
-- There are currently two:
-- 1) The Equational stage (should be called Specification)
-- 2) The Implemenation stage
--
-- The point is that information may be rendered differently depending
-- at what stage we're at. Being able to talk about stages lets us also
-- attach different display information.

data Stage = Equational -- AKA Specification
           | Implementation -- AKA Implementation

{- Note: Keep stages separate from StagedSymbols for lookup purposes, as we may
   have documents which look up both stages of a symbol and show them
   side-by-side or one after another. -}

-- | For better error messages.
instance Show Stage where
  show Equational     = "Specification Stage"
  show Implementation = "Implementation Stage"

-- Discussion:
-- It is odd that this is in drasil-lang, because it is meta-information
-- about the development process and is used for rendering. It is indeed
-- a very tiny language (it has 2 words in it!). But what sets it apart is
-- that it is at the bottom of the module uses hierarchy! Why? That still
-- needs to be investigated. It looks like rendering issues are somehow
-- driving things more strongly than they should.

-- FIXME: More fine-grained stages? [Only when they are actually needed.]
