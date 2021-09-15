-- | Defines classes and constructors for Makefile generators.
module Build.Drasil.Make.Import where

import Build.Drasil.Make.AST (Makefile(M), Rule)

-- * Classes

-- | Members can be transformed into a make rule.
class RuleTransformer c where
  makeRule :: c -> [Rule]

-- * Constructors

-- | Creates a Makefile (calls 'makeRules').
toMake :: RuleTransformer c => [c] -> Makefile
toMake = M . makeRules

-- | Helper for creating make rules for different document types.
makeRules :: RuleTransformer c => [c] -> [Rule]
makeRules = concatMap makeRule
