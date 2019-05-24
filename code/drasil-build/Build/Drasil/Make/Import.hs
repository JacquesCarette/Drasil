module Build.Drasil.Make.Import where

import Build.Drasil.Make.AST (Makefile(M), Rule)

class RuleTransformer c where
  makeRule :: c -> [Rule]

-- | Creates a Makefile (calls 'makeRules')
toMake :: RuleTransformer c => [c] -> Makefile
toMake = M . makeRules

-- | Helper for creating make rules for different document types
makeRules :: RuleTransformer c => [c] -> [Rule]
makeRules = concatMap makeRule
