module Build.Drasil.Make.Import where

import Build.Drasil.Make.AST (Rule, Makefile(M))

class RuleTransformer c where
  makeRule :: c -> [Rule]

-- | Creates a Makefile (calls 'makeRules')
toMake :: RuleTransformer c => [c] -> Makefile
toMake rl = M $ makeRules rl

-- | Helper for creating make rules for different document types
makeRules :: RuleTransformer c => [c] -> [Rule]
makeRules l = concatMap makeRule l
