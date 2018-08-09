module Language.Drasil.Make.Import where

import Language.Drasil.Output.Formats (DocSpec(..), DocType(..))
import Language.Drasil.Make.AST (Rule, Makefile(M), Type(Phony, TeX))

-- | Creates a Makefile (calls 'makeRules')
toMake :: [DocSpec] -> Makefile
toMake rl = M $ makeRules rl

makeRule :: DocSpec -> [Rule]
makeRule (DocSpec SRS fn)     = [(Phony, "srs", [fn ++ ".pdf"]), (TeX, fn, [])]
makeRule (DocSpec MG  fn)     = [(Phony, "mg" , [fn ++ ".pdf"]), (TeX, fn, [])]
makeRule (DocSpec MIS fn)     = [(Phony, "mis", [fn ++ ".pdf"]), (TeX, fn, [])]
makeRule (DocSpec Website _)  = []

-- | Helper for creating make rules for different document types
makeRules :: [DocSpec] -> [Rule]
makeRules l = concatMap makeRule l
