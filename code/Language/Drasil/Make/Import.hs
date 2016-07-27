module Language.Drasil.Make.Import where

import Language.Drasil.Output.Formats (DocType(..))
import Language.Drasil.Make.AST


toMake :: [DocType] -> Makefile
toMake rl = M $ makeRules rl

makeRules :: [DocType] -> [Rule]
makeRules [] = []
makeRules ((SRS fn):rs) =  [(Phony, "srs", [fn ++ ".pdf"]), (TeX, fn, [])]
                             ++ makeRules rs
makeRules ((MG fn):rs)  =  [(Phony, "mg", [fn ++ ".pdf"]), (TeX, fn, [])]
                             ++ makeRules rs
makeRules ((MIS fn):rs) =  [(Phony, "mis", [fn ++ ".pdf"]), (TeX, fn, [])]
                             ++ makeRules rs
makeRules (_:rs)        =  [] ++ makeRules rs

