module Language.Drasil.Make.Import where

import Language.Drasil.Output.Formats (DocType(..))
import Language.Drasil.Make.AST


toMake :: [DocType] -> Makefile
toMake rl = M $ makeRules rl

makeRules :: [DocType] -> [Rule]
makeRules [] = []
makeRules ((SRS fn):rs) =  [(Phony, "srs", [fn ++ ".pdf"]), (TeX, fn, [])] ++ makeRules rs
makeRules (_:rs)        =  [] ++ makeRules rs

