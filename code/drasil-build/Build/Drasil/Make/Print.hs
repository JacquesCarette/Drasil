module Build.Drasil.Make.Print where

import Prelude hiding ((<>))
import Text.PrettyPrint (Doc, empty, text, (<>), (<+>), ($+$), hsep, vcat) 

import Build.Drasil.Make.AST (Type(Phony, TeX), Target, Dependencies, Rule, Makefile(M))
import Build.Drasil.Make.Import (RuleTransformer, toMake)
import Build.Drasil.Make.Helpers (addCommonFeatures, tab)

-- | Generates the makefile by calling 'build' after 'toMake'
genMake :: RuleTransformer c => [c] -> Doc
genMake = build . toMake

-- | Renders the makefile rules
build :: Makefile -> Doc
build (M rules) = addCommonFeatures $ vcat $ map (\x -> printRule x $+$ text "") rules

-- | Renders specific makefile rules. Called by 'build'
printRule :: Rule -> Doc
printRule (Phony, nameLb, deps) = text (".PHONY: " ++ nameLb) $+$
                                  printTarget nameLb deps
printRule (TeX, nameLb, _)      = printTarget (nameLb ++ ".pdf") [(nameLb ++ ".tex")] $+$
                                  printLatexCmd nameLb
printRule _                     = error "Unimplemented makefile rule"


-- | Renders targets with their dependencies
printTarget :: Target -> [Dependencies] -> Doc
printTarget nameLb deps = text (nameLb ++ ": ") <+> hsep (map text deps)

lualatex :: Target -> Doc
lualatex = text . (++) "lualatex $(TEXFLAGS) "

bibtex :: Target -> Doc
bibtex = text . (++) "-bibtex $(BIBTEXFLAGS) "

-- | Renders LaTeX commands in the makefile
printLatexCmd :: Target -> Doc
printLatexCmd t = foldr (\x -> (tab <> x t $+$)) empty
  [lualatex, bibtex, lualatex, lualatex]
