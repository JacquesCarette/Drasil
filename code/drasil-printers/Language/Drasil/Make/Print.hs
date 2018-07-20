module Language.Drasil.Make.Print where

import Text.PrettyPrint (Doc, text, (<>), (<+>), ($+$), hsep, vcat) 

import Language.Drasil

import Language.Drasil.Output.Formats (DocSpec(..))
import Language.Drasil.Make.AST (Type(Phony, TeX), Target, Dependencies, Rule, Makefile(M))
import Language.Drasil.Make.Import (toMake)
import Language.Drasil.Make.Helpers (addCommonFeatures)
import Language.Drasil.Printing.Helpers (tab)

-- | Generates the makefile by calling 'build' after 'toMake'
genMake :: [DocSpec] -> Doc
genMake = build . toMake

-- | Renders the makefile rules
build :: Makefile -> Doc
build (M rules) = addCommonFeatures $ vcat $ map (\x -> printRule x $+$ text "") rules

-- | Renders specific makefile rules. Called by 'build'
printRule :: Rule -> Doc
printRule (Phony, name, deps)   = text (".PHONY: " ++ name) $+$
                                  printTarget name deps
printRule (TeX, name, _)        = printTarget (name ++ ".pdf") [(name ++ ".tex")] $+$
                                  printLatexCmd name
printRule _                     = error "Unimplemented makefile rule"


-- | Renders targets with their dependencies
printTarget :: Target -> [Dependencies] -> Doc
printTarget name deps = text (name ++ ": ") <+> hsep (map text deps)

-- | Renders LaTeX commands in the makefile
printLatexCmd :: Target -> Doc
printLatexCmd t =   tab <> text ("lualatex " ++ t) $+$
                    tab <> text ("-bibtex " ++ t) $+$
                    tab <> text ("lualatex " ++ t) $+$
                    tab <> text ("lualatex " ++ t)
