module Language.Drasil.Make.Print where

import Text.PrettyPrint hiding (render)

import Language.Drasil.Output.Formats (DocType(..))
import Language.Drasil.Make.AST
import Language.Drasil.Make.Import
import Language.Drasil.Make.Helpers
import Language.Drasil.Printing.Helpers (tab)

genMake :: [DocType] -> Doc
genMake = build . toMake

build :: Makefile -> Doc
build (M rules) = addCommonFeatures $ vcat $ map (\x -> printRule x $+$ text "") rules

printRule :: Rule -> Doc
printRule (Phony, name, deps)   = text (".PHONY: " ++ name) $+$
                                  printTarget name deps
printRule (TeX, name, _)        = printTarget (name ++ ".pdf") [(name ++ ".tex")] $+$
                                  printLatexCmd name
printRule _                     = error "Unimplemented makefile rule"



printTarget :: Target -> [Dependencies] -> Doc
printTarget name deps = text (name ++ ": ") <+> hsep (map text deps)

printLatexCmd :: Target -> Doc
printLatexCmd t =   tab <> text ("lualatex " ++ t) $+$
                    tab <> text ("-bibtex " ++ t) $+$
                    tab <> text ("lualatex " ++ t) $+$
                    tab <> text ("lualatex " ++ t)