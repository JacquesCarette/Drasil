module Build.Drasil.Make.Print where

import Prelude hiding ((<>))
import Data.List (elem)
import Text.PrettyPrint (Doc, empty, text, (<>), (<+>), ($+$), ($$), hsep, vcat)

import Build.Drasil.Make.AST (Command(C), CommandOpts(IgnoreReturnCode),
  Dependencies, Makefile(M), Rule(R), Target, Type(Abstract))
import Build.Drasil.Make.Import (RuleTransformer, toMake)
import Build.Drasil.Make.Helpers (addCommonFeatures, tab)

-- | Generates the makefile by calling 'build' after 'toMake'
genMake :: RuleTransformer c => [c] -> Doc
genMake = build . toMake

-- | Renders the makefile rules
build :: Makefile -> Doc
build (M rules) = addCommonFeatures $
  vcat (map (\x -> printRule x $+$ text "") rules) $$ printPhony rules

-- | Renders specific makefile rules. Called by 'build'
printRule :: Rule -> Doc
printRule (R t d _ c) = printTarget t d $+$ (printCmds c)

-- | Gathers all rules to abstract targets and tags them as phony.
printPhony :: [Rule] -> Doc
printPhony = (<+>) (text ".PHONY:") . hsep . map (\(R t _ _ _) -> text t) .
  filter (\(R _ _ t _) -> t == Abstract)

-- | Renders targets with their dependencies
printTarget :: Target -> [Dependencies] -> Doc
printTarget nameLb deps = text (nameLb ++ ":") <+> hsep (map text deps)

printCmd :: Command -> Doc
printCmd (C c opts) = text $ (if IgnoreReturnCode `elem` opts then "-" else "") ++ c

printCmds :: [Command] -> Doc
printCmds = foldr (($+$) . (<>) tab . printCmd) empty
