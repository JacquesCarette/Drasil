module Build.Drasil.Make.Print where

import Prelude hiding ((<>))
import Data.List (elem)
import Text.PrettyPrint (Doc, empty, text, (<>), (<+>), ($+$), hsep, vcat) 

import Build.Drasil.Make.AST (Command(C), CommandOpts(IgnoreReturnCode), Dependencies, Makefile(M), Rule(R), Target)
import Build.Drasil.Make.Import (RuleTransformer, toMake)
import Build.Drasil.Make.Helpers (addCommonFeatures, tab)

-- | Generates the makefile by calling 'build' after 'toMake'
genMake :: RuleTransformer c => [c] -> Doc
genMake = build . toMake

-- | Renders the makefile rules
build :: Makefile -> Doc
build (M rules) = addCommonFeatures $
  (vcat $ map (\x -> printRule x $+$ text "") rules) $$ printPhony rules

-- | Renders specific makefile rules. Called by 'build'
printRule :: Rule -> Doc
printRule (R t d ty c) = printTarget t d $+$ (printCmds c)

-- | Renders targets with their dependencies
printTarget :: Target -> [Dependencies] -> Doc
printTarget nameLb deps = text (nameLb ++ ":") <+> hsep (map text deps)

printCmd :: Command -> Doc
printCmd (C c opts) = tab <> (text $ (if IgnoreReturnCode `elem` opts then "-" else "") ++ c)

printCmds :: [Command] -> Doc
printCmds = foldr (($+$) . printCmd) empty
