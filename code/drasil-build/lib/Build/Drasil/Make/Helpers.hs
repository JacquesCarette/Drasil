-- | Helper functions for creating Makefiles.
module Build.Drasil.Make.Helpers (
  addCommonFeatures,
  tab, msIndent
) where

import Build.Drasil.Make.AST (Command(C), Rule(R))
import Build.Drasil.Make.MakeString (MakeString(..), MVar, varName, win, mac,
  linux, isOsVar)

import Data.List (nubBy)
import Text.PrettyPrint (Doc, empty, nest, text, vcat, ($+$))

-- | Assignment operator ("=").
($=) :: MVar -> String -> Doc
a $= b = text $ varName a ++ "=" ++ b

-- | Defines variables dependent on OS.
defineOsVars :: (MVar -> String) -> [MVar] -> Doc
defineOsVars f m = msIndent $ vcat $ map (\x -> x $= f x) m

-- | Helper for rendering OS-specific variables.
osDefinitions :: [MVar] -> Doc
osDefinitions [] = empty
osDefinitions m =
  text "ifeq \"$(OS)\" \"Windows_NT\"" $+$
  defineOsVars win m $+$
  text "else" $+$
  msIndent (vcat [text "UNAME_S := $(shell uname -s)",
    text "ifeq ($(UNAME_S), Linux)",
    defineOsVars linux m,
    text "endif",
    text "ifeq ($(UNAME_S), Darwin)",
    defineOsVars mac m,
    text "endif"]) $+$
  text "endif" $+$
  text ""

-- | Deduplicates a list of variables and ensures duplicate variables have the same definition.
uniqueVars :: [MVar] -> [MVar]
uniqueVars = nubBy (\x y -> varName x == varName y && (x == y ||
        error ("Found disparate variable definitions for " ++ varName x)))

-- | Extracts variables from a Makefile rule.
extractVars :: Rule -> [MVar]
extractVars (R _ t d _ cs) = concatMap getVars $ t : d ++ map (\(C s _) -> s) cs

-- | Gets one or more variables from a MakeString.
getVars :: MakeString -> [MVar]
getVars (Mr _) = []
getVars (Mv v) = [v]
getVars (Mc a b) = getVars a ++ getVars b

-- | Helper for prepending common features to a Makefile.
addCommonFeatures :: [Rule] -> Doc -> Doc
addCommonFeatures r m = osDefinitions (filter isOsVar $ uniqueVars $ concatMap extractVars r) $+$ m

-- | Recipes must be indented with tabs.
tab :: Doc
tab = text "\t"

-- | Makefile Syntax Indent (i.e. non recipes).
msIndent :: Doc -> Doc
msIndent = nest 4
