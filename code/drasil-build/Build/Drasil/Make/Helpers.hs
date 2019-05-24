module Build.Drasil.Make.Helpers where

import Build.Drasil.Make.AST (Command(C), Rule(R))
import Build.Drasil.Make.MakeString (MakeString(Mc, Mr, Mv), MVar(Free, Implicit, Os))

import Data.List (nubBy)
import Text.PrettyPrint (Doc, nest, text, vcat, ($+$))

($=) :: MVar -> String -> Doc
a $= b = text $ varName a ++ "=" ++ b

win :: MVar -> String
win (Os _ w _ _) = w
win _ = error "Expected Os Variable"

mac :: MVar -> String
mac (Os _ _ m _) = m
mac _ = error "Expected Os Variable"

linux :: MVar -> String
linux (Os _ _ _ l) = l
linux _ = error "Expected Os Variable"

defineOsVars :: (MVar -> String) -> [MVar] -> Doc
defineOsVars f m = msIndent $ vcat $ map (\x -> x $= f x) m

-- | Helper for rendering OS-specific variables
osDefinitions :: [MVar] -> Doc
osDefinitions m =
  text "ifeq \"$(OS)\" \"Windows_NT\"" $+$
  msIndent (text "TARGET_EXTENSION=.exe") $+$
  msIndent (text "RM=del") $+$
  defineOsVars win m $+$
  text "else" $+$
  msIndent (vcat [text "UNAME_S := $(shell uname -s)",
    text "ifeq ($(UNAME_S), Linux)",
    msIndent $ text "TARGET_EXTENSION=",
    msIndent $ text "RM=rm",
    defineOsVars linux m,
    text "endif",
    text "ifeq ($(UNAME_S), Darwin)",
    msIndent $ text "TARGET_EXTENSION=",
    msIndent $ text "RM=rm",
    defineOsVars mac m,
    text "endif"]) $+$
  text "endif" $+$
  text ""

-- | Deduplicates a list of variables and ensures duplicate variables have the same definition
uniqueVars :: [MVar] -> [MVar]
uniqueVars = nubBy (\x y -> varName x == varName y && (x == y ||
        error ("Found disparate variable definitions for " ++ varName x)))

varName :: MVar -> String
varName (Free s) = s
varName (Implicit s) = s
varName (Os s _ _ _) = s

-- | Extracts variables from a Makefile rule
extractVars :: Rule -> [MVar]
extractVars (R t d _ cs) = concatMap getVars $ t : d ++ map (\(C s _) -> s) cs

getVars :: MakeString -> [MVar]
getVars (Mr _) = []
getVars (Mv v) = [v]
getVars (Mc a b) = getVars a ++ getVars b

isOsVar :: MVar -> Bool
isOsVar Os{} = True
isOsVar _ = False

-- | Helper for prepending common features to a Makefile
addCommonFeatures :: [Rule] -> Doc -> Doc
addCommonFeatures r m = osDefinitions (filter isOsVar $ uniqueVars $ concatMap extractVars r) $+$ m

-- | Recipes must be indented with tabs
tab :: Doc
tab = text "\t"

-- | Makefile Syntax Indent (i.e. non recipes)
msIndent :: Doc -> Doc
msIndent = nest 4
