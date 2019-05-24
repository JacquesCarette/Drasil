module Build.Drasil.Make.Helpers where

import Text.PrettyPrint (Doc, nest, text, vcat, ($+$))

-- | Helper for rendering OS-specific variables
osDefinitions :: Doc
osDefinitions =
  text "ifeq \"$(OS)\" \"Windows_NT\"" $+$
  msIndent (text "TARGET_EXTENSION=.exe") $+$
  msIndent (text "RM=del") $+$
  text "else" $+$
  msIndent (vcat [text "UNAME_S := $(shell uname -s)",
    text "ifeq ($(UNAME_S), Linux)",
    msIndent $ text "TARGET_EXTENSION=",
    msIndent $ text "RM=rm",
    text "endif",
    text "ifeq ($(UNAME_S), Darwin)",
    msIndent $ text "TARGET_EXTENSION=",
    msIndent $ text "RM=rm",
    text "endif"]) $+$
  text "endif" $+$
  text ""

-- | Helper for prepending common features to a Makefile
addCommonFeatures :: Doc -> Doc
addCommonFeatures m = osDefinitions $+$ m

tab :: Doc
tab = text "\t"

-- | Makefile Syntax Indent (i.e. non recipes)
msIndent :: Doc -> Doc
msIndent = nest 4