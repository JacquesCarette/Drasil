module Language.Drasil.Make.Helpers where

import Text.PrettyPrint (Doc, text, ($+$))

-- | Helper for rendering OS-specific extensions
osDefinitions :: Doc
osDefinitions = text "ifeq \"$(OS)\" \"Windows_NT\"" $+$
                text "\tTARGET_EXTENSION=.exe" $+$
                text "\tRM=del" $+$
                text "else" $+$
                text "\tUNAME_S := $(shell uname -s)" $+$
                text "\tifeq ($(UNAME_S), Linux)" $+$
                text "\t\tTARGET_EXTENSION=" $+$
                text "\t\tRM=rm" $+$
                text "\tendif" $+$
                text "\tifeq ($(UNAME_S), Darwin)" $+$
                text "\t\tTARGET_EXTENSION=" $+$
                text "\t\tRM=rm" $+$
                text "\tendif" $+$
                text "endif" $+$
                text ""

-- | Helper for prepending common features to a Makefile
addCommonFeatures :: Doc -> Doc
addCommonFeatures m = osDefinitions $+$ m
