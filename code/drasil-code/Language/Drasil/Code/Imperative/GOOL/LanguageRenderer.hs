module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer (
  -- * Common Syntax
  doxConfigName, makefileName, sampleInputName, readMeName
) where

----------------------------------------
-- Syntax common to several renderers --
----------------------------------------

doxConfigName, makefileName, sampleInputName, readMeName :: String
doxConfigName = "doxConfig"
makefileName = "Makefile"
sampleInputName = "input.txt"
readMeName = "README.md"

