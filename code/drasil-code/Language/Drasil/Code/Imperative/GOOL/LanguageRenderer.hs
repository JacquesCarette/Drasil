module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer (
  -- * Common Syntax
  doxConfigName, makefileName, sampleInputName
) where

----------------------------------------
-- Syntax common to several renderers --
----------------------------------------

doxConfigName, makefileName, sampleInputName :: String
doxConfigName = "doxConfig"
makefileName = "Makefile"
sampleInputName = "input.txt"
