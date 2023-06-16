module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer (
  -- * Common Syntax
  doxConfigName, makefileName, sampleInputName, sampleOutputName, readMeName
) where

----------------------------------------
-- Syntax common to several renderers --
----------------------------------------

-- | Common syntax for several renderers.
doxConfigName, makefileName, sampleInputName, sampleOutputName, readMeName :: String
-- | "doxConfig".
doxConfigName = "doxConfig"
-- | \"Makefile\".
makefileName = "Makefile"
-- | "input.txt".
sampleInputName = "input.txt"
-- | "output.txt"
sampleOutputName = "output.txt"
-- | "README.md".
readMeName = "README.md"

