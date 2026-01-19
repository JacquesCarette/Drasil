module Language.Drasil.Code.FileNames (
  -- * Common Syntax
  doxConfigName, makefileName, sampleInputName, readMeName
) where

----------------------------------------
-- Syntax common to several renderers --
----------------------------------------

-- | Common syntax for several renderers.
doxConfigName, makefileName, sampleInputName, readMeName :: String
-- | "doxConfig".
doxConfigName = "doxConfig"
-- | \"Makefile\".
makefileName = "Makefile"
-- | "input.txt".
sampleInputName = "input.txt"
-- | "README.md".
readMeName = "README.md"
