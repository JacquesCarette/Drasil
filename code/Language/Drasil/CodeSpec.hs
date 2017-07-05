module Language.Drasil.CodeSpec where

import Language.Drasil.Chunk.Code
import Language.Drasil.Chunk.Constrained
import Language.Drasil.SystemInformation

data CodeSpec = CodeSpec {
  program :: CodeName,
  inputs :: [CodeChunk],
  outputs :: [CodeChunk],
  relations :: [CodeDefinition],
  cMap :: ConstraintMap,
  choices :: Choices
}

codeSpec :: SystemInformation -> CodeSpec
codeSpec si = codeSpec' si defaultChoices

codeSpec' :: SystemInformation -> Choices -> CodeSpec
codeSpec' (SI sys _ _ _ _ _ _ defs ins outs _ cs) ch = CodeSpec {
  program = NICN sys,
  inputs = map codevar ins,
  outputs = map codevar outs,
  relations = map qtoc defs,
  cMap = constraintMap cs,
  choices = ch
}

data Choices = Choices {
  logFile :: String,
  logging :: Logging
}

data Logging = LogNone
             | LogFunc
             | LogVar
             | LogAll
             
defaultChoices :: Choices
defaultChoices = Choices {
  logFile = "log.txt",
  logging = LogNone
}