module Language.Drasil.CodeSpec where

import Language.Drasil.Chunk.Code
import Language.Drasil.SystemInformation

data CodeSpec = CodeSpec {
  program :: CodeName,
  inputs :: [CodeChunk],
  outputs :: [CodeChunk],
  relations :: [CodeDefinition]  
}

codeSpec :: SystemInformation -> CodeSpec
codeSpec (SI sys _ _ _ _ _ _ defs ins outs _) = CodeSpec {
  program = NICN sys,
  inputs = map codevar ins,
  outputs = map codevar outs,
  relations = map qtoc defs
}