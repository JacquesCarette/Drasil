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
  impType :: ImplementationType,
  logFile :: String,
  logging :: Logging,
  onSfwrConstraint :: ConstraintBehaviour,
  onPhysConstraint :: ConstraintBehaviour
}

data ImplementationType = Library
                        | Program

data Logging = LogNone
             | LogFunc
             | LogVar
             | LogAll
             
data ConstraintBehaviour = Warning
                         | Exception
             
defaultChoices :: Choices
defaultChoices = Choices {
  impType = Program,
  logFile = "log.txt",
  logging = LogNone,
  onSfwrConstraint = Exception,
  onPhysConstraint = Warning
}