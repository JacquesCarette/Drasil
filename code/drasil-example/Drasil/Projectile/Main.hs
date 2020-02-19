module Main (main) where

import Language.Drasil (getAccStr)
import Language.Drasil.Code (Choices(..), Comments(..), 
  Verbosity(..), ConstraintBehaviour(..), ImplementationType(..), Lang(..), 
  Logging(..), Modularity(..), Structure(..), ConstantStructure(..), 
  ConstantRepr(..), InputModule(..), CodeConcept(..), matchConcepts, 
  AuxFile(..), Visibility(..), codeSpec)
import Language.Drasil.Generate (gen, genCode)
import Language.Drasil.Printers (DocSpec(DocSpec), DocType(SRS, Website))

import Data.Drasil.Quantities.Math (piConst)

import Drasil.Projectile.Body (printSetting, si, srs)
import Drasil.Projectile.Concepts (projectileTitle)

import Data.List (intercalate)

import System.Directory (createDirectoryIfMissing, getCurrentDirectory, 
  setCurrentDirectory)

main :: IO()
main = do
  gen (DocSpec SRS     "Projectile_SRS") srs printSetting
  gen (DocSpec Website "Projectile_SRS") srs printSetting
  genCodeWithChoices choiceCombos

genCodeWithChoices :: [Choices] -> IO ()
genCodeWithChoices [] = return ()
genCodeWithChoices (c:cs) = let dir = codedDirName (getAccStr projectileTitle) c
  in do
    workingDir <- getCurrentDirectory
    createDirectoryIfMissing False dir
    setCurrentDirectory dir
    genCode c (codeSpec si c [])
    setCurrentDirectory workingDir
    genCodeWithChoices cs

codedDirName :: String -> Choices -> String
codedDirName n Choices {
  modularity = m,
  logging = l,
  comments = c,
  inputStructure = is,
  constStructure = cs,
  constRepr = cr} = 
  intercalate "_" [n, codedMod m, codedLog l, codedComm c, codedStruct is, 
    codedConStruct cs, codedConRepr cr]
  
codedMod :: Modularity -> String
codedMod Unmodular = "U"
codedMod (Modular Combined) = "C"
codedMod (Modular Separated) = "S"

codedLog :: Logging -> String
codedLog LogNone = "NoL"
codedLog _ = "L"

codedComm :: [Comments] -> String
codedComm [] = "NoC"
codedComm _ = "C"

codedStruct :: Structure -> String
codedStruct Bundled = "B"
codedStruct Unbundled = "U"

codedConStruct :: ConstantStructure -> String
codedConStruct Inline = "I"
codedConStruct WithInputs = "WI"
codedConStruct (Store s) = codedStruct s

codedConRepr :: ConstantRepr -> String
codedConRepr Var = "V"
codedConRepr Const = "C"


choiceCombos :: [Choices]
choiceCombos = [choices1, choices2, choices3, choices4, choices5]

choices1 :: Choices
choices1 = Choices {
  lang = [Python, Cpp, CSharp, Java],
  modularity = Modular Combined,
  impType = Program,
  logFile = "log.txt",
  logging = LogNone,
  comments = [CommentFunc, CommentClass, CommentMod],
  doxVerbosity = Quiet,
  dates = Hide,
  onSfwrConstraint = Warning,
  onPhysConstraint = Warning,
  inputStructure = Bundled,
  constStructure = Store Unbundled,
  constRepr = Var,
  conceptMatch = matchConcepts [piConst] [[Pi]],
  auxFiles = [SampleInput]
}

choices2 :: Choices
choices2 = Choices {
  lang = [Python, Cpp, CSharp, Java],
  modularity = Modular Separated,
  impType = Program,
  logFile = "log.txt",
  logging = LogNone,
  comments = [CommentFunc, CommentClass, CommentMod],
  doxVerbosity = Quiet,
  dates = Hide,
  onSfwrConstraint = Warning,
  onPhysConstraint = Warning,
  inputStructure = Unbundled,
  constStructure = Store Unbundled,
  constRepr = Var,
  conceptMatch = matchConcepts [piConst] [[Pi]],
  auxFiles = [SampleInput]
}

choices3 :: Choices
choices3 = Choices {
  lang = [Python, Cpp, CSharp, Java],
  modularity = Unmodular,
  impType = Program,
  logFile = "log.txt",
  logging = LogAll,
  comments = [CommentFunc, CommentClass, CommentMod],
  doxVerbosity = Quiet,
  dates = Hide,
  onSfwrConstraint = Warning,
  onPhysConstraint = Warning,
  inputStructure = Bundled,
  constStructure = Store Bundled,
  constRepr = Const,
  conceptMatch = matchConcepts [piConst] [[Pi]],
  auxFiles = [SampleInput]
}

choices4 :: Choices
choices4 = Choices {
  lang = [Python, Cpp, CSharp, Java],
  modularity = Unmodular,
  impType = Program,
  logFile = "log.txt",
  logging = LogNone,
  comments = [],
  doxVerbosity = Quiet,
  dates = Hide,
  onSfwrConstraint = Warning,
  onPhysConstraint = Warning,
  inputStructure = Unbundled,
  constStructure = WithInputs,
  constRepr = Var,
  conceptMatch = matchConcepts [piConst] [[Pi]],
  auxFiles = [SampleInput]
}

choices5 :: Choices
choices5 = Choices {
  lang = [Python, Cpp, CSharp, Java],
  modularity = Unmodular,
  impType = Program,
  logFile = "log.txt",
  logging = LogAll,
  comments = [CommentFunc, CommentClass, CommentMod],
  doxVerbosity = Quiet,
  dates = Hide,
  onSfwrConstraint = Warning,
  onPhysConstraint = Warning,
  inputStructure = Bundled,
  constStructure = WithInputs,
  constRepr = Var,
  conceptMatch = matchConcepts [piConst] [[Pi]],
  auxFiles = [SampleInput]
}