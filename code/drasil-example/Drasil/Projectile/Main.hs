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

import System.Directory (createDirectoryIfMissing, getCurrentDirectory, 
  setCurrentDirectory)

main :: IO()
main = do
  gen (DocSpec SRS     "Projectile_SRS") srs printSetting
  gen (DocSpec Website "Projectile_SRS") srs printSetting
  genCodeWithChoices (zip choiceCombos [1..])

genCodeWithChoices :: [(Choices, Int)] -> IO ()
genCodeWithChoices [] = return ()
genCodeWithChoices ((c,n):cns) = let dir = getAccStr projectileTitle ++ show n 
  in do
    workingDir <- getCurrentDirectory
    createDirectoryIfMissing False dir
    setCurrentDirectory dir
    genCode c (codeSpec si c [])
    setCurrentDirectory workingDir
    genCodeWithChoices cns

modularities :: [Modularity]
modularities = [Unmodular, Modular Combined, Modular Separated]

loggings :: [Logging]
loggings = [LogNone, LogAll]

-- comms :: [[Comments]]
-- comms = [[], [CommentFunc, CommentClass, CommentMod]]

inputStructs :: [Structure]
inputStructs = [Bundled, Unbundled]

constStructs :: [ConstantStructure]
constStructs = [Inline, Store Bundled, Store Unbundled, WithInputs]

constReprs :: [ConstantRepr]
constReprs = [Var, Const]

choiceCombos :: [Choices]
choiceCombos = [buildChoices m l i cs cr | m <- modularities, l <- loggings, i <- inputStructs, cs <- constStructs, cr <- constReprs]

buildChoices :: Modularity -> Logging -> Structure -> 
  ConstantStructure -> ConstantRepr -> Choices
buildChoices m l i cs cr = Choices {
  lang = [Python, Cpp, CSharp, Java],
  modularity = m,
  impType = Program,
  logFile = "log.txt",
  logging = l,
  comments = [CommentFunc, CommentClass, CommentMod],
  doxVerbosity = Quiet,
  dates = Hide,
  onSfwrConstraint = Warning,
  onPhysConstraint = Warning,
  inputStructure = i,
  constStructure = cs,
  constRepr = cr,
  conceptMatch = matchConcepts [piConst] [[Pi]],
  auxFiles = [SampleInput]
}