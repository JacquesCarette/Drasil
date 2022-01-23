{-# LANGUAGE TupleSections #-}
module Drasil.Projectile.Choices where

import Language.Drasil (Space(..), abrv)
import Language.Drasil.Code (Choices(..), Comments(..), 
  Verbosity(..), ConstraintBehaviour(..), ImplementationType(..), Lang(..), 
  Logging(..), Modularity(..), Structure(..), ConstantStructure(..), 
  ConstantRepr(..), InputModule(..), CodeConcept(..), matchConcepts, SpaceMatch,
  matchSpaces, AuxFile(..), Visibility(..), defaultChoices, codeSpec)
import Language.Drasil.Generate (genCode)
import GOOL.Drasil (CodeType(..))
import Data.Drasil.Quantities.Math (piConst)
import Drasil.Projectile.Body (fullSI)
import SysInfo.Drasil (SystemInformation(SI, _sys))

import Data.List (intercalate)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, 
  setCurrentDirectory)
import Data.Char (toLower)

genCodeWithChoices :: [Choices] -> IO ()
genCodeWithChoices [] = return ()
genCodeWithChoices (c:cs) = let dir = map toLower $ codedDirName (getSysName fullSI) c
                                getSysName SI{_sys = sysName} = abrv sysName
  in do
    workingDir <- getCurrentDirectory
    createDirectoryIfMissing False dir
    setCurrentDirectory dir
    genCode c (codeSpec fullSI c [])
    setCurrentDirectory workingDir
    genCodeWithChoices cs

codedDirName :: String -> Choices -> String
codedDirName n Choices {
  modularity = m,
  impType = it,
  logging = l,
  inputStructure = is,
  constStructure = cs,
  constRepr = cr,
  spaceMatch = sm} = 
  intercalate "_" [n, codedMod m, codedImpTp it, codedLog l, codedStruct is, 
    codedConStruct cs, codedConRepr cr, codedSpaceMatch sm]
  
codedMod :: Modularity -> String
codedMod Unmodular = "U"
codedMod (Modular Combined) = "C"
codedMod (Modular Separated) = "S"

codedImpTp :: ImplementationType -> String
codedImpTp Program = "P"
codedImpTp Library = "L"

codedLog :: [Logging] -> String
codedLog [] = "NoL"
codedLog _ = "L"

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

codedSpaceMatch :: SpaceMatch -> String
codedSpaceMatch sm = case sm Real of [Double, Float] -> "D"
                                     [Float, Double] -> "F" 
                                     _ -> error 
                                       "Unexpected SpaceMatch for Projectile"

choiceCombos :: [Choices]
choiceCombos = [baseChoices, 
  baseChoices {
    modularity = Modular Combined,
    inputStructure = Bundled,
    constStructure = Store Unbundled},
  baseChoices {
    modularity = Modular Separated,
    impType = Library,
    constStructure = Store Unbundled,
    spaceMatch = matchToFloats},
  baseChoices {
    logging = [LogVar, LogFunc],
    inputStructure = Bundled,
    constStructure = Store Bundled,
    constRepr = Const},
  baseChoices {
    logging = [LogVar, LogFunc],
    inputStructure = Bundled,
    spaceMatch = matchToFloats}]

matchToFloats :: SpaceMatch
matchToFloats = matchSpaces (map (,[Float, Double]) [Real, Radians, Rational])

baseChoices :: Choices
baseChoices = defaultChoices {
  lang = [Python, Cpp, CSharp, Java, Swift],
  modularity = Unmodular,
  impType = Program,
  logFile = "log.txt",
  logging = [],
  comments = [CommentFunc, CommentClass, CommentMod],
  doxVerbosity = Quiet,
  dates = Hide,
  onSfwrConstraint = Warning,
  onPhysConstraint = Warning,
  inputStructure = Unbundled,
  constStructure = WithInputs,
  constRepr = Var,
  conceptMatch = matchConcepts [(piConst, [Pi])],
  auxFiles = [SampleInput "../../../datafiles/projectile/sampleInput.txt", ReadME]
}
