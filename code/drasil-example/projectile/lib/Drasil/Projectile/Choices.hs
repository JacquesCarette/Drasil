{-# LANGUAGE TupleSections #-}
module Drasil.Projectile.Choices where

import Language.Drasil (Space(..), programName)
import Language.Drasil.Code (Choices(..), Comments(..), 
  Verbosity(..), ConstraintBehaviour(..), ImplementationType(..), Lang(..), 
  Logging(..), Modularity(..), Structure(..), ConstantStructure(..), 
  ConstantRepr(..), CodeConcept(..), matchConcepts, SpaceMatch,
  matchSpaces, AuxFile(..), Visibility(..), defaultChoices, codeSpec, makeArchit, 
  Architecture(..), makeData, DataInfo(..), Maps(..), makeMaps, spaceToCodeType,
  makeConstraints, makeDocConfig, makeLogConfig, LogConfig(..), OptionalFeatures(..), 
  makeOptFeats)
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
                                getSysName SI{_sys = sysName} = programName sysName
  in do
    workingDir <- getCurrentDirectory
    createDirectoryIfMissing False dir
    setCurrentDirectory dir
    genCode c (codeSpec fullSI c [])
    setCurrentDirectory workingDir
    genCodeWithChoices cs

codedDirName :: String -> Choices -> String
codedDirName n Choices {
  architecture = a,
  optFeats = o,
  dataInfo = d,
  maps = m} = 
  intercalate "_" [n, codedMod $ modularity a, codedImpTp $ impType a, codedLog $ logging $ logConfig o, 
    codedStruct $ inputStructure d, codedConStruct $ constStructure d, 
    codedConRepr $ constRepr d, codedSpaceMatch $ spaceMatch m]

codedMod :: Modularity -> String
codedMod Unmodular = "U"
codedMod Modular = "M"

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
    architecture = makeArchit Modular Program,
    dataInfo = makeData Bundled (Store Unbundled) Var
  },
  baseChoices {
    architecture = makeArchit Modular Library,
    dataInfo = makeData Unbundled (Store Unbundled) Var,
    maps = makeMaps (matchConcepts [(piConst, [Pi])]) matchToFloats
  },
  baseChoices {
    dataInfo = makeData Bundled (Store Bundled) Const,
    optFeats = makeOptFeats
      (makeDocConfig [CommentFunc, CommentClass, CommentMod] Quiet Hide)
      (makeLogConfig [LogVar, LogFunc] "log.txt")
      [SampleInput "../../../datafiles/projectile/sampleInput.txt", ReadME],
    folderVal = 5
  },
  baseChoices {
    dataInfo = makeData Bundled WithInputs Var,
    maps = makeMaps (matchConcepts [(piConst, [Pi])]) matchToFloats,
    optFeats = makeOptFeats
      (makeDocConfig [CommentFunc, CommentClass, CommentMod] Quiet Hide)
      (makeLogConfig [LogVar, LogFunc] "log.txt")
      [SampleInput "../../../datafiles/projectile/sampleInput.txt", ReadME],
    folderVal = 5
  }]

matchToFloats :: SpaceMatch
matchToFloats = matchSpaces (map (,[Float, Double]) [Real, Rational])

baseChoices :: Choices
baseChoices = defaultChoices {
  lang = [Python, Cpp, CSharp, Java, Swift],
  architecture = makeArchit Unmodular Program,
  dataInfo = makeData Unbundled WithInputs Var,
  maps = makeMaps (matchConcepts [(piConst, [Pi])]) spaceToCodeType,
  optFeats = makeOptFeats
    (makeDocConfig [CommentFunc, CommentClass, CommentMod] Quiet Hide)
    (makeLogConfig [] "log.txt")
    [SampleInput "../../../datafiles/projectile/sampleInput.txt", ReadME],
  srsConstraints = makeConstraints Warning Warning,
  folderVal = 5
}
