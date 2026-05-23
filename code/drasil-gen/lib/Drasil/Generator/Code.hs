module Drasil.Generator.Code (
  -- * Generators
  exportCode, exportCodeZoo,
  -- * Internal Functions
  codedDirName
) where

import Prelude hiding (id)
import Control.Lens ((^.))
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (showGregorian)
import System.Directory (getCurrentDirectory, setCurrentDirectory, createDirectoryIfMissing)

import Drasil.GOOL (unJC, unPC, unCSC, unCPPC, unSC, CodeType(..))
import Drasil.GProc (unJLC)
import Language.Drasil (Space(..))
import Language.Drasil.Code (getSampleData, generateCode, generateCodeProc,
  generator, readWithDataDesc, sampleInputDD, codeSpec,
  Architecture(impType, modularity), Choices(Choices, maps, lang,
  architecture, optFeats, dataInfo), ConstantRepr(..),
  ConstantStructure(..), DataInfo(constRepr, inputStructure,
  constStructure), ImplementationType(..), LogConfig(logging), Logging,
  Maps(spaceMatch), Modularity(..), OptionalFeatures(logConfig), SpaceMatch,
  Structure(..), Lang(Julia, Java,
  Python, CSharp, Cpp, Swift), HasOldCodeSpec(extInputsO))
import Language.Drasil.GOOL (unPP, unJP, unCSP, unCPPP, unSP, unJLP)
import Drasil.System (SmithEtAlSRS, programName)

-- | Generate an ICO-style executable software artifact.
exportCode :: SmithEtAlSRS -> Choices -> IO ()
exportCode = genCode

-- | Internal: Generate a zoo of ICO-style executable softifact.
exportCodeZoo :: SmithEtAlSRS -> [Choices] -> IO ()
exportCodeZoo syst = mapM_ $ \chcs -> do
  let dir = map toLower $ codedDirName (syst ^. programName) chcs
  workingDir <- getCurrentDirectory
  createDirectoryIfMissing False dir
  setCurrentDirectory dir
  exportCode syst chcs
  setCurrentDirectory workingDir

-- | Calls the code generator.
genCode :: SmithEtAlSRS -> Choices -> IO ()
genCode syst chs = do
  let spec = codeSpec syst chs
  workingDir <- getCurrentDirectory
  time <- getCurrentTime
  sampData <- maybe (return []) (\sd -> readWithDataDesc sd $ sampleInputDD
    (spec ^. extInputsO)) (getSampleData chs)
  createDirectoryIfMissing False "src"
  setCurrentDirectory "src"
  let genLangCode Java = genCall Java unJC unJP
      genLangCode Python = genCall Python unPC unPP
      genLangCode CSharp = genCall CSharp unCSC unCSP
      genLangCode Cpp = genCall Cpp unCPPC unCPPP
      genLangCode Swift = genCall Swift unSC unSP
      genLangCode Julia = genCallProc Julia unJLC unJLP
      genCall lng unProgRepr unPackRepr = generateCode lng unProgRepr
        unPackRepr $ generator lng (showGregorian $ utctDay time) sampData chs spec
      genCallProc lng unProgRepr unPackRepr = generateCodeProc lng unProgRepr
        unPackRepr $ generator lng (showGregorian $ utctDay time) sampData chs spec
  mapM_ genLangCode (lang chs)
  setCurrentDirectory workingDir

-- | Find name of folders created for a "zoo" of executable softifacts.
--
-- FIXME: This is a hack. The generation phase should emit what artifacts it
-- created.
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
