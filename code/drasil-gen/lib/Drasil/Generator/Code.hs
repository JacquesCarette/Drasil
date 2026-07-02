{-# LANGUAGE FlexibleContexts, QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Drasil.Generator.Code (
  -- * Generators
  genCode, genCodeZoo,
  -- * Internal Functions
  codedDirName
) where

import Prelude hiding (id)
import Control.Lens ((^.))
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (showGregorian)

import Drasil.FileHandling (FileLayout, directory, ps)
import Drasil.GOOL (unJC, unPC, unCSC, unCPPC, unSC, CodeType(..), ProgData, OOProg, LoggingFor (unLC))
import qualified Drasil.GOOL as OO
import Drasil.GProc (unJLC, unMLC, ProcProg)
import qualified Drasil.GProc as Proc
import Language.Drasil (Space(..), Expr)
import Language.Drasil.Code (getSampleData, generateCode, generateCodeProc,
  generator, readWithDataDesc, sampleInputDD, codeSpec,
  Architecture(impType, modularity),
  Choices(Choices, maps, lang, architecture, optFeats, dataInfo),
  ConstantRepr(..), ConstantStructure(..),
  DataInfo(constRepr, inputStructure, constStructure), ImplementationType(..),
  LogConfig(logging), Logging(LogVar), Maps(spaceMatch), Modularity(..),
  OptionalFeatures(logConfig), SpaceMatch, Structure(..),
  Lang(Julia, Java, Python, CSharp, Cpp, Swift, Matlab),
  HasOldCodeSpec(extInputsO), CodeSpec, SomeProgGenerator(..))
import Language.Drasil.GOOL (unPP, unJP, unCSP, unCPPP, unSP, unJLP, unMLP,
  PackageData, SoftwareDossierSym)
import Drasil.System (SmithEtAlSRS, programName)

-- | Generate an ICO-style executable software artifact.
genCode :: SmithEtAlSRS -> Choices -> IO FileLayout
genCode syst chs = directory [ps|src|] <$> traverse genLangCode (lang chs)
  where
    genLangCode :: Lang -> IO FileLayout
    genLangCode Java = genCall Java unJC unJP
    genLangCode Python = genCall Python unPC unPP
    genLangCode CSharp = genCall CSharp unCSC unCSP
    genLangCode Cpp = genCall Cpp unCPPC unCPPP
    genLangCode Swift = genCall Swift unSC unSP
    genLangCode Julia = genCallProc Julia unJLC unJLP
    genLangCode Matlab = genCallProc Matlab unMLC unMLP

    genCall
      :: (OOProg progRepr, SoftwareDossierSym packRepr, Monad packRepr)
      => Lang
      -> (progRepr (OO.Program progRepr) -> ProgData)
      -> (packRepr PackageData -> PackageData)
      -> IO FileLayout
    genCall lng unProgRepr unPackRepr = do
      time <- showGregorian . utctDay <$> getCurrentTime
      samples <- readSampleData
      let loggingOpts = logging $ logConfig $ optFeats chs
      let realUnProgRepr = if LogVar `elem` loggingOpts then SomeProgGenerator (unProgRepr . unLC) else SomeProgGenerator unProgRepr
      pure $ generateCode lng realUnProgRepr unPackRepr $ generator lng time samples chs spec

    genCallProc
      :: (ProcProg progRepr, SoftwareDossierSym packRepr, Monad packRepr)
      => Lang
      -> (progRepr (Proc.Program progRepr) -> ProgData)
      -> (packRepr PackageData -> PackageData)
      -> IO FileLayout
    genCallProc lng unProgRepr unPackRepr = do
      time <- showGregorian . utctDay <$> getCurrentTime
      samples <- readSampleData
      pure $ generateCodeProc lng unProgRepr unPackRepr $ generator lng time samples chs spec

    spec :: CodeSpec
    spec = codeSpec syst chs

    readSampleData :: IO [Expr]
    readSampleData =
      case getSampleData chs of
        Just sd -> readWithDataDesc sd $ sampleInputDD (spec ^. extInputsO)
        Nothing -> pure []

genCodeZoo :: SmithEtAlSRS -> [Choices] -> IO [FileLayout]
genCodeZoo syst = mapM $ \chcs -> do
    let dir = map toLower $ codedDirName (syst ^. programName) chcs
    layout <- genCode syst chcs
    return $ directory [ps|{dir}|] [layout]

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
codedSpaceMatch sm = case sm Real of
  [Double, Float] -> "D"
  [Float, Double] -> "F"
  _ -> error "Unexpected SpaceMatch for Projectile"
