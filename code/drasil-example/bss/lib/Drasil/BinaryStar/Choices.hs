module Drasil.BinaryStar.Choices (choices) where

import Language.Drasil.Code (Choices(..), Comments(..), ExtLib(..),
  Verbosity(..), ConstraintBehaviour(..), ImplementationType(..), Lang(..),
  Modularity(..), Structure(..), ConstantStructure(..), ConstantRepr(..),
  SoftwareDossierFile(..), Visibility(..), defaultChoices, makeArchit, makeData,
  makeConstraints, makeODE, makeDocConfig, makeLogConfig, makeOptFeats)
import Data.Drasil.ExternalLibraries.ODELibraries (scipyODEPckg, osloPckg,
  apacheODEPckg, odeintPckg)

import Drasil.BinaryStar.ODEs (bssODEInfo)

choices :: Choices
choices = defaultChoices {
  lang = [Python, Cpp, CSharp, Java],
  architecture = makeArchit Modular Program,
  dataInfo = makeData Unbundled (Store Bundled) Const,
  optFeats = makeOptFeats
    (makeDocConfig [CommentFunc, CommentClass, CommentMod] Quiet Hide)
    (makeLogConfig [] "log.txt")
    [SampleInput "../datafiles/bss/sampleInput.txt", ReadME],
  srsConstraints = makeConstraints Warning Warning,
  extLibs = [Math (makeODE [bssODEInfo] [scipyODEPckg, osloPckg, apacheODEPckg, odeintPckg])]
}
