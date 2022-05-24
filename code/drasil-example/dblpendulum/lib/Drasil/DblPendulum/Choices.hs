module Drasil.DblPendulum.Choices where

import Language.Drasil.Code (Choices(..), CodeSpec, codeSpec, Comments(..),
  Verbosity(..), ConstraintBehaviour(..), ImplementationType(..), Lang(..), 
  Modularity(..), Structure(..), ConstantStructure(..), ConstantRepr(..), 
  InputModule(..), AuxFile(..), Visibility(..), defaultChoices, makeArchit,
  makeData, makeConstraints, makeODE, makeDocConfig, makeLogConfig, makeOptFeats,
  ExtLib(..))

import Data.Drasil.ExternalLibraries.ODELibraries (scipyODEPckg, osloPckg,
  apacheODEPckg, odeintPckg)
import Drasil.DblPendulum.Body (fullSI)
import Drasil.DblPendulum.ODEs (dblPenODEInfo)

code :: CodeSpec
code = codeSpec fullSI choices []
-- Sub interpolation mod into list when possible

choices :: Choices
choices = defaultChoices {
  lang = [Python],
  architecture = makeArchit (Modular Combined) Program,
  dataInfo = makeData Unbundled (Store Bundled) Const,
  optFeats = makeOptFeats
    (makeDocConfig [CommentFunc, CommentClass, CommentMod] Quiet Hide)
    (makeLogConfig [] "log.txt")
    [SampleInput "../../datafiles/dblpendulum/sampleInput.txt", ReadME],
  srsConstraints = makeConstraints Warning Warning,
  extLibs = [Math (makeODE [dblPenODEInfo] [scipyODEPckg, osloPckg, apacheODEPckg, odeintPckg])]
}