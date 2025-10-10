module Drasil.DblPend.Choices where

import Language.Drasil.Code (Choices(..), CodeSpec, codeSpec, Comments(..),
  Verbosity(..), ConstraintBehaviour(..), ImplementationType(..), Lang(..), 
  Modularity(..), Structure(..), ConstantStructure(..), ConstantRepr(..), 
  AuxFile(..), Visibility(..), defaultChoices, makeArchit, makeData,
  makeConstraints, makeDocConfig, makeLogConfig, makeOptFeats)

import Drasil.DblPend.Body (fullSI)

code :: CodeSpec
code = codeSpec fullSI choices []

choices :: Choices
choices = defaultChoices {
  lang = [Python], -- Only Python to minimize potential issues
  architecture = makeArchit Modular Program,
  dataInfo = makeData Unbundled (Store Bundled) Const,
  optFeats = makeOptFeats
    (makeDocConfig [CommentFunc, CommentClass, CommentMod] Quiet Hide)
    (makeLogConfig [] "log.txt")
    [SampleInput "../../datafiles/dblpend/sampleInput.txt", ReadME],
  srsConstraints = makeConstraints Warning Warning,
  extLibs = [] -- Temporarily disabled ODE libraries to test parse error
}