module Drasil.SglPend.Choices where

import Language.Drasil.Code (Choices(..), AuxFile(ReadME), Comments(..),
    ConstantStructure(Store), Lang(..), CodeSpec, defaultChoices, makeArchit,
    makeConstraints, makeData, makeDocConfig, makeLogConfig, makeOptFeats,
    ConstantRepr(Const), ConstraintBehaviour(Warning),
    ImplementationType(Program), Modularity(Modular), Structure(..),
    Verbosity(Quiet), Visibility(Hide), codeSpec)

import Drasil.SglPend.Body (fullSI)

code :: CodeSpec
code = codeSpec fullSI choices []

choices :: Choices
choices = defaultChoices {
  lang = [Python, Cpp, CSharp, Java],
  architecture = makeArchit Modular Program,
  dataInfo = makeData Unbundled (Store Bundled) Const,
  optFeats = makeOptFeats
    (makeDocConfig [CommentFunc, CommentClass, CommentMod] Quiet Hide)
    (makeLogConfig [] "log.txt")
    [ReadME],
  srsConstraints = makeConstraints Warning Warning,
  extLibs = []
}
