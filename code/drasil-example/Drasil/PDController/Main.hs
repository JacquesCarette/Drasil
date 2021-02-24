module Main (main) where

import Data.Drasil.ExternalLibraries.ODELibraries (scipyODELSodaPkg)

import Drasil.PDController.Body (pidODEInfo, printSetting, si, srs)

import Language.Drasil.Code
       (AuxFile(..), Choices(..), CodeSpec, Comments(..), ConstantRepr(..),
        ConstantStructure(..), ConstraintBehaviour(..), ImplementationType(..),
        InputModule(..), Lang(..), Modularity(..), Structure(..), Verbosity(..),
        Visibility(..), codeSpec, defaultChoices)

import Language.Drasil.Generate (gen, genCode)
import Language.Drasil.Printers (DocSpec(DocSpec), DocType(SRS, Website))

codeSpecs :: CodeSpec
codeSpecs = codeSpec si codeChoices []

codeChoices :: Choices
codeChoices
  = defaultChoices{lang = [Python], modularity = Modular Combined,
                   impType = Program, logFile = "log.txt", logging = [],
                   comments = [CommentFunc, CommentClass, CommentMod],
                   doxVerbosity = Verbose, dates = Hide,
                   onSfwrConstraint = Exception, onPhysConstraint = Exception,
                   inputStructure = Unbundled, constStructure = Store Bundled,
                   constRepr = Const,
                   auxFiles =
                     [SampleInput
                        "../../datafiles/PDController/sampleInput.txt",
                      ReadME],
                   odeLib = [scipyODELSodaPkg], odes = [pidODEInfo]}

main :: IO ()
main
  = do gen (DocSpec SRS "PDController_SRS") srs printSetting
       gen (DocSpec Website "PDController_SRS") srs printSetting
       genCode codeChoices codeSpecs
