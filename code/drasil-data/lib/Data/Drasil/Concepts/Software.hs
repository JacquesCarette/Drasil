-- | Defines concepts used in the field of software.
module Data.Drasil.Concepts.Software where

import Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S
import Drasil.Metadata.Documentation (srs)
import Drasil.Database (mkUid)

import Data.Drasil.Concepts.Computation (algorithm, dataStruct, inParam)
import Data.Drasil.Concepts.Documentation (input_, physical, physicalConstraint,
  softwareConstraint, quantity)
import Data.Drasil.Concepts.Math (equation)

-- * Common Software Concepts

-- | Collects all software-related concepts.
softwarecon :: [ConceptChunk]
softwarecon = [correctness, verifiability, physLib,
  understandability, reusability, maintainability, portability,
  performance, accuracy, reliability]

c, errMsg, physLib, program :: ConceptChunk

c       = cncpt''' (mkUid "c") (pn "C")
  (S "the C programming language")
physLib = cncpt''' (mkUid "physLib") (cnIES "physics library")
  (S "a programming library which provides functions for modelling physical phenomenon")
program = cncpt''' (mkUid "program") (cn' "program")
  (S "a series of coded software instructions to control the operation of a computer or other machine")
errMsg  = cncpt''' (mkUid "errMsg") (cn' "error message")
  (S "a message that indicates an incorrect instruction has been given, or that there is an error resulting from faulty software")

-- * Non-functional Requirements

accuracy, correctness, maintainability, performance, performanceSpd, portability,
  reliability, reusability, understandability, verifiability :: ConceptChunk

qualOfBeing :: String -> String
qualOfBeing s = "the quality or state of being" ++ s

accuracy          = cncpt''' (mkUid "accuracy")          (nounPhraseSP "accuracy")
  (S $ qualOfBeing "correct or precise")

correctness       = cncpt''' (mkUid "correctness")       (nounPhraseSP "correctness")
  (S $ qualOfBeing "free from error")

maintainability   = cncpt''' (mkUid "maintainability")   (nounPhraseSP "maintainability")
  (S "the probability of performing a successful repair action within a given time")

performance       = cncpt''' (mkUid "performance")       (nounPhraseSP "performance")
  (S "the action or process of carrying out or accomplishing an action, task, or function")

performanceSpd    = cncpt''' (mkUid "performanceSpd")    (cn' "performance speed")
  (S "the action or process of carrying out or accomplishing an action, task, or function quickly")

portability       = cncpt''' (mkUid "portability")       (nounPhraseSP "portability")
  (S "the ability of software to be transferred from one machine or system to another")

reliability       = cncpt''' (mkUid "reliability")       (nounPhraseSP "reliability")
  (S ("the degree to which the result of a measurement, calculation," ++
  "or specification can be depended on to be accurate"))

reusability       = cncpt''' (mkUid "reusability")       (nounPhraseSP "reusability")
  (S "the use of existing assets in some form within the software product development process")

understandability = cncpt''' (mkUid "understandability") (nounPhraseSP "understandability")
  (S $ qualOfBeing "understandable")

verifiability     = cncpt''' (mkUid "verifiability")     (nounPhraseSP "verifiability")
  (S $ qualOfBeing "capable of being verified, confirmed, or substantiated")

-- * Module Concepts

-- MODULES Concepts (Maybe move to D.D.C.Software.Modules ?)

--FIXME: "hiding" is not a noun.
hwHiding :: ConceptChunk
hwHiding = cncpt''' (mkUid "hwHiding") (cn "hardware hiding")
  (S ("hides the exact details of the hardware, and provides a uniform interface" ++
   " for the rest of the system to use"))

modBehavHiding :: ConceptChunk
modBehavHiding = dccWDS "modBehavHiding" (cn "behaviour hiding") (foldlSent_
  [S "includes programs that provide externally visible behaviour of the",
   S "system as specified in the", phrase srs, sParen $ short srs +:+. S "documents",
   S "This module serves as a communication layer between the hardware-hiding module",
   S "and the software decision module. The programs in this module will need",
   S "to change if there are changes in the", short srs])

modControl :: ConceptChunk
modControl = cncpt''' (mkUid "modControl") (cn' "control module") (S "provides the main program")

modSfwrDecision :: ConceptChunk
modSfwrDecision = dccWDS "modSfwrDecision" (cn' "software decision module") (foldlSent_
  [S "includes", plural dataStruct `S.and_` plural algorithm,
   S "used in the system that do not provide direct interaction with the user"])

modInputFormat :: ConceptChunk
modInputFormat = cncpt''' (mkUid "modInputFormat") (cn' "input format module")
  (S "converts the input data into the data structure used by the input parameters module")

modInputParam :: ConceptChunk
modInputParam = dccWDS "modInputParam" (cn' "input parameter module") (foldlSent_
  [S "stores the parameters needed for the program, including" +:+. foldlList Comma List
   [S "material properties", S "processing conditions", S "numerical parameters"],
   S "The values can be read as needed. This module knows how many parameters it stores"])

modInputConstraint :: ConceptChunk
modInputConstraint = cncpt''' (mkUid "modInputConstraint") (cn' "input constraint module")
  (S ("defines the constraints on the input data and gives an error if " ++
   "a constraint is violated"))

modInputVerif :: ConceptChunk
modInputVerif = dccWDS "modInputVerif" (cn' "input verification module") (foldlSent
  [S "verifies that the", plural inParam, S "comply with", phrase physical `S.and_`
   plural softwareConstraint, S "Throws an error if a parameter violates a" +:+.
   phrase physicalConstraint, S "Throws a warning if a parameter violates a",
   phrase softwareConstraint])

modDerivedVal :: ConceptChunk
modDerivedVal = dccWDS "modDerivedVal" (cn' "derived value module") (foldlSent_
  [S "defines the", plural equation, S "transforming the initial", plural input_,
   S "into derived", plural quantity])

modInterpolation :: ConceptChunk
modInterpolation = dccWDS "modInterpolation" (cn "interpolation module") (foldlSent_
  [S "provides the", plural equation, S "that take the", plural inParam `S.and_`
   S "interpolation data" `S.and_` S "return an interpolated value"])

modInterpDatum :: ConceptChunk
modInterpDatum = dccWDS "modInterpDatum" (cn "interpolation datum module") (foldlSent_
  [S "converts the input interpolation data into the", phrase dataStruct,
   S "used by the", phrase modInterpolation])

{-- Concept Chunks for Modules  --}

modSeqServ :: ConceptChunk
modSeqServ = dccWDS "modSeqServ" (cn' "sequence data structure")
  (S "Provides array manipulation operations, such as" +:+ foldlList Comma List
   [S "building an array", S "accessing a specific entry", S "slicing an array"])

modLinkedServ :: ConceptChunk
modLinkedServ = dccWDS "modLinkedServ" (cn' "linked data structure")
  (S "Provides tree manipulation operations, such as" +:+ foldlList Comma List
   [S "building a tree", S "accessing a specific entry"])

modAssocServ :: ConceptChunk
modAssocServ = dccWDS "modAssocServ" (cn' "associative data structure")
  (S "Provides operations on hash tables, such as" +:+ foldlList Comma List
   [S "building a hash table", S "accessing a specific entry"])

modVectorServ :: ConceptChunk
modVectorServ = dccWDS "modVectorServ" (cn' "vector")
  (S "Provides vector operations such as" +:+ foldlList Comma List [S "addition",
   S "scalar and vector multiplication", S "dot and cross products", S "rotations"])

modPlotDesc :: ConceptChunk
modPlotDesc = cncpt''' (mkUid "modPlotDesc") (cn' "plotting") (S "provides a plot function")

modOutputfDescFun :: Sentence -> ConceptChunk
modOutputfDescFun desc = dccWDS "modOutputfDescFun" (cn' "output format")
  (S "outputs the results of the calculations, including the" +:+ desc)

-- ODE Solver Module
modOdeDesc :: ConceptChunk
modOdeDesc = dccWDS "modOdeDesc" (nounPhraseSP "ODE solver")
  (S "provides solvers that take the" +:+ foldlList Comma List
   [S "governing equation", S "initial conditions", S "numerical parameters"] `S.and_`
   S "solve them")
