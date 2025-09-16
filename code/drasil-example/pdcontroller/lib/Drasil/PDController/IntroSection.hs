module Drasil.PDController.IntroSection where

import Drasil.PDController.Concepts
import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S

introPara, introPurposeOfDoc, introscopeOfReq :: Sentence
introPara
  = foldlSent
      [S "Automatic process control with a controller (" :+: 
       short proportionalCI :+: S "/PI/" :+: short pdControllerCI :+: S "/" :+: short pidCI :+: S ") is used",
         S "in a variety of applications such as thermostats, automobile",
         S "cruise-control, etc. The gains" `S.ofA` S "controller in an application" +:+. 
         S "must be tuned before the controller is ready for production",
         S "Therefore, a simulation" `S.ofThe` phrase pidC, S "with a",
         phrase secondOrderSystem,
         S "is created in this project based" `S.onThe` S "original, manually created version of" +:+
         namedRef externalLinkRef (S "PD Controller"),
         S "that can be used to tune the gain constants"]

externalLinkRef :: Reference
externalLinkRef = makeURI "PD_Controller_SRSLink" 
  "https://github.com/muralidn/CAS741-Fall20/tree/master" 
  (shortname' $ S "PD_Controller_SRSLink")

introscopeOfReq
  = foldlSent_
      [phraseNP (a_ pidCL),
       S "with three subsystems, namely:" +:+.
          foldlList Comma List (map (phraseNP.a_)
           [pidC, summingPt, powerPlant]),
       S "Only the Proportional and Derivative controllers" `S.are` S "used in this software;" +:+.
       (S "the Integral controller" `S.is` S "beyond the scope of this project"),
       S "Additionally, this software" `S.is` S "intended to aid with the manual",
       S "tuning" `S.ofThe` phrase pidC]

scope :: Sentence
scope = foldlSent_ [phraseNP (a_ pidCL),
  S "with three subsystems, namely:" +:+
  foldlList Comma List (map (phraseNP.a_)
  [pidC, summingPt, powerPlant])]

introPurposeOfDoc
  = foldlSent
      [S "The purpose of this document" `S.is` S "to capture all the necessary",
       S "information including assumptions, data definitions, constraints,",
       S "models, and requirements to facilitate an unambiguous development"
       `S.ofThe` phrase pidC, S "software and test procedures"]

introUserChar1, introUserChar2 :: [Sentence]
introUserChar1
  = [S "control systems (control theory and controllers) at the fourth-year undergraduate level"]
introUserChar2
  = [S "engineering mathematics at a second-year undergraduate level"]
