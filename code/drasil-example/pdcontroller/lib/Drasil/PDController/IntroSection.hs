module Drasil.PDController.IntroSection where

import Drasil.PDController.Concepts
import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S
import Drasil.PDController.MetaConcepts (progName)

introPara, introPurposeOfDoc, introscopeOfReq :: Sentence

-- template
background :: Sentence
background = S "Automatic process control with a controller (P/PI/PD/PID) is used in a variety of applications such as thermostats and automobile cruise-control."
longProgramName, shortProgramName :: Sentence
longProgramName  = phrase progName
shortProgramName = sParen (S "PDC")
origin, goal :: Sentence
origin =
  S ", which is based on the original, manually created version of"
  +:+ namedRef externalLinkRef (S "PD Controller")
goal =
  S ", whose goal is to provide a model of a"
  +:+ phrase pidC
  +:+ S "that can be used to tune the gain constants before deployment"

introPara = foldlSent
  [ background
  , S "This document describes the requirements of a program called"
      +:+ longProgramName
      +:+ shortProgramName
      +:+ origin
      +:+ goal
  ]

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
