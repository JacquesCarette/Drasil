module Drasil.PDController.IntroSection where

import Data.Drasil.Citations (smithLai2005)

import Drasil.PDController.Concepts
import Language.Drasil
import Utils.Drasil

introPara, introPurposeOfDoc, introscopeOfReq :: Sentence
introPara
  = foldlSent
      [S "Automatic process control with a controller (P/PI/PD/PID) is used "
         +:+ S "in a variety of applications such as thermostats, automobile "
         +:+
         S "cruise-control, etc. The gains of a controller in an application "
         +:+. S "must be tuned before the controller is ready for production",
       S "Therefore a simulation of the " +:+ phrase pidC +:+ S " with a " +:+
         phrase secondOrderSystem
         +:+ S "is created in this project that can be "
         +:+ S "used to tune the gain constants"]

introscopeOfReq
  = foldlSent_
      [S "a", phrase pidCL,
       S " with three subsystems namely a " +:+.
         foldlList Comma List
           [phrase pidC, S "a" +:+ phrase summingPt,
            S "a" +:+ phrase powerPlant],
       S "Only " +:+
         S "the Proportional and Derivative controllers are used in this software;"
         +:+. S " the Integral controller is beyond the scope of this project"
         +:+ S "Additionally, this software is intended to aid with the manual "
         +:+ S "tuning of the",
       phrase pidC]

introPurposeOfDoc
  = foldlSent
      [S "The purpose of this document is to capture all the necessary " +:+
         S "information including assumptions, data definitions, constraints, "
         +:+
         S "models, and requirements to facilitate an unambiguous development "
         +:+ S "of the PD controller software and test procedures"]

introUserChar1, introUserChar2 :: [Sentence]
introUserChar1
  = [S "control systems (control theory and controllers) at the fourth-year undergraduate level"]
introUserChar2
  = [S "engineering mathematics at a second-year undergraduate level"]

introDocOrg :: Sentence
introDocOrg
  = foldlSent
      [S "The sections in this document are based on " +:+
         makeCiteS smithLai2005]
