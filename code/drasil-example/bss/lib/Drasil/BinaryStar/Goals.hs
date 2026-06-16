{-# LANGUAGE PostfixOperators #-}
module Drasil.BinaryStar.Goals (goals, goalsInputs) where

import Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Documentation (goalStmtDom)

import Drasil.BinaryStar.Unitals (mass_1, mass_2,
  xPos_1_0, yPos_1_0, xPos_2_0, yPos_2_0,
  xVel_1_0, yVel_1_0, xVel_2_0, yVel_2_0, tFinal)

goals :: [ConceptInstance]
goals = [positionsOverTime]

goalsInputs :: [Sentence]
goalsInputs = [S "the masses" +:+ sParen (ch mass_1 `S.and_` ch mass_2) `sC`
  S "the initial positions" +:+
  sParen (ch xPos_1_0 `sC` ch yPos_1_0 `sC` ch xPos_2_0 `sC` ch yPos_2_0) `sC`
  S "the initial velocities" +:+
  sParen (ch xVel_1_0 `sC` ch yVel_1_0 `sC` ch xVel_2_0 `sC` ch yVel_2_0) `sC`
  S "and the simulation time" +:+ sParen (ch tFinal)]

positionsOverTime :: ConceptInstance
positionsOverTime = cic "positionsOverTime"
  (S "Determine the positions of both stars as functions of time" +:+
   S "over the specified simulation interval" !.)
  "positionsOverTime" goalStmtDom
