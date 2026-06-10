module Drasil.BinaryStar.Concepts where

import Language.Drasil
import Data.Drasil.Concepts.Documentation (first, second_)
import Language.Drasil.Chunk.Concept.NamedCombinators (compoundNC)

---------------------------------------------------------
-- Concepts used in text generation (IdeaDict)
-- These are used when you write `phrase starOne` in sentences
---------------------------------------------------------

concepts :: [IdeaDict]
concepts = map nw [star, starOne, starTwo]
  ++ map nw defs

star :: IdeaDict
star = nc "star" (cn' "star")

starOne :: IdeaDict
starOne = compoundNC first star

starTwo :: IdeaDict
starTwo = compoundNC second_ star

---------------------------------------------------------
-- Terms with definitions (ConceptChunk)
-- These appear in SRS Section "Terminology and Definitions"
-- Each one needs: unique ID, noun phrase, definition string
---------------------------------------------------------

defs :: [ConceptChunk]
defs = [binaryStarSys, starBody, gravInteraction, newtonLUG,
        initialConditions, trajectory, centerOfMass, inertialRefFrame,
        simTimeSpan]

-- | T1: binary star system
binaryStarSys :: ConceptChunk
binaryStarSys = dcc "binaryStarSys" (nounPhraseSP "binary star system")
  "a system consisting of two stars that orbit around their common center of mass due to gravitational interaction"

-- | T2: star
starBody :: ConceptChunk
starBody = dcc "starBody" (cn' "star")
  "a massive astronomical object that is treated as a point mass in this context"

-- | T3: gravitational interaction
gravInteraction :: ConceptChunk
gravInteraction = dcc "gravInteraction" (nounPhraseSP "gravitational interaction")
  "the mutual attractive force between two masses as described by Newtonian gravity"

-- | T3b: Newton's law of universal gravitation
newtonLUG :: ConceptChunk
newtonLUG = dcc "newtonLUG" (nounPhraseSP "Newton's law of universal gravitation")
  "the law stating that every mass attracts every other mass with a force proportional to the product of their masses and inversely proportional to the square of the distance between them"

-- | T4: initial conditions
initialConditions :: ConceptChunk
initialConditions = dcc "initialConditions" (nounPhraseSP "initial conditions")
  "the positions and velocities of the stars at the start of the simulation"

-- | T5: trajectory
trajectory :: ConceptChunk
trajectory = dcc "trajectory" (cn' "trajectory")
  "the path traced by a star in space as a function of time"

-- | T6: center of mass
centerOfMass :: ConceptChunk
centerOfMass = dcc "centerOfMass" (nounPhraseSP "center of mass")
  "the point representing the average position of the mass distribution of the system"

-- | T7: inertial reference frame
inertialRefFrame :: ConceptChunk
inertialRefFrame = dcc "inertialRefFrame" (nounPhraseSP "inertial reference frame")
  "a reference frame in which Newton's laws of motion are valid without the introduction of fictitious forces"

-- | T8: simulation time span
simTimeSpan :: ConceptChunk
simTimeSpan = dcc "simTimeSpan" (nounPhraseSP "simulation time span")
  "the duration over which the evolution of the system is computed"

---------------------------------------------------------
-- Terms reused from drasil-data (already defined)
-- gravity   → from Data.Drasil.Concepts.Physics
-- cartesian → from Data.Drasil.Concepts.Math
-- These can be added to the terms list in Body.hs if needed
---------------------------------------------------------
