module Drasil.BinaryStar.Concepts (
  ideaDicts, conceptChunks, starOne, starTwo, gravInteraction,
  ccsFortermsAndDefsTbl, newtonLUG
) where

import Drasil.Database (mkUid)
import Language.Drasil
import Data.Drasil.Concepts.Documentation (first, second_)
import Data.Drasil.Concepts.Physics (gravity)
import Language.Drasil.Chunk.Concept.NamedCombinators (compoundNC)

---------------------------------------------------------
-- Concepts used in text generation (IdeaDict)
-- These are used when you write `phrase starOne` in sentences
---------------------------------------------------------

ideaDicts :: [IdeaDict]
ideaDicts = [star, starOne, starTwo]

star :: IdeaDict
star = idea' (mkUid "star") (cn' "star")

starOne :: IdeaDict
starOne = compoundNC first star

starTwo :: IdeaDict
starTwo = compoundNC second_ star

---------------------------------------------------------
-- Terms with definitions (ConceptChunk)
-- These appear in SRS Section "Terminology and Definitions"
-- Each one needs: unique ID, noun phrase, definition string
---------------------------------------------------------

conceptChunks :: [ConceptChunk]
conceptChunks = gravity : ccsFortermsAndDefsTbl

ccsFortermsAndDefsTbl :: [ConceptChunk]
ccsFortermsAndDefsTbl = [binaryStarSys, starBody, gravInteraction, newtonLUG,
  initialConditions, trajectory, centerOfMass, inertialRefFrame, simTimeSpan]

-- | T1: binary star system
binaryStarSys :: ConceptChunk
binaryStarSys = cncpt''' (mkUid "binaryStarSys") (nounPhraseSP "binary star system")
  (S "a system consisting of two stars that orbit around their common center of mass due to gravitational interaction")

-- | T2: star
starBody :: ConceptChunk
starBody = cncpt''' (mkUid "starBody") (cn' "star")
  (S "a massive astronomical object that is treated as a point mass in this context")

-- | T3: gravitational interaction
gravInteraction :: ConceptChunk
gravInteraction = cncpt''' (mkUid "gravInteraction") (nounPhraseSP "gravitational interaction")
  (S "the mutual attractive force between two masses as described by Newtonian gravity")

-- | T3b: Newton's law of universal gravitation
newtonLUG :: ConceptChunk
newtonLUG = cncpt''' (mkUid "newtonLUG") (nounPhraseSP "Newton's law of universal gravitation")
  (S "the law stating that every mass attracts every other mass with a force proportional to the product of their masses and inversely proportional to the square of the distance between them")

-- | T4: initial conditions
initialConditions :: ConceptChunk
initialConditions = cncpt''' (mkUid "initialConditions") (nounPhraseSP "initial conditions")
  (S "the positions and velocities of the stars at the start of the simulation")

-- | T5: trajectory
trajectory :: ConceptChunk
trajectory = cncpt''' (mkUid "trajectory") (cn' "trajectory")
  (S "the path traced by a star in space as a function of time")

-- | T6: center of mass
centerOfMass :: ConceptChunk
centerOfMass = cncpt''' (mkUid "centerOfMass") (nounPhraseSP "center of mass")
  (S "the point representing the average position of the mass distribution of the system")

-- | T7: inertial reference frame
inertialRefFrame :: ConceptChunk
inertialRefFrame = cncpt''' (mkUid "inertialRefFrame") (nounPhraseSP "inertial reference frame")
  (S "a reference frame in which Newton's laws of motion are valid without the introduction of fictitious forces")

-- | T8: simulation time span
simTimeSpan :: ConceptChunk
simTimeSpan = cncpt''' (mkUid "simTimeSpan") (nounPhraseSP "simulation time span")
  (S "the duration over which the evolution of the system is computed")

---------------------------------------------------------
-- Terms reused from drasil-data (already defined)
-- gravity   → from Data.Drasil.Concepts.Physics
-- cartesian → from Data.Drasil.Concepts.Math
-- These can be added to the terms list in Body.hs if needed
---------------------------------------------------------
