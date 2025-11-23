module Drasil.Projectile.Lesson.Body where

import Data.List (nub)
import Language.Drasil hiding (Notebook)
import Drasil.Database (ChunkDB)
import Drasil.Generator (cdb)
import Drasil.System (System, mkSystem, SystemKind(Notebook))

-- TODO: Add export parameters in a module
import Drasil.DocLang (LsnDecl, LsnChapter(BibSec, LearnObj, Review, CaseProb, Example),
  LearnObj(..), Review(..), CaseProb(..), Example(..))

import qualified Data.Drasil.Quantities.Physics as Qs (iSpeed, ixSpeed, iySpeed,
  speed, constAccel, gravitationalAccel, xAccel, yAccel, time, ixPos, iyPos,
  xPos, yPos, ixVel, iyVel, xVel, yVel, scalarPos, iPos, height)
import qualified Data.Drasil.Concepts.Physics as CCs (motion, acceleration,
  velocity, force, verticalMotion, gravity, position)

import Data.Drasil.People (spencerSmith)

import Drasil.Projectile.Concepts (concepts)
import Drasil.Projectile.Expressions (eqnRefs)

import Drasil.Projectile.Lesson.LearnObj (learnObjContext)
import Drasil.Projectile.Lesson.Review (reviewContent)
import Drasil.Projectile.Lesson.CaseProb (caseProbCont, figRefs)
import Drasil.Projectile.Lesson.Example (exampleContent, horiz_velo)

nbDecl :: LsnDecl
nbDecl = [
    LearnObj $ LrnObjProg [learnObjContext],
    Review $ ReviewProg reviewContent,
    CaseProb $ CaseProbProg caseProbCont,
    Example $ ExampleProg exampleContent,
    BibSec
  ]

si :: System
si = mkSystem
  projectileMotionLesson Notebook [spencerSmith]
  [] [] [] []
  ([] :: [DefinedQuantityDict])
  [] [] [] [] []
  ([] :: [DefinedQuantityDict]) ([] :: [DefinedQuantityDict]) ([] :: [ConstrConcept]) []
  symbMap
  allRefs

symbMap :: ChunkDB
symbMap = cdb symbols ideaDicts conceptChunks ([] :: [UnitDefn]) [] [] [] [] [] [] []

ideaDicts :: [IdeaDict]
ideaDicts = nw projectileMotionLesson : concepts

conceptChunks :: [ConceptChunk]
conceptChunks = [CCs.motion, CCs.acceleration, CCs.velocity, CCs.force,
  CCs.verticalMotion, CCs.gravity, CCs.position]

symbols :: [DefinedQuantityDict]
symbols = map dqdWr [Qs.iSpeed, Qs.ixSpeed, Qs.iySpeed, Qs.speed, Qs.constAccel,
  Qs.gravitationalAccel, Qs.xAccel, Qs.yAccel, Qs.time, Qs.ixPos, Qs.iyPos,
  Qs.xPos, Qs.yPos, Qs.ixVel, Qs.iyVel, Qs.xVel, Qs.yVel, Qs.scalarPos,
  Qs.iPos, Qs.height, horiz_velo]

projectileMotionLesson :: CI
projectileMotionLesson = commonIdea "projMotLsn" (pn "Projectile Motion Lesson") "Projectile Motion" []

allRefs :: [Reference]
allRefs = nub (figRefs ++ eqnRefs)
