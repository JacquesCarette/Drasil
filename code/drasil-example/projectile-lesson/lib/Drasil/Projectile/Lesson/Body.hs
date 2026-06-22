module Drasil.Projectile.Lesson.Body (si, nbDecl) where

import Data.List (nub)
import Language.Drasil
import Language.Drasil.Document
import Drasil.Database (ChunkDB, mkUid)
import Drasil.Generator (withCommonKnowledge)
import Drasil.LessonPlan (LessonPlan, mkLessonPlan, LsnDesc, LsnChapter(..))
import Drasil.System (mkSystemMeta)

import qualified Data.Drasil.Quantities.Physics as Qs (iSpeed, ixSpeed, iySpeed,
  speed, constAccel, gravitationalAccel, xAccel, yAccel, time, ixPos, iyPos,
  xPos, yPos, ixVel, iyVel, xVel, yVel, scalarPos, iPos, height)
import qualified Data.Drasil.Concepts.Physics as CCs (motion, acceleration,
  velocity, force, verticalMotion, gravity, position)

import Data.Drasil.People (spencerSmith)

import Drasil.Projectile.Concepts (ideaDicts, defs)
import Drasil.Projectile.Expressions (eqnRefs)

import Drasil.Projectile.Lesson.LearnObj (learnObjContext)
import Drasil.Projectile.Lesson.Review (reviewSecs)
import Drasil.Projectile.Lesson.CaseProb (caseProbCont, caseProbSecs, figRefs)
import Drasil.Projectile.Lesson.Example (exampleContent, horiz_velo)

nbDecl :: LsnDesc
nbDecl = [
    LearnObj [learnObjContext],
    Review [] reviewSecs,
    CaseProb caseProbCont caseProbSecs,
    Example exampleContent,
    BibSec
  ]

si :: LessonPlan
si = mkLessonPlan
  (mkSystemMeta projectileMotionLesson [spencerSmith] [] [] [] [] symbMap)
  allRefs

symbMap :: ChunkDB
symbMap = withCommonKnowledge [] symbols ideaDicts cis conceptChunks [] [] [] [] [] [] [] []

cis :: [CI]
cis = [projectileMotionLesson]

conceptChunks :: [ConceptChunk]
conceptChunks = defs ++ [CCs.motion, CCs.acceleration, CCs.velocity, CCs.force,
  CCs.verticalMotion, CCs.gravity, CCs.position]

symbols :: [DefinedQuantityDict]
symbols = map dqdWr [Qs.iSpeed, Qs.ixSpeed, Qs.iySpeed, Qs.speed, Qs.constAccel,
  Qs.gravitationalAccel, Qs.xAccel, Qs.yAccel, Qs.time, Qs.ixPos, Qs.iyPos,
  Qs.xPos, Qs.yPos, Qs.ixVel, Qs.iyVel, Qs.xVel, Qs.yVel, Qs.scalarPos,
  Qs.iPos, Qs.height, horiz_velo]

projectileMotionLesson :: CI
projectileMotionLesson = commonIdea (mkUid "projMotLsn") (pn "Projectile Motion Lesson") "Projectile Motion" []

allRefs :: [Reference]
allRefs = nub (figRefs ++ eqnRefs)
