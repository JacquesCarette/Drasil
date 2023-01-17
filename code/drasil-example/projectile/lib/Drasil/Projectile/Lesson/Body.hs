module Drasil.Projectile.Lesson.Body where

import Data.List (nub)
import Language.Drasil
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil
import SysInfo.Drasil
import qualified Language.Drasil.Sentence.Combinators as S

-- TODO: Add export parameters in a module
import Drasil.DocLang (mkNb, NBDecl, NbSection(BibSec, LearnObj, BodySec), 
  LearnObj(..), BodySec(..), BodySub(..))

import Data.Drasil.Concepts.Documentation (doccon, doccon')
import Data.Drasil.Concepts.Math (mathcon)
import qualified Data.Drasil.Concepts.Documentation as Doc (notebook)
import Data.Drasil.Quantities.Physics (physicscon)
import Data.Drasil.Concepts.Physics (physicCon)

import Data.Drasil.People (spencerSmith)

import Drasil.Projectile.Concepts (concepts)
import Drasil.Projectile.Expressions (eqnRefs)

import Drasil.Projectile.Lesson.LearnObj (learnObjContext)
import Drasil.Projectile.Lesson.Review (reviewContent)
import Drasil.Projectile.Lesson.Motion (motionContextP1, figCSandA, figRefs,
  motionContextP2, horMotion, verMotion, summary)
import Drasil.Projectile.Lesson.Analysis (procforAnlsCont, coorSyst, kinematicEq, horMotionAna, verMotionAna)

nb :: Document
nb = mkNb mkNB (S.forGen titleize phrase) si

printSetting :: PrintingInformation
printSetting = PI symbMap Equational defaultConfiguration

mkNB :: NBDecl
mkNB = [
  LearnObj $ LrnObjProg [learnObjContext],
  BodySec $
       BodyProg
         [Review reviewContent,
          MainIdea [motionContextP1, LlC figCSandA, motionContextP2] [horMotion, verMotion, summary],
          MethsAndAnls procforAnlsCont [coorSyst, kinematicEq, horMotionAna, verMotionAna]],
  BibSec
  ]

si :: SystemInformation
si = SI {
  _sys         = projectileMotion,
  _kind        = Doc.notebook,
  _authors     = [spencerSmith],
  _purpose     = [],
  _quants      = [] :: [QuantityDict],
  _concepts    = [] :: [DefinedQuantityDict],
  _instModels  = [],
  _datadefs    = [],
  _configFiles  = [],
  _inputs      = [] :: [QuantityDict],
  _outputs     = [] :: [QuantityDict],
  _defSequence = [] :: [Block SimpleQDef],
  _constraints = [] :: [ConstrainedChunk],
  _constants   = [] :: [ConstQDef],
  _sysinfodb   = symbMap,
  _usedinfodb  = usedDB,
   refdb       = refDB
}

symbMap :: ChunkDB
symbMap = cdb (map qw physicscon) (nw projectileMotion : map nw doccon ++ 
  map nw doccon' ++ map nw physicCon ++ concepts ++ map nw mathcon) 
  ([] :: [ConceptChunk]) ([] :: [UnitDefn]) [] [] [] [] [] [] [] allRefs

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) ([] :: [IdeaDict]) ([] :: [ConceptChunk])
  ([] :: [UnitDefn]) [] [] [] [] ([] :: [ConceptInstance])
  ([] :: [Section]) ([] :: [LabelledContent]) ([] :: [Reference])

refDB :: ReferenceDB
refDB = rdb [] []

projectileMotion :: CI
projectileMotion = commonIdea "projectileMotion" (pn "Projectile Motion Lesson") "Projectile Motion" []


allRefs :: [Reference]
allRefs = nub (figRefs ++ eqnRefs) 
