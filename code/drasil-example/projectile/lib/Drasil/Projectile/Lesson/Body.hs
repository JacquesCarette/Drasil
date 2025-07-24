module Drasil.Projectile.Lesson.Body where

import Data.List (nub)
import Language.Drasil hiding (Notebook)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil
import System.Drasil
import qualified Language.Drasil.Sentence.Combinators as S

-- TODO: Add export parameters in a module
import Drasil.DocLang (mkNb, LsnDecl, LsnChapter(BibSec, LearnObj, Review, CaseProb, Example), 
  LearnObj(..), Review(..), CaseProb(..), Example(..))

import Data.Drasil.Concepts.Documentation (doccon, doccon')
import Data.Drasil.Concepts.Math (mathcon)
import Data.Drasil.Quantities.Physics (physicscon)
import Data.Drasil.Concepts.Physics (physicCon)

import Data.Drasil.People (spencerSmith)

import Drasil.Projectile.Concepts (concepts)
import Drasil.Projectile.Expressions (eqnRefs)

import Drasil.Projectile.Lesson.LearnObj (learnObjContext)
import Drasil.Projectile.Lesson.Review (reviewContent)
import Drasil.Projectile.Lesson.CaseProb (caseProbCont, figRefs)
import Drasil.Projectile.Lesson.Example (exampleContent, horiz_velo)

nb :: Document
nb = mkNb mkNB (S.forGen titleize phrase) si

printSetting :: PrintingInformation
printSetting = PI symbMap Equational defaultConfiguration

mkNB :: LsnDecl
mkNB = [
  LearnObj $ LrnObjProg [learnObjContext],
  Review $ ReviewProg reviewContent,
  CaseProb $ CaseProbProg caseProbCont,
  Example $ ExampleProg exampleContent,
  BibSec
  ]

si :: System
si = mkSystem
  projectileMotion Notebook [spencerSmith]
  [] [] [] []
  ([] :: [QuantityDict])
  [] [] [] [] []
  ([] :: [QuantityDict]) ([] :: [QuantityDict]) ([] :: [ConstrConcept]) []
  symbMap

symbMap :: ChunkDB
symbMap = cdb (map qw physicscon ++ symbols) (nw projectileMotion : map nw doccon ++ 
  map nw doccon' ++ map nw physicCon ++ concepts ++ map nw mathcon) 
  ([] :: [ConceptChunk]) ([] :: [UnitDefn]) [] [] [] [] [] [] allRefs []

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) (map nw symbols :: [IdeaDict]) ([] :: [ConceptChunk])
  ([] :: [UnitDefn]) [] [] [] [] ([] :: [ConceptInstance])
  ([] :: [LabelledContent]) ([] :: [Reference]) []

symbols :: [QuantityDict]
symbols = [qw horiz_velo]

projectileMotion :: CI
projectileMotion = commonIdea "projectileMotion" (pn "Projectile Motion Lesson") "Projectile Motion" []


allRefs :: [Reference]
allRefs = nub (figRefs ++ eqnRefs) 
