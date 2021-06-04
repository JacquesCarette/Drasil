module Drasil.Projectile.Lesson.Body where

import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)

-- **** Add export parameters in a module
import Drasil.DocumentLanguage.Notebook.DocumentLanguage (mkDoc)
import Drasil.DocDecl (NBDecl, DocSection'(Bibliography, IntroSec, BodySec, SmmrySec))
import Drasil.DocumentLanguage.Notebook.Core(IntroSec(..), BodySec(..), SmmrySec(..))

import qualified Data.Drasil.Concepts.Documentation as Doc (notebook)

import Drasil.Projectile.Concepts (concepts)

import Drasil.Projectile.Lesson.IntroSection (introPara)
import Drasil.Projectile.Lesson.Review (reviewContextP1, reviewEq, reviewContextP2)
import Drasil.Projectile.Lesson.Motion (motionContextP1, motionContextP2, horMotion, verMotion, summary)

nb :: Document
nb = mkDoc mkNB (for'' titleize phrase) si

printSetting :: PrintingInformation
printSetting = PI symbMap Equational defaultConfiguration

mkNB :: NBDecl
mkNB = [
  IntroSec $
    IntroProg introPara (phrase projMotionLesson)
  BodySec $
       BodyProg
         [Review [reviewContextP1, reviewEq, reviewContextP2],
          Motion [motionContextP1, motionContextP2] [horMotion, verMotion, summary],
          MethsAndAnls [mAndaintro] []],
  Bibliography
  ]

si :: SystemInformation
si = SI {
  _sys         = projectileMotionLesson,
  _kind        = Doc.notebook,
  _authors     = [spencerSmith],
  _purpose     = [],
  _quants      = [] :: [QuantityDict],
  _concepts    = [] :: [DefinedQuantityDict],
  _definitions = [] :: [QDefinition],
  _datadefs    = [] :: [DataDefinition],
  _configFiles  = [],
  _inputs      = [] :: [QuantityDict],
  _outputs     = [] :: [QuantityDict],
  _defSequence = [] :: [Block QDefinition],
  _constraints = [] :: [ConstrainedChunk],
  _constants   = [] :: [QDefinition],
  _sysinfodb   = symbMap,
  _usedinfodb  = usedDB,
   refdb       = refDB
}

symbMap :: ChunkDB
symbMap = cdb ([] :: [QuantityDict]) (nw projMotionLesson : [nw example] 
  ++ map nw doccon ++ map nw doccon’ ++ concepts) ([] :: [ConceptChunk])
  ([] :: [UnitDefn]) [] [] [] [] [] [] [] 

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) ([] :: [IdeaDict]) ([] :: [ConceptChunk])
  ([] :: [UnitDefn]) ([] :: [DataDefinition]) ([] :: [InstanceModel])
  ([] :: [GenDefn]) ([] :: [TheoryModel]) ([] :: [ConceptInstance])
  ([] :: [Section]) ([] :: [LabelledContent])

refDB :: ReferenceDB
refDB = rdb [] []

projMotionLesson :: CI
projMotionLesson = commonIdea "projMotionLesson" (pn "Projectile Motion Lesson") "Projectile Motion lesson" []

mAndaintro :: Contents
mAndaintro = foldlSP 
  [S "Free-flight projectile motion problems can be solved using the following procedure."]