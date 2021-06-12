module Drasil.Projectile.Lesson.Body where

import Language.Drasil hiding (Symbol(..), Vector)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block, ChunkDB, ReferenceDB, SystemInformation(SI),
  cdb, rdb, refdb, _authors, _purpose, _concepts, _constants, _constraints, 
  _datadefs, _configFiles, _definitions, _defSequence, _inputs, _kind, 
  _outputs, _quants, _sys, _sysinfodb, _usedinfodb)
import Utils.Drasil
import qualified Utils.Drasil.Sentence as S

-- **** Add export parameters in a module
import Drasil.DocumentLanguage.Notebook.DocumentLanguage (mkDoc)
import Drasil.DocumentLanguage.Notebook.NBDecl (NBDecl, NbSection(Bibliography, IntroSec, BodySec, SmmrySec))
import Drasil.DocumentLanguage.Notebook.Core(IntroSec(..), BodySec(..), BodySub(..), SmmrySec(..))

import Data.Drasil.Concepts.Documentation (doccon, doccon')
import qualified Data.Drasil.Concepts.Documentation as Doc (notebook)

import Data.Drasil.People (spencerSmith)

import Drasil.Projectile.Concepts (concepts)

import Drasil.Projectile.Lesson.IntroSection (introPara)
import Drasil.Projectile.Lesson.Review (reviewContextP1, reviewEq, reviewContextP2)
import Drasil.Projectile.Lesson.Motion (motionContextP1, motionContextP2, horMotion, verMotion, summary)

nb :: Document
nb = mkDoc mkNB (S.sFor'' titleize phrase) si

printSetting :: PrintingInformation
printSetting = PI symbMap Equational defaultConfiguration

mkNB :: NBDecl
mkNB = [
  IntroSec $
    IntroProg introPara (phrase projectileMotion) [],
  BodySec $
       BodyProg
         [Review [reviewContextP1, reviewEq, reviewContextP2],
          MainIdea [motionContextP1, motionContextP2] [horMotion, verMotion, summary],
          MethsAndAnls [mAndaintro] []],
  Bibliography
  ]

si :: SystemInformation
si = SI {
  _sys         = projectileMotion,
  _kind        = Doc.notebook,
  _authors     = [spencerSmith],
  _purpose     = [],
  _quants      = [] :: [QuantityDict],
  _concepts    = [] :: [DefinedQuantityDict],
  _definitions = [] :: [QDefinition],
  _datadefs    = [],
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
symbMap = cdb ([] :: [QuantityDict]) (nw projectileMotion : [] 
  ++ map nw doccon ++ map nw doccon' ++ concepts) 
  ([] :: [ConceptChunk]) ([] :: [UnitDefn]) [] [] [] [] [] [] [] 

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) ([] :: [IdeaDict]) ([] :: [ConceptChunk])
  ([] :: [UnitDefn]) [] [] [] [] ([] :: [ConceptInstance])
  ([] :: [Section]) ([] :: [LabelledContent])

refDB :: ReferenceDB
refDB = rdb [] []

projectileMotion :: CI
projectileMotion = commonIdea "projectileMotion" (pn "Projectile Motion") "Projectile Motion" []

mAndaintro :: Contents
mAndaintro = foldlSP 
  [S "Free-flight projectile motion problems can be solved using the following procedure."]