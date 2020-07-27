module Drasil.DblPendulum.Body where

import Language.Drasil hiding (Symbol(..), Vector)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block, ChunkDB, ReferenceDB, SystemInformation(SI),
  cdb, rdb, refdb, _authors, _purpose, _concepts, _constants, _constraints, 
  _datadefs, _configFiles, _definitions, _defSequence, _inputs, _kind, _outputs, 
  _quants, _sys, _sysinfodb, _usedinfodb)
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)
import Utils.Drasil
import Data.Drasil.People (olu)

import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Drasil.DocLang (AuxConstntSec(AuxConsProg),
  DocSection(AuxConstntSec, Bibliography, RefSec),
  Emphasis(Bold), RefSec(..), RefTab(..),
  TConvention(..), TSIntro(..), intro,tsymb, SRSDecl, mkDoc)


srs :: Document
srs = mkDoc mkSRS (for'' titleize phrase) si

printSetting :: PrintingInformation
printSetting = PI symbMap Equational defaultConfiguration

mkSRS :: SRSDecl
mkSRS = [RefSec $
    RefProg intro
      [ TUnits
      , tsymb [TSPurpose, TypogConvention [Vector Bold], SymbOrder, VectorUnits]
      , TAandA
      ],
  -- IntroSec $
  --   IntroProg justification (phrase dblpendulum)
  --     [ IScope scope ],
  --    SSDSec $ SSDProg
  --        [ SSDProblem $ PDProg prob []
  --          [ TermsAndDefs Nothing terms
  -- --     --  , PhySysDesc dblpendulum physSystParts figLaunch []
  -- --       , Goals goalsInputs]
  --        SSDSolChSpec $ SCSProg
  --        [ Assumptions
  --          , TMs [] (Label : stdFields)
  --       , GDs [] ([Label, Units] ++ stdFields) ShowDerivation
  --       , DDs [] ([Label, Symbol, Units] ++ stdFields) ShowDerivation
  --     --  , IMs [] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) ShowDerivation
  --     --  , Constraints EmptyS inConstraints
  --     --  , CorrSolnPpties outConstraints []
  --      ]
  --    ],
  --ReqrmntSec $
  --  ReqsProg
  --     [ FReqsSub EmptyS []
  --     , NonFReqsSub
  --     ],
  -- TraceabilitySec $ TraceabilityProg $ traceMatStandard si,
    AuxConstntSec $
     AuxConsProg dblpendulum [],
  Bibliography
  ]

-- justification, scope :: Sentence
-- justification = foldlSent [S "dblpendulum" +:+. S "dblpendulum",
--   phrase dblpendulum]
-- scope = foldlSent_ [S "the", (`sOf` S "a"), phrase dblpendulum]

si :: SystemInformation
si = SI {
  _sys         = dblpendulum,
  _kind        = Doc.srs,
  _authors     = [olu],
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
symbMap = cdb ([] :: [QuantityDict]) [nw dblpendulum] ([] :: [ConceptChunk])
  ([] :: [UnitDefn]) ([] :: [DataDefinition]) ([] :: [InstanceModel])
  ([] :: [GenDefn]) ([] :: [TheoryModel]) ([] :: [ConceptInstance])
  ([] :: [Section]) ([] :: [LabelledContent])

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) ([] :: [IdeaDict]) ([] :: [ConceptChunk])
  ([] :: [UnitDefn]) ([] :: [DataDefinition]) ([] :: [InstanceModel])
  ([] :: [GenDefn]) ([] :: [TheoryModel]) ([] :: [ConceptInstance])
  ([] :: [Section]) ([] :: [LabelledContent])

--stdFields :: Fields
--stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

refDB :: ReferenceDB
refDB = rdb [] []

-- MOVE TO CONCEPTS

dblpendulum :: CI
dblpendulum = commonIdeaWithDict "dblpendulum" (pn "Double Pendulum System") "Double Pendulum" []

-- MOVE TO DATA.PEOPLE
--authorName :: Person
--authorName = person "Author" "Name"

------------------------------------
--Problem Description
------------------------------------

-- prob :: Sentence
-- prob = foldlSent_ [S "Problem Description" `sAnd` S "Problem Description"]

-- ---------------------------------
-- -- Terminology and Definitions --
-- ---------------------------------

-- terms :: [ConceptChunk]
-- terms = [gravity]


-- ---------------------------------
-- -- Physical System Description --
-- ---------------------------------

-- physSystParts :: [Sentence]
-- physSystParts = map foldlSent [
--   [S "The", phrase dblpendulum],
--   [S "The", phrase dblpendulum],
--   [S "The", phrase gravity]]

-- ------------------------------

-- goalsInputs :: [Sentence]
-- goalsInputs = [phrase dblpendulum `ofThe` dblpendulum ]-}
 

