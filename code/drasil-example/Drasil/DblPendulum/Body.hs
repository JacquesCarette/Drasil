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
import Data.Drasil.Concepts.Software (errMsg, program)

import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Drasil.DocLang (AuxConstntSec(AuxConsProg),
  DocSection(AuxConstntSec, Bibliography, RefSec, IntroSec),
  Emphasis(Bold), RefSec(..), RefTab(..),
  TConvention(..), TSIntro(..), intro,tsymb, SRSDecl, mkDoc, IntroSec(IntroProg), IntroSub(IScope))


srs :: Document
srs = mkDoc mkSRS (for'' titleize phrase) si

printSetting :: PrintingInformation
printSetting = PI symbMap Equational defaultConfiguration

mkSRS :: SRSDecl
mkSRS = [RefSec $      --This creates the Reference section of the SRS
    RefProg intro      -- This add the introduction blob to the reference section  
      [ TUnits         -- Adds table of unit section with a table frame
      , tsymb [TSPurpose, TypogConvention [Vector Bold], SymbOrder, VectorUnits] -- Adds table of symbol section with a table frame
      --introductory blob (TSPurpose), TypogConvention, bolds vector parameters (Vector Bold), orders the symbol, and adds units to symbols 
      , TAandA         -- Add table of abbreviation and acronym section
      ],
  IntroSec $
    IntroProg justification (phrase dblpendulum)
      [ IScope scope ],
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
     AuxConsProg dblpendulum [],  --Adds Auxilliary constraint section
  Bibliography                    -- Adds reference section
  ]

justification :: Sentence
justification = foldlSent [S "dblpendulum is the subject" +:+. S "dblpendulum is the focus",
  phrase dblpendulum]

scope :: Sentence
scope = foldlSent [S "dblpendulum is the subject" +:+. S "dblpendulum is the focus", phrase dblpendulum]

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
symbMap = cdb ([] :: [QuantityDict]) (nw dblpendulum : [nw errMsg, nw program]) ([] :: [ConceptChunk])
  ([] :: [UnitDefn]) ([] :: [DataDefinition]) ([] :: [InstanceModel])
  ([] :: [GenDefn]) ([] :: [TheoryModel]) ([] :: [ConceptInstance])
  ([] :: [Section]) ([] :: [LabelledContent])

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) ([] :: [IdeaDict]) ([] :: [ConceptChunk])
  ([] :: [UnitDefn]) ([] :: [DataDefinition]) ([] :: [InstanceModel])
  ([] :: [GenDefn]) ([] :: [TheoryModel]) ([] :: [ConceptInstance])
  ([] :: [Section]) ([] :: [LabelledContent])
{-symbMap :: ChunkDB
symbMap = cdb (map qw SSP.iMods ++ map qw symbols) (map nw symbols
  ++ map nw acronyms ++ map nw doccon ++ map nw prodtcon ++ map nw generalDefinitions ++ map nw SSP.iMods
  ++ map nw defs ++ map nw defs' ++ map nw softwarecon ++ map nw physicCon 
  ++ map nw physicsTMs
  ++ map nw mathcon ++ map nw mathcon' ++ map nw solidcon ++ map nw physicalcon
  ++ map nw doccon' ++ map nw derived ++ map nw fundamentals ++ map nw educon
  ++ map nw compcon ++ [nw algorithm, nw ssp] ++ map nw units)
  (map cw SSP.iMods ++ map cw symbols ++ srsDomains) units SSP.dataDefs SSP.iMods
  generalDefinitions tMods concIns section labCon-}
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
 

