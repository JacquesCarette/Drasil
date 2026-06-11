-- Changes to this template should be reflected in the 'Creating Your Project
-- in Drasil' tutorial found on the wiki:
-- https://github.com/JacquesCarette/Drasil/wiki/Creating-Your-Project-in-Drasil
-- This comment can be removed after copying this template to build your own example.

module Drasil.Template.Body (mkSRS, si) where

import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE

import Drasil.Database (ChunkDB)
import Drasil.System (SmithEtAlSRS, mkSmithEtAlICO)
import Language.Drasil
import Language.Drasil.Document
import Language.Drasil.Display (Symbol(Atop, Integ), Decoration(..))
import Language.Drasil.ShortHands (lT)
import Drasil.SRS
import Drasil.Generator (withCommonKnowledge)
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel, ddENoRefs)
import Data.Drasil.Concepts.Documentation (output_, funcReqDom)
import Data.Drasil.SI_Units (second)

import qualified Drasil.SRS.Concepts as SRS
import Data.Drasil.Citations
import Data.Drasil.Concepts.Theory (inModel)

mkSRS :: SRSDecl
mkSRS = [TableOfContents,
  RefSec $      --This creates the Reference section of the SRS
  RefProg intro      -- This add the introduction blob to the reference section
    [ TUnits      -- Adds table of unit section with a table frame
    , tsymb [TSPurpose, SymbOrder, VectorUnits] -- Adds table of symbol section with a table frame
    --introductory blob (TSPurpose), TypogConvention, bolds vector parameters (Vector Bold), orders the symbol, and adds units to symbols
    ],
  IntroSec $
  IntroProg EmptyS (phrase progName)
    [ IPurpose $ purpDoc progName Verbose,
      IScope EmptyS,
      IChar [] [] [],
      IOrgSec inModel (SRS.inModel [] []) Nothing
    ],
  GSDSec $
    GSDProg
      [ SysCntxt [],
        UsrChars [],
        SystCons [] []
        ],
  SSDSec $
    SSDProg
      [ SSDProblem $ PDProg EmptyS []                --  This adds a is used to define the problem your system will solve
      [ TermsAndDefs Nothing ([] :: [ConceptChunk])   -- This is used to define the terms to be defined in terminology sub section
      , PhySysDesc progName [] figTemp [] -- This defines the Physicalsystem sub-section, define the parts
                                                          -- of the system using physSysParts, figMotion is a function in figures for the image
      , Goals []
      ] -- This adds a goals section and goals input is defined for the preample of the goal.
      , SSDSolChSpec $ SCSProg
        [ Assumptions
        , TMs [] []
        , GDs [] [] HideDerivation
        , DDs [] [Label, Symbol, Units, DefiningEquation, Description Verbose IncludeUnits] HideDerivation
        , IMs [] [] HideDerivation
        , Constraints EmptyS ([] :: [UncertQ])
        , CorrSolnPpties ([] :: [UncertQ]) []

        ]
      ],
  ReqrmntSec $ ReqsProg
    [
       FReqsSub [inputValuesTable]
     , NonFReqsSub
    ],
  LCsSec,
  UCsSec,
  TraceabilitySec $ TraceabilityProg $ traceMatStandard si,
  AuxConstntSec $
     AuxConsProg progName [],
  Bibliography]

inputs :: NE.NonEmpty DefinedQuantityDict
inputs = NE.map dqdWr $ t0 :| [dt]

outputs :: NE.NonEmpty DefinedQuantityDict
outputs = NE.singleton (dqdWr t1)

t0 :: UnitalChunk
t0 = uc (dcc "t0" (cn' "start time") "the start time") (sub lT (Integ 0)) Real second

t1 :: UnitalChunk
t1 = uc (dcc "t1" (cn' "end time") "the end time") (sub lT (Integ 1)) Real second

dt :: UnitalChunk
dt = uc (dcc "dt" (cn' "time delta") "the time delta") (Atop Delta lT) Real second

inputValues :: ConceptInstance
inputValuesTable :: LabelledContent
(inputValues, inputValuesTable) = inReqWTab Nothing inputs

outputValues :: ConceptInstance
outputValues = cic "outputValues" (atStart output_ +:+. ch t1) "Output-Values" funcReqDom

dataDefs :: [DataDefinition]
dataDefs = [t1DD]

t1DD :: DataDefinition
t1DD = ddENoRefs t1QD Nothing "t1" []

t1QD :: SimpleQDef
t1QD = mkQuantDef t1 $ sy t0 $+ sy dt

si :: SmithEtAlSRS
si = mkSmithEtAlICO
  progName [authorName]
  [] [] [] []
  ([] :: [TheoryModel]) ([] :: [GenDefn]) dataDefs ([] :: [InstanceModel])
  inputs outputs
  ([] :: [ConstrConcept]) ([] :: [ConstQDef]) symbols
  [] symbMap []

symbols :: [DefinedQuantityDict]
symbols = NE.toList $ inputs <> outputs

ideaDicts :: [IdeaDict]
ideaDicts =
  -- CIs
  [nw progName]

conceptChunks :: [ConceptChunk]
conceptChunks = []

concIns :: [ConceptInstance]
concIns = [inputValues, outputValues]

symbMap :: ChunkDB
symbMap = withCommonKnowledge []
  symbols ideaDicts conceptChunks
  ([] :: [UnitDefn]) dataDefs ([] :: [InstanceModel])
  ([] :: [GenDefn]) ([] :: [TheoryModel]) concIns
  citations [inputValuesTable]

citations :: BibRef
citations = [parnasClements1986]

resourcePath :: String
resourcePath = "../../../../datafiles/dblpend/" -- FIXME: Change to your resource path!

figTemp :: LabelledContent
figTemp = llccFig "dblpend" $ figWithWidth EmptyS
  (resourcePath ++ "dblpend.png") 60

-- MOVE TO CONCEPTS
progName :: CI -- FIXME: Replace "template" with the name of your project!
progName = commonIdeaWithDict "templateName" (pn "Template") "Template" []

-- MOVE TO DATA.PEOPLE
authorName :: Person
authorName = person "Author" "Name"
