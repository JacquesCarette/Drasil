-- Changes to this template should be reflected in the 'Creating Your Project 
-- in Drasil' tutorial found on the wiki:
-- https://github.com/JacquesCarette/Drasil/wiki/Creating-Your-Project-in-Drasil
-- This comment can be removed after copying this template to build your own example.

module Drasil.Template.Body where

import Language.Drasil
import Drasil.SRSDocument
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)
import qualified Language.Drasil.Sentence.Combinators as S
import Data.Drasil.Concepts.Documentation (doccon, doccon', srsDomains)
import Data.Drasil.Concepts.Computation (inValue, algorithm)
import Data.Drasil.Concepts.Software (errMsg, program)
import Data.Drasil.Concepts.Math (mathcon)

import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import qualified Drasil.DocLang.SRS as SRS
import Data.Drasil.Software.Products
import Data.Drasil.TheoryConcepts
import Data.Drasil.Citations
import Drasil.DocumentLanguage.TraceabilityGraph
import Drasil.DocLang (tunitNone)

srs :: Document
srs = mkDoc mkSRS (S.forGen titleize phrase) si

fullSI :: SystemInformation
fullSI = fillcdbSRS mkSRS si

printSetting :: PrintingInformation
printSetting = piSys fullSI Equational defaultConfiguration

mkSRS :: SRSDecl
mkSRS = [TableOfContents,
  RefSec $      --This creates the Reference section of the SRS
  RefProg intro      -- This add the introduction blob to the reference section  
    [ tunitNone []      -- Adds table of unit section with a table frame
    , tsymb [] -- Adds table of symbol section with a table frame
    --introductory blob (TSPurpose), TypogConvention, bolds vector parameters (Vector Bold), orders the symbol, and adds units to symbols 
    , TAandA         -- Add table of abbreviation and acronym section
    ],
  IntroSec $
  IntroProg EmptyS (phrase progName)
    [ IPurpose $ purpDoc progName Verbose,
      IScope EmptyS,
      IChar [] [] [],
      IOrgSec inModel (SRS.inModel [] []) EmptyS
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
        , DDs [] [] HideDerivation
        , IMs [] [] HideDerivation
        , Constraints EmptyS ([] :: [UncertQ])
        , CorrSolnPpties ([] :: [UncertQ]) []

        ]
      ],
  ReqrmntSec $ ReqsProg
    [
       FReqsSub EmptyS []
     , NonFReqsSub
    ],
  LCsSec,
  UCsSec,
  TraceabilitySec $ TraceabilityProg $ traceMatStandard si,
  AuxConstntSec $
     AuxConsProg progName [],
  Bibliography]

si :: SystemInformation
si = SI {
  _sys         = progName,
  _kind        = Doc.srs,
  _authors     = [authorName],
  _background  = [],
  _purpose     = [],
  _quants      = [] :: [QuantityDict],
  _concepts    = [] :: [DefinedQuantityDict],
  _instModels  = [] :: [InstanceModel],
  _datadefs    = [] :: [DataDefinition],
  _configFiles = [],
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
symbMap = cdb ([] :: [QuantityDict]) (nw progName : nw inValue : [nw errMsg, 
  nw program] ++ map nw doccon ++ map nw doccon' ++ [nw algorithm] ++ 
  map nw prodtcon ++ map nw mathcon) srsDomains
  ([] :: [UnitDefn]) ([] :: [DataDefinition]) ([] :: [InstanceModel])
  ([] :: [GenDefn]) ([] :: [TheoryModel]) ([] :: [ConceptInstance])
  ([] :: [Section]) ([] :: [LabelledContent]) ([] :: [Reference])

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) ([] :: [IdeaDict]) ([] :: [ConceptChunk])
  ([] :: [UnitDefn]) ([] :: [DataDefinition]) ([] :: [InstanceModel])
  ([] :: [GenDefn]) ([] :: [TheoryModel]) ([] :: [ConceptInstance])
  ([] :: [Section]) ([] :: [LabelledContent]) ([] :: [Reference])

refDB :: ReferenceDB
refDB = rdb citations []

citations :: BibRef
citations = [parnasClements1986, koothoor2013, smithEtAl2007, smithLai2005,
             smithKoothoor2016]

inConstraints :: [UncertQ]
inConstraints = []

outConstraints :: [UncertQ]
outConstraints = []

figTemp :: LabelledContent
figTemp = llcc (makeFigRef "dblpend") $ figWithWidth EmptyS
  (resourcePath ++ "dblpend.png") 60


-- MOVE TO CONCEPTS
progName :: CI
progName = commonIdeaWithDict "progName" (pn "ProgName") "ProgName" []

-- MOVE TO DATA.PEOPLE
authorName :: Person
authorName = person "Author" "Name"
