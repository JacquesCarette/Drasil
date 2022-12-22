module Drasil.WebsiteLayout.Body where

import Language.Drasil
import Drasil.SRSDocument
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)
import qualified Language.Drasil.Sentence.Combinators as S

import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Data.Drasil.People (pMichalski)
import Drasil.WebsiteLayout.Concepts (websitelayout, progName, theWebsite, drasil)
import Data.Drasil.Concepts.Software (program)
import Data.Drasil.Concepts.Documentation as Doc (doccon, doccon', srsDomains, software, 
  endUser, functional, documentation, information, organization)
import Data.Drasil.Concepts.Computation (algorithm)
import Drasil.WebsiteLayout.References (citations)
import Drasil.WebsiteLayout.Goals (goals)
import Drasil.WebsiteLayout.Requirements (funcReqs, nonfuncReqs, inReqDesc)

srs :: Document
srs = mkDoc mkSRS (S.forGen titleize phrase) si

fullSI :: SystemInformation
fullSI = fillcdbSRS mkSRS si

printSetting :: PrintingInformation
printSetting = piSys fullSI Equational defaultConfiguration

mkSRS :: SRSDecl
mkSRS = [TableOfContents,
  IntroSec $
    IntroProg (startIntro)
    (S "the" +:+ short progName)
    [IPurpose $ purpDoc progName Verbose,
        IScope scope,
        IChar [short drasil] [phrase documentation] infoOrg],
    StkhldrSec $
      StkhldrProg
        [Cstmr drasil],
    GSDSec $
      GSDProg
      [SysCntxt [sysCtxDesc],
      UsrChars [userCharacteristicsIntro],
      SystCons [] []],
    ReqrmntSec $
      ReqsProg 
        [FReqsSub inReqDesc [],
        NonFReqsSub]      
    ]

si :: SystemInformation
si = SI {
  _sys         = progName,
  _kind        = Doc.srs,
  _authors     = [pMichalski],
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
symbMap = cdb ([] :: [QuantityDict]) (nw progName : nw websitelayout : 
  nw theWebsite : nw drasil : [nw program] 
  ++ map nw doccon ++ map nw doccon' ++ [nw algorithm]) (srsDomains)
  ([] :: [UnitDefn]) ([] :: [DataDefinition]) ([] :: [InstanceModel])
  ([] :: [GenDefn]) ([] :: [TheoryModel]) concIns
  ([] :: [Section]) ([] :: [LabelledContent]) ([] :: [Reference])

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) ([] :: [IdeaDict]) ([] :: [ConceptChunk])
  ([] :: [UnitDefn]) ([] :: [DataDefinition]) ([] :: [InstanceModel])
  ([] :: [GenDefn]) ([] :: [TheoryModel]) ([] :: [ConceptInstance])
  ([] :: [Section]) ([] :: [LabelledContent]) ([] :: [Reference])

refDB :: ReferenceDB
refDB = rdb citations concIns


--------------------------------------------------------------------------------

{--INTRODUCTION--}

startIntro :: Sentence
startIntro = S "The Drasil website introduces users to the framework and directs" +:+
  S "them to appropriate resources."

scope :: Sentence
scope = foldlSent_ [S "determining the", phrase websitelayout, S "considering an" +:+
  S "appropriate audience"]

infoOrg :: [Sentence]
infoOrg = [phrase information +:+ phrase Doc.organization]

--------------------------------
-- GSD --
--------------------------------

sysCtxDesc :: Contents
sysCtxDesc = foldlSP [S "The", phrase progName, S "will introduce the",
  phrase endUser, S "to the", phrase software]

userCharacteristicsIntro :: Contents
userCharacteristicsIntro = foldlSP
  [S "The", phrase endUser `S.ofThe` phrase progName,
   S "should have an understanding of", 
   phrase functional +:+ phrase software]



concIns :: [ConceptInstance]
concIns = goals ++ funcReqs ++ nonfuncReqs