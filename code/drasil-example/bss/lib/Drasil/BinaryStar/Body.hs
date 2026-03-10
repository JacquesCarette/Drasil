module Drasil.BinaryStar.Body (mkSRS, si) where

import Drasil.System (SystemKind(Specification), mkSystem)
import Language.Drasil
import Drasil.SRSDocument
import Drasil.DocLang (tunitNone)
import Drasil.Generator (withCommonKnowledge)
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)

import qualified Drasil.DocLang.SRS as SRS
import Data.Drasil.Citations
import Data.Drasil.Concepts.Theory (inModel)
import Drasil.DocumentLanguage.TraceabilityGraph ()

import Drasil.BinaryStar.MetaConcepts (progName)

mkSRS :: SRSDecl
mkSRS = [TableOfContents,
  RefSec $
  RefProg intro
    [ tunitNone []
    , tsymb []
    ],
  IntroSec $
  IntroProg introBlurb (phrase progName)
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
      [ SSDProblem $ PDProg EmptyS []
      [ TermsAndDefs Nothing ([] :: [ConceptChunk])
      , PhySysDesc progName [] figBSS []
      , Goals []
      ]
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
       FReqsSub []
     , NonFReqsSub
    ],
  LCsSec,
  UCsSec,
  TraceabilitySec $ TraceabilityProg $ traceMatStandard si,
  AuxConstntSec $
     AuxConsProg progName [],
  Bibliography]

introBlurb :: Sentence
introBlurb = foldlSent
  [S "Binary star systems are common in astronomy.",
   S "Two stars orbit because of gravity.",
   S "This software simulates how a binary star system evolves over time"]

si :: System
si = mkSystem
  progName Specification [authorName]
  [] [] [] []
  ([] :: [TheoryModel]) ([] :: [GenDefn]) ([] :: [DataDefinition]) ([] :: [InstanceModel])
  ([] :: [DefinedQuantityDict]) ([] :: [DefinedQuantityDict]) ([] :: [ConstrConcept]) ([] :: [ConstQDef])
  symbMap
  []

ideaDicts :: [IdeaDict]
ideaDicts =
  [nw progName]

conceptChunks :: [ConceptChunk]
conceptChunks = [] :: [ConceptChunk]

symbMap :: ChunkDB
symbMap = withCommonKnowledge []
  ([] :: [DefinedQuantityDict]) ideaDicts conceptChunks
  ([] :: [UnitDefn]) ([] :: [DataDefinition]) ([] :: [InstanceModel])
  ([] :: [GenDefn]) ([] :: [TheoryModel]) ([] :: [ConceptInstance])
  citations ([] :: [LabelledContent])

citations :: BibRef
citations = [parnasClements1986, koothoor2013, smithEtAl2007, smithLai2005,
             smithKoothoor2016]

figBSS :: LabelledContent
figBSS = llccFig "bssPhysSys" $ figWithWidth EmptyS
  (bssResourcePath ++ "bss.png") 60

bssResourcePath :: String
bssResourcePath = "../../../../datafiles/bss/"

authorName :: Person
authorName = person "Xinlu" "Yan"
