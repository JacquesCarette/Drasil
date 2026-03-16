module Drasil.BinaryStar.Body (mkSRS, si) where

import Drasil.System (SystemKind(Specification), mkSystem)
import Language.Drasil
import Drasil.SRSDocument
import Drasil.DocLang ()
import Drasil.Generator (withCommonKnowledge)
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)

import qualified Drasil.DocLang.SRS as SRS
import Data.Drasil.Concepts.Theory (inModel)
import Drasil.DocumentLanguage.TraceabilityGraph ()

import Drasil.BinaryStar.MetaConcepts (progName)
import Drasil.BinaryStar.Concepts (concepts, defs)
import Drasil.BinaryStar.LabelledContent (labelledContent, figBSS, sysCtxFig1)
import Drasil.BinaryStar.References (citations)
import Drasil.BinaryStar.Unitals (symbols, acronyms, inputs, outputs,
  inConstraints, outConstraints, constants, mass_1, mass_2)

mkSRS :: SRSDecl
mkSRS = [TableOfContents,
  RefSec $
  RefProg intro
    [ TUnits
    , tsymb [TSPurpose, TypogConvention [Vector Bold], SymbOrder, VectorUnits]
    , TAandA abbreviationsList
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
      [ SysCntxt [LlC sysCtxFig1],
        UsrChars [],
        SystCons [] []
        ],
  SSDSec $
    SSDProg
      [ SSDProblem $ PDProg EmptyS []
      [ TermsAndDefs Nothing defs
      , PhySysDesc progName physSystParts figBSS []
      , Goals []
      ]
      , SSDSolChSpec $ SCSProg
        [ Assumptions
        , TMs [] []
        , GDs [] [] HideDerivation
        , DDs [] [] HideDerivation
        , IMs [] [] HideDerivation
        , Constraints EmptyS inConstraints
        , CorrSolnPpties outConstraints []
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
  inputs outputs ([] :: [ConstrConcept]) constants
  symbMap
  []

abbreviationsList :: [IdeaDict]
abbreviationsList = map nw symbols ++ nw progName : map nw acronyms

ideaDicts :: [IdeaDict]
ideaDicts = nw progName : concepts

conceptChunks :: [ConceptChunk]
conceptChunks = defs

symbMap :: ChunkDB
symbMap = withCommonKnowledge []
  symbols ideaDicts conceptChunks
  ([] :: [UnitDefn]) ([] :: [DataDefinition]) ([] :: [InstanceModel])
  ([] :: [GenDefn]) ([] :: [TheoryModel]) ([] :: [ConceptInstance])
  citations labelledContent

physSystParts :: [Sentence]
physSystParts = map (!.)
  [S "The first star with mass" +:+ ch mass_1,
   S "The second star with mass" +:+ ch mass_2,
   S "The gravitational interaction between the two stars"]

authorName :: Person
authorName = person "Xinlu" "Yan"
