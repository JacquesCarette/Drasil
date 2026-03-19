module Drasil.BinaryStar.Body (mkSRS, si) where

import Drasil.System (SystemKind(Specification), mkSystem)
import Language.Drasil
import Drasil.SRSDocument
import Drasil.DocLang ()
import Drasil.Generator (withCommonKnowledge)
import Theory.Drasil (GenDefn)

import qualified Drasil.DocLang.SRS as SRS
import Data.Drasil.Concepts.Theory (inModel)
import Data.Drasil.Concepts.Math (ode)
import Data.Drasil.Quantities.Physics (velocity, position, acceleration,
  force, gravitationalConst, time)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Drasil.DocumentLanguage.TraceabilityGraph ()

import Drasil.BinaryStar.MetaConcepts (progName)
import Drasil.BinaryStar.Concepts (concepts, defs)
import Drasil.BinaryStar.LabelledContent (labelledContent, figBSS, sysCtxFig1)
import Drasil.BinaryStar.References (citations)
import Drasil.BinaryStar.Unitals (symbols, acronyms, inputs, outputs,
  inConstraints, outConstraints, constants, mass_1, mass_2)
import Drasil.BinaryStar.Assumptions (assumptions)
import Drasil.BinaryStar.Goals (goals, goalsInputs)
import Drasil.BinaryStar.Requirements (funcReqs, funcReqsTables, nonFuncReqs)
import Drasil.BinaryStar.DataDefs (dataDefs)
import Drasil.BinaryStar.IMods (iMods)
import Drasil.BinaryStar.TMods (tMods)

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
      , Goals goalsInputs
      ]
      , SSDSolChSpec $ SCSProg
        [ Assumptions
        , TMs [] (Label : stdFields)
        , GDs [] ([Label, Units] ++ stdFields) HideDerivation
        , DDs [] ([Label, Symbol, Units] ++ stdFields) HideDerivation
        , IMs [] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) HideDerivation
        , Constraints EmptyS inConstraints
        , CorrSolnPpties outConstraints []
        ]
      ],
  ReqrmntSec $ ReqsProg
    [
       FReqsSub funcReqsTables
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
  tMods ([] :: [GenDefn]) dataDefs iMods
  inputs outputs ([] :: [ConstrConcept]) constants
  symbMap
  []

abbreviationsList :: [IdeaDict]
abbreviationsList = map nw symbols ++ nw progName : map nw acronyms

ideaDicts :: [IdeaDict]
ideaDicts = nw progName : nw ode : concepts

conceptChunks :: [ConceptChunk]
conceptChunks = defs

symbMap :: ChunkDB
symbMap = withCommonKnowledge []
  symbols ideaDicts conceptChunks
  ([] :: [UnitDefn]) dataDefs iMods
  ([] :: [GenDefn]) tMods concIns
  citations (labelledContent ++ funcReqsTables)

concIns :: [ConceptInstance]
concIns = assumptions ++ goals ++ funcReqs ++ nonFuncReqs

physSystParts :: [Sentence]
physSystParts = map (!.)
  [S "The first star with mass" +:+ ch mass_1,
   S "The second star with mass" +:+ ch mass_2,
   S "The gravitational interaction between the two stars"]

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

authorName :: Person
authorName = person "Xinlu" "Yan"
