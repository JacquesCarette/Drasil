{-# LANGUAGE PostfixOperators #-}
module Drasil.BinaryStar.Body (mkSRS, si) where

import Drasil.System (SystemKind(Specification), mkSystem)
import Language.Drasil
import qualified Language.Drasil.Development as D
import Language.Drasil.Chunk.Concept.NamedCombinators
import Drasil.SRSDocument
import Drasil.DocLang ()
import Drasil.Generator (withCommonKnowledge)
import Theory.Drasil (GenDefn)

import qualified Drasil.DocLang.SRS as SRS
import qualified Language.Drasil.Sentence.Combinators as S
import Data.Drasil.Concepts.Theory (inModel)
import Data.Drasil.Concepts.Math (ode)
import Data.Drasil.Concepts.Documentation (assumption, endUser, input_,
  interface, output_, physical, software, sysCont, softwareConstraint,
  softwareSys, system, user, environment, product_, datum)
import Data.Drasil.Concepts.Education (undergraduate, calculus)
import Data.Drasil.Concepts.Computation (inDatum)
import Data.Drasil.Concepts.Physics (gravity, twoD)
import Data.Drasil.Quantities.Physics (velocity, position, acceleration,
  energy, force, gravitationalConst, time)
import Data.Drasil.Quantities.PhysicalProperties ()
import Drasil.DocumentLanguage.TraceabilityGraph ()
import Drasil.Document.Contents (unlbldExpr, foldlSP, foldlSPCol)
import Drasil.Sentence.Combinators (bulletNested, bulletFlat)

import Drasil.BinaryStar.MetaConcepts (progName)
import Drasil.BinaryStar.Concepts (concepts, defs)
import Drasil.BinaryStar.LabelledContent (labelledContent, figBSS, sysCtxFig1)
import Drasil.BinaryStar.References (citations)
import Drasil.BinaryStar.Unitals (symbols, acronyms, inputs, outputs,
  inConstraints, outConstraints, constants, mass_1, mass_2,
  xVel_1, yVel_1, xVel_2, yVel_2, sepDist)
import Drasil.BinaryStar.Assumptions (assumptions, isolated, constantMass)
import Drasil.BinaryStar.Goals (goals, goalsInputs)
import Drasil.BinaryStar.Requirements (funcReqs, funcReqsTables, nonFuncReqs)
import Drasil.BinaryStar.Changes (likelyChgs, unlikelyChgs)
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
      IScope scope,
      IChar [] charsOfReader [],
      IOrgSec inModel (SRS.inModel [] []) EmptyS
    ],
  GSDSec $
    GSDProg
      [ SysCntxt [sysCtxIntro, LlC sysCtxFig1, sysCtxDesc, sysCtxList],
        UsrChars [usrCharsIntro],
        SystCons [] []
        ],
  SSDSec $
    SSDProg
      [ SSDProblem $ PDProg probDescIntro []
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
        , CorrSolnPpties outConstraints corrSolnProps
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
     AuxConsProg progName constants,
  Bibliography]

------------------------------
-- Section 2: INTRODUCTION --
------------------------------

introBlurb :: Sentence
introBlurb = foldlSent
  [S "Binary star systems are common in astronomy.",
   S "Two stars orbit because of gravity.",
   S "This software simulates how a binary star system evolves over time"]

---------------------------------
-- 2.2 : Scope of Requirements --
---------------------------------
scope :: Sentence
scope = foldlSent_
  [S "the analysis of the", phrase twoD,
   S "motion of a binary star system under Newtonian",
   phrase gravity `sC`
   S "given the masses, initial positions, initial velocities,",
   S "and simulation time span"]

----------------------------------------------
-- 2.3 : Characteristics of Intended Reader --
----------------------------------------------
charsOfReader :: [Sentence]
charsOfReader =
  [phrase undergraduate +:+ S "level 1 physics (Newtonian mechanics)",
   phrase undergraduate +:+ S "level 1" +:+ phrase calculus,
   plural ode]

--------------------------------------------
-- Section 3: GENERAL SYSTEM DESCRIPTION --
--------------------------------------------

--------------------------
-- 3.1 : System Context --
--------------------------
sysCtxIntro :: Contents
sysCtxIntro = foldlSP
  [refS sysCtxFig1, S "shows the" +:+. phrase sysCont,
   S "A circle represents an entity external" `S.toThe` phrase software
   `sC` D.toSent (phraseNP (the user)), S "in this case. A rectangle represents the",
   phrase softwareSys, S "itself", sParen (short progName) +:+. EmptyS,
   S "Arrows" `S.are` S "used to show the data flow between the",
   D.toSent (phraseNP (system `andIts` environment))]

sysCtxDesc :: Contents
sysCtxDesc = foldlSPCol
  [S "The interaction between the", D.toSent (phraseNP (product_
   `andThe` user)), S "is through an application programming" +:+.
   phrase interface, S "The responsibilities" `S.ofThe`
   D.toSent (phraseNP (user `andThe` system)),
   S "are as follows"]

sysCtxUsrResp :: [Sentence]
sysCtxUsrResp =
  [S "Provide the physical parameters of the binary star system" `sC`
     S "including stellar masses, initial positions, and initial velocities" `sC`
     S "ensuring no errors in the" +:+ plural datum +:+. S "entry",
   S "Ensure that consistent units" `S.are` S "used for" +:+.
     plural input_,
   S "Ensure required" +:+
     namedRef (SRS.assumpt ([]::[Contents]) ([]::[Section]))
       (phrase software +:+ plural assumption) +:+
     S "are appropriate for any particular problem input to the" +:+.
     phrase software]

sysCtxSysResp :: [Sentence]
sysCtxSysResp =
  [S "Detect data type mismatch, such as a string of characters" +:+
     phrase input_ +:+. (S "instead of a floating point number"),
   S "Determine if the" +:+ plural input_ +:+ S "satisfy the required" +:+.
     D.toSent (pluralNP (physical `and_` softwareConstraint)),
   S "Calculate the required" +:+. plural output_]

sysCtxResp :: [Sentence]
sysCtxResp = [titleize user +:+ S "Responsibilities",
  short progName +:+ S "Responsibilities"]

sysCtxList :: Contents
sysCtxList = UlC $ ulcc $ Enumeration $ bulletNested sysCtxResp $
  map bulletFlat [sysCtxUsrResp, sysCtxSysResp]

--------------------------------
-- 3.2 : User Characteristics --
--------------------------------
usrCharsIntro :: Contents
usrCharsIntro = foldlSP
  [S "The", phrase endUser `S.of_` short progName,
   S "should have an understanding of",
   S "undergraduate level 1 physics (Newtonian mechanics)" `sC`
   S "undergraduate level 1" +:+ phrase calculus `S.and_` plural ode]

--------------------------------------------
-- Section 4: SPECIFIC SYSTEM DESCRIPTION --
--------------------------------------------

probDescIntro :: Sentence
probDescIntro = foldlSent_
  [short progName, S "is intended to simulate the motion of a binary",
   S "star system under mutual gravitational interaction"]

physSystParts :: [Sentence]
physSystParts = map (!.)
  [S "The first star with mass" +:+ ch mass_1,
   S "The second star with mass" +:+ ch mass_2,
   S "The gravitational interaction between the two stars"]

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

------------------------------
-- System Information --
------------------------------

si :: System
si = mkSystem
  progName Specification [authorName]
  [] [] [] []
  tMods ([] :: [GenDefn]) dataDefs iMods
  inputs outputs ([] :: [ConstrConcept]) constants
  symbMap
  []

authorName :: Person
authorName = person "Xinlu" "Yan"

abbreviationsList :: [IdeaDict]
abbreviationsList = map nw symbols ++ nw progName : map nw acronyms

ideaDicts :: [IdeaDict]
ideaDicts = nw progName : nw ode : nw twoD : nw gravity : concepts

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
  ++ likelyChgs ++ unlikelyChgs

---------------------------------------------------------
-- Properties of a Correct Solution
-- Total mechanical energy must be conserved.
---------------------------------------------------------
corrSolnProps :: [Contents]
corrSolnProps = [corrSolnDesc, corrSolnEqn]

corrSolnDesc :: Contents
corrSolnDesc = foldlSP
  [S "The total mechanical energy of the system must remain",
   S "constant over time (up to numerical tolerance),",
   S "since the system is isolated", sParen (refS isolated),
   S "and masses are constant", sParen (refS constantMass)]

corrSolnEqn :: Contents
corrSolnEqn = unlbldExpr energyExpr

-- E = const
energyExpr :: ModelExpr
energyExpr = sy energy $=
  half (sy mass_1 $* (square (sy xVel_1) $+ square (sy yVel_1)))
  $+ half (sy mass_2 $* (square (sy xVel_2) $+ square (sy yVel_2)))
  $- (sy gravitationalConst $* sy mass_1 $* sy mass_2 $/ sy sepDist)
