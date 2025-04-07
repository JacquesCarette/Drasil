module Drasil.Projectile.Body (printSetting, si, srs, projectileTitle, fullSI) where

import Language.Drasil
import Drasil.SRSDocument
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.NounPhrase.Combinators as NP
import qualified Language.Drasil.Sentence.Combinators as S
import qualified Drasil.DocLang.SRS as SRS

import Data.Drasil.Concepts.Computation (inValue, algorithm, inDatum, compcon)
import Data.Drasil.Concepts.Documentation (analysis, doccon, doccon', physics,
  problem, srsDomains, assumption, goalStmt, physSyst, sysCont, software, user,
  requirement, refBy, refName, typUnc, example, softwareSys, system, environment, 
  product_, interface, condition, physical, datum, input_, softwareConstraint, 
  output_, endUser)
import qualified Data.Drasil.Concepts.Documentation as Doc (srs, physics, variable)
import Data.Drasil.Concepts.Math (cartesian, mathcon)
import Data.Drasil.Concepts.PhysicalProperties (mass)
import Data.Drasil.Concepts.Physics (gravity, physicCon, physicCon',
  rectilinear, oneD, twoD, motion)
import Data.Drasil.Concepts.Software (errMsg, program)
import Data.Drasil.Software.Products (sciCompS)

import Data.Drasil.Quantities.Math (pi_, piConst)
import Data.Drasil.Quantities.Physics (acceleration, constAccel,
  gravitationalAccelConst, iPos, iSpeed, iVel, ixPos, iyPos, ixVel, iyVel,
  position, scalarPos, time, velocity, xAccel, xConstAccel, xPos,
  xVel, yAccel, yConstAccel, yPos, yVel, physicscon)

import Data.Drasil.People (brooks, samCrawford, spencerSmith)
import Data.Drasil.SI_Units (metre, radian, second)
import Data.Drasil.Theories.Physics (accelerationTM, velocityTM)
import Data.Drasil.TheoryConcepts (dataDefn, genDefn, inModel, thModel)
import Data.Drasil.Concepts.Education(calculus, educon, undergraduate, 
  highSchoolPhysics, highSchoolCalculus)

import Drasil.Projectile.Assumptions (assumptions)
import Drasil.Projectile.Concepts (concepts, launcher, projectile, target)
import Drasil.Projectile.DataDefs (dataDefs)
import Drasil.Projectile.Figures (figLaunch, sysCtxFig1)
import Drasil.Projectile.GenDefs (genDefns)
import Drasil.Projectile.Goals (goals)
import Drasil.Projectile.IMods (iMods)
import Drasil.Projectile.References (citations)
import Drasil.Projectile.Requirements (funcReqs, nonfuncReqs)
import Drasil.Projectile.Unitals

import Theory.Drasil (TheoryModel)

srs :: Document
srs = mkDoc mkSRS (S.forGen titleize phrase) si

fullSI :: SystemInformation
fullSI = fillcdbSRS mkSRS si

printSetting :: PrintingInformation
printSetting = piSys fullSI Equational defaultConfiguration

mkSRS :: SRSDecl
mkSRS = [TableOfContents,
  RefSec $
    RefProg intro
      [ TUnits
      , tsymb [TSPurpose, TypogConvention [Vector Bold], SymbOrder, VectorUnits]
      , TAandA
      ],
  IntroSec $
    IntroProg justification (phrase projectileTitle)
      [ IPurpose $ purpDoc projectileTitle Verbose
      , IScope scope
      , IChar [] charsOfReader []
      , IOrgSec inModel (SRS.inModel [] []) EmptyS],
  GSDSec $ 
      GSDProg 
        [ SysCntxt [sysCtxIntro, LlC sysCtxFig1, sysCtxDesc, sysCtxList]
        , UsrChars [userCharacteristicsIntro]
        , SystCons [] []],  
  SSDSec $
    SSDProg
      [ SSDProblem $ PDProg purp []
        [ TermsAndDefs Nothing terms
        , PhySysDesc projectileTitle physSystParts figLaunch []
        , Goals [(phrase iVel +:+ S "vector") `S.the_ofThe` phrase projectile, 
                  S "geometric layout" `S.the_ofThe` phrase launcher `S.and_` phrase target]]
      , SSDSolChSpec $ SCSProg
        [ Assumptions
        , TMs [] (Label : stdFields)
        , GDs [] ([Label, Units] ++ stdFields) ShowDerivation
        , DDs [] ([Label, Symbol, Units] ++ stdFields) ShowDerivation
        , IMs [] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) ShowDerivation
        , Constraints EmptyS inConstraints
        , CorrSolnPpties outConstraints []
        ]
      ],
  ReqrmntSec $
    ReqsProg
      [ FReqsSub EmptyS []
      , NonFReqsSub
      ],
  TraceabilitySec $ TraceabilityProg $ traceMatStandard si,
  AuxConstntSec $
    AuxConsProg projectileTitle constants,
  Bibliography
  ]

justification, scope :: Sentence
justification = foldlSent [atStart projectile, phrase motion, S "is a common" +:+.
  phraseNP (problem `in_` physics), S "Therefore" `sC` S "it is useful to have a",
  phrase program, S "to solve and model these types of" +:+. plural problem, 
  S "Common", plural example `S.of_` phraseNP (combineNINI projectile motion), 
  S "include" +:+. foldlList Comma List projectileExamples,
  S "The document describes the program called", phrase projectileTitle,
  S ", which is based" `S.onThe` S "original, manually created version of" +:+
  namedRef externalLinkRef (S "Projectile")]
scope = foldlSent_ [phraseNP (NP.the (analysis `ofA` twoD)),
  sParen (getAcc twoD), phraseNP (combineNINI projectile motion), phrase problem, 
  S "with", phrase constAccel]

externalLinkRef :: Reference
externalLinkRef = makeURI "projectileSRSLink" 
  "https://github.com/smiths/caseStudies/tree/master/CaseStudies/projectile" 
  (shortname' $ S "projectileSRSLink")

projectileExamples :: [Sentence]
projectileExamples = [S "ballistics" +:+ plural problem +:+ sParen (S "missiles" `sC` 
  S "bullets" `sC` S "etc."), S "the flight" `S.of_` S "balls" `S.in_` 
  S "various sports" +:+ sParen (S "baseball" `sC` S "golf" `sC` S "football" `sC`
  S "etc.")]

projectileTitle :: CI
projectileTitle = commonIdea "projectileTitle" (pn "Projectile Motion Analysis") "Projectile" []

si :: SystemInformation
si = SI {
  _sys         = projectileTitle,
  _kind        = Doc.srs,
  _authors     = [samCrawford, brooks, spencerSmith],
  _purpose     = [purp],
  _background  = [background],
  _motivation  = [motivation],
  _scope       = [scope],
  _quants      = symbols,
  _instModels  = iMods,
  _datadefs    = dataDefs,
  _configFiles = [],
  _inputs      = inputs,
  _outputs     = outputs,
  _constraints = map cnstrw constrained,
  _constants   = constants,
  _sysinfodb   = symbMap,
  _usedinfodb  = usedDB
}

purp :: Sentence
purp = foldlSent_ [S "predict whether a launched", phrase projectile, S "hits its", phrase target]

motivation :: Sentence
motivation = foldlSent_ [phrase projectile, phrase motion, S "is a common" +:+
  phraseNP (problem `in_` physics)]

background :: Sentence
background = foldlSent_ [S "Common examples of", phrase projectile, phrase motion, S "include",
    S "ballistics", plural problem, S "(missiles and bullets)" `S.andThe` S "flight of the balls",
    S "in various sports (baseball, golf, football, etc.)"]

tMods :: [TheoryModel]
tMods = [accelerationTM, velocityTM]

symbMap :: ChunkDB
symbMap = cdb (qw pi_ : map qw physicscon ++ unitalQuants ++ symbols)
  (nw projectileTitle : nw mass : nw inValue : [nw errMsg, nw program] ++
    map nw doccon ++ map nw doccon' ++ map nw physicCon ++ map nw physicCon' ++
    map nw physicscon ++ map nw mathcon ++ [nw algorithm] ++ concepts ++ 
    [nw sciCompS] ++ unitalIdeas ++ map nw acronyms ++ map nw symbols ++ 
    map nw educon ++ map nw [metre, radian, second] ++ map nw compcon) 
  (cw pi_ : map cw constrained ++ srsDomains) (map unitWrapper [metre, radian, second]) 
  dataDefs iMods genDefns tMods concIns [] allRefs citations

-- | Holds all references and links used in the document.
allRefs :: [Reference]
allRefs = [externalLinkRef]

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) (nw pi_ : map nw acronyms ++ map nw symbols)
  (cw pi_ : srsDomains) ([] :: [UnitDefn]) [] [] [] [] [] [] ([] :: [Reference]) []

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

concIns :: [ConceptInstance]
concIns = assumptions ++ funcReqs ++ goals ++ nonfuncReqs

----------------------------------------
-- Characteristics of Intended Reader --
----------------------------------------

charsOfReader :: [Sentence]
charsOfReader = [phrase undergraduate +:+ S "level 1" +:+ phrase Doc.physics,
                 phrase undergraduate +:+ S "level 1" +:+ phrase calculus]

-----------------
--SystemContext--
-----------------

sysCtxIntro :: Contents
sysCtxIntro = foldlSP
  [refS sysCtxFig1, S "shows the" +:+. phrase sysCont,
   S "A circle represents an entity external" `S.toThe` phrase software
   `sC` phraseNP (the user), S "in this case. A rectangle represents the",
   phrase softwareSys, S "itself" +:+. sParen (short projectileTitle),
   S "Arrows are used to show the data flow between the", phraseNP (system
   `andIts` environment)]

sysCtxDesc :: Contents
sysCtxDesc = foldlSPCol [S "The interaction between the", phraseNP (product_
   `andThe` user), S "is through an application programming" +:+.
   phrase interface, S "responsibilities" `S.the_ofTheC` phraseNP (user 
   `andThe` system), S "are as follows"]

sysCtxUsrResp :: [Sentence]
sysCtxUsrResp = map foldlSent [[S "Provide initial", pluralNP (condition `ofThePS`
  physical), S "state" `S.ofThe` phrase motion `S.andThe` plural inDatum, S "related" `S.toThe`
  phrase projectileTitle `sC` S "ensuring no errors" `S.inThe` plural datum, S "entry"], 
  [S "Ensure that consistent units" `S.are` S "used for", pluralNP (combineNINI input_ Doc.variable)],
  [S "Ensure required", namedRef (SRS.assumpt ([]::[Contents]) ([]::[Section])) 
   (phrase software +:+ plural assumption), S "are appropriate for any particular",
  phrase problem, phrase input_ `S.toThe` phrase software]]

sysCtxSysResp :: [Sentence]
sysCtxSysResp = map foldlSent [[S "Detect data type mismatch" `sC` S "such as a string of characters",
  phrase input_, S "instead of a floating point number"],
  [S "Determine if the", plural input_, S "satisfy the required",
  pluralNP (physical `and_` softwareConstraint)],
  [S "Calculate the required", plural output_]]

sysCtxResp :: [Sentence]
sysCtxResp = map (\x -> x +:+ S "Responsibilities") 
  [titleize user, short projectileTitle]

sysCtxList :: Contents
sysCtxList = UlC $ ulcc $ Enumeration $ bulletNested sysCtxResp $
  map bulletFlat [sysCtxUsrResp, sysCtxSysResp]

-------------------------
--User Characteristics --
-------------------------

userCharacteristicsIntro :: Contents
userCharacteristicsIntro = foldlSP
  [S "The", phrase endUser `S.of_` short projectileTitle,
   S "should have an understanding of", 
   phrase highSchoolPhysics `S.and_` phrase highSchoolCalculus]

-------------------------
-- Problem Description --
-------------------------

-- The text for the problem description is the same as that of the purpose of
-- this example.

---------------------------------
-- Terminology and Definitions --
---------------------------------

terms :: [ConceptChunk]
terms = [launcher, projectile, target, gravity, cartesian, rectilinear]

---------------------------------
-- Physical System Description --
---------------------------------

physSystParts :: [Sentence]
physSystParts = map (!.)
  [atStartNP (the launcher),
  atStartNP (the projectile) +:+ sParen (S "with" +:+ getTandS iVel `S.and_` getTandS launAngle),
  atStartNP (the target)]

----------------------------------------------------
-- Various gathered data that should be automated --
----------------------------------------------------
symbols :: [QuantityDict]
symbols = qw gravitationalAccelConst : unitalQuants ++ map qw constants ++
  map qw [acceleration, constAccel, iPos, iSpeed, iVel, ixPos,
  iyPos, ixVel, iyVel, position, scalarPos, projPos, projSpeed, time, velocity, xAccel,
  xConstAccel, xPos, xVel, yAccel, yConstAccel, yPos, yVel]

constants :: [ConstQDef]
constants = [gravitationalAccelConst, piConst, tol]

inputs :: [QuantityDict]
inputs = map qw [launSpeed, launAngle, targPos]

outputs :: [QuantityDict]
outputs = [message, qw offset, qw flightDur]

unitalQuants :: [QuantityDict]
unitalQuants = message : map qw constrained

unitalIdeas :: [IdeaDict]
unitalIdeas = nw message : map nw constrained

inConstraints :: [UncertQ]
inConstraints = [launAngleUnc, launSpeedUnc, targPosUnc]

outConstraints :: [UncertQ]
outConstraints = [landPosUnc, offsetUnc, flightDurUnc]

constrained :: [ConstrConcept]
constrained = [flightDur, landPos, launAngle, launSpeed, offset, targPos]

acronyms :: [CI]
acronyms = [oneD, twoD, assumption, dataDefn, genDefn, goalStmt, inModel,
  physSyst, requirement, Doc.srs, refBy, refName, thModel, typUnc]

