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
projectileTitle = commonIdea "projectileTitle" (pn "Projectile") "Projectile" []

si :: SystemInformation
si = SI {
  -- SystemInformation (SI) is a huge record that intimately couples many "core"
  -- things to Projectile (I will number these things). SI was created for
  -- operational purposes, but it's come to be something that contains many
  -- intimately related things, which do make sense to be related somehow (not
  -- necessarily by a record, but that's a different story). The question is
  -- _how they're related_, and from that, we are likely to find a better design
  -- for SI. I will number discussion points, not components of SI.

  -- (1) I'm starting this with one large assumption (I'm noting this
  -- retroactively): SI is our version of the the "root model" (our version of
  -- MDE's Single Model Principle --
  -- https://www.researchgate.net/publication/2843964_The_Single_Model_Principle).
  -- The assumption here is that the SI is meant to be the root thing that we
  -- operate on from the POV of the softifact generator.

  -- (2) The first thing the SI contains is a "notice" -- that we are interested
  -- in modelling a software computation problem, using standard/conventional
  -- terminology. I say notice because we only carry `Doc.srs` (a CI/"common
  -- idea," defining the acronym SRS and its long form). Considering the lesson
  -- plan's SI only changes this to "notebook" and uses very few of the other SI
  -- fields, I think its fair to say that this "notice" should be a data type
  -- that includes a lot more of the fields from the SI, but I'm not sure what
  -- yet! I believe that `Doc.srs` is also something more specific than just
  -- "software requirements specification." I think it really means the
  -- SmithEtAl SRS template in particular, because that brings about the
  -- terminology we use in organizing scientific knowledge (e.g., Theory Model,
  -- General Definition, etc. -- yes, some of this will change in the future,
  -- but that's too into the weeds now). It also brings about/hooks in some of
  -- our nonfunctional requirements: traceable, correct, and verifiable. Not
  -- necessarily: reusable, understandable, maintainable, and portable.
  _kind        = Doc.srs,

  -- (3) Implicitly, (2) is saying that some sort of software problem will be
  -- presented in the document. We don't necessarily capture this information
  -- that we're presenting a software problem, so this is implicit.
  
  -- (4) The implicit software problem (2) is an "input-calculate-output
  -- computational software problem." I'm not going to call this "Projectile,"
  -- but something more precise (which we can retract later): a
  -- "solution/calculation schematic," which is not necessarily about projectile
  -- motion problem.

  -- (5) Now, the solution can come from anywhere and/or nowhere, but we make a
  -- choice: that if it comes from anywhere, it is grounded in science and
  -- theory. This is arguably brought about as a restriction by the SRS
  -- "notice."

  -- (6) Before explaining what the "solution/calculation schematic" is, we need
  -- to define the (calculation) problem. The problem has two components to it:
  -- metainformation (containing background information about the problem,
  -- sources for the problem, authorship, etc.) and a mathematical description
  -- of the problem.

  -- (7) The metainformation for the calculation problem begins with: a title,
  -- authors, and rationale for why bothering with encoding this at all. Yes,
  -- the title is metainformation.
  _sys         = projectileTitle, -- the title (Projectile),
  _authors     = [samCrawford, brooks, spencerSmith], -- the list of authors,
  -- and a bit of explanation of why this is encoded at all (background and
  -- motivation).
  _motivation  = [motivation],
  -- ^ "Projectile motion is a common problem in physics."
  _background  = [background],
  -- ^ "Common examples of projectile motion include ballistics problems
  -- (missiles and bullets) and the flight of the balls in various sports
  -- (baseball, golf, football, etc.)."

  -- (8) I believe this metainformation brings about the "Characteristics of the
  -- Intended Reader" into scope as well, bringing about what DSLs, terms,
  -- theories, etc. are relevant to us and into scope. We don't explicitly have
  -- this written down anywhere, but it brings about the swath of theory needed
  -- to possibly discuss the mathematical description of the problem (e.g., what
  -- is a position, what is 2D collision, what are our abstract objects of
  -- interest [launcher/cannon, target, and projectile], kinematics equations,
  -- etc.). Specifically, I believe that this brings about the "Theory Models"
  -- in scope of the project.

  -- (9) Now, the mathematical description explains a specific calculation
  -- problem (knowns/unknowns) situated within the theory. Funny enough, this
  -- mathematical description does not necessarily imply that a solution exists.
  -- It only creates one arbitrarily! Hence, it contains 2 things (which appears
  -- as 3 in the SI):
  _purpose     = [purp], -- an English description of the knowns/unknowns problem, and
  -- ^ "Predict whether a launched projectile hits its target."
  _inputs      = inputs, -- (inputs, outputs), a designation of an unknown function: f(inputs) = outputs
  _outputs     = outputs,

  -- (10) Finally, we can return to the specific "solution/calculation
  -- schematic." Here, we carve out a scope (assumptions) for which we can
  -- define `f` (from (9)) using sound theory. This may mean a few things:
  -- 
  -- - That we're only looking to carve out a solution for a subset of the
  --   inputs.
  -- - That we need more inputs to possibly solve this. We don't do that in
  --   Projectile, but we do this in GlassBR, where we wire in custom
  --   handwritten code.
  -- - That we will add extra outputs (e.g., because we have intermediate
  --   variables of interest to users of the software).
  -- 
  -- Unfortunately, `_inputs` and `_outputs` are not divided into two lists each
  -- (one containing purely the abstract problem inputs/outputs, and the other
  -- containing the specific inputs/outputs the software solution schematic will
  -- actually work with). However, if they were, they would be added to the
  -- below items:
  _scope       = [scope], -- An English summary of the other ("more mathematical") items.
  -- ^ "The analysis of a 2D projectile motion problem with constant acceleration."
  _constants   = constants, -- Constant variable assumptions.
  _constraints = map cnstrw constrained, -- Variable input constraints.
  _quants      = symbols, -- _All_ relevant variables (including immediate variables).
  _datadefs    = dataDefs, -- "Let" bindings of variables (?).
  
  -- (11) The "Instance Models" collectively carry the calculation scheme, being
  -- derived from theory models and general definitions. We extract `f` from the
  -- instance models. Note: the calculation scheme may have "holes" that need to
  -- be filled in by the code generator. For example, ODEs need choices to be
  -- made about which method/library to use.
  _instModels  = iMods,

  -- (12) TODO: I believe we are commiting to "Naturalism"
  -- (https://plato.stanford.edu/entries/philosophy-mathematics/#NatInd) with
  -- our theories. What does this mean? That we don't care for pure formalism
  -- and mathematics, but accept our best scientific theories as truth despite
  -- being backed only by empirical evidence. 

  -- (13) At this point, everything necessary for a software generator to be
  -- defined and configured (to generate a solution for the Projectile problem)
  -- is here and ready.

  -- (14) `sysinfodb` and chunk UIDs is really a symptom of a greater issue:
  -- that we're encoding chunks as individual Haskell data types rather than
  -- operating on ASTs for our various languages.
  _sysinfodb   = symbMap,

  -- (15) https://github.com/JacquesCarette/Drasil/issues/1661#issuecomment-1021450950
  _usedinfodb  = usedDB,

  -- (16) `configFiles` is only used for GlassBR. It contains a list of files
  -- necessary for the software problem to run. I'm not actually sure why it's
  -- put here rather than `CodeSpec`.
  _configFiles = []
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

