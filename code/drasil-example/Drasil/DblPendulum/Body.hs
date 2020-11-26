module Drasil.DblPendulum.Body where

import Language.Drasil hiding (Symbol(..), Vector)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block, ChunkDB, ReferenceDB, SystemInformation(SI),
  cdb, rdb, refdb, _authors, _purpose, _concepts, _constants, _constraints, 
  _datadefs, _configFiles, _definitions, _defSequence, _inputs, _kind, _outputs, 
  _quants, _sys, _sysinfodb, _usedinfodb)
import Utils.Drasil
import Data.Drasil.People (olu)
import Data.Drasil.SI_Units (metre, second, newton, kilogram, degree, radian, hertz)
import Data.Drasil.Concepts.Software (program, errMsg)
import Data.Drasil.Concepts.Physics (gravity, physicCon, physicCon', pendulum, twoD)
import Data.Drasil.Quantities.Physics (physicscon)
import Data.Drasil.Concepts.PhysicalProperties (mass, len, physicalcon)
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Data.Drasil.Concepts.Documentation (doccon, doccon', srsDomains, problem, analysis)
import Data.Drasil.Concepts.Computation (inValue)
import Drasil.DocLang (AuxConstntSec(AuxConsProg),
  DerivationDisplay(ShowDerivation),
  DocSection(AuxConstntSec, Bibliography, IntroSec, RefSec, ReqrmntSec, SSDSec, TraceabilitySec),
  Emphasis(Bold), Field(..), Fields, InclUnits(IncludeUnits),
  IntroSec(..), IntroSub(IScope), ProblemDescription(PDProg), PDSub(..),
  RefSec(..), RefTab(..), ReqrmntSec(..), ReqsSub(..), SCSSub(..), SRSDecl,
  SSDSec(..), SSDSub(SSDProblem, SSDSolChSpec), SolChSpec(SCSProg),
  TConvention(..), TSIntro(..), TraceabilitySec(TraceabilityProg),
  Verbosity(Verbose), intro, mkDoc, traceMatStandard, tsymb)
import Drasil.DblPendulum.Figures (figMotion)
import Data.Drasil.Concepts.Math (mathcon, cartesian)
import Data.Drasil.Quantities.Math (unitVect, unitVectj)
import Drasil.DblPendulum.Assumptions (assumptions)
import Drasil.DblPendulum.Concepts (pendulumTitle)
import Drasil.DblPendulum.Goals (goals, goalsInputs)
import Drasil.DblPendulum.DataDefs (dataDefs)
import Drasil.DblPendulum.TMods (tMods)
import Drasil.DblPendulum.IMods (iMods)
import Drasil.DblPendulum.GenDefs (genDefns)
import Drasil.DblPendulum.Unitals (symbols, inputs, outputs,
  inConstraints, outConstraints, acronyms)
import Drasil.DblPendulum.Requirements (funcReqs, nonFuncReqs)
import Drasil.DblPendulum.References (citations)


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
  IntroSec $            -- This adds an introduction with an overview of the sub-sections
    IntroProg justification (phrase pendulumTitle) -- This adds an introductory blob before the overview paragraph above.
      [IScope scope],                            -- This section add a Scope section with the content of 'scope' constructor.
  SSDSec $ 
    SSDProg                               -- This adds a Specific system description section and an introductory blob.
      [ SSDProblem $ PDProg prob []                --  This adds a is used to define the problem your system will solve
        [ TermsAndDefs Nothing terms               -- This is used to define the terms to be defined in terminology sub section
      , PhySysDesc pendulumTitle physSystParts figMotion [] -- This defines the Physicalsystem sub-section, define the parts
                                                          -- of the system using physSysParts, figMotion is a function in figures for the image
      , Goals goalsInputs] -- This adds a goals section and goals input is defined for the preample of the goal.
      , SSDSolChSpec $ SCSProg --This creates the solution characteristics section with a preamble
        [ Assumptions -- This adds the assumption section
        , TMs [] (Label : stdFields)
        , GDs [] ([Label, Units] ++ stdFields) ShowDerivation
        , DDs [] ([Label, Symbol, Units] ++ stdFields) ShowDerivation
        , IMs [] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) ShowDerivation
        , Constraints EmptyS inConstraints
        , CorrSolnPpties outConstraints []
       ]
     ],
  ReqrmntSec $ ReqsProg
    [ FReqsSub EmptyS []
    , NonFReqsSub
    ],
  TraceabilitySec $ TraceabilityProg $ traceMatStandard si,
  AuxConstntSec $
     AuxConsProg pendulumTitle [],  --Adds Auxilliary constraint section
  Bibliography                    -- Adds reference section
  ]

justification :: Sentence
justification = foldlSent [S "A", phrase pendulum, S "consists" `sOf` S "mass", 
                            S "attached to the end of a rod" `sC` S "its moving curve" `sIs`
                            S "highly sensitive to initial conditions" +:+ S "Therefore" `sC`
                            S "it is useful to have a", phrase program, S "to simulate the motion"
                            `ofThe` phrase pendulum, S "to exhibit the chaotic characteristics" `sOf` S "it",
                            S "The", phrase program, S "documented here is called", phrase pendulum]
scope :: Sentence
scope = foldlSent [S "the", phrase analysis `sOf` S "a", phrase twoD, 
  sParen (getAcc twoD), phrase pendulum, S "motion", phrase problem,
                   S "with various initial conditions"]

si :: SystemInformation
si = SI {
  _sys         = pendulumTitle, 
  _kind        = Doc.srs,
  _authors     = [olu],
  _purpose     = [],
  _quants      = symbols,
  _concepts    = [] :: [DefinedQuantityDict],
  _definitions = [] :: [QDefinition],
  _datadefs    = dataDefs,
  _configFiles  = [],
  _inputs      = inputs,
  _outputs     = outputs,
  _defSequence = [] :: [Block QDefinition],
  _constraints = inConstraints,
  _constants   = [] :: [QDefinition],
  _sysinfodb   = symbMap,
  _usedinfodb  = usedDB,
   refdb       = refDB
}

symbMap :: ChunkDB
symbMap = cdb (map qw iMods ++ map qw symbols)
  (nw pendulumTitle : nw mass : nw len : nw kilogram : nw inValue : nw newton : nw degree : nw radian
    : nw unitVect : nw unitVectj : [nw errMsg, nw program] ++ map nw symbols ++
   map nw doccon ++ map nw doccon' ++ map nw physicCon ++ map nw mathcon  ++ map nw physicCon' ++
   map nw physicscon ++ map nw physicalcon ++ map nw acronyms ++ map nw symbols ++ map nw [metre, hertz])
  (map cw iMods ++ srsDomains) (map unitWrapper [metre, second, newton, kilogram, degree, radian, hertz]) dataDefs
  iMods genDefns tMods concIns [] []


usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) (map nw acronyms ++ map nw symbols) ([] :: [ConceptChunk])
  ([] :: [UnitDefn]) [] [] [] [] [] [] []

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

refDB :: ReferenceDB
refDB = rdb citations concIns

concIns :: [ConceptInstance]
concIns = assumptions ++ goals ++ funcReqs ++ nonFuncReqs
-- ++ likelyChgs ++ unlikelyChgs

------------------------------------
--Problem Description
------------------------------------

prob :: Sentence
prob = foldlSent_ [ S "is needed to efficiently and correctly to predict the motion",  
                   phrase pendulum]

---------------------------------
-- Terminology and Definitions --
---------------------------------

terms :: [ConceptChunk]
terms = [gravity, cartesian]


-- ---------------------------------
-- -- Physical System Description --
-- ---------------------------------

physSystParts :: [Sentence]
physSystParts = map foldlSent [
  [S "The rod"],
  [S "The", phrase mass]]
  


 

