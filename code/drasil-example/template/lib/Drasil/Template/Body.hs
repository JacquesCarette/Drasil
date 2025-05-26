-- Changes to this template should be reflected in the 'Creating Your Project 
-- in Drasil' tutorial found on the wiki:
-- https://github.com/JacquesCarette/Drasil/wiki/Creating-Your-Project-in-Drasil
-- This comment can be removed after copying this template to build your own example.

module Drasil.Template.Body where

import Language.Drasil
import Drasil.SRSDocument
import Theory.Drasil
    ( DataDefinition,
      GenDefn,
      InstanceModel,
      TheoryModel,
      tmNoRefs,
      equationalModel',
      imNoDerivNoRefs,
      ddENoRefs )
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
import Language.Drasil.ShortHands (cP, lP, cS, cD, lL, lM, lB, cA)

srs :: Document
srs = mkDoc mkSRS (S.forGen titleize phrase) si

fullSI :: System
fullSI = fillcdbSRS mkSRS si

printSetting :: PrintingInformation
printSetting = piSys fullSI Equational defaultConfiguration

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

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
        , TMs [] (Label : stdFields)
        , GDs [] [] HideDerivation
        , DDs [] (Label : stdFields) HideDerivation
        , IMs [] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) HideDerivation
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

si :: System
si = SI {
  _sys         = progName,
  _kind        = Doc.srs,
  _authors     = [authorName],
  _background  = [],
  _purpose     = [],
  _motivation  = [],
  _scope       = [],
  _quants      = quantities, -- Odd: The Table of Symbols is mostly filled without this list!!!! Also, even though the table of symbols is being filled in, there is text "There are no symbols." placed above the table!
  _instModels  = instanceModels, -- Surprisingly, putting this here is not what triggers it to get rendered.
  _datadefs    = dataDefinitions, -- ^
  _configFiles = [],
  _inputs      = [] :: [QuantityDict],
  _outputs     = [] :: [QuantityDict],
  _constraints = [] :: [ConstrainedChunk],
  _constants   = [] :: [ConstQDef],
  _systemdb   = symbMap,
  _usedinfodb  = usedDB
}

quantities :: [QuantityDict]
quantities = [
    equilibriumQ, priceQ, supplyQ, demandQ,  -- abstract variables
    applePriceQ, linearSupplyQ, linearDemandQ, -- concrete variables
    mSQ, mDQ, bSQ, bDQ, equilibriumApplePriceLinearSDQ
  ]

dataDefinitions :: [DataDefinition]
dataDefinitions = [linearSupplyDD, linearDemandDD]

instanceModels :: [InstanceModel]
instanceModels = [equilibriumPriceLinearSDQIM]

theoryModels :: [TheoryModel]
theoryModels = [equilibriumTM]

-- our theories are only rendered because they are added to this chunkDB, which is a problem.
symbMap :: ChunkDB
symbMap = cdb
  quantities
  ideaDicts
  srsDomains
  ([] :: [UnitDefn])
  dataDefinitions
  instanceModels
  ([] :: [GenDefn])
  theoryModels
  ([] :: [ConceptInstance])
  ([] :: [LabelledContent])
  ([] :: [Reference])
  citations
  where
    originalIdeaDicts = nw progName : nw inValue : [nw errMsg, nw program] ++ map nw doccon ++ map nw doccon' ++ [nw algorithm] ++ map nw prodtcon ++ map nw mathcon
    addedIdeaDicts = map nw quantities
    ideaDicts = originalIdeaDicts ++ addedIdeaDicts

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) ([] :: [IdeaDict]) ([] :: [ConceptChunk])
  ([] :: [UnitDefn]) ([] :: [DataDefinition]) ([] :: [InstanceModel])
  ([] :: [GenDefn]) ([] :: [TheoryModel]) ([] :: [ConceptInstance])
  ([] :: [LabelledContent]) ([] :: [Reference]) []

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

{-------------------------------------------------------------------------------
- Equilibrium
-------------------------------------------------------------------------------}

equilibrium :: IdeaDict
equilibrium = nc "equilibrium" $ nounPhrase "equilibrium" "equilibria"

-- There is a generic copy between economics and physics, but we won't bother
-- with that.

{-------------------------------------------------------------------------------
- Equilibrium (Economics)
-------------------------------------------------------------------------------}

equilibriumQ :: QuantityDict
equilibriumQ = mkQuant' -- A somewhat odd variant of `mkQuant` that re-orders argument and adds staging to the `Symbol`
  "inEquilibrium"
  (nounPhrase "in equilibrium" "all equilibria") -- duplicates the IdeaDict
  Nothing -- UnitDefn
  Boolean
  (autoStage $ label "inEquilibrium")
  Nothing -- "abbreviation String"

price :: IdeaDict -- I guess making this is completely unnecessary because the QuantityDict will make its own.
price = nc "price" $ cn "price"

priceQ :: QuantityDict
priceQ = mkQuant'
  "priceQuant" -- duplicates the IdeaDict
  (cn "price")
  Nothing -- UnitDefn // FIXME: Uh oh! There's seemingly no way we can define a new unit: $CAD.s
  Real -- Not quite a "Real," more of a "dollar amount with two decimal places, or a tuple containing two integers, with the second capped at 100"
  (autoStage cP)
  Nothing -- Abbreviation

supply :: IdeaDict
supply = nc "supply" $ cnIES "supply"

supplyDemandFuncSpace :: Space
supplyDemandFuncSpace = mkFunction [Real] Integer -- Real should be Dollar, output should be strictly non-negative integers (Z^+)

supplyQ :: QuantityDict
supplyQ = mkQuant'
  "supplyQuant"
  (cnIES "supply")
  Nothing -- UnitDefn -- Should units be a part of expressions? Supply is a function, so it doesn't have a unit. However, the output of the function should have a unit. The input of the function should have units as well.
  supplyDemandFuncSpace
  (autoStage cS)
  Nothing -- Abbreviation

demand :: IdeaDict
demand = nc "demand" $ cnIES "demand"

demandQ :: QuantityDict
demandQ = mkQuant'
  "demandQuant"
  (cnIES "demand")
  Nothing -- UnitDefn
  supplyDemandFuncSpace
  (autoStage cD)
  Nothing -- Abbreviation

equilibriumQD :: QDefinition ModelExpr -- equilibrium = S(P) == D(P)
equilibriumQD = mkQuantDef equilibriumQ $ apply1 supplyQ priceQ $= apply1 demandQ priceQ

equilibriumTM :: TheoryModel
equilibriumTM = tmNoRefs
  (equationalModel' equilibriumQD)
  [qw demandQ, qw supplyQ, qw priceQ]
  ([] :: [ConceptChunk]) -- This looks like just a wart? IIRC, no tmNoRefs/tm constructor uses this parameter
  [equilibriumQD] -- defined quantities
  [] -- defined invariants
  [] -- defined functions
  "equilibriumTM"
  [S "Equilibrium occurs when the supply and demand curves intersect at current price."]

-- Note: In the above, the label for "equilibrium" (the QuantityDict) is only
-- assigned in the TheoryModel!

applePriceQ :: QuantityDict
applePriceQ = mkQuant'
  "applePriceQ"
  (cn "apple price")
  Nothing -- UnitDefn
  Real -- Space
  (autoStage $ sub lP cA)
  Nothing -- Abbreviation

linearSupplyQ :: QuantityDict
linearSupplyQ = mkQuant'
  "linearSupplyQuant"
  (pn "price-dependant apple supply")
  Nothing -- UnitDefn
  (mkFunction [Real] Integer)
  (autoStage $ sub cS lL)
  Nothing -- Abbreviation

linearDemandQ :: QuantityDict
linearDemandQ = mkQuant'
  "linearDemandQuant"
  (pn "price-dependant apple demand")
  Nothing -- UnitDefn
  (mkFunction [Real] Integer)
  (autoStage $ sub cD lL)
  Nothing -- Abbreviation

mSQ, mDQ, bSQ, bDQ :: QuantityDict
mSQ = mkQuant'
  "ms"
  (pn "marginal apple supply by price") -- proper noun?
  Nothing -- UnitDefn
  Real -- Should be UNITS/DOLLAR
  (autoStage $ sub lM cS)
  Nothing -- Abbreviation
mDQ = mkQuant'
  "md"
  (pn "marginal apple demand by price") -- proper noun?
  Nothing -- UnitDefn
  Real -- Should be UNITS/DOLLAR
  (autoStage $ sub lM cD)
  Nothing -- Abbreviation
bSQ = mkQuant'
  "bs"
  (pn "minimum supply of apples") -- proper noun?
  Nothing -- UnitDefn
  Real -- Should be UNITS
  (autoStage $ sub lB cS)
  Nothing -- Abbreviation
bDQ = mkQuant'
  "bd"
  (pn "maximum demand of apples") -- proper noun?
  Nothing -- UnitDefn
  Real -- Should be UNITS
  (autoStage $ sub lB cD)
  Nothing -- Abbreviation

linearSupplyQD :: QDefinition Expr
-- TODO: Missing "Real -> Integer" truncation function for the below function
linearSupplyQD = mkFuncDefByQ linearSupplyQ [applePriceQ] $ sy mSQ $* sy applePriceQ $+ sy bSQ
-- ^ I'm deliberately choosing to use applePrice here because I want to use this
-- in an instance model, which (I believe) should mean that there are only
-- 'concrete' variables (i.e., problem-related variables, not the abstract ones
-- imported from the hypothetical library that would contain equilibrium)

linearSupplyDD :: DataDefinition
linearSupplyDD = ddENoRefs
  linearSupplyQD
  Nothing -- Derivation
  "linearSupplyDD" -- "ShortName"
  [] -- Notes

linearDemandQD :: QDefinition Expr
linearDemandQD = mkFuncDefByQ linearDemandQ [applePriceQ] $ sy mDQ $* sy applePriceQ $+ sy bDQ

linearDemandDD :: DataDefinition
linearDemandDD = ddENoRefs
  linearSupplyQD
  Nothing -- Derivation
  "linearDemandDD" -- "ShortName"
  [] -- Notes

equilibriumApplePriceLinearSDQ :: QuantityDict
equilibriumApplePriceLinearSDQ = mkQuant'
  "equilibriumApplePriceQ"
  (cn "equilibrium apple price")
  Nothing -- UnitDefn
  Real -- Space
  (autoStage $ sub lP $ label "A,Eq") -- Hack because of lack of support for symbol "corners" in Drasil.
  Nothing -- Abbreviation

equilibriumApplePriceLinearSDQD :: QDefinition Expr
equilibriumApplePriceLinearSDQD = mkQuantDef equilibriumApplePriceLinearSDQ $
  (sy bDQ $- sy bSQ) $/ (sy mSQ $- sy mDQ)

equilibriumPriceLinearSDQIM :: InstanceModel
equilibriumPriceLinearSDQIM = imNoDerivNoRefs
  (equationalModel' equilibriumApplePriceLinearSDQD)
  [] -- Inputs -- What are they? Why is this only here and not in the TM?
  equilibriumApplePriceLinearSDQ -- Output -- not everything will have an output!
  [UpFrom (Exc, int 0)] -- Output constraints: price > 0 -- why can't I put gtZerConstr
  "equilibriumApplePriceIM"
  [] -- Notes ([Sentence])

{-------------------------------------------------------------------------------
- Equilibrium (Physics)
-------------------------------------------------------------------------------}

