module Drasil.GlassBR.MIS (glassBRMis, printSetting)  where

import qualified Data.Map as Map
import Language.Drasil hiding (organization)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (ChunkDB, RefbyMap, ReferenceDB, SystemInformation(SI),
  TraceMap, cdb, collectUnits, generateRefbyMap, rdb, refdb, _authors,
  _concepts, _constants, _constraints, _datadefs, _definitions, _defSequence,
  _inputs, _kind, _outputs, _quants, _sys, _sysinfodb, _usedinfodb)

import Drasil.DocLang (DocDesc, DocSection(..), IntroSec(IntroMIS),
  MISModSub(..), MISModSec(..), MISSemanticsSub(..), MISSyntaxSub(..),
  ModHierarchSec(ModHierarchProg), NotationSec(NotationProg),
  generateTraceMap, generateTraceMap', mkDoc)

import Drasil.DocLang.MIS (hwModIntro, inputModIntro, misCitations)

import Data.Drasil.Concepts.Computation (compcon, algorithm)
import Data.Drasil.Concepts.Documentation as Doc (doccon, doccon', mis, notation,
  srsDomains, templateModule)
import Data.Drasil.Concepts.Education as Edu (educon)
import Data.Drasil.Concepts.Math (mathcon, mathcon')
import Data.Drasil.Concepts.PhysicalProperties (physicalcon)
import Data.Drasil.Concepts.Physics (distance)
import Data.Drasil.Concepts.Software (softwarecon)
import Data.Drasil.Software.Products (sciCompS)

import Data.Drasil.People (spencerSmith)
import Data.Drasil.Phrase (for'')
import Data.Drasil.SI_Units (kilogram, metre, newton, pascal, second)
  
import Drasil.GlassBR.Assumptions (assumptionConstants)
import Drasil.GlassBR.Concepts (acronyms, gLassBR, glasscon, glasscon')
import Drasil.GlassBR.DataDefs (dataDefns, gbQDefns, glaTyFac, hFromt)
import Drasil.GlassBR.IMods (glassBRsymb)
import Drasil.GlassBR.Symbols (symbolsForTable, thisSymbols)
import Drasil.GlassBR.Unitals (controlConsts, controlStVars, gBRSpecParamVals,
  gbConstants, gbConstrained, gbInputs, gbOutputs, maxOrderConst)

{--}

gbSymbMap :: ChunkDB
gbSymbMap = cdb thisSymbols (map nw acronyms ++ map nw thisSymbols ++ map nw glasscon
  ++ map nw glasscon' ++ map nw doccon ++ map nw doccon' ++ map nw educon
  ++ [nw sciCompS] ++ map nw compcon ++ map nw mathcon ++ map nw mathcon'
  ++ map nw softwarecon ++ [nw distance, nw algorithm] ++ map nw physicalcon
  ++ map nw [notation, templateModule])
  (map cw glassBRsymb ++ Doc.srsDomains) (map unitWrapper [metre, second, kilogram]
  ++ map unitWrapper [pascal, newton]) glassBRLabel glassBRRefby
  [] [] [] [] [] [] []

glassBRRefby :: RefbyMap
glassBRRefby = generateRefbyMap glassBRLabel 

glassBRLabel :: TraceMap
glassBRLabel = Map.union (generateTraceMap mkMIS) $ generateTraceMap' []

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) (map nw acronyms ++ map nw thisSymbols ++ map nw checkSi)
 ([] :: [ConceptChunk]) checkSi glassBRLabel glassBRRefby
  [] [] [] [] [] [] []

gbRefDB :: ReferenceDB
gbRefDB = rdb misCitations []

printSetting :: PrintingInformation
printSetting = PI gbSymbMap defaultConfiguration

checkSi :: [UnitDefn]
checkSi = collectUnits gbSymbMap thisSymbols 

{------}

glassBRMis :: Document
glassBRMis = mkDoc mkMIS (for'' titleize phrase) glassSystInfo

mkMIS :: DocDesc
mkMIS = [IntroSec (IntroMIS (S "https://github.com/smiths/caseStudies/tree/master/CaseStudies/glass")),
  NotationSec (NotationProg []),
  ModHierarchSec (ModHierarchProg $ S "section 3 of the MG (Link)"), --FIXME: hardcoded link
  Bibliography,
  MISModSec (MISModProg "Control" Nothing [MISUses ["Input", "LoadASTM", "Calc", "Output"],
    MISSyntax [MISExportedCs controlConsts, MISExportedAPs [{-FILL IN-}]],
    MISSemantics [MISStateVars controlStVars, MISAccessRoutines [{-FILL IN-}]]]
    {-ctrlLabel-} False),
  MISModSec (MISModProg "Input" (Just inputModIntro) [MISUses ["GlassType", "Thickness", "Constants", "Hardware"],
    MISSyntax [{-MISSyntaxSubVerb [{-FILL IN-}]-}],
    MISSemantics [MISEnvVars [{-FILL IN-}], MISStateVars ([] :: [QDefinition]), MISAssumptions [{-FILL IN-}], MISAccessRoutines [{-FILL IN-}]],
    MISConsiderations [inputCons]]
    {-inputLabel-} False),
  MISModSec (MISModProg "LoadASTM" Nothing [MISUses ["Funct", "Contours"],
    MISSyntax [MISExportedCs ([] :: [QDefinition]), MISExportedAPs [{-FILL IN-}]],
    MISSemantics [MISEnvVars [{-FILL IN-}], MISStateVars ([] :: [QDefinition]), MISStateInvariant [], MISAssumptions [{-FILL IN-}], MISAccessRoutines [{-FILL IN-}]]]
    {-loadLabel-} False),
  MISModSec (MISModProg "Calc" Nothing [MISUses ["Input", "Contours", "Constants"],
    MISSyntax [MISExportedCs ([] :: [QDefinition]), MISExportedAPs [{-FILL IN-}]],
    MISSemantics [MISStateVars ([] :: [QDefinition]), MISStateInvariant [], MISAssumptions [], MISAccessRoutines [{-FILL IN-}]]]
    {-calcLabel-} False),
  MISModSec (MISModProg "GlassType" (Just (mkParagraph (S "from" +:+ makeRef2S glaTyFac))) --FIXME: link is broken
    [MISUses [],
    MISSyntax [MISExportedCs ([] :: [QDefinition]), MISExportedAPs [{-FILL IN-}]],
    MISSemantics [MISStateVars ([] :: [QDefinition]), MISStateInvariant [], MISAssumptions [], MISAccessRoutines [{-FILL IN-}]]]
    {-glTypeLabel-} True),
  MISModSec (MISModProg "Thickness" (Just (mkParagraph (S "following" +:+ makeRef2S hFromt))) --FIXME: link is broken
    [MISUses [],
    MISSyntax [MISExportedCs ([] :: [QDefinition]), MISExportedAPs [{-FILL IN-}]],
    MISSemantics [MISStateVars ([] :: [QDefinition]), MISStateInvariant [], MISAssumptions [], MISAccessRoutines [{-FILL IN-}]]]
    {-thicknessLabel-} True),
  MISModSec (MISModProg "Funct" Nothing [MISUses ["SeqServices"],
    MISSyntax [MISExportedCs maxOrderConst, MISExportedAPs [{-FILL IN-}]],
    MISSemantics [MISStateVars ([] :: [QDefinition]), MISStateInvariant [], MISAssumptions [], MISAccessRoutines [{-FILL IN-}]],
    MISConsiderations [fxnCons]]
    {-functLabel-} True),
  MISModSec (MISModProg "Contours" Nothing [MISUses ["Funct"],
    MISSyntax [MISExportedCs maxOrderConst, MISExportedAPs [{-FILL IN-}]],
    MISSemantics [MISStateVars ([] :: [QDefinition]), MISStateInvariant [], MISAssumptions [], MISAccessRoutines [{-FILL IN-}]]]
    {-contoursLabel-} True),
  MISModSec (MISModProg "SeqServices" Nothing [MISUses [],
    MISSyntax [MISExportedCs ([] :: [QDefinition]), MISExportedAPs [{-FILL IN-}]],
    MISSemantics [MISStateVars ([] :: [QDefinition]), MISStateInvariant [], MISAssumptions [{-FILL IN-}], MISAccessRoutines [{-FILL IN-}]]]
    {-seqServLabel-} False),
  MISModSec (MISModProg "Output" Nothing [MISUses ["Input", "Thickness", "GlassType", "Hardware"],
    MISSyntax [MISExportedCs ([] :: [QDefinition]), MISExportedAPs [{-FILL IN-}]], 
    MISSemantics [MISEnvVars [{-FILL IN-}], MISStateVars ([] :: [QDefinition]), MISStateInvariant [], MISAssumptions [], MISAccessRoutines [{-FILL IN-}]]]
    {-outputLabel-} False),
  MISModSec (MISModProg "Constants" Nothing [MISUses [], 
    MISSyntax [MISExportedCs auxiliaryConstants, MISExportedTyps [], MISExportedAPs []],
    MISSemantics [MISStateVars ([] :: [QDefinition]), MISStateInvariant []]]
    {-constantsLabel-} False),
  MISModSec (MISModProg "Hardware" (Just hwModIntro) [MISUses []] 
    {-hwLabel-} False)]

auxiliaryConstants :: [QDefinition]
auxiliaryConstants = assumptionConstants ++ gBRSpecParamVals

fxnCons :: Contents
fxnCons = mkParagraph $ S "For simplicity the function evaluation is" +:+
  S "not defined within one step of the boundaries. By considering" +:+ 
  S "the special cases it would be possible to get right to the edge."

inputCons :: Contents
inputCons = mkParagraph $ S "The value of each state variable can be accessed" +:+ 
  S "through its name (getter). An access program is available for each state" +:+
  S "variable. There are no setters for the state variables, since the values" +:+
  S "will be set and checked by load params and will not be changed for the" +:+ 
  S "life of the program."

{-----}

  --FIXME: All named ideas, not just acronyms.

--FIXME: modify SystemInformation to hold information for both MIS and SRS?
glassSystInfo :: SystemInformation
glassSystInfo = SI {
  _sys         = gLassBR,
  _kind        = mis,
  _authors     = [spencerSmith],
  _quants      = symbolsForTable,
  _concepts    = [] :: [DefinedQuantityDict],
  _definitions = [] :: [QDefinition],
  _datadefs    = dataDefns,
  _inputs      = map qw gbInputs,
  _outputs     = map qw gbOutputs,
  _defSequence = gbQDefns,
  _constraints = gbConstrained,
  _constants   = gbConstants,
  _sysinfodb   = gbSymbMap,
  _usedinfodb  = usedDB,
   refdb       = gbRefDB
}
