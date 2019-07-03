module Drasil.HGHC.HGHC (srsBody, thisSI, allSymbols, printSetting) where

import qualified Data.Map as Map
import Language.Drasil hiding (Manual) -- Citation name conflict. FIXME: Move to different namespace
import Drasil.DocLang (DocSection(RefSec, SSDSec), Literature(Lit, Manual), 
    RefSec(..), RefTab(TUnits), TSIntro(SymbConvention, TSPurpose), DocDesc, 
    intro, mkDoc, tsymb, InclUnits(IncludeUnits), Verbosity(Verbose),
    Field(DefiningEquation, Description, Label, Symbol, Units), SolChSpec(SCSProg), 
    SCSSub(DDs), DerivationDisplay(HideDerivation), SSDSub(SSDSolChSpec), 
    SSDSec(SSDProg))
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block, ChunkDB, SystemInformation(SI), cdb,
  rdb, refdb, _authors, _concepts, _constants, _constraints,
  _datadefs, _definitions, _defSequence, _inputs, _kind, _outputs, _quants, 
  _sys, _sysinfodb, _usedinfodb)
import Utils.Drasil

import Drasil.HGHC.HeatTransfer (fp, hghc, dataDefs, htInputs, htOutputs, 
    nuclearPhys, symbols)

import Data.Drasil.SI_Units (siUnits, fundamentals, derived, degree)
import Data.Drasil.People (spencerSmith)
import Data.Drasil.Concepts.Documentation (srs, doccon, doccon')
  
thisSI :: SystemInformation
thisSI = SI {
  _sys = hghc,
  _kind = srs,
  _authors = [spencerSmith],
  _quants = symbols,
  _concepts = [] :: [UnitaryConceptDict],
  _definitions = [] :: [QDefinition],
  _datadefs = dataDefs,
  _inputs = htInputs,
  _outputs = htOutputs,
  _defSequence = [] :: [Block QDefinition],
  _constraints = [] :: [ConstrainedChunk],
  _constants = [],
  _sysinfodb = allSymbols,
  _usedinfodb = usedDB,
   refdb = rdb [] [] -- FIXME?
}

allSymbols :: ChunkDB
allSymbols = cdb symbols (map nw symbols ++ map nw doccon ++ map nw fundamentals ++ map nw derived
  ++ [nw fp, nw nuclearPhys, nw hghc, nw degree] ++ map nw doccon')
 ([] :: [ConceptChunk])-- FIXME: Fill in concepts
  siUnits Map.empty Map.empty [] [] [] [] [] [] []

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) (map nw symbols)
           ([] :: [ConceptChunk]) ([] :: [UnitDefn]) Map.empty Map.empty []
           [] [] [] [] [] []

printSetting :: PrintingInformation
printSetting = PI allSymbols defaultConfiguration
  
thisSRS :: DocDesc
thisSRS = [RefSec $
    RefProg intro [TUnits, tsymb [TSPurpose, SymbConvention [Lit $ nw nuclearPhys, Manual $ nw fp]]],
    SSDSec $ SSDProg [
      SSDSolChSpec $ SCSProg [
        DDs [] [Label, Symbol, Units, DefiningEquation,
          Description Verbose IncludeUnits] dataDefs HideDerivation
      ]]]
  
srsBody :: Document
srsBody = mkDoc thisSRS for thisSI
