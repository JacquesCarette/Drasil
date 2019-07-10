module Drasil.HGHC.Body (srs, si, allSymbols, printSetting) where

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
  collectUnits, rdb, refdb, _authors, _concepts, _constants, _constraints,
  _datadefs, _definitions, _defSequence, _inputs, _kind, _outputs, _quants, 
  _sys, _sysinfodb, _usedinfodb)
import Utils.Drasil

import Drasil.HGHC.HeatTransfer (fp, hghc, dataDefs, htInputs, htOutputs, 
    nuclearPhys, symbols)

import Data.Drasil.SI_Units (siUnits, fundamentals, derived, degree)
import Data.Drasil.People (spencerSmith)
import Data.Drasil.Concepts.Documentation (doccon, doccon')
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
  
srs :: Document
srs = mkDoc mkSRS for si

printSetting :: PrintingInformation
printSetting = PI allSymbols defaultConfiguration

si :: SystemInformation
si = SI {
  _sys = hghc,
  _kind = Doc.srs,
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

mkSRS :: DocDesc
mkSRS = [RefSec $
    RefProg intro [TUnits, tsymb [TSPurpose, SymbConvention [Lit $ nw nuclearPhys, Manual $ nw fp]]],
    SSDSec $ SSDProg [
      SSDSolChSpec $ SCSProg [
        DDs [] [Label, Symbol, Units, DefiningEquation,
          Description Verbose IncludeUnits] dataDefs HideDerivation
      ]]]

unitsColl :: [UnitDefn] -- FIXME? Probably shouldn't be done here
unitsColl = collectUnits allSymbols symbols 

allSymbols :: ChunkDB
allSymbols = cdb symbols (map nw symbols ++ map nw doccon ++ map nw fundamentals ++ map nw derived
  ++ [nw fp, nw nuclearPhys, nw hghc, nw degree] ++ map nw doccon')
 ([] :: [ConceptChunk])-- FIXME: Fill in concepts
  siUnits Map.empty Map.empty [] [] [] [] [] [] []

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) (map nw symbols ++ map nw unitsColl)
           ([] :: [ConceptChunk]) unitsColl Map.empty Map.empty [] [] [] []
           [] [] []