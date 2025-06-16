module Drasil.HGHC.Body (srs, si, symbMap, printSetting, fullSI) where

import Language.Drasil hiding (Manual) -- Citation name conflict. FIXME: Move to different namespace
import Drasil.SRSDocument
import qualified Language.Drasil.Sentence.Combinators as S

import Drasil.HGHC.HeatTransfer (fp, dataDefs, htInputs, htOutputs, 
    nuclearPhys, symbols)
import Drasil.HGHC.MetaConcepts (progName)

import Data.Drasil.SI_Units (siUnits)
import Data.Drasil.People (spencerSmith)
import Data.Drasil.Concepts.Thermodynamics as CT (heatTrans)  
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
  
srs :: Document
srs = mkDoc mkSRS S.forT si

fullSI :: System
fullSI = fillcdbSRS mkSRS si

printSetting :: PrintingInformation
printSetting = piSys fullSI Equational defaultConfiguration

si :: System
si = SI {
  _sys         = progName,
  _kind        = Doc.srs,
  _authors     = [spencerSmith],
  _quants      = symbols,
  _purpose     = [purp],
  _background  = [],
  _motivation  = [],
  _scope       = [],
  _instModels  = [], -- FIXME; empty _instModels
  _datadefs    = dataDefs,
  _configFiles = [],
  _inputs      = htInputs,
  _outputs     = htOutputs,
  _constraints = [] :: [ConstrainedChunk],
  _constants   = [],
  _systemdb   = symbMap,
  _usedinfodb  = usedDB
}
  
mkSRS :: SRSDecl
mkSRS = [TableOfContents,
    RefSec $
    RefProg intro [TUnits, tsymb [TSPurpose, SymbConvention [Lit $ nw nuclearPhys, Manual $ nw fp]]],
    SSDSec $ SSDProg [
      SSDSolChSpec $ SCSProg [
          TMs [] []
        , GDs [] [] HideDerivation
        , DDs [] [Label, Symbol, Units, DefiningEquation,
          Description Verbose IncludeUnits] HideDerivation
        , IMs [] [] HideDerivation
      ]]]

purp :: Sentence
purp = foldlSent [S "describe", phrase CT.heatTrans, S "coefficients related to clad"]

ideaDicts :: [IdeaDict]
ideaDicts =
  -- Actual IdeaDicts
  [fp, nuclearPhys] ++
  -- CIs
  nw progName :
  -- QuantityDicts
  map nw symbols


symbMap :: ChunkDB
symbMap = cdb symbols ideaDicts
  ([] :: [ConceptChunk])-- FIXME: Fill in concepts
  siUnits dataDefs [] [] [] [] [] [] []

tableOfAbbrvsIdeaDicts :: [IdeaDict]
tableOfAbbrvsIdeaDicts =
  -- QuantityDicts
  map nw symbols

usedDB :: ChunkDB
usedDB = cdb' ([] :: [QuantityDict]) tableOfAbbrvsIdeaDicts
           ([] :: [ConceptChunk]) ([] :: [UnitDefn])
           [] [] [] [] [] [] ([] :: [Reference]) []
