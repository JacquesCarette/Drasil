module Drasil.HGHC.Body (srs, si, symbMap, printSetting, fullSI) where

import Language.Drasil hiding (Manual) -- Citation name conflict. FIXME: Move to different namespace
import Drasil.SRSDocument
import qualified Language.Drasil.Sentence.Combinators as S

import Drasil.HGHC.HeatTransfer (fp, dataDefs, htInputs, htOutputs, 
    nuclearPhys, symbols)
import Drasil.HGHC.MetaConcepts (progName)

import Data.Drasil.SI_Units (siUnits)
import Data.Drasil.People (spencerSmith)
import Data.Drasil.Concepts.Documentation (doccon, doccon')
import Data.Drasil.Concepts.Math (mathcon)
import Data.Drasil.Concepts.Thermodynamics as CT (heatTrans)  
 
import System.Drasil (SystemKind(Specification))

srs :: Document
srs = mkDoc mkSRS S.forT si

fullSI :: System
fullSI = fillcdbSRS mkSRS si

printSetting :: PrintingInformation
printSetting = piSys fullSI Equational defaultConfiguration

si :: System
si = SI {
  _sys         = progName,
  _kind        = Specification,
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
  _constraints = [] :: [ConstrConcept],
  _constants   = [],
  _systemdb   = symbMap
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
  [fp, nuclearPhys] ++ doccon ++
  -- CIs
  nw progName : map nw doccon'

symbMap :: ChunkDB
symbMap = cdb symbols ideaDicts mathcon
  siUnits dataDefs [] [] [] [] [] [] []

