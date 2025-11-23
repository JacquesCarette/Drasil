module Drasil.HGHC.Body (si, mkSRS) where

import Language.Drasil hiding (Manual) -- Citation name conflict. FIXME: Move to different namespace
import Drasil.SRSDocument
import Drasil.Generator (cdb)
import Drasil.System (mkSystem, SystemKind(Specification))

import Drasil.HGHC.HeatTransfer (fp, dataDefs, htInputs, htOutputs,
    nuclearPhys, symbols)
import Drasil.HGHC.MetaConcepts (progName)

import Data.Drasil.People (spencerSmith)
import Data.Drasil.Concepts.Thermodynamics as CT (heatTrans)

si :: System
si = mkSystem
  progName Specification [spencerSmith]
  [purp] [] [] []
  symbols
  [] [] dataDefs [] []
  htInputs htOutputs ([] :: [ConstrConcept]) []
  symbMap []

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
  [nw progName]

symbMap :: ChunkDB
symbMap = cdb symbols ideaDicts ([] :: [ConceptChunk])
  ([] :: [UnitDefn]) dataDefs [] [] [] [] [] []
