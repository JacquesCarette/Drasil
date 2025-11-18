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
  symbMap

mkSRS :: SRSDecl
mkSRS = [TableOfContents,
    RefSec $
    RefProg intro [TUnits, tsymb [TSPurpose, SymbConvention [Lit $ nw nuclearPhys, Manual $ nw fp]]],
    IntroSec $
    IntroProg introPara (phrase progName) [],
    SSDSec $ SSDProg [
      SSDSolChSpec $ SCSProg [
          TMs [] []
        , GDs [] [] HideDerivation
        , DDs [] [Label, Symbol, Units, DefiningEquation,
          Description Verbose IncludeUnits] HideDerivation
        , IMs [] [] HideDerivation
      ]]]

-- Introduction first paragraph
introPara :: Sentence
introPara = foldlSent [
  S "Heat transfer through the cladding of a nuclear fuel element influences",
  S "performance and safety. Engineers therefore rely on dependable calculations",
  S "of the heat transfer coefficients used for simulating the temperature.",
  S "This document describes the requirements of a program called",
  programNameWithShortForm, provenanceInfo]

-- Optional short name
programNameWithShortForm :: Sentence
programNameWithShortForm = 
  if hasDistinctShortForm progName
     then phrase progName +:+ sParen (short progName)
     else phrase progName
hasDistinctShortForm :: CI -> Bool
hasDistinctShortForm ci = case getA ci of
  Nothing -> False
  Just abbr -> not (null abbr)

-- Optional provenance information
provenanceInfo :: Sentence
provenanceInfo = 
  let hasProvenance = False
  in if hasProvenance
     then S ", which is based on the original, manually created version of" -- +:+ provenanceRef
     else EmptyS

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
  ([] :: [UnitDefn]) dataDefs [] [] [] [] [] [] []
