module Drasil.HGHC.Body (si, mkSRS) where

import Drasil.Database (ChunkDB)
import Drasil.SRS
import Drasil.Generator (withCommonKnowledge)
import Language.Drasil hiding (Manual) -- Citation name conflict. FIXME: Move to different namespace

import Drasil.HGHC.HeatTransfer (fp, dataDefs, htInputs, htOutputs,
    nuclearPhys, symbols)
import Drasil.HGHC.MetaConcepts (projName)

import Data.Drasil.People (spencerSmith)
import Data.Drasil.Concepts.Thermodynamics as CT (heatTrans)
import Drasil.System (projTitleS)

si :: SmithEtAlSRS
si = mkSmithEtAlICO
  projName [spencerSmith]
  [purp] [] [] []
  [] [] dataDefs []
  htInputs htOutputs ([] :: [ConstrConcept]) [] symbols
  symbMap

mkSRS :: SRSDecl
mkSRS = [TableOfContents,
    RefSec $
    RefProg intro [TUnits, tsymb [TSPurpose, SymbConvention [Lit nuclearPhys, Manual fp]]],
    IntroSec $
    IntroProg introPara [] [],
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
  projTitleS projName]

purp :: Sentence
purp = foldlSent [S "describe", phrase CT.heatTrans, S "coefficients related to clad"]

ideaDicts :: [IdeaDict]
ideaDicts = [fp, nuclearPhys]

symbMap :: ChunkDB
symbMap = withCommonKnowledge projName [] symbols ideaDicts [] [] [] dataDefs
  [] [] [] [] [] []
