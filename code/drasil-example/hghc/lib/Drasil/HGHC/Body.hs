module Drasil.HGHC.Body (si, mkSRS) where

import Drasil.Database (ChunkDB)
import Drasil.SRS
import Drasil.Generator (withCommonKnowledge)
import Language.Drasil hiding (Manual) -- Citation name conflict. FIXME: Move to different namespace
import Drasil.System (SmithEtAlSRS, mkSmithEtAlICO)

import Drasil.HGHC.HeatTransfer (fp, dataDefs, htInputs, htOutputs,
    nuclearPhys, symbols)
import Drasil.HGHC.MetaConcepts (progName)

import Data.Drasil.People (spencerSmith)
import Data.Drasil.Concepts.Thermodynamics as CT (heatTrans)

si :: SmithEtAlSRS
si = mkSmithEtAlICO
  progName [spencerSmith]
  [purp] [] [] []
  [] [] dataDefs []
  htInputs htOutputs ([] :: [ConstrConcept]) [] symbols
  [] symbMap []

mkSRS :: SRSDecl
mkSRS = [TableOfContents,
    RefSec $
    RefProg intro [TUnits, tsymb [TSPurpose, SymbConvention [Lit nuclearPhys, Manual fp]]],
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
  phrase progName]

purp :: Sentence
purp = foldlSent [S "describe", phrase CT.heatTrans, S "coefficients related to clad"]

ideaDicts :: [IdeaDict]
ideaDicts = [fp, nuclearPhys]

cis :: [CI]
cis = [progName]

symbMap :: ChunkDB
symbMap = withCommonKnowledge [] symbols ideaDicts cis [] [] dataDefs [] [] [] [] [] []
