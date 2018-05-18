module Drasil.SSP.Requirements (sspRequirements, sspInputDataTable) where

import Language.Drasil

import Drasil.SSP.Defs (slice, slope, slpSrf,
  crtSlpSrf, morPrice)
import Drasil.SSP.Unitals (fs, fs_min, sspInputs, coords)

import Data.Drasil.Utils (mkInputDatTb)
import Data.Drasil.SentenceStructures (acroR,
  ofThe, foldlSent)
import Data.Drasil.Concepts.Documentation (datum,
  input_, value, method_, element, requirement)
import Data.Drasil.Concepts.Computation (inDatum)

sspRequirements :: [Sentence]
sspRequirements = [readAndStore, generateCSS, testSlipSrf, prepareSlipS,
  calculateFS, rankSlope, generateCSS', repeatFindFS, prepareCSS, 
  calculateFS', displayGraph]

readAndStore, generateCSS, testSlipSrf, prepareSlipS,
  calculateFS, rankSlope, generateCSS', repeatFindFS,
  prepareCSS, calculateFS', displayGraph :: Sentence

readAndStore = foldlSent [S "Read the", phrase input_,
  S "file, and store the" +:+. plural datum, S "Necessary",
  plural inDatum, S "summarized in", makeRef sspInputDataTable]

generateCSS  = foldlSent [S "Generate potential", phrase crtSlpSrf :+:
  S "'s for the", phrase input_, phrase slope]

testSlipSrf  = foldlSent [S "Test the", plural slpSrf,
  S "to determine if they are physically realizable based on",
  S "a set of pass or fail criteria"]

prepareSlipS = foldlSent [S "Prepare the", plural slpSrf, S "for a", 
  phrase method_, S "of", plural slice, S "or limit equilibrium analysis"]

calculateFS  = S "Calculate" +:+. (plural fs `ofThe` plural slpSrf)

rankSlope    = foldlSent [S "Rank and weight the", plural slope, 
  S "based on their", phrase fs `sC` S "such that a", phrase slpSrf,
  S "with a smaller", phrase fs, S "has a larger weighting"]

generateCSS' = foldlSent [S "Generate new potential", plural crtSlpSrf,
  S "based on previously analysed", plural slpSrf, S "with low",
  plural fs]

repeatFindFS = foldlSent [S "Repeat", plural requirement, acroR 3, S "to",
  acroR 7, S "until the", phrase fs_min, S "remains" +:+. 
  S "approximately the same over a predetermined number of repetitions",
  S "Identify the", (phrase slpSrf), S "that generates the",
  phrase fs_min, S "as the", phrase crtSlpSrf]

prepareCSS   = foldlSent [S "Prepare the", phrase crtSlpSrf, S "for", 
  phrase method_, S "of", plural slice, S "or limit equilibrium analysis"]

calculateFS' = foldlSent [S "Calculate", (phrase fs `ofThe` phrase crtSlpSrf),
  S "using the", titleize morPrice, phrase method_]

displayGraph = foldlSent [S "Display the", phrase crtSlpSrf, S "and the",
  phrase slice, phrase element +:+. S "displacements graphically",
  S "Give", (plural value `ofThe` plural fs), S "calculated",
  S "by the", titleize morPrice, phrase method_]

------------------
sspInputDataTable :: Contents
sspInputDataTable = mkInputDatTb ([dqdWr coords] ++ map dqdWr sspInputs)
  --FIXME: this has to be seperate since coords is a different type