module Drasil.SWHS.Concepts where --all of this file is exported

import Control.Lens ((^.))

import Language.Drasil
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (assumption, goalStmt,
  likelyChg, physSyst, requirement, srs, typUnc, unlikelyChg)
import Data.Drasil.Concepts.Math (ode, parameter, rightSide)
import Data.Drasil.IdeaDicts (dataDefn, genDefn, inModel, materialEng, thModel)

con :: [ConceptChunk]
con = [charging, coil, discharging, gaussDiv,
  perfectInsul, phaseChangeMaterial, tank,
  tankPCM, transient, water, sWHT, tankParam]

---Acronyms---
acronyms :: [CI]
acronyms = [assumption, dataDefn, genDefn, goalStmt, inModel, likelyChg, ode,
  progName, physSyst, requirement, srs, thModel, typUnc, unlikelyChg]

acronymsFull :: [CI]
acronymsFull = acronyms ++ [phsChgMtrl, rightSide]

phsChgMtrl, progName :: CI

phsChgMtrl  = commonIdeaWithDict "phsChgMtrl" (nounPhrase "phase change material"
  "phase change materials") "PCM" [materialEng]

progName    = commonIdeaWithDict "swhsName"   (nounPhrase "solar water heating system"
  "solar water heating systems") "SWHS" [materialEng]

full :: NamedChunk
full = nc "full" (progName `with` phsChgMtrl)
-- I want to include SI as an acronym, but I can't find a way for the
-- description to have accents when using dcc.

---ConceptChunks---

charging, coil, discharging, gaussDiv,
  perfectInsul, phaseChangeMaterial, tank,
  tankPCM, transient, water, sWHT, tankParam :: ConceptChunk

charging = dcc "charging" (nounPhraseSP "charging") "charging of the tank"

coil = dcc "coil" (cn' "heating coil")
  "coil in tank that heats by absorbing solar energy"

discharging = dcc "discharging" (nounPhraseSP "discharging")
  "discharging of the tank"

transient = dcc "transient" (nounPhraseSP "transient") "changing with time"

gaussDiv = dcc "gaussDiv" (nounPhraseSP "gauss's divergence theorem")
  ("a result that relates the flow of a vector field through a surface" ++
  "to the behavior of the vector field inside the surface")
--TODO: Physical property.

perfectInsul = dcc "perfectInsul" (nounPhraseSP "perfectly insulated")
  ("describes the property of a material not allowing" ++
  "heat transfer through its boundaries")

phaseChangeMaterial = dcc "pcm" (phsChgMtrl ^. term)
  ("a substance that uses phase changes (such as melting) to absorb or " ++
  "release large amounts of heat at a constant temperature")
  
tankParam = dcc "tankParam" (compoundPhrase' (tank ^. term)
  (parameter ^. term))
  "values associated with the tank"

tank  = dcc "tank"  (cn' "tank") "enclosure containing some kind of substance"
sWHT  = dcc "sWHT"  (cn' "solar water heating tank") "solar water heating tank"
water = dcc "water" (cn' "water") "the liquid with which the tank is filled"

tankPCM = dcc "tankPCM" (nounPhrase''
  (phrase sWHT +:+ S "incorporating" +:+ short phsChgMtrl)
  (phrase sWHT +:+ S "incorporating" +:+ short phsChgMtrl)
  CapFirst CapWords)
  "solar water heating tank incorporating phase change material"

swhsPCM :: CommonConcept
-- Nounphrase'' hack to get nounPhraseSP words to accept
-- nounPhrases instead of strings
-- Another capitalization hack.
swhsPCM = dcc' "swhsPCM" (nounPhrase''
  (S "solar water heating systems" +:+ S "incorporating" +:+ short phsChgMtrl)
  (S "solar water heating systems" +:+ S "incorporating" +:+ short phsChgMtrl)
  CapFirst CapWords)
  "solar water heating systems incorporating phase change material"
  "SWHS"
