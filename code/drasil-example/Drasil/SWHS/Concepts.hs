module Drasil.SWHS.Concepts where --all of this file is exported

import Language.Drasil
import Control.Lens ((^.))

import Data.Drasil.Concepts.Documentation (assumption, goalStmt,
  likelyChg, physSyst, requirement, srs, typUnc, unlikelyChg)
import Data.Drasil.Concepts.Math (ode, parameter)
import Data.Drasil.Phrase (with)
import Data.Drasil.IdeaDicts (dataDefn, genDefn, inModel, materialEng, thModel)

con :: [ConceptChunk]
con = [charging, coil, discharging, gaussDiv,
  perfectInsul, phaseChangeMaterial, tank,
  tank_pcm, transient, water, sWHT, tankParam]

---Acronyms---
acronyms :: [CI]
acronyms = [assumption, dataDefn, genDefn, goalStmt, inModel, likelyChg, ode, 
            progName, physSyst, requirement, srs, thModel, typUnc, unlikelyChg]

acronymsFull :: [CI]
acronymsFull = acronyms ++ [phsChgMtrl, rightSide]

phsChgMtrl, rightSide, progName :: CI

phsChgMtrl  = commonIdeaWithDict "phsChgMtrl" (nounPhrase "phase change material"
  "phase change materials") "PCM" [materialEng]

rightSide   = commonIdeaWithDict "rightSide"  (nounPhrase "right hand side"
  "right hand sides") "RHS" [materialEng]

progName    = commonIdeaWithDict "swhsName"   (nounPhrase "solar water heating system"
  "solar water heating systems") "SWHS" [materialEng]

full :: NamedChunk
full    = nc "full" (progName `with` phsChgMtrl)
-- I want to include SI as an acronym, but I can't find a way for the
-- description to have accents when using dcc.

---ConceptChunks---

charging, coil, discharging, gaussDiv,
  perfectInsul, phaseChangeMaterial, tank,
  tank_pcm, transient, water, sWHT, tankParam :: ConceptChunk

charging = dcc "charging" (nounPhraseSP "charging") "charging of the tank"

coil = dcc "coil" (cn' "heating coil")
  "Coil in tank that heats by absorbing solar energy"

discharging = dcc "discharging" (nounPhraseSP "discharging")
  "discharging of the tank"

transient = dcc "transient" (nounPhraseSP "transient") "Changing with time"

gaussDiv = dcc "gaussDiv" (nounPhraseSP "gauss's divergence theorem")
  ("A result that relates the flow of a vector field through a surface" ++
  "to the behavior of the vector field inside the surface")
--TODO: Physical property.

perfectInsul = dcc "perfectInsul" (nounPhraseSP "perfectly insulated")
  ("Describes the property of a material not allowing" ++
  "heat transfer through its boundaries")

phaseChangeMaterial = dcc "pcm" (phsChgMtrl ^. term)
  ("A substance that uses phase changes (such as melting) to absorb or " ++
  "release large amounts of heat at a constant temperature")
  
tankParam = dcc "tankParam" (compoundPhrase' (tank ^. term)
  (parameter ^. term))
  "Values associated with the tank"

tank = dcc "tank" (cn' "tank") "Enclosure containing some kind of substance"
sWHT = dcc "sWHT" (cn' "solar water heating tank") "Solar water heating tank"
water = dcc "water" (cn' "water") "The liquid with which the tank is filled"

tank_pcm = dcc "tank_pcm" (nounPhrase''
  (phrase sWHT +:+ S "incorporating" +:+ short phsChgMtrl)
  (phrase sWHT +:+ S "incorporating" +:+ short phsChgMtrl)
  CapFirst CapWords)
  "Solar water heating tank incorporating phase change material"

swhsPCM :: CommonConcept
-- Nounphrase'' hack to get nounPhraseSP words to accept
-- nounPhrases instead of strings
-- Another capitalization hack.
swhsPCM = dcc' "swhsPCM" (nounPhrase''
  (S "solar water heating systems" +:+ S "incorporating" +:+ short phsChgMtrl)
  (S "solar water heating systems" +:+ S "incorporating" +:+ short phsChgMtrl)
  CapFirst CapWords)
  "Solar water heating systems incorporating phase change material"
  "SWHS"
