module Drasil.SWHS.Concepts where --all of this file is exported

import Language.Drasil
import Control.Lens ((^.))

import Data.Drasil.Concepts.Documentation (assumption, dataDefn, genDefn, 
  goalStmt, inModel, likelyChg, physSyst, requirement, srs, thModel, 
  typUnc, unlikelyChg)
import Data.Drasil.Concepts.Math (ode, parameter)
import Data.Drasil.Phrase (with)

---Acronyms---
acronyms :: [CI]
acronyms = [assumption, dataDefn, genDefn, goalStmt, inModel, likelyChg, ode, 
            progName, physSyst, requirement, srs, thModel, typUnc, unlikelyChg]

acronymsFull :: [CI]
acronymsFull = acronyms ++ [phsChgMtrl, rightSide]

phsChgMtrl, rightSide, progName :: CI

phsChgMtrl  = commonIdea "phsChgMtrl" (nounPhrase "phase change material"
  "phase change materials") "PCM" ["domain specific"]

rightSide   = commonIdea "rightSide"  (nounPhrase "right hand side"
  "right hand sides") "RHS" ["domain specific"]

progName    = commonIdea "swhsName"   (nounPhrase "solar water heating system"
  "solar water heating systems") "SWHS" ["domain specific"]

swhsFull :: NamedChunk
swhsFull    = nc "swhsFull" (progName `with` phsChgMtrl)
-- I want to include SI as an acronym, but I can't find a way for the
-- description to have accents when using dcc.

---ConceptChunks---

charging, coil, discharging, gauss_div,
  perfect_insul, phase_change_material, tank,
  tank_pcm, transient, water, sWHT, tank_para :: ConceptChunk

charging = dcc "charging" (nounPhraseSP "charging") "charging of the tank"

coil = dcc "coil" (cn' "heating coil")
  "Coil in tank that heats by absorbing solar energy"

discharging = dcc "discharging" (nounPhraseSP "discharging")
  "discharging of the tank"

transient = dcc "transient" (nounPhraseSP "transient") "Changing with time"

gauss_div = dcc "gauss_div" (nounPhraseSP "gauss's divergence theorem")
  ("A result that relates the flow of a vector field through a surface" ++
  "to the behavior of the vector field inside the surface")
--TODO: Physical property.

perfect_insul = dcc "perfect_insul" (nounPhraseSP "perfectly insulated")
  ("Describes the property of a material not allowing" ++
  "heat transfer through its boundaries")

phase_change_material = dcc "pcm" (phsChgMtrl ^. term)
  ("A substance that uses phase changes (such as melting) to absorb or " ++
  "release large amounts of heat at a constant temperature")
  
tank_para = dcc "tank_para" (compoundPhrase' (tank ^. term)
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

swhs_pcm :: CommonConcept
-- Nounphrase'' hack to get nounPhraseSP words to accept
-- nounPhrases instead of strings
swhs_pcm = dcc' "swhs_pcm" (nounPhrase''
  (plural progName +:+ S "incorporating" +:+ short phsChgMtrl)
  (plural progName +:+ S "incorporating" +:+ short phsChgMtrl)
  CapFirst CapWords)
  "Solar water heating systems incorporating phase change material"
  "SWHS"