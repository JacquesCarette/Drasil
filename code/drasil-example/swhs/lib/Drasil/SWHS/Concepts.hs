module Drasil.SWHS.Concepts (
  coil, tank, phsChgMtrl, water, perfectInsul, charging, discharging,
  con, phaseChangeMaterial, sWHT, tankPCM, transient, gaussDiv
) where

import Control.Lens ((^.))

import Drasil.Database (mkUid)
import Language.Drasil
import qualified Language.Drasil.NaturalLanguage.English.NounPhrase.Combinators as NP

import Data.Drasil.Concepts.Math (parameter)
import Data.Drasil.Domains (materialEng)

con :: [ConceptChunk]
con = [charging, coil, discharging, gaussDiv,
  perfectInsul, phaseChangeMaterial, tank,
  tankPCM, transient, water, sWHT, tankParam]

phsChgMtrl:: CI
phsChgMtrl  = commonIdeaWithDict (mkUid "phsChgMtrl") (nounPhrase "phase change material"
  "phase change materials") "PCM" [materialEng]

---ConceptChunks---

charging, coil, discharging, gaussDiv,
  perfectInsul, phaseChangeMaterial, tank,
  tankPCM, transient, water, sWHT, tankParam :: ConceptChunk

charging = cncpt''' (mkUid "charging") (nounPhraseSP "charging") (S "charging of the tank")

coil = cncpt''' (mkUid "coil") (cn' "heating coil")
  (S "coil in tank that heats by absorbing solar energy")

discharging = cncpt''' (mkUid "discharging") (nounPhraseSP "discharging")
  (S "discharging of the tank")

transient = cncpt''' (mkUid "transient") (nounPhraseSP "transient") (S "changing with time")

gaussDiv = cncpt''' (mkUid "gaussDiv") (nounPhraseSP "gauss's divergence theorem")
  (S ("a result that relates the flow of a vector field through a surface" ++
  "to the behavior of the vector field inside the surface"))
--TODO: Physical property.

perfectInsul = cncpt''' (mkUid "perfectInsul") (nounPhraseSP "perfectly insulated")
  (S ("describes the property of a material not allowing" ++
  "heat transfer through its boundaries"))

phaseChangeMaterial = cncpt''' (mkUid "pcm") (phsChgMtrl ^. term)
  (S ("a substance that uses phase changes (such as melting) to absorb or " ++
  "release large amounts of heat at a constant temperature"))

tankParam = cncpt''' (mkUid "tankParam") (compoundPhrase' (tank ^. term)
  (parameter ^. term))
  (S "values associated with the tank")

tank  = cncpt''' (mkUid "tank")  (cn' "tank") (S "enclosure containing some kind of substance")
sWHT  = cncpt''' (mkUid "sWHT")  (cn' "solar water heating tank") (S "solar water heating tank")
water = cncpt''' (mkUid "water") (cn' "water") (S "the liquid with which the tank is filled")

-- TODO: extract 'PCM' from 'phsChgMtrl' again instead of hard-coding it
tankPCM = cncpt''' (mkUid "tankPCM") (nounPhrase''
  (phraseNP (sWHT ^. term) NP.:+: NP.S "incorporating PCM")
  (phraseNP (sWHT ^. term) NP.:+: NP.S "incorporating PCM")
  CapFirst CapWords)
  (S "solar water heating tank incorporating phase change material")
