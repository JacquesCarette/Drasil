module Drasil.SWHS.GenDefs where

import Prelude hiding (sin, cos, tan)

import Language.Drasil
import Data.Drasil.SentenceStructures (foldlSent)

---------------------------
--  General Definitions  --
---------------------------

swhsGenDefs :: [RelationConcept]
swhsGenDefs = [nwtnCooling, rocTempSimp]

--
nwtnCooling :: RelationConcept
nwtnCooling = makeRC "nwtnCooling" (nounPhraseSP "Newton's law of cooling") 
  nwtnCooling_desc nwtnCooling_rel

nwtnCooling_rel :: Relation
nwtnCooling_rel = Int 0 := Int 0

nwtnCooling_desc :: Sentence
nwtnCooling_desc = foldlSent [S "Newton's law of cooling describes convective", 
  S "cooling from a surface. The law is stated as: the rate of heat loss from a", 
  S "body is proportional to the difference in temperatures between the body",
  S "and its surroundings", fixmeS]

--
rocTempSimp :: RelationConcept
rocTempSimp = makeRC "rocTempSimp" (nounPhraseSP "Simplified rate of change of temperature") 
  rocTempSimp_desc rocTempSimp_rel

rocTempSimp_rel :: Relation
rocTempSimp_rel = Int 0 := Int 0

rocTempSimp_desc :: Sentence
rocTempSimp_desc = fixmeS

--

fixmeS :: Sentence
fixmeS = S "Add Description"