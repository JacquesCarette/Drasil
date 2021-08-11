module Drasil.GlassBR.TMods (tMods, pbIsSafe, lrIsSafe) where

import Language.Drasil
import Theory.Drasil (TheoryModel, tm, equationalModel')

import Drasil.GlassBR.References (astm2009)
import Drasil.GlassBR.Unitals (isSafeLoad, isSafeProb, pbTolfail, probFail,
  tmDemand, tmLRe)

{--}

tMods :: [TheoryModel]
tMods = [pbIsSafe, lrIsSafe]

-- FIXME: This is a hack to see if TheoryModel printing will work. This chunk
-- needs to be updated properly.
-- this is the new function but it still uses the lrIsSafeRC,
-- so basically we have to combine the old function with the new function
-- glass_concept :: [ConceptInstance]
-- glass_concept = []


lrIsSafe :: TheoryModel
lrIsSafe = tm (equationalModel' lrIsSafeQD)
   [qw isSafeLoad, qw tmLRe, qw tmDemand] ([] :: [ConceptChunk])
   [lrIsSafeQD] [] [] [dRef astm2009] 
   "isSafeLoad" [lrIsSafeDesc]

lrIsSafeQD :: QDefinition ModelExpr
lrIsSafeQD = mkQuantDef' isSafeLoad (nounPhraseSP "Safety Load") $ express lrIsSafeExpr

lrIsSafeExpr :: Expr
lrIsSafeExpr = sy tmLRe $> sy tmDemand

lrIsSafeDesc :: Sentence
lrIsSafeDesc = tModDesc isSafeLoad

pbIsSafe :: TheoryModel
pbIsSafe = tm (equationalModel' pbIsSafeQD) 
  [qw isSafeProb, qw probFail, qw pbTolfail] ([] :: [ConceptChunk])
  [pbIsSafeQD] [] [] [dRef astm2009]
  "isSafeProb" [pbIsSafeDesc]

pbIsSafeQD :: QDefinition ModelExpr
pbIsSafeQD = mkQuantDef' isSafeProb (nounPhraseSP "Safety Probability") $ express pbIsSafeExpr

pbIsSafeExpr :: Expr
pbIsSafeExpr = sy probFail $< sy pbTolfail

pbIsSafeDesc :: Sentence
pbIsSafeDesc = tModDesc isSafeProb

tModDesc :: QuantityDict -> Sentence
tModDesc main = S "If" +:+. (ch main `sC` S "the structure is considered safe")
