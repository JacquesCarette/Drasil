module Drasil.Projectile.Requirements (funcReqs, nonfuncReqs) where

import Language.Drasil
import Drasil.DocLang.SRS (datCon, propCorSol)
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Computation (inValue)
import Data.Drasil.Concepts.Documentation (assumption, code, datumConstraint,
  environment, funcReqDom, likelyChg, mg, mis, module_, nonFuncReqDom, output_,
  property, requirement, srs, traceyMatrix, unlikelyChg, value, vavPlan, propOfCorSol)
import Data.Drasil.Concepts.Math (calculation)
import Data.Drasil.Concepts.Software (errMsg)
import Data.Drasil.TheoryConcepts (dataDefn, genDefn, inModel, thModel)

import Drasil.Projectile.IMods (landPosIM, messageIM, offsetIM, timeIM)
import Drasil.Projectile.Unitals (flightDur, landPos, message, offset)

{--Functional Requirements--}

funcReqs :: [ConceptInstance]
funcReqs = [calcValues]

calcValues :: ConceptInstance
calcValues = cic "calcValues" calcValuesDesc "Calculate-Values" funcReqDom

calcValuesDesc :: Sentence
calcValuesDesc = foldlSent [S "Calculate the following" +: plural value,
  foldlList Comma List [
    ch flightDur +:+ fromSource timeIM,
    ch landPos   +:+ fromSource landPosIM,
    ch offset    +:+ fromSource offsetIM,
    ch message   +:+ fromSource messageIM
  ]]

{--Nonfunctional Requirements--}

nonfuncReqs :: [ConceptInstance]
nonfuncReqs = [correct, verifiable, understandable, reusable, maintainable, portable]

correct :: ConceptInstance
correct = cic "correct" (foldlSent [
  atStartNP' (output_ `the_ofThePS` code), S "have the",
  plural property, S "described in", namedRef (propCorSol [] []) (titleize' propOfCorSol)
  ]) "Correct" nonFuncReqDom
 
verifiable :: ConceptInstance
verifiable = cic "verifiable" (foldlSent [
  atStartNP (the code), S "is tested with complete",
  phrase vavPlan]) "Verifiable" nonFuncReqDom

understandable :: ConceptInstance
understandable = cic "understandable" (foldlSent [
  atStartNP (the code), S "is modularized with complete",
  phraseNP (mg `and_` mis)]) "Understandable" nonFuncReqDom

reusable :: ConceptInstance
reusable = cic "reusable" (foldlSent [atStartNP (the code), S "is modularized"]) "Reusable" nonFuncReqDom

maintainable :: ConceptInstance
maintainable = cic "maintainable" (foldlSent [
  S "The traceability between", foldlList Comma List [plural requirement,
  plural assumption, plural thModel, plural genDefn, plural dataDefn, plural inModel,
  plural likelyChg, plural unlikelyChg, plural module_], S "is completely recorded in",
  plural traceyMatrix `S.inThe` getAcc srs `S.and_` phrase mg]) "Maintainable" nonFuncReqDom

portable :: ConceptInstance
portable = cic "portable" (foldlSent [
  atStartNP (the code), S "is able to be run in different", plural environment])
  "Portable" nonFuncReqDom
