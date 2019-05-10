module Drasil.SSP.GenDefs (normForcEq, bsShrFEq, resShr, mobShr,
  normShrR, momentEql, generalDefinitions,
  normForcEqGD, bsShrFEqGD, resShrGD, mobShrGD, normShrRGD, momentEqlGD,
  mobShearWOGD, resShearWOGD, srfWtrFGD) where

import Prelude hiding (sin, cos, tan)
import Language.Drasil

import Drasil.DocLang.SRS as SRS (physSyst)

import Data.Drasil.Utils (weave)

import Data.Drasil.SI_Units (metre, newton)

import Data.Drasil.Concepts.Documentation (assumption, constant, definition, 
  method_, value)
import Data.Drasil.Concepts.Math (area, equation)
import Data.Drasil.Concepts.PhysicalProperties (len)
import Data.Drasil.Concepts.Physics (distance, weight)
import Data.Drasil.Quantities.Physics (force)
import Data.Drasil.Theories.Physics (weightGD)

import Data.Drasil.SentenceStructures (foldlSent, foldlSent_, foldlSentCol, 
  getTandS, isThe, ofThe, sAnd, sIs, sOf, andThe)

import Drasil.SSP.Assumptions (assumpFOSL, assumpSLH, assumpSP, assumpSLI,
  assumpINSFL, assumpPSC, assumpSBSBISL, assumpWISE)
import Drasil.SSP.BasicExprs (eqlExpr, eqlExprN, momExpr)
import Drasil.SSP.DataDefs (sliceWght, baseWtrF, intersliceWtrF,
  angleA, angleB, lengthB, lengthLb, slcHeight, stressDD, 
  ratioVariation)
import Drasil.SSP.Defs (slice, slope, slopeSrf, slpSrf, soil, soilPrpty,
  waterTable)
import Drasil.SSP.Figures (fig_forceacting)
import Drasil.SSP.References (chen2005, fredlund1977, karchewski2012)
import Drasil.SSP.TMods (factOfSafety, equilibrium, mcShrStrgth, effStress)
import Drasil.SSP.Unitals (baseAngle, baseHydroForce, baseLngth, baseWthX, 
  effCohesion, fricAngle, fs, genericA, intNormForce, intShrForce, index, inxi,
  inxiM1, midpntHght, mobShrI, momntOfBdy, normToShear, nrmFSubWat, scalFunc,
  shearFNoIntsl, shrResI, shrResI, shrStress, totNrmForce, shearRNoIntsl, 
  shrResI, slcWght, slopeHght, surfHydroForce,
  surfAngle, watrForce, waterHght, waterWeight, waterVol, zcoord)

---------------------------
--  General Definitions  --
---------------------------
generalDefinitions :: [GenDefn]
generalDefinitions = [normForcEqGD, bsShrFEqGD, resShrGD, mobShrGD,
 effNormFGD, resShearWOGD, mobShearWOGD, normShrRGD, momentEqlGD, weightGD, srfWtrFGD]

normForcEqGD, bsShrFEqGD, resShrGD, mobShrGD, effNormFGD, resShearWOGD, mobShearWOGD, normShrRGD, momentEqlGD, srfWtrFGD :: GenDefn
normForcEqGD = gd' normForcEq (getUnit totNrmForce)   [nmFEq_deriv]    
  [chen2005]                   "normForcEq"  [nmFEq_desc]
bsShrFEqGD   = gd' bsShrFEq   (getUnit mobShrI)       [bShFEq_deriv]
  [chen2005]                   "bsShrFEq"    [bShFEq_desc]
resShrGD     = gd' resShr     (getUnit shrResI)       [resShr_deriv]   
  [chen2005]                   "resShr"      [resShr_desc]
mobShrGD     = gd' mobShr     (getUnit mobShrI)       [mobShr_deriv]   
  [chen2005]                   "mobShr"      [mobShr_desc]
effNormFGD   = gd' effNormF   (getUnit nrmFSubWat)    [effNormF_deriv] 
  [chen2005]                   "effNormF"    [effNormF_desc]
resShearWOGD = gd' resShearWO (getUnit shearRNoIntsl)  []         
  [chen2005, karchewski2012]   "resShearWO"  [resShearWO_desc]
mobShearWOGD = gd' mobShearWO (getUnit shearFNoIntsl)  []
  [chen2005, karchewski2012]   "mobShearWO"  [mobShearWO_desc]
normShrRGD   = gd' normShrR   (getUnit intShrForce)    [] 
  [chen2005]                   "normShrR"    [nmShrR_desc]
momentEqlGD  = gd' momentEql  (Just newton)           [momEql_deriv]  
  [chen2005]                   "momentEql"   [momEql_desc]
srfWtrFGD    = gd' srfWtrF    (getUnit surfHydroForce) srfWtrFDeriv   
  [fredlund1977]               "srfWtrF"    [srfWtrFNotes]

--
normForcEq :: RelationConcept
normForcEq = makeRC "normForcEq" (nounPhraseSP "normal force equilibrium")
  nmFEq_desc nmFEq_rel

nmFEq_rel :: Relation
nmFEq_rel = inxi totNrmForce $= eqlExprN cos sin
  (\x y -> x - inxiM1 intShrForce + inxi intShrForce + y)

nmFEq_desc :: Sentence
nmFEq_desc = foldlSent [S "This equation satisfies", makeRef2S equilibrium +:+.
  S "in the normal direction", ch slcWght, S "is defined in", 
  makeRef2S sliceWght `sC` ch surfHydroForce,  S "is defined in", 
  makeRef2S srfWtrFGD `sC` ch surfAngle, S "is defined in",
  makeRef2S angleB `sC` S "and", ch baseAngle, S "is defined in", 
  makeRef2S angleA]

nmFEq_deriv :: Sentence
nmFEq_deriv = foldlSent [at_start normForcEq, S "is derived from the free",
  S "body diagram of", makeRef2S fig_forceacting, S "in", 
  (makeRef2S $ SRS.physSyst ([]::[Contents]) ([]::[Section]))]

--
bsShrFEq :: RelationConcept
bsShrFEq = makeRC "bsShrFEq" (nounPhraseSP "base shear force equilibrium")
  bShFEq_desc bShFEq_rel

bShFEq_rel :: Relation
bShFEq_rel = inxi mobShrI $= eqlExpr sin cos
  (\x y -> x - inxiM1 intShrForce + inxi intShrForce + y)

bShFEq_desc :: Sentence
bShFEq_desc = foldlSent [S "This equation satisfies", makeRef2S equilibrium +:+.
  S "in the shear direction", ch slcWght, S "is defined in", 
  makeRef2S sliceWght `sC` ch surfHydroForce, S "is defined in", 
  makeRef2S srfWtrFGD `sC` ch surfAngle, S "is defined in",
  makeRef2S angleB `sC` S "and", ch baseAngle, S "is defined in", 
  makeRef2S angleA]

bShFEq_deriv :: Sentence
bShFEq_deriv = foldlSent [at_start bsShrFEq, S "is derived from the free",
  S "body diagram of", makeRef2S fig_forceacting, S "in", 
  (makeRef2S $ SRS.physSyst ([]::[Contents]) ([]::[Section]))]

--
shrResEqn :: Expr
shrResEqn = inxi nrmFSubWat * tan (inxi fricAngle) + inxi effCohesion *
  inxi baseLngth

resShr :: RelationConcept
resShr = makeRC "resShr" (nounPhraseSP "resistive shear force")
  resShr_desc resShr_rel -- genDef3Label

resShr_rel :: Relation
resShr_rel = inxi shrResI $= shrResEqn

resShr_desc :: Sentence
resShr_desc = foldlSent_ [ch baseLngth, S "is defined in" +:+. 
  makeRef2S lengthLb]

resShr_deriv :: Sentence
resShr_deriv = foldlSent_ [S "Derived by substituting", makeRef2S stressDD,
  S "into the Mohr-Coulomb", phrase shrStress `sC` makeRef2S mcShrStrgth `sC`
  S "and multiplying both sides of the", phrase equation, S "by", 
  phrase genericA `ofThe` phrase slice, S "in the shear-" :+: ch zcoord  +:+. 
  S "plane", S "Since the", phrase slope, S "is assumed to extend infinitely",
  S "in the", ch zcoord :+: S "-direction", sParen (makeRef2S assumpPSC) `sC` 
  S "the resulting", plural force, S "are expressed per", phrase metre,
  S "in the", ch zcoord :+: S "-direction.", S "The", 
  getTandS fricAngle `andThe` getTandS effCohesion, S "are not indexed by",
  ch index, S "becaused they are assumed to be isotropic", 
  sParen (makeRef2S assumpSLI) `andThe` phrase soil, S "is assumed to be",
  S "homogeneous, with", phrase constant, plural soilPrpty, S "throughout" +:+.
  sParen (makeRef2S assumpSLH `sC` makeRef2S assumpSP)]

--
mobShr :: RelationConcept
mobShr = makeRC "mobShr"
  (nounPhraseSP "mobilized shear force") mobShr_desc mobShr_rel -- genDef4Label

mobShr_rel :: Relation
mobShr_rel = inxi mobShrI $= inxi shrResI / sy fs $= shrResEqn / sy fs

mobShr_desc :: Sentence
mobShr_desc = foldlSent_ [ch baseLngth, S "is defined in" +:+. 
  makeRef2S lengthLb]

mobShr_deriv :: Sentence
mobShr_deriv = foldlSent_ [at_start mobShrI, S "is derived by dividing",
  phrase definition `ofThe` ch shrResI, S "from" +:+. makeRef2S resShrGD,
  S "by", phrase definition `ofThe` phrase fs, S "from" +:+.
  makeRef2S factOfSafety, S "The", getTandS fs, S "is not indexed by", ch index,
  S "because it is assumed to be", phrase constant, S "for the entire",
  phrase slpSrf +:+. sParen (makeRef2S assumpFOSL)]

--
effNormF :: RelationConcept
effNormF = makeRC "effNormF"
  (nounPhraseSP "effective normal force") effNormF_desc effNormF_rel

effNormF_rel :: Relation
effNormF_rel = inxi nrmFSubWat $= inxi totNrmForce - inxi baseHydroForce

effNormF_desc :: Sentence
effNormF_desc = ch baseHydroForce +:+ S "is defined in" +:+. makeRef2S baseWtrF

effNormF_deriv :: Sentence
effNormF_deriv = foldlSent [
  S "Derived by substituting", makeRef2S stressDD, S "into", 
  makeRef2S effStress `sAnd` S "multiplying both sides of the", phrase equation,
  S "by the", phrase genericA `ofThe` phrase slice, S "in the shear-" :+: 
  ch zcoord +:+. S "plane", S "Since the", phrase slope, 
  S "is assumed to extend infinitely in the", ch zcoord :+: S "-direction", 
  sParen (makeRef2S assumpPSC) `sC` S "the resulting", plural force, S "are expressed per", phrase metre, S "in the", ch zcoord :+: S "-direction" ]

--
normShrR :: RelationConcept
normShrR = makeRC "normShrR"
  (nounPhraseSP "interslice normal and shear force proportionality") 
  nmShrR_desc nmShrR_rel -- genDef5Label

nmShrR_rel :: Relation
nmShrR_rel = sy intShrForce $= sy normToShear * sy scalFunc * sy intNormForce

nmShrR_desc :: Sentence
nmShrR_desc = foldlSent [S "Mathematical representation of the primary", 
  phrase assumption, S "for the Morgenstern-Price", phrase method_ +:+.
  sParen (makeRef2S assumpINSFL), ch scalFunc, S "is defined in", 
  makeRef2S ratioVariation]

--
resShearWO :: RelationConcept
resShearWO = makeRC "resShearWO"
  (nounPhraseSP "resistive shear force, without interslice normal and shear forces") resShearWO_desc resShearWO_rel

resShearWO_rel :: Relation
resShearWO_rel = inxi shearRNoIntsl $= 
  (((inxi slcWght) + (inxi surfHydroForce) * (cos (inxi surfAngle))) *
  (cos (inxi baseAngle)) + (negate (inxi watrForce) + (inxiM1 watrForce) + 
  (inxi surfHydroForce) * sin (inxi surfAngle)) * (sin (inxi baseAngle)) -
  (inxi baseHydroForce)) * tan (inxi fricAngle) + (inxi effCohesion) *
  (inxi baseLngth)

resShearWO_desc :: Sentence
resShearWO_desc = foldlSent_ [ch slcWght, S "is defined in", 
  makeRef2S sliceWght `sC` ch surfHydroForce, S "is defined in", 
  makeRef2S srfWtrFGD `sC` ch surfAngle, S "is defined in", 
  makeRef2S angleB `sC` ch baseAngle, S "is defined in",
  makeRef2S angleA `sC` ch watrForce, S "is defined in",
  makeRef2S intersliceWtrF `sC` ch baseHydroForce, S "is defined in",
  makeRef2S baseWtrF `sC` S "and", ch baseLngth, S "is defined in" +:+. 
  makeRef2S lengthLb]

--
--
mobShearWO :: RelationConcept
mobShearWO = makeRC "mobShearWO"
  (nounPhraseSP "mobilized shear force, without interslice normal and shear forces") mobShearWO_desc mobShearWO_rel

mobShearWO_rel :: Relation
mobShearWO_rel = inxi shearFNoIntsl $= ((inxi slcWght) + (inxi surfHydroForce) *
  (cos (inxi surfAngle))) * (sin (inxi baseAngle)) - (negate (inxi watrForce) + 
  (inxiM1 watrForce) + (inxi surfHydroForce) * sin (inxi surfAngle)) * (cos (inxi baseAngle))

mobShearWO_desc :: Sentence
mobShearWO_desc = foldlSent_ [ch slcWght, S "is defined in", 
  makeRef2S sliceWght `sC` ch surfHydroForce, S "is defined in", 
  makeRef2S srfWtrFGD `sC` ch surfAngle, S "is defined in", 
  makeRef2S angleB `sC` ch baseAngle, S "is defined in",
  makeRef2S angleA `sC` S "and", ch watrForce, S "is defined in" +:+.
  makeRef2S intersliceWtrF]

--

momentEql :: RelationConcept
momentEql = makeRC "momentEql" (nounPhraseSP "moment equilibrium")
  momEql_desc momEql_rel -- genDef6Label

momEql_rel :: Relation
momEql_rel = 0 $= momExpr (\ x y -> x -
  (inxi baseWthX / 2 * (inxi intShrForce + inxiM1 intShrForce)) + y)

momEql_desc :: Sentence
momEql_desc = foldlSent [S "This", phrase equation, S "satisfies", 
  makeRef2S equilibrium, S "for the" +:+. phrase momntOfBdy, ch baseWthX,
  S "is defined in", makeRef2S lengthB `sC` ch baseAngle, S "is defined in",
  makeRef2S angleA `sC` ch slcWght, S "is defined in", makeRef2S sliceWght `sC`
  ch midpntHght, S "is defined in", makeRef2S slcHeight `sC` ch surfHydroForce,
  S "is defined in", makeRef2S srfWtrFGD `sC` S "and", ch surfAngle, 
  S "is defined in", makeRef2S angleB]

momEql_deriv :: Sentence
momEql_deriv = foldlSent_ [at_start momentEql, S "is derived from the free",
  S "body diagram of" +:+. 
  (makeRef2S $ SRS.physSyst ([]::[Contents]) ([]::[Section]))]

--

srfWtrF :: RelationConcept
srfWtrF = makeRC "srfWtrF" (nounPhraseSP "surface hydrostatic force") 
  srfWtrFNotes srfWtrFEqn

srfWtrFEqn :: Relation
srfWtrFEqn = inxi surfHydroForce $= inxi baseWthX * sy waterWeight * 0.5 * 
  case_ [case1, case2]
  where case1 = ((inxi waterHght - inxi slopeHght) + (inxiM1 waterHght - inxiM1 slopeHght), inxi waterHght $>= inxi slopeHght)
        case2 = (0, inxi waterHght $< inxi slopeHght)

srfWtrFNotes :: Sentence
srfWtrFNotes = foldlSent [S "This", phrase equation, S "is based on the",
  phrase assumption, S "that the surface of a slice is a straight line" +:+.
  sParen (makeRef2S assumpSBSBISL), ch baseWthX, S "is defined in",
  makeRef2S lengthB]

srfWtrFDeriv :: Derivation
srfWtrFDeriv = weave [srfWtrFDerivSentences, srfWtrFDerivEqns] ++ 
  srfWtrFDerivEndSentence

srfWtrFDerivEqns :: [Sentence]
srfWtrFDerivEqns = map E [srfWtrFDerivWeightEqn, srfWtrFDerivSliceEqn]

srfWtrFDerivSentences :: [Sentence]
srfWtrFDerivSentences = map foldlSentCol [srfWtrFDerivIntroSentence, srfWtrFDeriv2DSentence]

srfWtrFDerivIntroSentence, srfWtrFDeriv2DSentence, srfWtrFDerivEndSentence :: [Sentence]

srfWtrFDerivWeightEqn, srfWtrFDerivSliceEqn :: Expr

srfWtrFDerivIntroSentence = [S "The", phrase surfHydroForce, S "come from", 
  phrase weight `ofThe` S "water standing on top of" +:+. (phrase soil `ofThe` 
  phrase slopeSrf), S "Substituting", plural value, S "for water into the",
  phrase equation, S "for", phrase weight, S "from", makeRef2S weightGD, 
  S "yields"] 

srfWtrFDeriv2DSentence = [S "Due to", makeRef2S assumpPSC `sC` 
  S "only 2 dimensions are considered, so the", plural area `sOf` 
  S "water are considered instead of the" +:+. phrase waterVol, S "For a given",
  phrase slice `sC` S "the case where", S "height" `ofThe` phrase waterTable, 
  S "is below", S "height" `ofThe` phrase slopeSrf, S "at one edge and above", 
  S "height" `ofThe` phrase slopeSrf, S "at the other edge is", 
  S "assumed not to occur" +:+. sParen (makeRef2S assumpWISE), 
  S "In the case where", S "height" `ofThe` phrase waterTable,
  S "is below" +:+. ((S "height" `ofThe` phrase slopeSrf) `sC`
  (phrase area `ofThe` S "water is zero")), S "In the case where", 
  S "height" `ofThe` phrase waterTable, S "is above", S "height" `ofThe` 
  phrase slopeSrf `sC` S "the water forms a trapezoid on top of the" +:+. 
  phrase slopeSrf, S "The", phrase area, S "of a trapezoid is the average of",
  plural len `ofThe` S "parallel sides multiplied by the", phrase len +:+.
  S "between the parallel sides", S "The parallel sides in this case are the",
  plural distance, S "between the", phrase waterTable `andThe` phrase slopeSrf,
  S "for the edges of the", phrase slice `sC` S "and the", phrase len, 
  S "between them" `isThe` S "width of the" +:+. phrase slice, S "Thus" `sC`
  S "the", phrase surfHydroForce, S "are defined as"]

srfWtrFDerivEndSentence = [S "This" +:+ phrase equation `sIs`
    S "a rearrangement of the non-zero case of" +:+. makeRef2S srfWtrFGD]

srfWtrFDerivWeightEqn = inxi surfHydroForce $= inxi waterVol * sy waterWeight

srfWtrFDerivSliceEqn = inxi surfHydroForce $= inxi baseWthX * 0.5 *
  (((inxi waterHght) - (inxi slopeHght)) + ((inxiM1 waterHght) - (inxiM1 slopeHght))) * sy waterWeight