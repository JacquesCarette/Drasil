module Drasil.SSP.GenDefs (normForcEq, bsShrFEq, resShr, mobShr,
  normShrR, momentEql, generalDefinitions,
  normForcEqGD, bsShrFEqGD, resShrGD, mobShrGD, normShrRGD, momentEqlGD,
  mobShearWOGD, resShearWOGD, srfWtrFGD) where

import Prelude hiding (sin, cos, tan)
import Language.Drasil
import Theory.Drasil (GenDefn, gd, ModelKinds (OthModel))
import Utils.Drasil
import qualified Utils.Drasil.Sentence as S

import Drasil.DocLang.SRS as SRS (physSyst)

import Data.Drasil.SI_Units (metre, newton)

import Data.Drasil.Concepts.Documentation (analysis, assumption, component,
  constant, definition, method_, value)
import Data.Drasil.Concepts.Math (area, equation, zDir)
import Data.Drasil.Concepts.PhysicalProperties (len)
import Data.Drasil.Concepts.Physics (twoD, weight)
import Data.Drasil.Concepts.SolidMechanics (normForce, shearForce)
import Data.Drasil.Quantities.PhysicalProperties (specWeight)
import Data.Drasil.Quantities.Physics (displacement, force, height,
  pressure, torque)
import Data.Drasil.Theories.Physics (weightGD, hsPressureGD, torqueDD)

import Drasil.SSP.Assumptions (assumpFOSL, assumpSLH, assumpSP, assumpSLI,
  assumpINSFL, assumpPSC, assumpSBSBISL, assumpWIBE, assumpWISE, assumpNESSS,
  assumpHFSM)
import Drasil.SSP.BasicExprs (eqlExpr, eqlExprN, momExpr)
import Drasil.SSP.DataDefs (intersliceWtrF, angleA, angleB, lengthB, lengthLb,
  lengthLs, slcHeight, normStressDD, tangStressDD, ratioVariation)
import Drasil.SSP.Defs (intrslce, slice, slope, slopeSrf, slpSrf, soil,
  soilPrpty, waterTable)
import Drasil.SSP.Figures (figForceActing)
import Drasil.SSP.References (chen2005, fredlund1977, karchewski2012)
import Drasil.SSP.TMods (factOfSafety, equilibrium, mcShrStrgth, effStress)
import Drasil.SSP.Unitals (baseAngle, baseHydroForce, baseLngth, baseWthX,
  dryWeight, earthqkLoadFctr, effCohesion, fricAngle, fs, genericA, genericM,
  genericSpWght, impLoadAngle, intNormForce, intShrForce, index, inxi, inxiM1,
  midpntHght, mobShrI, momntArm, normToShear, nrmFSubWat, rotForce, satWeight,
  scalFunc, shearFNoIntsl, shrResI, shrStress, totNrmForce, shearRNoIntsl,
  slcWght, sliceHght, sliceHghtW, slipHght, slopeHght, surfHydroForce,
  surfAngle, surfLngth, surfLoad, watrForce, waterHght, waterWeight, dryVol,
  satVol, yi, zcoord)

---------------------------
--  General Definitions  --
---------------------------
generalDefinitions :: [GenDefn]
generalDefinitions = [normForcEqGD, bsShrFEqGD, resShrGD, mobShrGD,
 effNormFGD, resShearWOGD, mobShearWOGD, normShrRGD, momentEqlGD, weightGD,
 sliceWghtGD, hsPressureGD, baseWtrFGD, srfWtrFGD]

normForcEqGD, bsShrFEqGD, resShrGD, mobShrGD, effNormFGD, resShearWOGD,
  mobShearWOGD, normShrRGD, momentEqlGD, sliceWghtGD, baseWtrFGD,
  srfWtrFGD :: GenDefn
normForcEqGD = gd (OthModel normForcEq) (getUnit totNrmForce)   (Just nmFEqDeriv)
  [makeCite chen2005]                      "normForcEq"  [nmFEqDesc]
bsShrFEqGD   = gd (OthModel bsShrFEq)   (getUnit mobShrI)       (Just bShFEqDeriv)
  [makeCite chen2005]                      "bsShrFEq"    [bShFEqDesc]
resShrGD     = gd (OthModel resShr)     (getUnit shrResI)       (Just resShrDeriv)
  [makeCite chen2005]                      "resShr"      [resShrDesc]
mobShrGD     = gd (OthModel mobShr)     (getUnit mobShrI)       (Just mobShrDeriv)
  [makeCite chen2005]                      "mobShr"      [mobShrDesc]
effNormFGD   = gd (OthModel effNormF)   (getUnit nrmFSubWat)    (Just effNormFDeriv)
  [makeCite chen2005]                      "effNormF"    [effNormFDesc]
resShearWOGD = gd (OthModel resShearWO) (getUnit shearRNoIntsl) Nothing
  (map makeCite[chen2005, karchewski2012]) "resShearWO"  [resShearWODesc]
mobShearWOGD = gd (OthModel mobShearWO) (getUnit shearFNoIntsl) Nothing
  (map makeCite[chen2005, karchewski2012]) "mobShearWO"  [mobShearWODesc]
normShrRGD   = gd (OthModel normShrR)   (getUnit intShrForce)   Nothing
  [makeCite chen2005]                      "normShrR"    [nmShrRDesc]
momentEqlGD  = gd (OthModel momentEql)  (Just newton)           (Just momEqlDeriv)
  [makeCite chen2005]                      "momentEql"   [momEqlDesc]
sliceWghtGD  = gd (OthModel sliceWght)  (getUnit slcWght)       (Just sliceWghtDeriv)
  [makeCite fredlund1977]                  "sliceWght"   [sliceWghtNotes]
baseWtrFGD   = gd (OthModel baseWtrF)   (getUnit baseHydroForce) (Just bsWtrFDeriv)
  [makeCite fredlund1977]                  "baseWtrF"    [bsWtrFNotes]
srfWtrFGD    = gd (OthModel srfWtrF)    (getUnit surfHydroForce) (Just srfWtrFDeriv)
  [makeCite fredlund1977]                  "srfWtrF"     [srfWtrFNotes]

--
normForcEq :: RelationConcept
normForcEq = makeRC "normForcEq" (nounPhraseSP "normal force equilibrium")
  nmFEqDesc nmFEqRel

nmFEqRel :: Relation
nmFEqRel = inxi totNrmForce $= eqlExprN cos sin
  (\x y -> x $- inxiM1 intShrForce `addRe` inxi intShrForce `addRe` y)

nmFEqDesc :: Sentence
nmFEqDesc = foldlSent [S "This equation satisfies", makeRef2S equilibrium +:+.
  S "in the normal direction", ch slcWght, S "is defined in",
  makeRef2S sliceWghtGD `sC` ch surfHydroForce,  S "is defined in",
  makeRef2S srfWtrFGD `sC` ch surfAngle, S "is defined in",
  makeRef2S angleB `sC` S "and", ch baseAngle, S "is defined in",
  makeRef2S angleA]

nmFEqDeriv :: Derivation
nmFEqDeriv = mkDerivNoHeader [foldlSent [atStart normForcEq `S.sIs`
  S "derived from the free body diagram" `S.sOf`
  (makeRef2S figForceActing `S.sIn` makeRef2S (SRS.physSyst [] []))]]

--
bsShrFEq :: RelationConcept
bsShrFEq = makeRC "bsShrFEq" (nounPhraseSP "base shear force equilibrium")
  bShFEqDesc bShFEqRel

bShFEqRel :: Relation
bShFEqRel = inxi mobShrI $= eqlExpr sin cos
  (\x y -> x $- inxiM1 intShrForce `addRe` inxi intShrForce `addRe` y)

bShFEqDesc :: Sentence
bShFEqDesc = foldlSent [S "This equation satisfies", makeRef2S equilibrium +:+.
  S "in the shear direction", ch slcWght, S "is defined in",
  makeRef2S sliceWghtGD `sC` ch surfHydroForce, S "is defined in",
  makeRef2S srfWtrFGD `sC` ch surfAngle, S "is defined in",
  makeRef2S angleB `sC` S "and", ch baseAngle, S "is defined in",
  makeRef2S angleA]

bShFEqDeriv :: Derivation
bShFEqDeriv = mkDerivNoHeader [foldlSent [atStart bsShrFEq `S.sIs`
  S "derived from the free body diagram" `S.sOf`
  (makeRef2S figForceActing `S.sIn` makeRef2S (SRS.physSyst [] []))]]
--
shrResEqn :: Expr
shrResEqn = inxi nrmFSubWat `mulRe` tan (inxi fricAngle) `addRe` (inxi effCohesion `mulRe`
  inxi baseLngth)

resShr :: RelationConcept
resShr = makeRC "resShr" (nounPhraseSP "resistive shear force")
  resShrDesc resShrRel -- genDef3Label

resShrRel :: Relation
resShrRel = inxi shrResI $= shrResEqn

resShrDesc :: Sentence
resShrDesc = foldlSent [ch baseLngth, S "is defined in", makeRef2S lengthLb]

resShrDeriv :: Derivation
resShrDeriv = mkDerivNoHeader [foldlSent [S "Derived by substituting",
  makeRef2S normStressDD, S "and", makeRef2S tangStressDD, S "into the Mohr-Coulomb", phrase shrStress `sC`
  makeRef2S mcShrStrgth `sC` S "and multiplying both sides of the",
  phrase equation, S "by",  phrase genericA `S.the_ofThe` phrase slice `S.sIn`
  S "the shear-" :+: ch zcoord +:+. S "plane", S "Since the", phrase slope,
  S "is assumed to extend infinitely in the", phrase zDir,
  sParen (makeRef2S assumpPSC) `sC` S "the resulting", plural force,
  S "are expressed per", phrase metre, S "in the" +:+. phrase zDir, S "The",
  getTandS fricAngle `S.andThe` getTandS effCohesion, S "are not indexed by",
  ch index, S "because they are assumed to be isotropic",
  sParen (makeRef2S assumpSLI) `S.andThe` phrase soil, S "is assumed to be",
  S "homogeneous, with", phrase constant, plural soilPrpty, S "throughout",
  sParen (makeRef2S assumpSLH `sC` makeRef2S assumpSP)]]

--
mobShr :: RelationConcept
mobShr = makeRC "mobShr"
  (nounPhraseSP "mobilized shear force") mobShrDesc mobShrRel -- genDef4Label

mobShrRel :: Relation
mobShrRel = inxi mobShrI $= inxi shrResI $/ sy fs $= shrResEqn $/ sy fs

mobShrDesc :: Sentence
mobShrDesc = foldlSent [ch baseLngth, S "is defined in", makeRef2S lengthLb]

mobShrDeriv :: Derivation
mobShrDeriv = mkDerivNoHeader [foldlSent_ [atStart mobShrI `S.sIs` S "derived by dividing",
  phrase definition `S.the_ofThe` ch shrResI, S "from" +:+. makeRef2S resShrGD,
  S "by", phrase definition `S.the_ofThe` phrase fs, S "from" +:+.
  makeRef2S factOfSafety, S "The", getTandS fs, S "is not indexed by", ch index,
  S "because it is assumed to be", phrase constant, S "for the entire",
  phrase slpSrf +:+. sParen (makeRef2S assumpFOSL)]]

--
effNormF :: RelationConcept
effNormF = makeRC "effNormF"
  (nounPhraseSP "effective normal force") effNormFDesc effNormFRel

effNormFRel :: Relation
effNormFRel = inxi nrmFSubWat $= inxi totNrmForce $- inxi baseHydroForce

effNormFDesc :: Sentence
effNormFDesc = ch baseHydroForce +:+ S "is defined in" +:+. makeRef2S baseWtrFGD

effNormFDeriv :: Derivation
effNormFDeriv = mkDerivNoHeader [foldlSent [
  S "Derived by substituting", makeRef2S normStressDD, S "into",
  makeRef2S effStress `S.sAnd` S "multiplying both sides of the", phrase equation,
  S "by", phrase genericA `S.the_ofThe` phrase slice, S "in the shear-" :+:
  ch zcoord +:+. S "plane", S "Since the", phrase slope,
  S "is assumed to extend infinitely in the", phrase zDir,
  sParen (makeRef2S assumpPSC) `sC` S "the resulting", plural force,
  S "are expressed per", phrase metre, S "in the", phrase zDir]]

--
normShrR :: RelationConcept
normShrR = makeRC "normShrR"
  (nounPhraseSP "interslice normal and shear force proportionality")
  nmShrRDesc nmShrRRel -- genDef5Label

nmShrRRel :: Relation
nmShrRRel = sy intShrForce $= sy normToShear `mulRe` sy scalFunc `mulRe` sy intNormForce

nmShrRDesc :: Sentence
nmShrRDesc = foldlSent [S "Mathematical representation of the primary",
  phrase assumption, S "for the Morgenstern-Price", phrase method_ +:+.
  sParen (makeRef2S assumpINSFL), ch scalFunc, S "is defined in",
  makeRef2S ratioVariation]

--
resShearWO :: RelationConcept
resShearWO = makeRC "resShearWO"
  (nounPhraseSP "resistive shear force, without interslice normal and shear forces") resShearWODesc resShearWORel

resShearWORel :: Relation
resShearWORel = inxi shearRNoIntsl $=
  ((inxi slcWght `addRe` (inxi surfHydroForce `mulRe` cos (inxi surfAngle))) `mulRe`
  cos (inxi baseAngle) `addRe` (neg (inxi watrForce) `addRe` inxiM1 watrForce `addRe`
  (inxi surfHydroForce `mulRe` sin (inxi surfAngle)) `mulRe` sin (inxi baseAngle) $-
  inxi baseHydroForce) `mulRe` tan (inxi fricAngle) `addRe` (inxi effCohesion `mulRe`
  inxi baseLngth))

resShearWODesc :: Sentence
resShearWODesc = foldlSent_ [ch slcWght, S "is defined in",
  makeRef2S sliceWghtGD `sC` ch surfHydroForce, S "is defined in",
  makeRef2S srfWtrFGD `sC` ch surfAngle, S "is defined in",
  makeRef2S angleB `sC` ch baseAngle, S "is defined in",
  makeRef2S angleA `sC` ch watrForce, S "is defined in",
  makeRef2S intersliceWtrF `sC` ch baseHydroForce, S "is defined in",
  makeRef2S baseWtrFGD `sC` S "and", ch baseLngth, S "is defined in" +:+.
  makeRef2S lengthLb]

--
--
mobShearWO :: RelationConcept
mobShearWO = makeRC "mobShearWO"
  (nounPhraseSP "mobilized shear force, without interslice normal and shear forces") mobShearWODesc mobShearWORel

mobShearWORel :: Relation
mobShearWORel = inxi shearFNoIntsl $= ((inxi slcWght `addRe` (inxi surfHydroForce `mulRe`
  cos (inxi surfAngle))) `mulRe` sin (inxi baseAngle) $- ((neg (inxi watrForce) `addRe`
  inxiM1 watrForce `addRe` (inxi surfHydroForce `mulRe` sin (inxi surfAngle))) `mulRe` cos (inxi baseAngle)))

mobShearWODesc :: Sentence
mobShearWODesc = foldlSent_ [ch slcWght, S "is defined in",
  makeRef2S sliceWghtGD `sC` ch surfHydroForce, S "is defined in",
  makeRef2S srfWtrFGD `sC` ch surfAngle, S "is defined in",
  makeRef2S angleB `sC` ch baseAngle, S "is defined in",
  makeRef2S angleA `sC` S "and", ch watrForce, S "is defined in" +:+.
  makeRef2S intersliceWtrF]

--

momentEql :: RelationConcept
momentEql = makeRC "momentEql" (nounPhraseSP "moment equilibrium")
  momEqlDesc momEqlRel -- genDef6Label

momEqlRel :: Relation
momEqlRel = dbl 0 $= momExpr (\ x y -> x `addRe`
  (half (inxi baseWthX) `mulRe` (inxi intShrForce `addRe` inxiM1 intShrForce)) `addRe` y)

momEqlDesc :: Sentence
momEqlDesc = foldlSent [S "This", phrase equation, S "satisfies",
  makeRef2S equilibrium, S "for the net" +:+. phrase genericM, ch baseWthX,
  S "is defined in", makeRef2S lengthB `sC` ch baseAngle, S "is defined in",
  makeRef2S angleA `sC` ch slcWght, S "is defined in",
  makeRef2S sliceWghtGD `sC` ch midpntHght, S "is defined in",
  makeRef2S slcHeight `sC` ch surfHydroForce, S "is defined in",
  makeRef2S srfWtrFGD `sC` S "and", ch surfAngle, S "is defined in",
  makeRef2S angleB]

momEqlDeriv :: Derivation
momEqlDeriv = mkDerivNoHeader (weave [momEqlDerivSentences, momEqlDerivEqns])

momEqlDerivSentences :: [Sentence]
momEqlDerivSentences = map foldlSentCol [momEqlDerivTorqueSentence,
  momEqlDerivMomentSentence, momEqlDerivNormaliSentence,
  momEqlDerivNormaliM1Sentence, momEqlDerivWateriSentence,
  momEqlDerivWateriM1Sentence, momEqlDerivSheariSentence,
  momEqlDerivSheariM1Sentence, momEqlDerivSeismicIntSentence,
  momEqlDerivSeismicSentence, momEqlDerivSeismicWSentence,
  momEqlDerivHydroSentence, momEqlDerivExtSentence, momEqlDerivFinalSentence]

momEqlDerivEqns :: [Sentence]
momEqlDerivEqns = map E [momEqlDerivTorqueEqn, momEqlDerivMomentEqn,
  momEqlDerivNormaliEqn, momEqlDerivNormaliM1Eqn, momEqlDerivWateriEqn,
  momEqlDerivWateriM1Eqn, momEqlDerivSheariEqn,
  momEqlDerivSheariM1Eqn, momEqlDerivSeismicIntEqn,
  momEqlDerivSeismicEqn, momEqlDerivSeismicWEqn,
  momEqlDerivHydroEqn, momEqlDerivExtEqn,
  momEqlDerivFinalEqn]

momEqlDerivTorqueSentence, momEqlDerivMomentSentence,
  momEqlDerivNormaliSentence, momEqlDerivNormaliM1Sentence,
  momEqlDerivWateriSentence, momEqlDerivWateriM1Sentence,
  momEqlDerivSheariSentence, momEqlDerivSheariM1Sentence,
  momEqlDerivSeismicIntSentence, momEqlDerivSeismicSentence,
  momEqlDerivSeismicWSentence, momEqlDerivHydroSentence,
  momEqlDerivExtSentence, momEqlDerivFinalSentence :: [Sentence]

momEqlDerivTorqueEqn, momEqlDerivMomentEqn,
  momEqlDerivNormaliEqn, momEqlDerivNormaliM1Eqn, momEqlDerivWateriEqn,
  momEqlDerivWateriM1Eqn, momEqlDerivSheariEqn,
  momEqlDerivSheariM1Eqn, momEqlDerivSeismicIntEqn,
  momEqlDerivSeismicEqn, momEqlDerivSeismicWEqn,
  momEqlDerivHydroEqn, momEqlDerivExtEqn,
  momEqlDerivFinalEqn :: Expr

momEqlDerivTorqueSentence = [atStart genericM, S "is equal to",
  phrase torque `sC` S "so the", phrase equation, S "from", makeRef2S torqueDD,
  S "will be used to calculate", plural genericM]

momEqlDerivMomentSentence = [S "Considering one dimension, with",
  plural genericM, S "in the clockwise direction as positive and",
  plural genericM, S "in the counterclockwise direction as negative" `sC`
  S "and replacing the", phrase torque, S "symbol with the", phrase genericM,
  S "symbol, the", phrase equation, S "simplifies to"]

momEqlDerivNormaliSentence = [S "where", ch rotForce `S.isThe`
  phrase rotForce `S.sAnd` ch momntArm `S.isThe` phrase momntArm `sC`
  S "or the distance between the", phrase force `S.andThe` S "axis about" +:+.
  S "which the rotation acts",
  S "To represent the", phrase momentEqlGD `sC` S "the", plural genericM,
  S "from each", phrase force, S "acting on a", phrase slice +:+.
  S "must be considered and added together", S "The", plural force,
  S "acting on a", phrase slice, S "are all shown in" +:+.
  makeRef2S figForceActing,
  S "The midpoint of the base of a", phrase slice, S "is considered as the",
  S "axis of rotation, from which the", phrase momntArm +:+. S "is measured",
  S "Considering first the", phrase intrslce, phrase normForce,
  S "acting on", phrase slice, S "interface", ch index `sC` S "the",
  phrase genericM, S "is negative because the", phrase force,
  S "tends to rotate the", phrase slice, S "in a counterclockwise",
  S "direction" `sC` S "and the", phrase momntArm `S.sIs` (S "height" `S.the_ofThe`
  phrase force), S "plus the difference in height between the base at",
  phrase slice, S "interface", ch index `S.andThe` S "base at the midpoint of",
  phrase slice +:+. ch index,
  S "Thus, the", phrase genericM, S "is expressed as"]

momEqlDerivNormaliM1Sentence = [S "For the", E (sy index $- int 1) :+: S "th",
  phrase slice, S "interface" `sC` S "the", phrase genericM `S.sIs`
  S "similar but in the opposite direction"]

momEqlDerivWateriSentence = [S "Next, the", phrase intrslce, S "normal water",
  phrase force +:+. S "is considered", S "This", phrase force, S "is zero at",
  S "height" `S.the_ofThe` phrase waterTable `sC` S "then increases linearly towards",
  S "base" `S.the_ofThe` phrase slice, S "due to the increasing water" +:+.
  phrase pressure, S "For such a triangular distribution, the resultant",
  phrase force +:+. S "acts at one-third of the height", S "Thus, for the",
  phrase intrslce, S "normal water", phrase force, S "acting on", phrase slice,
  S "interface", ch index `sC` S "the", phrase genericM, S "is"]

momEqlDerivWateriM1Sentence = [S "The", phrase genericM, S "for the",
  phrase intrslce, S "normal water", phrase force, S "acting on", phrase slice,
  S "interface", E (sy index $- int 1), S "is"]

momEqlDerivSheariSentence = [S "The", phrase intrslce, phrase shearForce,
  S "at", phrase slice, S "interface", ch index, S "tends to rotate in the",
  S "clockwise direction, and the", phrase momntArm, S "is the", phrase len,
  S "from the", phrase slice, S "edge to the", phrase slice, S "midpoint" `sC`
  S "equivalent to half of", S "width" `S.the_ofThe` phrase slice `sC` S "so the",
  phrase genericM, S "is"]

momEqlDerivSheariM1Sentence = [S "The", phrase intrslce, phrase shearForce,
  S "at", phrase slice, S "interface", E (sy index $- int 1), S "also tends to",
  S "rotate in the clockwise direction, and has the same", phrase momntArm `sC`
  S "so the", phrase genericM, S "is"]

-- FIXME: Once differentials are expressible in Expr (issue #1407), change "sy yi" to the differential dy. "ch yi" actually means y and should stay as-is.
momEqlDerivSeismicIntSentence = [S "Seismic", plural force, S "act over the",
  S "entire height of the" +:+. phrase slice, S "For each horizontal segment",
  S "of the", phrase slice `sC` S "the seismic", phrase force, S "is",
  E (sy earthqkLoadFctr `mulRe` inxi slcWght), S "where", E (inxi slcWght),
  S "can be expressed as", E (sy genericSpWght `mulRe` inxi baseWthX `mulRe` sy yi),
  S "using", makeRef2S weightGD, S "where", E (sy yi), S "is the height of" +:+.
  S "the segment under consideration", S "The corresponding", phrase momntArm `S.sIs`
  ch yi `sC` S "the height from the base of the", phrase slice +:+.
  S "to the segment under consideration", S "In reality, the", plural force,
  S "near the surface of the", phrase soil, S "mass are slightly different",
  S "due to the slope of the surface, but this difference is assumed to be",
  S "negligible" +:+. sParen (makeRef2S assumpNESSS), S "The resultant",
  phrase genericM, S "from the", plural force, S "on all of the segments",
  S "with an equivalent resultant", phrase momntArm, S "is determined by",
  S "taking the integral over the", phrase slice +:+. S "height", S "The",
  plural force, S "tend to rotate in the counterclockwise direction, so the",
  phrase genericM, S "is negative"]

momEqlDerivSeismicSentence = [S "Solving the definite integral yields"]

momEqlDerivSeismicWSentence = [S "Using", makeRef2S weightGD,
  S "again to express", E (sy genericSpWght `mulRe` inxi baseWthX `mulRe` inxi midpntHght),
  S "as", E (inxi slcWght) `sC` S "the", phrase genericM, S "is"]

momEqlDerivHydroSentence = [S "The surface hydrostatic", phrase force,
  S "acts into the midpoint of the surface of the", phrase slice +:+.
  sParen (makeRef2S assumpHFSM),
  S "Thus, the vertical", phrase component, S "of the", phrase force,
  S "acts directly towards the point of rotation, and has a",
  phrase genericM +:+. S "of zero", S "The horizontal", phrase component,
  S "of the", phrase force, S "tends to rotate in a clockwise direction" `S.sAnd`
  S "the", phrase momntArm, S "is the entire height of the" +:+. phrase slice,
  S "Thus, the", phrase genericM, S "is"]

momEqlDerivExtSentence = [S "The external", phrase force, S "again acts into",
  S "midpoint" `S.the_ofThe` phrase slice, S "surface, so the vertical",
  phrase component, S "does not contribute to the", phrase genericM `sC`
  S "and the", phrase momntArm, S "is again the entire height of the" +:+.
  phrase slice, S "The", phrase genericM, S "is"]

momEqlDerivFinalSentence = [S "The base hydrostatic", phrase force `S.sAnd`
  phrase slice, phrase weight, S "both act in the direction of the point of",
  S "rotation", sParen (makeRef2S assumpHFSM) `sC` S "therefore both have",
  plural genericM +:+. S "of zero", S "Thus, all of the", plural genericM +:+.
  S "have been determined", S "The", phrase momentEqlGD `S.sIs`
  S "then represented by the sum of all", plural genericM]

momEqlDerivTorqueEqn = sy torque $= cross (sy displacement) (sy force)

momEqlDerivMomentEqn = sy genericM $= sy rotForce `mulRe` sy momntArm

momEqlDerivNormaliEqn = neg (inxi intNormForce) `mulRe` (inxi sliceHght `addRe`
  (half (inxi baseWthX) `mulRe` tan (inxi baseAngle)))

momEqlDerivNormaliM1Eqn = inxiM1 intNormForce `mulRe` (inxiM1 sliceHght $-
  (half (inxi baseWthX) `mulRe` tan (inxi baseAngle)))

momEqlDerivWateriEqn = neg (inxi watrForce) `mulRe` (frac 1 3 `mulRe` inxi sliceHghtW `addRe`
  (half (inxi baseWthX) `mulRe` tan (inxi baseAngle)))

momEqlDerivWateriM1Eqn = inxiM1 watrForce `mulRe` (frac 1 3 `mulRe` inxiM1 sliceHghtW `addRe`
  (half (inxi baseWthX) `mulRe` tan (inxi baseAngle)))

momEqlDerivSheariEqn = inxi intShrForce `mulRe` half (inxi baseWthX)

momEqlDerivSheariM1Eqn = inxiM1 intShrForce `mulRe` half (inxi baseWthX)

momEqlDerivSeismicIntEqn = neg $ defint (eqSymb yi) (exactDbl 0) (inxi midpntHght)
  (sy earthqkLoadFctr `mulRe` sy genericSpWght `mulRe` inxi baseWthX `mulRe` sy yi)

momEqlDerivSeismicEqn = neg $ sy earthqkLoadFctr `mulRe` sy genericSpWght `mulRe`
  inxi baseWthX `mulRe` half (square (inxi midpntHght))

momEqlDerivSeismicWEqn = neg $ sy earthqkLoadFctr `mulRe` inxi slcWght `mulRe`
  half (inxi midpntHght)

momEqlDerivHydroEqn = inxi surfHydroForce `mulRe` sin (inxi surfAngle) `mulRe`
  inxi midpntHght

momEqlDerivExtEqn = inxi surfLoad `mulRe` sin (inxi impLoadAngle) `mulRe` inxi midpntHght

momEqlDerivFinalEqn = dbl 0 $= momExpr (\ x y -> x `addRe`
  (half (inxi baseWthX) `mulRe` (inxi intShrForce `addRe` inxiM1 intShrForce)) `addRe` y)

--

sliceWght :: RelationConcept
sliceWght = makeRC "sliceWght" (nounPhraseSP "slice weight") sliceWghtNotes
  sliceWghtEqn

sliceWghtEqn :: Expr
sliceWghtEqn = inxi slcWght $= inxi baseWthX `mulRe` oneHalf `mulRe` completeCase [case1, case2, case3]
  where case1 = (((inxi slopeHght $- inxi slipHght) `addRe`
          (inxiM1 slopeHght $- inxiM1 slipHght)) `mulRe` sy satWeight,
          (inxi waterHght $> inxi slopeHght) $||
          (inxiM1 waterHght $> inxiM1 slopeHght))
        case2 = (((inxi slopeHght $- inxi waterHght) `addRe`
          (inxiM1 slopeHght $- inxiM1 waterHght)) `mulRe` sy dryWeight `addRe`
          (((inxi waterHght $- inxi slipHght) `addRe`
          (inxiM1 waterHght $- inxiM1 slipHght)) `mulRe` sy satWeight),
          (inxi slopeHght $>= inxi waterHght $>= inxi slipHght) $&&
          (inxiM1 slopeHght $>= inxiM1 waterHght $>= inxiM1 slipHght))
        case3 = (((inxi slopeHght $- inxi slipHght) `addRe`
          (inxiM1 slopeHght $- inxiM1 slipHght)) `mulRe` sy dryWeight,
          (inxi waterHght $< inxi slipHght) $||
          (inxiM1 waterHght $< inxiM1 slipHght))

sliceWghtNotes :: Sentence
sliceWghtNotes = foldlSent [S "This", phrase equation, S "is based on the",
  phrase assumption, S "that the surface and the base of a", phrase slice,
  S "are straight lines" +:+. sParen (makeRef2S assumpSBSBISL), S "The",
  getTandS dryWeight `S.andThe` getTandS satWeight, S "are not indexed by",
  ch index, S "because the", phrase soil, S "is assumed to be homogeneous" `sC`
  S "with", phrase constant, plural soilPrpty, S "throughout" +:+.
  sParen (makeRef2S assumpSLH), ch baseWthX +:+ S "is defined in",
  makeRef2S lengthB]

sliceWghtDeriv :: Derivation
sliceWghtDeriv = mkDerivNoHeader (weave [sliceWghtDerivSentences, sliceWghtDerivEqns])

sliceWghtDerivEqns :: [Sentence]
sliceWghtDerivEqns = map E [sliceWghtDerivSatCaseWeightEqn,
  sliceWghtDerivSatCaseSliceEqn, sliceWghtDerivDryCaseWeightEqn,
  sliceWghtDerivDryCaseSliceEqn, sliceWghtDerivMixCaseWeightEqn,
  sliceWghtDerivMixCaseSliceEqn]

sliceWghtDerivSentences :: [Sentence]
sliceWghtDerivSentences = map foldlSentCol [sliceWghtDerivSatCaseIntroSentence,
  sliceWghtDerivSatCase2DSentence, sliceWghtDerivDryCaseIntroSentence,
  sliceWghtDerivDryCase2DSentence, sliceWghtDerivMixCaseIntroSentence,
  sliceWghtDerivMixCase2DSentence]

sliceWghtDerivSatCaseIntroSentence, sliceWghtDerivSatCase2DSentence,
  sliceWghtDerivDryCaseIntroSentence, sliceWghtDerivDryCase2DSentence,
  sliceWghtDerivMixCaseIntroSentence,
  sliceWghtDerivMixCase2DSentence :: [Sentence]

sliceWghtDerivSatCaseWeightEqn, sliceWghtDerivSatCaseSliceEqn,
  sliceWghtDerivDryCaseWeightEqn, sliceWghtDerivDryCaseSliceEqn,
  sliceWghtDerivMixCaseWeightEqn, sliceWghtDerivMixCaseSliceEqn :: Expr

sliceWghtDerivSatCaseIntroSentence = [S "For the case where the",
  phrase waterTable, S "is above the", phrase slopeSrf `sC` S "the",
  phrase slcWght, S "come from", phrase weight `S.the_ofThe` S "saturated" +:+.
  phrase soil,
  S "Substituting", plural value, S "for saturated", phrase soil, S "into the",
  phrase equation, S "for", phrase weight, S "from", makeRef2S weightGD,
  S "yields"]

sliceWghtDerivSatCase2DSentence = [S "Due to", makeRef2S assumpPSC `sC`
  S "only two dimensions are considered, so the", plural area `S.sOf`
  S "saturated", phrase soil, S "are considered instead of the" +:+.
  phrase satVol,
  S "Any given", phrase slice +:+. S "has a trapezoidal shape", S "The",
  phrase area, S "of a trapezoid is the average of",
  plural len `S.the_ofThe` S "parallel sides multiplied by the", phrase len +:+.
  S "between the parallel sides", S "The parallel sides in this case are the",
  phrase intrslce, S "edges and the", phrase len, S "between them" `S.isThe`
  S "width of the" +:+. phrase slice, S "Thus" `sC` S "the", phrase slcWght,
  S "are defined as"]

sliceWghtDerivSatCaseWeightEqn = inxi slcWght $= inxi satVol `mulRe` sy satWeight

sliceWghtDerivSatCaseSliceEqn = inxi slcWght $= inxi baseWthX `mulRe` oneHalf `mulRe`
  ((inxi slopeHght $- inxi slipHght) `addRe` (inxiM1 slopeHght $- inxiM1 slipHght)) `mulRe` sy satWeight

sliceWghtDerivDryCaseIntroSentence = [S "For the case where the",
  phrase waterTable, S "is below the", phrase slpSrf `sC` S "the",
  phrase slcWght, S "come from", phrase weight `S.the_ofThe` S "dry" +:+. phrase soil,
  S "Substituting", plural value, S "for dry", phrase soil, S "into the",
  phrase equation, S "for", phrase weight, S "from", makeRef2S weightGD,
  S "yields"]

sliceWghtDerivDryCase2DSentence = [makeRef2S assumpPSC, S "again allows for",
  phrase twoD, phrase analysis, S "so the", plural area `S.sOf` S "dry",
  phrase soil, S "are considered instead of the" +:+. phrase dryVol,
  S "The trapezoidal", phrase slice,
  S "shape is the same as in the previous case" `sC` S "so the", phrase slcWght,
  S "are defined as"]

sliceWghtDerivDryCaseWeightEqn = inxi slcWght $= inxi dryVol `mulRe` sy dryWeight

sliceWghtDerivDryCaseSliceEqn = inxi slcWght $= inxi baseWthX `mulRe` oneHalf `mulRe`
  ((inxi slopeHght $- inxi slipHght) `addRe` (inxiM1 slopeHght $- inxiM1 slipHght)) `mulRe` sy dryWeight

sliceWghtDerivMixCaseIntroSentence = [S "For the case where the",
  phrase waterTable, S "is between the", phrase slopeSrf `S.sAnd`
  phrase slpSrf `sC` S "the", phrase slcWght, S "are the sums of",
  plural weight `S.the_ofThe` S "dry portions"  `S.sAnd` plural weight `S.ofThe`
  S "saturated portions of the" +:+. phrase soil,
  S "Substituting", plural value, S "for dry and saturated", phrase soil,
  S "into the", phrase equation, S "for", phrase weight, S "from",
  makeRef2S weightGD, S "and adding them together yields"]

sliceWghtDerivMixCase2DSentence = [makeRef2S assumpPSC, S "again allows for",
  phrase twoD, phrase analysis, S "so the", plural area `S.sOf` S "dry",
  phrase soil `S.sAnd` plural area `S.sOf` S "saturated", phrase soil,
  S "are considered instead of the" +:+. (phrase dryVol `S.sAnd` phrase satVol),
  S "The", phrase waterTable, S "is assumed to only intersect a", phrase slice,
  S "surface or base at a", phrase slice, S "edge",
  sParen (makeRef2S assumpWISE `sC` makeRef2S assumpWIBE) `sC` S "so the" +:+.
  S "dry and saturated portions each have trapezoidal shape", S "For the dry",
  S "portion, the parallel sides of the trapezoid are the", plural len,
  S "between the", phrase slopeSrf `S.sAnd` phrase waterTable, S "at the",
  phrase slice +:+. S "edges", S "For the saturated portion, the parallel",
  S "sides of the trapezoid are the", plural len, S "between the",
  phrase waterTable `S.sAnd` phrase slpSrf, S "at the", phrase slice +:+.
  S "edges", S "Thus" `sC` S "the", phrase slcWght,  S "are defined as"]

sliceWghtDerivMixCaseWeightEqn = inxi slcWght $= inxi dryVol `mulRe` sy dryWeight `addRe`
  (inxi satVol `mulRe` sy satWeight)

sliceWghtDerivMixCaseSliceEqn = inxi slcWght $= (inxi baseWthX `mulRe` oneHalf `mulRe`
  (((inxi slopeHght $- inxi waterHght) `addRe`
  (inxiM1 slopeHght $- inxiM1 waterHght)) `mulRe` sy dryWeight `addRe`
  (((inxi waterHght $- inxi slipHght) `addRe`
  (inxiM1 waterHght $- inxiM1 slipHght)) `mulRe` sy satWeight)))

-- 

baseWtrF :: RelationConcept
baseWtrF = makeRC "baseWtrF" (nounPhraseSP "base hydrostatic force")
  bsWtrFNotes bsWtrFEqn

bsWtrFEqn :: Expr
bsWtrFEqn = inxi baseHydroForce $= inxi baseLngth `mulRe` sy waterWeight `mulRe` oneHalf `mulRe`
  completeCase [case1, case2]
  where case1 = ((inxi waterHght $- inxi slipHght) `addRe`
          (inxiM1 waterHght $- inxiM1 slipHght),
          (inxi waterHght $> inxi slipHght) $||
          (inxiM1 waterHght $> inxiM1 slipHght))
        case2 = (dbl 0, (inxi waterHght $<= inxi slipHght) $&&
          (inxiM1 waterHght $<= inxiM1 slipHght))

bsWtrFNotes :: Sentence
bsWtrFNotes = foldlSent [S "This", phrase equation, S "is based on the",
  phrase assumption, S "that the base of a", phrase slice,
  S "is a straight line" +:+. sParen (makeRef2S assumpSBSBISL), ch baseLngth,
  S "is defined in", makeRef2S lengthLb]

bsWtrFDeriv :: Derivation
bsWtrFDeriv = mkDerivNoHeader (weave [bsWtrFDerivSentences, bsWtrFDerivEqns] ++
  bsWtrFDerivEndSentence)

bsWtrFDerivEqns :: [Sentence]
bsWtrFDerivEqns = map E [bsWtrFDerivWeightEqn, bsWtrFDerivHeightEqn,
  bsWtrFDerivSliceEqn]

bsWtrFDerivSentences :: [Sentence]
bsWtrFDerivSentences = map foldlSentCol [bsWtrFDerivIntroSentence,
  bsWtrFDerivHeightSentence, bsWtrFDeriv2DSentence]

bsWtrFDerivIntroSentence, bsWtrFDerivHeightSentence, bsWtrFDeriv2DSentence,
  bsWtrFDerivEndSentence :: [Sentence]

bsWtrFDerivWeightEqn, bsWtrFDerivHeightEqn, bsWtrFDerivSliceEqn :: Expr

bsWtrFDerivIntroSentence = [S "The", phrase baseHydroForce, S "come from the",
  S "hydrostatic", phrase pressure, S "exerted by the water above the base of",
  S "each" +:+. phrase slice,
  S "The", phrase equation, S "for hydrostatic", phrase pressure, S "from",
  makeRef2S hsPressureGD, S "is"]

bsWtrFDerivHeightSentence = [S "The", phrase specWeight, S "in this case is",
  S "the" +:+. getTandS waterWeight,
  S "The", phrase height, S "in this case is the height from the", phrase slice,
  S "base to the" +:+. phrase waterTable,
  S "This", phrase height, S "is measured from", S "midpoint" `S.the_ofThe`
  phrase slice, S "because the resultant hydrostatic", phrase force,
  S "is assumed to act at the", phrase slice, S "midpoint" +:+.
  sParen (makeRef2S assumpHFSM),
  S "The", phrase height, S "at the midpoint is the average of the",
  phrase height, S "at", phrase slice, S "interface", ch index `S.andThe`
  phrase height, S "at", phrase slice, S "interface", E (sy index $- int 1)]

bsWtrFDeriv2DSentence = [S "Due to", makeRef2S assumpPSC `sC`
  S "only two dimensions are considered, so the", phrase baseHydroForce,
  S "are expressed as", plural force +:+. S "per meter", S "The",
  plural pressure, S "acting on the", plural slice, S "can thus be converted",
  S "to", phrase baseHydroForce, S "by multiplying by the corresponding",
  phrase len, S "of the", phrase slice, S "base", E (inxi baseLngth) `sC`
  S "assuming the", phrase waterTable, S "does not intersect a", phrase slice,
  S "base except at a", phrase slice, S "edge" +:+.
  sParen (makeRef2S assumpWIBE),
  S "Thus, in the case where", S "height" `S.the_ofThe` phrase waterTable,
  S "is above", S "height" `S.the_ofThe` phrase slpSrf `sC` S "the",
  phrase baseHydroForce, S "are defined as"]

bsWtrFDerivEndSentence = [foldlSent [S "This", phrase equation `S.sIs`
  S "the non-zero case of" +:+. makeRef2S baseWtrFGD,
  S "The zero case is when", S "height" `S.the_ofThe` phrase waterTable,
  S "is below", S "height" `S.the_ofThe` phrase slpSrf `sC` S "so there is no",
  S "hydrostatic", phrase force]]

bsWtrFDerivWeightEqn = sy pressure $= sy specWeight `mulRe` sy height

bsWtrFDerivHeightEqn = oneHalf `mulRe` ((inxi waterHght $- inxi slipHght) `addRe` (inxiM1 waterHght $- inxiM1 slipHght))

bsWtrFDerivSliceEqn = inxi baseHydroForce $= inxi baseLngth `mulRe` sy waterWeight `mulRe`
  bsWtrFDerivHeightEqn

--

srfWtrF :: RelationConcept
srfWtrF = makeRC "srfWtrF" (nounPhraseSP "surface hydrostatic force")
  srfWtrFNotes srfWtrFEqn

srfWtrFEqn :: Relation
srfWtrFEqn = inxi surfHydroForce $= inxi surfLngth `mulRe` sy waterWeight `mulRe` oneHalf `mulRe`
  completeCase [case1, case2]
  where case1 = ((inxi waterHght $- inxi slopeHght) `addRe`
          (inxiM1 waterHght $- inxiM1 slopeHght),
          (inxi waterHght $> inxi slopeHght) $||
          (inxiM1 waterHght $> inxiM1 slopeHght))
        case2 = (dbl 0, (inxi waterHght $<= inxi slopeHght) $&&
          (inxiM1 waterHght $<= inxiM1 slopeHght))

srfWtrFNotes :: Sentence
srfWtrFNotes = foldlSent [S "This", phrase equation, S "is based on the",
  phrase assumption, S "that the surface of a", phrase slice,
  S "is a straight line" +:+. sParen (makeRef2S assumpSBSBISL), ch surfLngth,
  S "is defined in", makeRef2S lengthLs]

srfWtrFDeriv :: Derivation
srfWtrFDeriv = mkDerivNoHeader (weave [srfWtrFDerivSentences, srfWtrFDerivEqns] ++
  srfWtrFDerivEndSentence)

srfWtrFDerivEqns :: [Sentence]
srfWtrFDerivEqns = map E [srfWtrFDerivWeightEqn, srfWtrFDerivHeightEqn,
  srfWtrFDerivSliceEqn]

srfWtrFDerivSentences :: [Sentence]
srfWtrFDerivSentences = map foldlSentCol [srfWtrFDerivIntroSentence,
  srfWtrFDerivHeightSentence, srfWtrFDeriv2DSentence]

srfWtrFDerivIntroSentence, srfWtrFDerivHeightSentence, srfWtrFDeriv2DSentence,
  srfWtrFDerivEndSentence :: [Sentence]

srfWtrFDerivWeightEqn, srfWtrFDerivHeightEqn, srfWtrFDerivSliceEqn :: Expr

srfWtrFDerivIntroSentence = [S "The", phrase surfHydroForce, S "come from the",
  S "hydrostatic", phrase pressure, S "exerted by the water above the surface",
  S "of each" +:+. phrase slice,
  S "The", phrase equation, S "for hydrostatic", phrase pressure, S "from",
  makeRef2S hsPressureGD, S "is"]

srfWtrFDerivHeightSentence = [S "The", phrase specWeight, S "in this case is",
  S "the" +:+. getTandS waterWeight,
  S "The", phrase height, S "in this case is the height from the", phrase slice,
  S "surface to the" +:+. phrase waterTable,
  S "This", phrase height, S "is measured from", S "midpoint" `S.the_ofThe`
  phrase slice, S "because the resultant hydrostatic", phrase force,
  S "is assumed to act at the", phrase slice, S "midpoint" +:+.
  sParen (makeRef2S assumpHFSM),
  S "The", phrase height, S "at the midpoint is the average of the",
  phrase height, S "at", phrase slice, S "interface", ch index `S.andThe`
  phrase height, S "at", phrase slice, S "interface", E (sy index $- int 1)]

srfWtrFDeriv2DSentence = [S "Due to", makeRef2S assumpPSC `sC`
  S "only two dimensions are considered, so the", phrase surfHydroForce,
  S "are expressed as", plural force +:+. S "per meter", S "The",
  plural pressure, S "acting on the", plural slice, S "can thus be converted",
  S "to", phrase surfHydroForce, S "by multiplying by the corresponding",
  phrase len, S "of the", phrase slice, S "surface", E (inxi surfLngth) `sC`
  S "assuming the", phrase waterTable, S "does not intersect a", phrase slice,
  S "surface except at a", phrase slice, S "edge" +:+.
  sParen (makeRef2S assumpWISE),
  S "Thus, in the case where", S "height" `S.the_ofThe` phrase waterTable,
  S "is above", S "height" `S.the_ofThe` phrase slopeSrf `sC` S "the",
  phrase surfHydroForce, S "are defined as"]

srfWtrFDerivEndSentence = [foldlSent [S "This" +:+ phrase equation `S.sIs`
  S "the non-zero case of" +:+. makeRef2S srfWtrFGD,
  S "The zero case is when", S "height" `S.the_ofThe` phrase waterTable,
  S "is below", S "height" `S.the_ofThe` phrase slopeSrf `sC` S "so there is no",
  S "hydrostatic", phrase force]]

srfWtrFDerivWeightEqn = sy pressure $= sy specWeight `mulRe` sy height

srfWtrFDerivHeightEqn = oneHalf `mulRe` ((inxi waterHght $- inxi slopeHght) `addRe` (inxiM1 waterHght $- inxiM1 slopeHght))

srfWtrFDerivSliceEqn = inxi surfHydroForce $= inxi surfLngth `mulRe` sy waterWeight `mulRe`
  srfWtrFDerivHeightEqn