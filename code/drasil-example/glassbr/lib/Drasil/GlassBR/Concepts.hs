module Drasil.GlassBR.Concepts (
  -- * IdeaDicts
  beam, cantilever, edge, glaSlab, glass, plane, responseTy, blastRisk, glaPlane,
  idglass, ptOfExplsn, con', iGlass, lGlass,

  -- * ConceptChunks
  annealedGl, aspectRatioCon, fTemperedGl, glTyFac, hStrengthGl, loadDurFac,
  loadResis, loadShareFac, nonFactoredL, stdOffDist, blast, blastResisGla,
  blastTy, bomb, capacity, demandq, eqTNTChar, explosion, glassGeo, glassTy,
  glassWL, glBreakage, lateral, lite, load, longDurLoad, modE, notSafe, probBreak,
  safeMessage, shortDurLoad, specA, specDeLoad,

  -- * Tools
  glassType, glassTypeAbbrs
) where

import Drasil.Database (mkUid)
import Language.Drasil
import Language.Drasil.Document
import Language.Drasil.Chunk.Concept.NamedCombinators (compoundNC)

import Data.Drasil.Concepts.Documentation (response, type_)
import Drasil.GlassBR.References (astm2009, astm2012, astm2016)

con' :: [IdeaDict]
con' = [idglass, iGlass, lGlass, beam, blastRisk, cantilever, edge, glaPlane, glaSlab, plane,
  glass, ptOfExplsn, responseTy]

{-Terminology-}

idglass, iGlass, lGlass, beam, blastRisk, cantilever, edge, glaPlane, glaSlab,
  plane, glass, ptOfExplsn, responseTy :: IdeaDict
idglass    = idea' (mkUid "glass")      (cn' "Glass")
iGlass     = idea  (mkUid "iGlass")     (nounPhraseSP "insulating glass") "IG"
lGlass     = idea  (mkUid "lGlass")     (nounPhraseSP "laminated glass") "LG"
beam       = idea' (mkUid "beam")       (cn' "beam")
blastRisk  = idea' (mkUid "blastRisk")  (nounPhraseSP "blast risk")
cantilever = idea' (mkUid "cantilever") (nounPhraseSP "cantilever")
edge       = idea' (mkUid "edge")       (cn'          "edge")
glass      = idea' (mkUid "glass")      (nounPhraseSP "glass")
glaSlab    = idea' (mkUid "glaSlab")    (cn' "glass slab")
plane      = idea' (mkUid "plane")      (cn' "plane")
ptOfExplsn = idea' (mkUid "ptOfExplsn") (cn' "point of explosion")
glaPlane   = compoundNC glass plane
responseTy = compoundNC response type_

annealedGl, aspectRatioCon, fTemperedGl, glTyFac, hStrengthGl, loadDurFac, loadResis, loadShareFac,
  nonFactoredL, stdOffDist, blast, blastResisGla, blastTy, bomb, capacity, demandq, eqTNTChar, explosion,
  glassGeo, glassTy, glassWL, glBreakage, lateral, lite, load, longDurLoad, modE, notSafe, probBreak,
  safeMessage, shortDurLoad, specA, specDeLoad :: ConceptChunk

annealedGl  = cncpt'' (mkUid "annealed")
  (nounPhraseSP "annealed")
  (S "a flat, monolithic, glass lite which has uniform thickness where" +:+
  S "the residual surface stresses are almost zero, as defined in"+:+ refS astm2016)
  "AN"

aspectRatioCon = cncpt'' (mkUid "aR")
  (nounPhraseSP "aspect ratio")
  (S $ "the ratio of the long dimension of the glass to the short dimension of " ++
    "the glass. For glass supported on four sides, the aspect ratio is " ++
    "always equal to or greater than 1.0. For glass supported on three " ++
    "sides, the ratio of the length of one of the supported edges " ++
    "perpendicular to the free edge, to the length of the free edge, is " ++
    "equal to or greater than 0.5")
  "AR"

fTemperedGl = cncpt'' (mkUid "fullyT")
  (nounPhraseSP "fully tempered")
  (foldlSent_ [S "a flat, monolithic, glass lite of uniform thickness that has",
  S "been subjected to a special heat treatment process where the residual",
  S "surface compression is not less than 69 MPa (10 000 psi) or the edge",
  S "compression not less than 67 MPa (9700 psi), as defined in", refS astm2012])
  "FT"

glTyFac     = cncpt'' (mkUid "glassTypeFac")
  (nounPhraseSP "glass type factor")
  (foldlSent_ [S "a multiplying factor for adjusting the", short loadResis,
  S "of different glass type, that is,", foldlList Comma Options glassTypeAbbrs
  `sC` S "in monolithic glass" `sC` short lGlass, sParen (titleize lGlass) `sC`
   S "or", short iGlass, sParen (titleize iGlass), S "constructions"])
  "GTF"

hStrengthGl = cncpt'' (mkUid "heatS")
  (nounPhraseSP "heat strengthened")
  (foldlSent_ [S "a flat, monolithic, glass lite of uniform thickness that has",
  S "been subjected to a special heat treatment process where the residual",
  S "surface compression is not less than 24 MPa (3500psi) or greater than",
  S "52 MPa (7500 psi), as defined in", refS astm2012])
  "HS"

loadDurFac  = cncpt'' (mkUid "loadDurFactor")
  (nounPhraseSP "load duration factor")
  (S "factor related to the effect of sustained loading on glass strength")
  "LDF"

loadResis   = cncpt'' (mkUid "lResistance")
  (nounPhraseSP "load resistance")
  (foldlSent_ [S "the uniform lateral load that a glass construction can sustain",
  S "based upon a given probability of breakage and load duration as defined in",
  complexRef astm2009 $ Page [1, 53]])
  "LR"

loadShareFac = cncpt'' (mkUid "lShareFac")
  (nounPhraseSP "load share factor")
  (foldlSent_ [S "a multiplying factor derived from the load sharing between the",
  S "double glazing, of equal or different thicknesses and types (including the",
  S "layered behaviour of", short lGlass, S "under long duration",
  S "loads), in a sealed", short iGlass, S "unit"])
  "LSF"

nonFactoredL = cncpt'' (mkUid "nFL")
  (nounPhraseSP "non-factored load")
  (foldlSent_ [S "three second duration uniform load associated with a",
  S "probability of breakage less than or equal to 8", plural lite,
  S "per 1000 for monolithic", short annealedGl, S "glass"])
  "NFL"

stdOffDist    = cncpt'' (mkUid "stdOffDist")
  (nounPhraseSP "stand off distance")
  (S "the distance from the glazing surface to the centroid of a hemispherical" +:+
   S "high explosive charge")
  "SD"

blast         = cncpt''' (mkUid "blast")         (cn' "blast")
  (S "any kind of man-made explosion")
blastResisGla = cncpt''' (mkUid "blastResisGla") (nounPhraseSP "blast resistant glazing")
  (S "glazing that provides protection against air blast pressure generated by explosions")
blastTy       = cncpt''' (mkUid "blastTy")       (cn' "blast type")
  (S "the blast type input includes parameters like weight of charge, TNT equivalent factor, and stand off distance from the point of explosion")
bomb          = cncpt''' (mkUid "bomb")          (cn' "bomb")
  (S "a container filled with a destructive substance designed to exlode on impact or via detonation")
capacity      = cncpt''' (mkUid "capacity")      (nounPhraseSP "capacity or load resistance")
  (S "load resistance calculated")
demandq       = cncpt''' (mkUid "demandq")       (nounPhraseSP "applied load (demand)")
  (S "3 second duration equivalent pressure")
eqTNTChar     = cncpt''' (mkUid "eqTNTChar")     (nounPhraseSP "equivalent TNT charge mass")
  (S "mass of TNT placed on the ground in a hemisphere that represents the design explosive threat")
explosion     = cncpt''' (mkUid "explosion")     (cn' "explosion")
  (S "a destructive shattering of something")
glassGeo      = cncpt''' (mkUid "glassGeo")      (cnIES "glass geometry")
  (S "the glass geometry based inputs include the dimensions of the" +:+
    foldlList Comma List [phrase glaPlane, phrase glassTy, phrase responseTy])
glassTy       = cncpt''' (mkUid "glassTy")       (cn' "glass type")
  (S "type of glass")
glassWL       = cncpt''' (mkUid "glassWL")       (nounPhraseSP "glass weight load")
  (S "the dead load component of the glass weight")
glBreakage    = cncpt''' (mkUid "glBreakage")    (nounPhraseSP "glass breakage")
  (S "the fracture or breakage of any lite or ply in monolithic, laminated, or insulating glass")
lateral       = cncpt''' (mkUid "lateral")       (nounPhraseSP "lateral")
  (S "perpendicular to the glass surface")
lite          = cncpt''' (mkUid "lite")          (cn' "lite")
  (S "pieces of glass that are cut, prepared, and used to create the window or door")
load          = cncpt''' (mkUid "load")          (nounPhraseSP "applied load (demand) or pressure")
  (S "a uniformly distributed lateral pressure")
longDurLoad   = cncpt''' (mkUid "longDurLoad")   (nounPhraseSP "long duration load")
  (S "any load lasting approximately 30 days")
modE          = cncpt''' (mkUid "modElas")       (nounPhraseSP "modulus of elasticity of glass")
  (S "the ratio of tensile stress to tensile strain of glass")
notSafe       = cncpt''' (mkUid "notSafe")       (nounPhraseSP "not safe")
  (S "For the given input parameters, the glass is NOT considered safe.")
probBreak     = cncpt''' (mkUid "probBr")        (nounPhraseSP "probability of breakage")
  (foldlSent_ [S "the fraction of glass lites or plies that would break at the",
  S "first occurrence of a specified load and duration, typically expressed",
  S "in lites per 1000", sParen $ refS astm2016])
safeMessage   = cncpt''' (mkUid "safeMessage")   (nounPhraseSP "safe")
  (S "For the given input parameters, the glass is considered safe.")
shortDurLoad  = cncpt''' (mkUid "shortDurLoad")  (nounPhraseSP "short duration load")
  (S "any load lasting 3 seconds or less")
specA         = cncpt''' (mkUid "specA")         (nounPhraseSP "specifying authority")
  (S "the design professional responsible for interpreting applicable regulations of authorities having jurisdiction and considering appropriate site specific factors to determine the appropriate values used to calculate the specified design load, and furnishing other information required to perform this practice")
specDeLoad    = cncpt''' (mkUid "specDeLoad")    (nounPhraseSP "specified design load")
  (S "the magnitude in Pa (psf), type (for example, wind or snow) and duration of the load given by the specifying authority")

glassTypeAbbrs :: [Sentence]
glassTypeAbbrs = map (short . snd) glassType

glassType :: [(Integer, ConceptChunk)]
glassType = [(1, annealedGl), (4, fTemperedGl), (2, hStrengthGl)]
