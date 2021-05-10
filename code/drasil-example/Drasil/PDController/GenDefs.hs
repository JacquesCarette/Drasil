module Drasil.PDController.GenDefs where

import Data.Drasil.Quantities.PhysicalProperties (mass)
import Drasil.PDController.Assumptions
import Drasil.PDController.Concepts
import Drasil.PDController.References
import Drasil.PDController.TModel
import Language.Drasil
import Theory.Drasil (GenDefn, gd, ModelKinds (OthModel))
import Utils.Drasil
import Data.Drasil.Citations ( pidWiki )
import Drasil.PDController.Unitals

genDefns :: [GenDefn]
genDefns = [gdPowerPlant]

----------------------------------------------

gdPowerPlant :: GenDefn
gdPowerPlant
  = gd (OthModel gdPowerPlantRC) (Nothing :: Maybe UnitDefn) Nothing
      [makeCite pidWiki, makeCite abbasi2015]
      "gdPowerPlant"
      [gdPowerPlantNote]

gdPowerPlantRC :: RelationConcept
gdPowerPlantRC
  = makeRC "gdPowerPlantRC"
      (nounPhraseSP "The Transfer Function of the Power Plant.")
      EmptyS
      gdPowerPlantEqn

gdPowerPlantEqn :: Expr
gdPowerPlantEqn
  = 1 / (square (sy qdFreqDomain) + sy qdFreqDomain + 20)

gdPowerPlantNote :: Sentence
gdPowerPlantNote
  = foldlSent
      [S "The " +:+ phrase ccTransferFxn +:+ S "of the" +:+
         phrase secondOrderSystem
         +:+ S "(from "
         <> makeRef2S tmSOSystem
         <> S ") is reduced to this equation by substituting the"
         +:+ phrase mass
         +:+ S "(m)"
         +:+ S "to 1 Kg (from "
         <> makeRef2S aMass
         <> S "), the"
         +:+ phrase ccDampingCoeff
         +:+ S "("
         <> P symDampingCoeff
         <> S ") to 1 (from "
         <> makeRef2S aDampingCoeff
         <> S "), and the"
         +:+ phrase ccStiffCoeff
         +:+ S "("
         <> P symStifnessCoeff
         <> S ") to 20 (from "
         <> makeRef2S aStiffnessCoeff
         <> S "). The equation is" +:+ S "converted to the frequency" +:+
         S "domain by applying the Laplace"
         +:+ S "transform (from "
         <> makeRef2S tmLaplace
         <> S ").",
       S "Additionally, there are no external disturbances to the power plant"
         +:+ S "(from "
         <> makeRef2S aExtDisturb
         <> S ")"]
