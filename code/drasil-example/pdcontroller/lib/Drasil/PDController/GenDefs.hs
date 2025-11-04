{-# LANGUAGE PostfixOperators #-}
module Drasil.PDController.GenDefs where

import Language.Drasil
import qualified Language.Drasil.Development as D
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Citations ( pidWiki )
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Concepts.Math (equation)

import Theory.Drasil (GenDefn, gd, othModel')

import Drasil.PDController.Unitals
import Drasil.PDController.Assumptions
import Drasil.PDController.Concepts
import Drasil.PDController.References
import Drasil.PDController.TModel

genDefns :: [GenDefn]
genDefns = [gdPowerPlant]

----------------------------------------------

gdPowerPlant :: GenDefn
gdPowerPlant
  = gd (othModel' gdPowerPlantRC) (Nothing :: Maybe UnitDefn) Nothing
      [dRef pidWiki, dRef abbasi2015]
      "gdPowerPlant"
      [gdPowerPlantNote]

gdPowerPlantRC :: RelationConcept
gdPowerPlantRC
  = makeRC "gdPowerPlantRC"
      (ccTransferFxn `the_ofThe` powerPlant)
      EmptyS
      gdPowerPlantEqn

gdPowerPlantEqn :: Expr
gdPowerPlantEqn
  = recip_ (square (sy dqdFreqDomain) $+ sy dqdFreqDomain $+ exactDbl 20)

gdPowerPlantNote :: Sentence
gdPowerPlantNote
  = foldlSent
      [D.toSent (atStartNP (ccTransferFxn `the_ofThe` secondOrderSystem)),
         fromSource tmSOSystem,
         S "is reduced to this equation by substituting the",
         phrase mass, S "(m) to 1 Kg", fromSource aMass `sC`
         D.toSent (phraseNP (the ccDampingCoeff)), sParen (P symDampingCoeff),
         S "to 1", fromSource aDampingCoeff `sC` EmptyS
         `S.andThe` phrase ccStiffCoeff, sParen (P symStifnessCoeff),
         S "to 20" +:+. fromSource aStiffnessCoeff,
       D.toSent (atStartNP (the equation)) `S.is` S "converted" `S.toThe` phrase ccFrequencyDomain,
         S "by applying the", atStart ccLaplaceTransform +:+. fromSource tmLaplace,
       S "Additionally, there" `S.are` S "no external disturbances" `S.toThe` S "power plant",
         fromSource aExtDisturb]
