module Drasil.Projectile.Lesson.Analysis where

import qualified Drasil.DocLang.Notebook as NB (coorSyst, kinematic, hormotion, vermotion)

import Data.Drasil.Concepts.Documentation (coordinate)
import Data.Drasil.Concepts.Math (component, direction, equation)
import Data.Drasil.Concepts.Physics (acceleration, gravity, velocity, position, motion)
import Drasil.Projectile.Concepts (projectile, projMotion)
import Language.Drasil
import Language.Drasil.ShortHands
import Utils.Drasil (foldlSP, foldlSP_, foldlSent, enumBulletU)

import qualified Utils.Drasil.Sentence as S

coorSyst, kinematicEq, horMotion, verMotion :: Section
coorSyst = NB.coorSyst [coorSystContext] []
kinematicEq = NB.kinematic [kinematicContext] []
horMotion = NB.hormotion [] []
verMotion = NB.vermotion [] []

coorSystContext, kinematicContext :: Contents
coorSystContext = enumBulletU $ map foldlSent 
  [[S "Establish the fixed", P lX `sC` P lY, phrase coordinate +:+. S "axes and sketch the trajectory of the particle",
    S "Between any *two points* on the path specify the given problem data and the *three unknowns*.",
    S "In all cases the", phrase acceleration `S.sOf` phrase gravity +:+. S "acts downward",
    S "The particle's initial and final", plural velocity +:+ S "should be represented in terms of their",
    P lX `S.sAnd` P lY, plural component],
  [S "Remember that positive and negative", phrase position `sC` phrase velocity, S "," `S.sAnd`
    phrase acceleration, plural component +:+ S "always act in accordance with their associated",
    phrase coordinate +:+ plural direction],
  [S "The two points that are selected should be significant points where something about the",
    phrase motion `S.ofThe` S "particle is known. Potential significant points include the initial point",
    S "of launching the", phrase projectile `S.andThe` S "final point where it lands." +:+ 
    S "The landing point often has a known", P lY +:+ S "value"]]

kinematicContext = enumBulletU $ map foldlSent 
  [[S "Depending upon the known data and what is to be determined, a choice should be made",
    S "as to which three of the following four", plural equation, S "should be applied between",
    S "the two points on the path to obtain the most direct solution to the problem"]]