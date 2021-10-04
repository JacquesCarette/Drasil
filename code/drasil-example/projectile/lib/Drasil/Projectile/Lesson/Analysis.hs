module Drasil.Projectile.Lesson.Analysis where

import qualified Drasil.DocLang.Notebook as NB (coorSyst, kinematic, hormotion, vermotion)

import Data.Drasil.Concepts.Documentation (coordinate)
import Data.Drasil.Concepts.Math (component, direction, equation)
import Data.Drasil.Concepts.Physics (acceleration, gravity, velocity, position, motion)

import qualified Data.Drasil.Quantities.Physics as QP (yVel)

import Drasil.Projectile.Concepts (projectile)
import Language.Drasil
import Language.Drasil.ShortHands
import Utils.Drasil

import qualified Utils.Drasil.Sentence as S

import Drasil.Projectile.Derivations (horMotionEqn1, horMotionEqn2)

coorSyst, kinematicEq, horMotionAna, verMotionAna :: Section
coorSyst = NB.coorSyst [coorSystContext] []
kinematicEq = NB.kinematic [kinematicContext] []
horMotionAna = NB.hormotion [horMotionContext] []
verMotionAna = NB.vermotion [verMotionContext] []

coorSystContext, kinematicContext, horMotionContext, verMotionContext :: Contents
coorSystContext = enumBulletU $ map foldlSent 
  [[S "Establish the fixed", P lX `sC` P lY, phrase coordinate +:+. S "axes and sketch the trajectory of the particle",
    S "Between any *two points* on the path specify the given problem data and the *three unknowns*.",
    S "In all cases the", phrase acceleration `S.of_` phrase gravity +:+. S "acts downward",
    S "The particle's initial and final", plural velocity +:+ S "should be represented in terms of their",
    P lX `S.and_` P lY, plural component],
  [S "Remember that positive and negative", phrase position `sC` phrase velocity, S "," `S.and_`
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

horMotionContext = enumBulletU $ map foldlSent 
  [[S "The *velocity* in the horizontal" `S.or_` P lX, phrase direction, S "is *constant*, i.e.,", 
    eS horMotionEqn1 `S.and_` eS horMotionEqn2]]

verMotionContext = enumBulletU $ map foldlSent
  [[S "In the vertical" `S.or_` P lY, phrase direction, 
    S "*only two* of the following three equations can be used for solution"],
   [S "For example, if the particle's final", phrase velocity +:+ eS (sy QP.yVel),
    S "is not needed, then the first and third of these questions", sParen (S "for" +:+ P lY),
    S "will not be useful"]]
