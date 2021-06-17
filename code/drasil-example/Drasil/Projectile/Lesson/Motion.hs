module Drasil.Projectile.Lesson.Motion where

import qualified Drasil.DocLang.Notebook as NB (mainIdea, summary, hormotion, vermotion)

import Data.Drasil.Concepts.Physics (motion, acceleration, velocity, force, time,
  constAccel, horizontalMotion, verticalMotion)
import Drasil.Projectile.Concepts (projectile, projMotion)
import Drasil.Projectile.Expressions (lcrectVel, lcrectPos, lcrectNoTime, horMotionEqn1, horMotionEqn2,
  verMotionEqn1, verMotionEqn2, verMotionEqn3)
import qualified Data.Drasil.Quantities.Physics as QP (ixDist, iyDist, iSpeed, ixVel, iyVel, speed,
  constAccel, gravitationalAccel, xAccel, yAccel, time, xVel, yVel)
import Data.Drasil.Concepts.Documentation (coordinateSystem)
import Language.Drasil
import Language.Drasil.ShortHands
import Utils.Drasil
import qualified Utils.Drasil.Sentence as S


import qualified Drasil.Projectile.Expressions as E (speed', scalarPos', rectNoTime)

-- ****ADD figure
-- Note: define a combinator to avoid repetition.
-- grab embedded symbols from Drasil

motionContextP1, motionContextP2 :: Contents
motionContextP1
  = foldlSP
      [S "The free flight", phrase motion `S.sOfA` phrase projectile, 
        S "is often studied in terms of its rectangular components, since the",
        phrasePoss projectile, phrase acceleration +:+. S "always acts in the vertical direciton",
       S "To illustrate the kinematic analysis, consider a ", phrase projectile,
         S "launched at point", sParen ((E (sy QP.ixDist)) `sC` (E (sy QP.iyDist))),
         S "as shown in" +:+. makeRef2S figCSandA,
       S "The path is defined in the x-y plane such that the initial", phrase velocity +:+ S "is",
         E (sy QP.iSpeed), S ", having components", E (sy QP.ixVel) `S.sAnd` E (sy QP.iyVel),
       S "When air resistance is neglected, the only", phrase force, S "acting on the",
         phrase projectile, S"is its weight, which causes the", phrase projectile, 
         S "to have a *constant downward acceleration* of approximately",
         E (sy QP.constAccel $= sy QP.gravitationalAccel $= dbl 9.81) `S.sOr` 
         E (sy QP.gravitationalAccel $= dbl 32.2)]

motionContextP2
  = foldlSP_
      [S "The equations for rectilinear kinematics given above (ref) are in one dimension.",
       S "These equations can be applied for both the", phrase verticalMotion `S.andThe`
       phrase horizontalMotion, S ", as follows:"]

horMotion, verMotion, summary :: Section
horMotion = NB.hormotion [intro, equations, concl] []
  where intro = foldlSP_ [
                  S "For", phrase projMotion +:+ S "the", phrase acceleration, 
                  S "in the horizontal direction is and equal to zero" +:+. 
                  sParen(E (sy QP.xAccel $= 0)), motionSent]
        equations = foldlSP_ $ weave [equationsSents, map E horMotionEqns]
        concl = foldlSP [
                  S "Since the", phrase acceleration, S "in the x direction", sParen (E (sy QP.xAccel)),
                  S "is zero, the horizontal component of ", phrase velocity,
                  S "always remains constant during" +:+. phrase motion,
                  S "In addition to knowing this, we have one more equation"]
                
verMotion = NB.vermotion [intro, equations, concl] []
  where intro = foldlSP_ [
                  S "Since the positive y axis is directed upward, the", phrase acceleration,
                  S "in the vertical direction is" +:+. 
                  E (sy QP.yAccel $= negate (sy QP.gravitationalAccel)), motionSent]
        equations = foldlSP_ $ weave [equationsSents, map E verMotionEqns]
        concl = foldlSP [
                  S "Recall that the last equation can be formulated on the basis of eliminating the",
                  phrase time +:+ E (sy QP.time), S "between the first two equations, and therefore only ",
                  S "two of the above three equations are independent of one another"]

summary = NB.summary [smmryCon] []
  where smmryCon = foldlSP [
                  S "In addition to knowing that the horizontal component of", phrase velocity,
                  S "is constant [Hibbler doesn't say this, but it seems necessary for completeness],",
                  S "problems involving the", phrase motion `S.sOfA` phrase projectile +:
                  S "can have at most three unknowns since only three independent equations can be written",
                  S "that is, one equation in the horizontal direction and two in the vertical direction.",
                  S "Once", E (sy QP.xVel)  `S.sAnd`  E (sy QP.yVel),  S "are obtained, the resultant",
                  phrase velocity +:+ E (sy QP.speed), S "which is always tangent to the path,",
                  S "is defined by the vector sum as shown in", makeRef2S figCSandA]

resourcePath :: String
resourcePath = "../../../datafiles/Projectile/"

figCSandA :: LabelledContent
figCSandA = llcc (makeFigRef "CoordSystAndAssumpts") $ fig (atStartNP (the coordinateSystem))
  (resourcePath ++ "CoordSystAndAssumpts.png") 

equationsSents :: [Sentence]
equationsSents = [S "From Equation" +: makeRef2S lcrectVel,
                  S "From Equation" +: makeRef2S lcrectPos,
                  S "From Equation" +: makeRef2S lcrectNoTime]
                
horMotionEqns, verMotionEqns :: [Expr]
horMotionEqns = [horMotionEqn1, horMotionEqn2, horMotionEqn1]
verMotionEqns = [verMotionEqn1, verMotionEqn2, verMotionEqn3]

motionSent :: Sentence
motionSent = S "This value can be substituted in the equations for" +:+ phrase constAccel +:
             S "given above (ref) to yield the following"


