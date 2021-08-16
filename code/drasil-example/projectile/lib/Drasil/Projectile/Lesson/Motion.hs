module Drasil.Projectile.Lesson.Motion where

import Data.List

import qualified Drasil.DocLang.Notebook as NB (summary, hormotion, vermotion)

import Data.Drasil.Concepts.Physics (motion, acceleration, velocity, force, time,
  constAccel, horizontalMotion, verticalMotion)
import Data.Drasil.Units.Physics (accelU)
import Data.Drasil.Concepts.Math (xDir, yAxis)
import Drasil.Projectile.Concepts (projectile, projMotion)
import Drasil.Projectile.Derivations
import Drasil.Projectile.Expressions
import qualified Data.Drasil.Quantities.Physics as QP (ixDist, iyDist, iSpeed, ixVel, iyVel, speed,
  constAccel, gravitationalAccel, xAccel, yAccel, time, xVel, yVel)
import Data.Drasil.Concepts.Documentation (coordinateSystem)
import Language.Drasil
import Language.Drasil.ShortHands
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S
import Data.Drasil.SI_Units (s_2)

motionContextP1, motionContextP2 :: Contents
motionContextP1
  = foldlSP
      [S "The free flight", phrase motion `S.ofA` phrase projectile, 
        S "is often studied in terms of its rectangular components, since the",
        phrasePoss projectile, phrase acceleration +:+. S "always acts in the vertical direciton",
       S "To illustrate the kinematic analysis, consider a ", phrase projectile,
         S "launched at point", sParen (eS (sy QP.ixDist) `sC` eS (sy QP.iyDist)),
         S "as shown in" +:+. refS figCSandA,
       S "The path is defined in the", P lX `sDash` P lY, S "plane such that the initial", 
         phrase velocity, S "is", eS (sy QP.iSpeed) :+: S ", having components", 
         eS (sy QP.ixVel) `S.and_` eS (sy QP.iyVel),
       S "When air resistance is neglected, the only", phrase force, S "acting on the",
         phrase projectile, S"is its weight, which causes the", phrase projectile, 
         S "to have a *constant downward acceleration* of approximately",
         eS (sy QP.constAccel $= sy QP.gravitationalAccel $= dbl 9.81), Sy (usymb accelU) `S.or_` 
         eS (sy QP.gravitationalAccel $= dbl 32.2), Sy (usymb accelinftU)]
--
motionContextP2
  = foldlSP_
      [S "The equations for rectilinear kinematics given above", refS lcrectVel, S "are in one dimension.",
       S "These equations can be applied for both the", phrase verticalMotion `S.andThe`
       phrase horizontalMotion :+: S ", as follows:"]

horMotion, verMotion, summary :: Section
horMotion = NB.hormotion [intro, equations, concl] []
  where intro = foldlSP_ [
                  S "For", phrase projMotion +:+ S "the", phrase acceleration, 
                  S "in the horizontal direction is and equal to zero" +:+. 
                  sParen(eS (sy QP.xAccel $= exactDbl 0)), motionSent]
        equations = foldlSP_ $ intersperse (S ",") (weave [equationsSents, map eS horMotionEqns])
        concl = foldlSP [
                  S "Since the", phrase acceleration, S "in the" +:+ phrase xDir, 
                  sParen (eS (sy QP.xAccel)), S "is zero, the horizontal component of ", phrase velocity,
                  S "always remains constant during" +:+. phrase motion,
                  S "In addition to knowing this, we have one more equation"]
                
verMotion = NB.vermotion [intro, equations, concl] []
  where intro = foldlSP_ [
                  S "Since the positive", phrase yAxis, S "is directed upward, the", phrase acceleration,
                  S "in the vertical direction is" +:+. eS (sy QP.yAccel $= neg (sy QP.gravitationalAccel)), motionSent]
        equations = foldlSP_ $ weave [equationsSents, map eS verMotionDeriv]
        concl = foldlSP [
                  S "Recall that the last equation can be formulated on the basis of eliminating the",
                  phrase time +:+ eS (sy QP.time), S "between the first two equations, and therefore only ",
                  S "two of the above three equations are independent of one another"]

summary = NB.summary [smmryCon] []
  where smmryCon = foldlSP [
                  S "In addition to knowing that the horizontal component of", phrase velocity,
                  S "is constant [Hibbler doesn't say this, but it seems necessary for completeness],",
                  S "problems involving the", phrase motion `S.ofA` phrase projectile +:
                  S "can have at most three unknowns since only three independent equations can be written",
                  S "that is, one equation in the horizontal direction and two in the vertical direction.",
                  S "Once", eS (sy QP.xVel)  `S.and_` eS (sy QP.yVel),  S "are obtained, the resultant",
                  phrase velocity +:+ eS (sy QP.speed), S "which is always tangent to the path,",
                  S "is defined by the vector sum as shown in", refS figCSandA]

resourcePath :: String
resourcePath = "../../../datafiles/Projectile/"

figCSandA :: LabelledContent
figCSandA = llcc (makeFigRef "CoordSystAndAssumpts") $ fig (atStartNP $ the coordinateSystem)
  (resourcePath ++ "CoordSystAndAssumpts.png") 

equationsSents :: [Sentence]
equationsSents = [S "From Equation" +: refS lcrectVel,
                  S "From Equation" +: refS lcrectPos,
                  S "From Equation" +: refS lcrectNoTime]
                
horMotionEqns :: [ModelExpr]
horMotionEqns = [horMotionEqn1, horMotionEqn2, horMotionEqn1]

motionSent :: Sentence
motionSent = S "This value can be substituted in the equations for" +:+ phrase constAccel +:
             S "given above (ref) to yield the following"

-- References --
figRefs :: [Reference]
figRefs = [ref figCSandA]

foot, accelinftU :: UnitDefn
foot = fund "foot" "length" "ft"
accelinftU = newUnit "acceleration" $ foot /: s_2
