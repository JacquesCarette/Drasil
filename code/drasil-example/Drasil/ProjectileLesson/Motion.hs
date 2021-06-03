module Drasil.ProjectileLesson.Motion where

import qualified Drasil.DocLang.Notebook as NB

import Drasil.Projectile.Concepts (projectile, projMotion)
import qualified Data.Drasil.Quantities.Physics QP (ixVel, iyVel)
import Language.Drasil
import Utils.Drasil

import qualified Drasil.ProjectileLesson.Expressions as E (rectVel, rectPos, rectNoTime)

-- ****ADD figure

motionContextP1, motionContextP2 :: Contents
motionContextP1
  = foldlSP
      [S "The free flight motion of a ", phrase projectile, S "is often studied in terms of its rectangular components,"
        S "since the projectile's acceleration always acts in the vertical direciton",
       S "To illustrate the kinematic analysis, consider a ", phrase projectile,
         S "launched at point  (", E (sy QP.ixVel) +:+ S "," +:+ E (sy QP.iyVel) +:+
         S ") , as shown in Figure [hyperref here?]",
       S "The path is defined in the $x-y$ plane such that the initial velocity is"]

motionContextP2
  = foldlSP
      [S "The equations for rectilinear kinematics given above (ref) are in one dimension",
       S "These equations can be applied for both the vertical motion and the horizontal directions, as follows:"]

horMotion, verMotion, summary :: Section
horMotion = NB.horizontalMotion [intro, equations, concl] []
  where intro = foldlSP_ [
                  S "For projectile motion the acceleration in the horizontal direction is constant",
                  S "and equal to zero. This value can be substituted in the equations for constant",
                  S "acceleration given above to yield the following:"]
        equations = foldlSP_ [
                  S "From Equation", makeRef2S E.rectVel, 
                  S "From Equation", makeRef2S E.rectPos,
                  S "From Equation", makeRef2S E.rectNoTime]
        concl = foldlSP_ [
                  S "Since the acceleration in the x direction is zero, ",
                  S "the horizontal component of velocity always remains constant during motion",
                  S "In addition to knowing this, we have one more equation"]
                
verMotion = NB.verticalMotion [intro, equations, concl] []
  where intro = foldlSP_ [
                  S "Since the positive y axis is directed upward, the acceleration in the vertical direction is",
                  S "This value can be substituted in the equations for constant",
                  S "acceleration given above to yield the following:"]
        equations = foldlSP_ [
                  S "From Equation", makeRef2S E.rectVel, 
                  S "From Equation", makeRef2S E.rectPos,
                  S "From Equation", makeRef2S E.rectNoTime]
        concl = foldlSP_ [
                  S "Recall that the last equation can be formulated on the basis of eliminating the time t ",
                  S "between the first two equations , and therefore only two of the above",
                  S "three equations are independent of one another."]

summary = NB.summary [smmryCon] []
  where smmryCon = foldlSP_ [
                  S "In addition to knowing that the horizontal component of velocity is constant",
                  S "[Hibbler doesn't say this, but it seems necessary for completeness],",
                  S "problems involving the motion of a projectile can have at most three unknowns",
                  S "since only three independent equations can be written: ",
                  S "that is, one equation in the horizontal direction and two in the vertical direction.",
                  S "Once  𝑣𝑥  and  𝑣𝑦  are obtained, the resultant velocity  𝐯 , ",
                  S "which is always tangent to the path, is defined by the vector sum as shown in Figure"]