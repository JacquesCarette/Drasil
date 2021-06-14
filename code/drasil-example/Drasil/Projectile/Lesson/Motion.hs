module Drasil.Projectile.Lesson.Motion where

import qualified Drasil.DocLang.Notebook as NB (mainIdea, summary, hormotion, vermotion)

import Data.Drasil.Concepts.Physics (motion, acceleration)
import Drasil.Projectile.Concepts (projectile, projMotion)
import qualified Data.Drasil.Quantities.Physics as QP (ixDist, iyDist)
import Data.Drasil.Concepts.Documentation (coordinateSystem)
import Language.Drasil
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
        phrasePoss projectile, phrase acceleration +:+ S "always acts in the vertical direciton",
       S "To illustrate the kinematic analysis, consider a ", phrase projectile,
         S "launched at point", sParen ((E (sy QP.ixDist)) `sC` (E (sy QP.iyDist))),
         S "as shown in" +:+. makeRef2S figCSandA,
       S "The path is defined in the x-y plane such that the initial velocity is"]

motionContextP2
  = foldlSP
      [S "The equations for rectilinear kinematics given above (ref) are in one dimension",
       S "These equations can be applied for both the vertical motion and the horizontal directions, as follows:"]

-- **TODO: mainIdea should be hor and ver motions
horMotion, verMotion, summary :: Section
horMotion = NB.hormotion [intro, equations, concl] []
  where intro = foldlSP_ [
                  S "For projectile motion the acceleration in the horizontal direction is constant",
                  S "and equal to zero. This value can be substituted in the equations for constant",
                  S "acceleration given above to yield the following:"]
        equations = foldlSP_ [
                  S "From Equation", 
                  S "From Equation", 
                  S "From Equation"] 
        concl = foldlSP_ [
                  S "Since the acceleration in the x direction is zero, ",
                  S "the horizontal component of velocity always remains constant during motion",
                  S "In addition to knowing this, we have one more equation"]
                
verMotion = NB.vermotion [intro, equations, concl] []
  where intro = foldlSP_ [
                  S "Since the positive y axis is directed upward, the acceleration in the vertical direction is",
                  S "This value can be substituted in the equations for constant",
                  S "acceleration given above to yield the following:"]
        equations = foldlSP_ [
                  S "From Equation", 
                  S "From Equation",
                  S "From Equation"]
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

resourcePath :: String
resourcePath = "../../../datafiles/Projectile/"

figCSandA :: LabelledContent
figCSandA = llcc (makeFigRef "CoordSystAndAssumpts") $ figWithWidth (atStartNP (the coordinateSystem))
  (resourcePath ++ "CoordSystAndAssumpts.png") 70


