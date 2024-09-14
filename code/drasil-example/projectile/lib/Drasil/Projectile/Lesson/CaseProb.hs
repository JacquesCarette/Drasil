module Drasil.Projectile.Lesson.CaseProb where

import Utils.Drasil (weave)

import Data.Drasil.Concepts.Physics (motion, acceleration, velocity, force, time,
  constAccel, horizontalMotion, verticalMotion, gravity, position)
import Data.Drasil.Units.Physics (accelU)
import Data.Drasil.Concepts.Math (component, direction, equation, xDir, yAxis)
import Drasil.Projectile.Concepts (projectile, projMotion)
import Drasil.Projectile.Expressions
import Drasil.Projectile.Lesson.Figures (figCSandA)
import qualified Data.Drasil.Quantities.Physics as QP (iSpeed, ixSpeed, iySpeed, speed,
  constAccel, gravitationalAccel, xAccel, yAccel, time,
  ixPos, iyPos, xPos, yPos, ixVel, iyVel, xVel, yVel)
import Data.Drasil.Concepts.Documentation (coordinate, procedure)
import Language.Drasil
import Language.Drasil.ShortHands
import qualified Language.Drasil.Sentence.Combinators as S
import Data.Drasil.SI_Units (s_2)

caseProbCont :: [Contents]
caseProbCont = [projMotionHead, motionContextP1, LlC figCSandA, motionContextP2, 
  horMotionHead, hMintro, horizMotionEqn1, horizMotionEqn2, horizMotionEqn3, hMconcl, 
  verMotionHead, vMintro, vertMotionEqn1, vertMotionEqn2, vertMotionEqn3, vMconcl, summaryHead, summary, 
  procforAnlsHead, procforAnls, stepOneHead, stepOneCont, stepTwoHead, stepTwoCont, stepThreeHead, stepThreeCont, 
  stepFourHead, stepFourCont, stepFourOneHead, horizMotionEqn1, horizMotionEqn1Sent, horizMotionEqn2, stepFourTwoHead, 
  verMotionCont, vertMotionEqn1, vertMotionEqn2, vertMotionEqn3, stepFiveHead, stepFiveCont]

projMotionHead, horMotionHead, verMotionHead, summaryHead :: Contents
projMotionHead = foldlSP_ [headSent 1 (S "Motion of a Projectile")]
horMotionHead  = foldlSP_ [headSent 2 (S "Horizontal Motion")] 
verMotionHead  = foldlSP_ [headSent 2 (S "Vertical Motion")] 
summaryHead    = foldlSP_ [headSent 2 (S "Summary")]

motionContextP1, motionContextP2 :: Contents
motionContextP1
  = foldlSP
      [S "The free flight", phrase motion `S.ofA` phrase projectile, 
        S "is often studied in terms" `S.of_` S "its rectangular components" `sC` S "since the",
        phrasePoss projectile, phrase acceleration +:+. (S "always acts" `S.inThe` S "vertical direciton"),
       S "To illustrate the kinematic analysis" `sC` S "consider a ", phrase projectile,
         S "launched at point", sParen (P lX `sC` P lY),
         S "as shown in" +:+. refS figCSandA,
       S "The path" `S.is` S "defined in the", P lX `sDash` P lY, S "plane such that the initial", 
         phrase velocity, S "is", eS (sy QP.iSpeed) :+: S ", having components", 
         eS (sy QP.ixSpeed) `S.and_` eS (sy QP.iySpeed),
       S "When air resistance is neglected, the only", phrase force, S "acting on the",
         phrase projectile, S"is its weight" `sC` S "which causes the", phrase projectile, 
         S "to have a *constant downward acceleration* of approximately",
         eS (sy QP.constAccel $= sy QP.gravitationalAccel $= dbl 9.81), Sy (usymb accelU) `S.or_` 
         eS (sy QP.gravitationalAccel $= dbl 32.2), Sy (usymb accelinftU)]

motionContextP2
  = foldlSP_
      [S "The equations" `S.for` S "rectilinear kinematics given above", refS lcrectVel, S "are in one dimension.",
       S "These equations can be applied for both the", phrase verticalMotion `S.andThe`
       phrase horizontalMotion :+: S ", as follows:"]

hMintro, hMequations, hMconcl, vMintro, vMequations, vMconcl, summary :: Contents
hMintro = foldlSP_ [
            S "For", phrase projMotion +:+ S "the", phrase acceleration, 
            S "in the horizontal direction is and equal to zero" +:+. 
            sParen(eS (sy QP.xAccel $= exactDbl 0)), motionSent]
hMequations = foldlSP_ $ weave [equationsSents, map eS horMotionEqns]
hMconcl = foldlSP [
            S "Since the", phrase acceleration, S "in the" +:+ phrase xDir, 
            sParen (eS (sy QP.xAccel)), S "is zero" `sC` S "the horizontal component of ", phrase velocity,
            S "always remains constant during" +:+. phrase motion,
            S "In addition to knowing this" `sC` S "we have one more equation"]

vMintro = foldlSP_ [
            S "Since the positive", phrase yAxis, S "is directed upward, the", phrase acceleration,
            S "in the vertical direction is" +:+. eS (sy QP.yAccel $= neg (sy QP.gravitationalAccel)), motionSent]
vMequations = foldlSP_ $ weave [equationsSents, map eS verMotionEqns]
vMconcl = foldlSP [
            S "Recall that the last equation can be formulated" `S.onThe` S "basis of eliminating the",
            phrase time +:+ eS (sy QP.time), S "between the first two equations" `sC` S "and therefore only ",
            S "two of the above three equations" `S.are` S "independent of one another"]

summary = foldlSP [S "In addition to knowing that the horizontal component" `S.of_` phrase velocity,
                   S "is constant [Hibbler doesn't say this, but it seems necessary for completeness],",
                   S "problems involving the", phrase motion `S.ofA` phrase projectile +:
                   S "can have at most three unknowns since only three independent equations can be written",
                   S "that is" `sC` S "one equation" `S.inThe` S "horizontal direction and two" `S.inThe` S "vertical direction.",
                   S "Once", eS (sy QP.xVel)  `S.and_` eS (sy QP.yVel),  S "are obtained, the resultant",
                   phrase velocity +:+ eS (sy QP.speed), S "which is always tangent to the path,",
                   S "is defined by the vector sum as shown in", refS figCSandA]

procforAnls :: Contents
procforAnls = foldlSP [S "Free-flight", phrase projMotion, S "problems can be solved using the following", phrase procedure]

procforAnlsHead, stepOneHead, stepTwoHead, stepThreeHead, stepFourHead, stepFourOneHead, stepFourTwoHead, stepFiveHead :: Contents
procforAnlsHead = foldlSP_ [headSent 2 (S "Procedure for Analysis")]
stepOneHead     = foldlSP_ [headSent 3 (S "Step 1: Coordinate System")]
stepTwoHead     = foldlSP_ [headSent 3 (S "Step 2: Identify Knowns")] 
stepThreeHead   = foldlSP_ [headSent 3 (S "Step 3: Identify Unknowns")] 
stepFourHead    = foldlSP_ [headSent 3 (S "Step 4: Kinematic Equations")]
stepFourOneHead = foldlSP_ [headSent 4 (S "Step 4.1: Horizontal Motion")]
stepFourTwoHead = foldlSP_ [headSent 4 (S "Step 4.2: Vertical Motion")]
stepFiveHead    = foldlSP_ [headSent 3 (S "Step 5: Solve for Unknowns")]

stepOneCont, stepTwoCont, stepThreeCont, stepFourCont, horizMotionEqn1, horizMotionEqn1Sent, horizMotionEqn2, horizMotionEqn3,
  vertMotionEqn1, vertMotionEqn2, vertMotionEqn3, verMotionCont, stepFiveCont :: Contents
stepOneCont = enumBulletU $ map foldlSent 
  [[S "Establish the fixed", P lX `sC` P lY, phrase coordinate +:+. (S "axes and sketch the trajectory" `S.ofThe` S "particle"),
    S "Between any *two points*" `S.onThe` S "path specify the given problem data and the *three unknowns*.",
    S "In all cases the", phrase acceleration `S.of_` phrase gravity +:+. S "acts downward",
    S "The particle's initial and final", plural velocity +:+ S "should be represented in terms of their",
    P lX `S.and_` P lY, plural component],
  [S "Remember that positive and negative", phrase position `sC` phrase velocity, S "," `S.and_`
    phrase acceleration, plural component +:+ S "always act in accordance with their associated",
    phrase coordinate +:+ plural direction],
  [S "The two points that are selected should be significant points where something about the",
    phrase motion `S.ofThe` S "particle is known. Potential significant points include the initial point",
    S "of launching the", phrase projectile `S.andThe` S "final point where it lands." +:+ 
    S "The landing point often has a known", P lY +:+ S "value"],
  [S "The variables" `S.inThe` plural equation, S "may need to be changed to match the notation" `S.ofThe` S "specific problem.",
    S "For instance" `sC` S "a distinction may need to be made between the", P lX, phrase coordinate `S.of_` S "points", P cA `S.and_` P cB,
    S "via notation like"]]

stepTwoCont = foldlSP [S "Using the notation" `S.for` S "the problem in question" `sC` S "write out the known variables and their values.",
  S "The known variables will be a subset of the following:" +:+. ((eS (sy QP.ixPos) `sC` eS (sy QP.xPos)) `sC` (eS (sy QP.iyPos) `sC` eS (sy QP.yPos)) `sC` 
  (eS (sy QP.ixVel) `sC` eS (sy QP.xVel)) `sC` (eS (sy QP.iyVel) `sC` eS (sy QP.yVel)) `S.and_` P lT),
  S "The knowns should be written" `S.inThe` S "notation adopted" `S.for` S "the particular problem"]

stepThreeCont = foldlSP [S "Each problem will have at most 4 unknowns that need to be determined" `sC` S "selected from the variables listed in the Step 2 that are not known." +:+.
  (S "The number of relevant unknowns will usually be less than 4" `sC` S "since questions will often focus on one or two unknowns"),
  S "As an example" `sC` S "the equation that horizontal", phrase velocity +:+. S "is constant is so trivial that most problems will not look for this as an unknown",
  S "The unknowns should be written" `S.inThe` S "notation adopted" `S.for` S "the particular problem"]

stepFourCont = foldlSP [S "Depending upon the known data and what is to be determined" `sC` S "a choice should be made as to which four of the following five equations" +:+
  S "should be applied between the two points" `S.onThe` S "path to obtain the most direct solution to the problem"]

horizMotionEqn1 = foldlSP_ [S "From equation" +: refS lcrectVel, eS horizVel] 
horizMotionEqn1Sent = foldlSP_ [sParen (S "The *velocity* in the horizontal" `S.or_` P lX +:+ phrase direction +:+ S "is *constant*")]
horizMotionEqn2 = foldlSP_ [S "From equation" +: refS lcrectPos, eS horizPos]
horizMotionEqn3 = foldlSP_ [S "From equation" +: refS lcrectNoTime, eS horizVel]

vertMotionEqn1 = foldlSP_ [S "From equation" +: refS lcrectVel, eS vertVel]
vertMotionEqn2 = foldlSP_ [S "From equation" +: refS lcrectPos, eS vertPos]
vertMotionEqn3 = foldlSP_ [S "From equation" +: refS lcrectNoTime, eS vertNoTime]

verMotionCont = foldlSP [S "In the vertical" `S.or_` P lY, phrase direction, S "*only two*" `S.ofThe` S "following three equations", 
  sParen (S "using" +:+ eS (sy QP.yAccel $= neg (sy QP.gravitationalAccel))) +:+. S "can be used for solution",
  sParen (S "The sign" `S.of_` P lG +:+ S "will change to positive if the positive" +:+ P lY +:+. S "axis is downward"),
  S "For example, if the particle's final velocity", eS (sy QP.yVel), S "is not needed" `sC` S "then the first and third of these questions",
  sParen (S "for" +:+ P lY) +:+ S "will not be useful"] 

stepFiveCont = foldlSP [S "Use the equations from Step 4" `sC` S "together with the known values from Step 2 to find the unknown values from Step 3." +:+.
  S "We can do this systematically by going through each equation and determining how many unknowns are in that equation",
  S "Any equations with one unknown can be used to solve for that unknown directly"]


equationsSents :: [Sentence]
equationsSents = [S "From Equation" +: refS lcrectVel,
                  S "From Equation" +: refS lcrectPos,
                  S "From Equation" +: refS lcrectNoTime]
                
horMotionEqns :: [ModelExpr]
horMotionEqns = [horizVel, horizPos, horizVel]

verMotionEqns :: [ModelExpr]
verMotionEqns = [vertVel, vertPos, vertNoTime]

motionSent :: Sentence
motionSent = S "This value can be substituted" `S.inThe` S "equations for" +:+ phrase constAccel +:
             S "given above (ref) to yield the following"

-- References --
figRefs :: [Reference]
figRefs = [ref figCSandA]

foot, accelinftU :: UnitDefn
foot = fund "foot" "length" "ft"
accelinftU = newUnit "acceleration" $ foot /: s_2
