module Drasil.Projectile.Lesson.Analysis where

import qualified Drasil.DocLang.Notebook as NB (coorSyst, kinematic, hormotion, vermotion)

import Data.Drasil.Concepts.Documentation (coordinate, procedure)
import Data.Drasil.Concepts.Math (component, direction, equation)
import Data.Drasil.Concepts.Physics (acceleration, gravity, velocity, position, motion)

import qualified Data.Drasil.Quantities.Physics as QP (gravitationalAccel, yAccel, ixPos, iyPos, xPos, yPos, ixVel, iyVel, xVel, yVel)

import Drasil.Projectile.Concepts (projectile, projMotion)
import Language.Drasil
import Language.Drasil.ShortHands

import qualified Language.Drasil.Sentence.Combinators as S
import Drasil.Projectile.Expressions

procforAnls :: Contents
procforAnls = foldlSP [S "Free-flight", phrase projMotion, S "problems can be solved using the following", phrase procedure]

procforAnlsCont :: [Contents]
procforAnlsCont = [procforAnls, stepOneHead, stepOneCont, stepTwoHead, stepTwoCont, stepThreeHead, stepThreeCont, stepFourHead, stepFourCont, 
  stepFourOneHead, horizMotionEqn1, horizMotionEqn2, stepFourTwoHead, verMotionCont, vertMotionEqn1, vertMotionEqn2, vertMotionEqn3,
  stepFiveHead, stepFiveCont]


stepOneHead, stepTwoHead, stepThreeHead, stepFourHead, stepFourOneHead, stepFourTwoHead, stepFiveHead:: Contents
stepOneHead     = foldlSP_ [S "__Step 1: Coordinate System__"]
stepTwoHead     = foldlSP_ [S "__Step 2: Identify Knowns__"] 
stepThreeHead   = foldlSP_ [S "__Step 3: Identify Unknowns__"] 
stepFourHead    = foldlSP_ [S "__Step 4: Kinematic Equations__"]
stepFourOneHead = foldlSP_ [S "__Step 4.1: Horizontal Motion__"]
stepFourTwoHead = foldlSP_ [S "__Step 4.2: Vertical Motion__"]
stepFiveHead    = foldlSP_ [S "__Step 5: Solve for Unknowns__"]

stepOneCont, stepTwoCont, stepThreeCont, stepFourCont, horizMotionEqn1, horizMotionEqn2, vertMotionEqn1, vertMotionEqn2, vertMotionEqn3, verMotionCont, stepFiveCont :: Contents
stepOneCont = enumBulletU $ map foldlSent 
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
    S "The landing point often has a known", P lY +:+ S "value"],
  [S "The variables in the" , plural equation, S "may need to be changed to match the notation of the specific problem.",
    S "For instance, a distinction may need to be made between the", P lX, phrase coordinate `S.of_` S "points", P cA `S.and_` P cB,
    S "via notation like"]]

stepTwoCont = foldlSP [S "Using the notation for the problem in question, write out the known variables and their values.",
  S "The known variables will be a subset of the following:", (eS (sy QP.ixPos) `sC` eS (sy QP.xPos)) `sC` (eS (sy QP.iyPos) `sC` eS (sy QP.yPos)) `sC` 
  (eS (sy QP.ixVel) `sC` eS (sy QP.xVel)) `sC` (eS (sy QP.iyVel) `sC` eS (sy QP.yVel)) `S.and_` P lT,
  S "The knowns should be written in the notation adopted for the particular problem"]

stepThreeCont = foldlSP [S "Each problem will have at most 4 unknowns that need to be determined, selected from the variables listed in the Step 2 that are not known",
  S "The number of relevant unknowns will usually be less than 4, since questions will often focus on one or two unknowns",
  S "As an example, the equation that horizontal", phrase velocity, S "is constant is so trivial that most problems will not look for this as an unknown",
  S "The unknowns should be written in the notation adopted for the particular problem"]

stepFourCont = foldlSP [S "Depending upon the known data and what is to be determined, a choice should be made as to which four of the following five equations" +:+.
  S "should be applied between the two points on the path to obtain the most direct solution to the problem"]

horizMotionEqn1 = foldlSP_ [S "From equation" +: refS lchorizVel, eS horizVel, 
  sParen (S "The *velocity* in the horizontal" `S.or_` P lX +:+ phrase direction +:+ S "is *constant*")]
horizMotionEqn2 = foldlSP_ [S "From equation" +: refS lchorizPos, eS horizPos]

vertMotionEqn1 = foldlSP_ [S "From equation" +: refS lcvertVel, eS vertVel]
vertMotionEqn2 = foldlSP_ [S "From equation" +: refS lcvertPos, eS vertPos]
vertMotionEqn3 = foldlSP_ [S "From equation" +: refS lcvertNoTime, eS vertNoTime]

verMotionCont = foldlSP [S "In the vertical" `S.or_` P lY, phrase direction, S "*only two*" `S.ofThe` S "following three equations", 
  sParen (S "using" +:+ eS (sy QP.yAccel $= neg (sy QP.gravitationalAccel))) +:+. S "can be used for solution",
  sParen (S "The sign" `S.of_` P lG +:+ S "will change to positive if the positive" +:+ P lY +:+. S "axis is downward"),
  S "For example, if the particle's final velocity", eS (sy QP.yVel), S "is not needed, then the first and third of these questions",
  sParen (S "for" +:+ P lY) +:+. S "will not be useful"] 

stepFiveCont = foldlSP [S "Use the equations from Step 4, together with the known values from Step 2 to find the unknown values from Step 3",
  S "We can do this systematically by going through each equation and determining how many unknowns are in that equation",
  S "Any equations with one unknown can be used to solve for that unknown directly"]


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

kinematicContext = enumBulletU
  [foldlSent 
    [S "Depending upon the known data and what is to be determined, a choice should be made",
    S "as to which three of the following four", plural equation, S "should be applied between",
    S "the two points on the path to obtain the most direct solution to the problem"]]

horMotionContext = enumBulletU [foldlSent
    [S "The *velocity* in the horizontal" `S.or_` P lX, phrase direction, S "is *constant*, i.e.,", 
    eS horizVel `S.and_` eS horizPos]]

verMotionContext = enumBulletU $ map foldlSent
  [[S "In the vertical" `S.or_` P lY, phrase direction, 
    S "*only two* of the following three equations can be used for solution"],
   [S "For example, if the particle's final", phrase velocity +:+ eS (sy QP.yVel),
    S "is not needed, then the first and third of these questions", sParen (S "for" +:+ P lY),
    S "will not be useful"]]
