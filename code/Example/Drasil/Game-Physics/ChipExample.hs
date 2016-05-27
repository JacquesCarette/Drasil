{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
module ChipExample where
import ASTInternal (Expr(..), Relation(..))
import SI_Units
import Unicode (Tau(..), Delta(..), Rho(..), Nabla(..), Phi(..),
                Alpha(..), Omega(..), Zeta(..))
import Symbol
import UnitalChunk
import ChipUnits
import SymbolAlphabet
import Chunk (ConceptChunk(..),VarChunk,symbol,makeCC,makeVC)
-- import EqChunk (fromEqn,EqChunk(..))
import Spec (Sentence(..))
import Control.Lens ((^.))
import RelationChunk
import Instances ()

-- import Control.Lens ((^.))

accel, angAccel, restCoeff, disp, prt_axisDist, force, gravAccel,        gravConst, momtInert, impulse, springConst, len, mass, angVel, initPos, finalPos, cmPos, forcePos, orientation, frc_axisDist, density, time, torque, initVel, finalVel, volume, springDisp, dampCoeff, mass_1, mass_2, force_1, force_2, centreDist, centreDisp, sqrDist :: UnitalChunk

chipSymbols :: [UnitalChunk]
chipSymbols = [accel, angAccel, restCoeff, disp, prt_axisDist, force,
  gravAccel, gravConst, momtInert, impulse, springConst, len, mass, angVel, initPos, finalPos, cmPos, forcePos, orientation, frc_axisDist, density, time, torque, initVel, finalVel, volume, springDisp, dampCoeff]

accel       = makeUC "a" "acceleration" lA accelU
angAccel    = makeUC "alpha" "angular acceleration" (Special Alpha_L) angAccelU
restCoeff   = makeUC "C_R" "coefficient of restitution" (sub cC cR) unitless
disp        = makeUC "d" "displacement" lD metre
prt_axisDist = makeUC "d_p" "distance between particle and the axis of rotation"
                      (sub lD lP) metre
force       = makeUC "F" "force" (vec cF) newton

-- crude fixes for Newton's Third Law
force_1     = makeUC "F_1" "force on second object by first" (sub (vec cF) (Atomic "1")) newton
force_2     = makeUC "F_2" "force on first object by second" (sub (vec cF) (Atomic "2")) newton
-- end crude fixes

gravAccel   = makeUC "g" "acceleration due to gravity" lG newton
gravConst   = makeUC "G" "gravitational constant" cG gravConstU
momtInert   = makeUC "I" "moment of inertia" (vec cI) momtInertU
impulse     = makeUC "J" "impulse" (vec cJ) impulseU
springConst = makeUC "k" "spring constant" lK springConstU
len         = makeUC "L" "length" cL metre

-- crude fixes for Newton's Universal Law of Gravitation
mass        = makeUC "m" "mass" lM kilogram
mass_1      = makeUC "m_1" "mass_1" (sub lM (Atomic "1")) kilogram
mass_2      = makeUC "m_2" "mass_2" (sub lM (Atomic "2")) kilogram
centreDist = makeUC "||r||" "distance between the centre of two objects"
              (Atomic "||r||") metre
sqrDist     = makeUC "||r||^2" ("square of the distance between the centre " ++
              "of two objects") (sup (Atomic "||r||") (Atomic "2")) m_2
centreDisp = makeUC "r" ("displacement vector between the centre of " ++
               "two objects") (vec lR) metre
-- end crude fixes

angVel      = makeUC "omega" "angular velocity" (Special Omega_L) angVelU
initPos     = makeUC "p_i" "initial position" (sub (vec lP) lI) metre
finalPos    = makeUC "p_f" "final position" (sub (vec lP) lF) metre
cmPos       = makeUC "p_com" "position of centre of mass"
                     (sub (vec lP) (Atomic "com")) metre
forcePos    = makeUC "p_force" ("position the force vector is acting on the" ++
                     " object") (sub (vec lP) (Atomic "force")) metre
orientation = makeUC "phi" "orientation" (Special Phi_L) radians
frc_axisDist = makeUC "r" "distance between the force and the axis of rotation"
                      lR metre
density     = makeUC "rho" "density" (Special Rho_L) densityU
time        = makeUC "t" "time" lT second
torque      = makeUC "tau" "torque" (Special Tau_L) torqueU
initVel     = makeUC "v_i" "initial velocity" (sub (vec lV) lI) velU
finalVel    = makeUC "v_f" "final velocity" (sub (vec lV) lF) velU
volume      = makeUC "V" "volume" cV m_3
springDisp  = makeUC "X" "displacement of spring from equilibrium" cX metre
dampCoeff   = makeUC "zeta" "damping coefficient" (Special Zeta_L) unitless

{-
----VarChunks----
gradient :: VarChunk
gradient = makeVC "gradient" "the gradient operator" (Special Nabla)
-}

----Acronyms-----
assumption, centreMass, dataDefn, genDefn, goalStmt, instanceMod, likelyChange, oDE, physSysDescr, requirement, softwareRS, theoreticMod :: ConceptChunk

acronyms :: [ConceptChunk]
acronyms = [assumption, centreMass, dataDefn, genDefn, goalStmt, instanceMod, likelyChange, physSysDescr, oDE, requirement, softwareRS, theoreticMod]

assumption    = makeCC "A" "Assumption"
centreMass    = makeCC "COM" "Centre of Mass"
dataDefn      = makeCC "DD" "Data Definition"
genDefn       = makeCC "GD" "General Definition"
goalStmt      = makeCC "GS"  "Goal Statement"
instanceMod   = makeCC "IM" "Instance Model"
likelyChange  = makeCC "LC" "Likely Change"
oDE           = makeCC "ODE" "Ordinary Differential Equation"
physSysDescr  = makeCC "PS" "Physical System Description"
requirement   = makeCC "R" "Requirement"
softwareRS    = makeCC "SRS" "Software Requirements Specification"
theoreticMod  = makeCC "T" "Theoretical Model"

----EqChunks----

--Theoretical models--

--Newton's Second Law--

t1newtonSndLaw :: RelationChunk
t1newtonSndLaw = makeRC "Newton's second law of motion" t1descr snd_law_rel

snd_law_rel :: Relation
snd_law_rel = (C force) := (C mass) * (C accel)

t1descr :: Sentence
t1descr = S "The net force on an object is proportional to the object's " :+:
  S "acceleration."

--Newton's Third Law--

t2newtonThdLaw :: RelationChunk
t2newtonThdLaw = makeRC "Newton's third law of motion" t2descr thd_law_rel

thd_law_rel :: Relation
thd_law_rel = (C force_1) := (Neg (C force_2))

t2descr :: Sentence
t2descr = S "Every action has an equal and opposite reaction."

-- Newton's Law of Universal Gravitation --

t3newtonUnivGravLaw :: RelationChunk
t3newtonUnivGravLaw = makeRC "Newton's law of universal gravitation" t3descr grav_law_rel

grav_law_rel :: Relation
grav_law_rel = (C force) := (C gravConst) *
               (((C mass_1) * (C mass_2) * (C centreDisp)) /
                ((C sqrDist) * (C centreDist)))

t3descr :: Sentence
t3descr = S "Any two bodies in the universe attract each other with a " :+:
  S "force that is directly proportional to the product of their masses " :+:
  S "and inversely proportional to the square of the distance between them."

-- Hooke's Law --

t4hookeLaw :: RelationChunk
t4hookeLaw = makeRC "Hooke's law" t4descr hooke_law_rel

hooke_law_rel :: Relation
hooke_law_rel = (C force) := (C springConst) * (C springDisp)

t4descr :: Sentence
t4descr = S "The force needed to extend or compress a spring by some " :+:
  S "distance is proportional to that distance."

-- Equation for Rotational Motion --

t5rotMotionEq :: RelationChunk
t5rotMotionEq = makeRC "Equation for rotational motion" t5descr rotation_eq_rel

rotation_eq_rel :: Relation
rotation_eq_rel = (C torque) := (C momtInert) * (C angAccel)

t5descr :: Sentence
t5descr = S "The net torque on an object is proportional to its angular " :+:
  S "acceleration."


{-
PCM Example kept for reference:

t1consThermE :: RelationChunk
t1consThermE = makeRC "Conservation of thermal energy" t1descr cons_therm_rel

cons_therm_rel :: Relation
cons_therm_rel = (Neg (C gradient)) :. (C thFluxVect) + (C ht_gen_vol) :=
  (C density) * (C htCap) * (Deriv (C temp) (C time))

t1descr :: Sentence
t1descr =
  (S ("This equation gives the conservation of energy for time varying heat " ++
  "transfer in a material of specific heat capacity ") :+:
  (U $ htCap ^. symbol) :+: S " and density " :+: (U $ density ^. symbol) :+:
  S ", where " :+: (U $ thFluxVect ^. symbol))
  --TODO: Finish this description and do it better. I need to
  --  figure out the best way to encode this information.
-}
