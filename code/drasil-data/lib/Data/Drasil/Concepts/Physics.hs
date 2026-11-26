-- | Defines concepts used in the field of physics.
module Data.Drasil.Concepts.Physics where
--This is obviously a bad name, but for now it will do until we come
--  up with a better one.
import Drasil.Database (mkUid)
import Language.Drasil hiding (space)
import Language.Drasil.Document (fromSource)
import qualified Language.Drasil.Sentence.Combinators as S

import Drasil.Metadata.Domains (mathematics, physics)
import Data.Drasil.Concepts.Documentation (property, value)
import Control.Lens((^.)) --need for parametrization hack
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (mass)
import Data.Drasil.Citations (dampingSource)
import Data.Drasil.Concepts.Education (mechanics)

-- | Collects all physics-related concepts.
physicCon :: [ConceptChunk]
physicCon = [acceleration, angular,
  collision, compression, constAccel, damping, dampingCoeff,
  displacement, distance, elasticity, energy, fbd, force,
  friction, gravity,
  impulseV, isotropy,
  joint, linear, mechEnergy,
  position, rectilinear,
  rigidBody, shm, space, speed, stiffCoeff, stress,
  time, velocity,
  pendulum, body, kinematics, motion, horizontalMotion, verticalMotion]

-- | Collects all physics-related common ideas (like a concept, but with no definition).
physicCon' :: [CI]
physicCon' = [oneD, twoD, threeD]

-- * Physics Concepts
--
-- $PhysicCon
--
-- In alphabetical order.

acceleration, angular,
  collision, compression, constAccel, damping, dampingCoeff, displacement,
  distance, elasticity, energy, fbd, force, friction, gravity,
  impulseV, isotropy, joint,
  linear, mechEnergy, position,
  rectilinear, rigidBody, shm,
  space, speed, stiffCoeff, stress, time, velocity,
  pendulum, body,
  kinematics, motion, horizontalMotion, verticalMotion :: ConceptChunk

oneD, twoD, threeD :: CI
oneD   = commonIdea (mkUid "oneD")   (cn "one-dimensional")   "1D" [mathematics, physics]
twoD   = commonIdea (mkUid "twoD")   (cn "two-dimensional")   "2D" [mathematics, physics]
threeD = commonIdea (mkUid "threeD") (cn "three-dimensional") "3D" [mathematics, physics]

acceleration = cncpt''' (mkUid "acceleration") (cn' "acceleration")
  (S "the rate of change of a body's" +:+ phrase velocity)
angular = cncpt''' (mkUid "angular") (cn' "angular")
  (S "denoting physical properties or quantities measured with reference to or by means of an angle")
body = cncpt''' (mkUid "physicsBody") (cnIES "body")
  (S "an object with" +:+ phrase QPP.mass)
collision = cncpt''' (mkUid "collision") (cn' "collision")
  (S "an encounter between particles resulting in an exchange or transformation of energy")
compression = cncpt''' (mkUid "compression") (cn' "compression")
  (S "a" +:+ phrase stress +:+ S "that causes displacement of the body towards its center")
damping = cncpt''' (mkUid "damping") (pn' "damping")
  $ S "an influence within or upon an oscillatory system that has the effect of reducing," +:+
  S "restricting or preventing its oscillations" +:+ fromSource dampingSource
dampingCoeff = cncpt''' (mkUid "dampingCoeff") (cn' "damping coefficient")
 (S "Quantity that characterizes a second order system's oscillatory response")
displacement = cncpt''' (mkUid "displacement") (cn' "displacement")
  (S "the change in" +:+ (position ^. defn))
distance = cncpt''' (mkUid "distance") (cn' "distance")
  (S "the interval measured along a path connecting two locations")
elasticity = cncpt''' (mkUid "elasticity") (cnIES "elasticity")
  (S "the ratio of the relative velocities of two colliding objects after and before a collision")
energy = cncpt''' (mkUid "energy") (cn "energy")
  (S "power derived from the utilization of physical or chemical resources")
fbd = cncpt''' (mkUid "FBD") (cn' "free body diagram")
  (S ("a graphical illustration used to visualize the applied forces, movements, and resulting " <>
   "reactions on a body in a steady state condition"))
force = cncpt''' (mkUid "force") (cn' "force")
  (S "an interaction that tends to produce change in the motion of an object")
friction = cncpt''' (mkUid "friction") (cn' "friction")
  (S "the force resisting the relative motion of two surfaces")
gravity = cncpt''' (mkUid "gravity") (cn "gravity")
  (S "the force that attracts one physical body with mass to another")
horizontalMotion = cncpt''' (mkUid "horizontalMotion") (cn "horizontal motion")
  (S "the result of the tendency of any object in motion to remain in motion at constant velocity")
isotropy = cncpt''' (mkUid "isotropy") (cn "isotropy")
  (S "a condition where the" +:+ (phrase value `S.ofA` phrase property) `S.is`
   S "independent of the direction in which it is measured")
joint = cncpt''' (mkUid "joint") (cn' "joint")
  (S "a connection between two rigid bodies which allows movement with one or more degrees of freedom")
kinematics = cncpt''' (mkUid "kinematics") (cn "kinematics")
  (S "branch" `S.of_` phrase mechanics +:+ S "that describes the motion" `S.of_`
    S "objects without reference to the causes of motion")
linear = cncpt''' (mkUid "linear") (cn' "linear")
  (S "arranged in or extending along a straight or nearly straight line")
mechEnergy = cncpt''' (mkUid "mechEnergy") (cn "mechanical energy")
  (S "the energy that comes from motion and position")
motion = cncpt''' (mkUid "motion") (cn "motion")
  (S "change in position of a physical body over time")
pendulum = cncpt''' (mkUid "pendulum") (cn "pendulum")
 (S "a body suspended from a fixed support so that it swings freely back and forth under the influence"
       `S.of_` phrase gravity)
position = cncpt''' (mkUid "position") (cn' "position")
  (S "an object's location relative to a reference point")
rectilinear = cncpt''' (mkUid "rectilinear") (cn "rectilinear")
  (S "occurring in one dimension")
rigidBody = cncpt''' (mkUid "rigidBody") (cnIES "rigid body")
  (S "a solid body in which deformation is neglected")
space = cncpt''' (mkUid "space") (cn' "space")
  (S "a two-dimensional extent where objects and events have relative positions and directions")
shm = cncpt''' (mkUid "SHM") (nounPhraseSP "simple harmonic motion") (S ("Periodic motion through an equilibrium position. " <>
                                                        "The motion is sinusoidal in time and demonstrates a" <>
                                                        " single resonant frequency")) -- source: Wikipedia
speed = cncpt''' (mkUid "speed") (cn' "speed")
  (S "magnitude" `S.the_ofThe` phrase velocity +:+ S "vector")
stiffCoeff = cncpt''' (mkUid "stiffnessCoeff") (cn' "stiffness coefficient")
  (S "Quantity that characterizes a spring's stiffness")
stress = cncpt''' (mkUid "stress") (cn''' "stress")
  (S "the ratio of an applied force to a cross-sectional area")
  --definition of stress used in SSP, can be made clearer
time = cncpt''' (mkUid "time") (cn' "time")
  (S "the indefinite continued progress of existence and events in the past, present, and future regarded as a whole")
velocity = cncpt''' (mkUid "velocity") (cnIES "velocity")
  (S "the rate of change of a body's" +:+ phrase position)
verticalMotion = cncpt''' (mkUid "verticalMotion") (cn "vertical motion")
  (S " the movement of the object against the gravitational pull")

--FIXME: COMBINATION HACK (for all below)
--FIXME: should use compoundPhrase instead? Or better yet, use combineNINI instead of interacting with the terms directly?
constAccel = cncpt''' (mkUid "constantAcceleration") (cn "constant acceleration")
  (S "a one-dimensional acceleration that is constant")

--FIXME: These two should be built off "impulse"
impulseV = cncpt''' (mkUid "impulseV") (cn "impulse (vector)")
  (S "a force acting briefly on a body and producing a finite change of momentum in a given direction")
