-- | Defines concepts used in the field of math.
module Data.Drasil.Concepts.Math (
    module Data.Drasil.Concepts.Math
  , module Drasil.Metadata.Concepts.Math
  ) where

-- General Drasil
import Drasil.Database (mkUid)
import Language.Drasil hiding (number, norm, matrix, Sentence(P, S, (:+:)))
import qualified Language.Drasil as D
import Language.Drasil.Chunk.Concept.NamedCombinators
import Language.Drasil.Development (NPStruct(P, S, (:-:)))
import Language.Drasil.ShortHands (lX, lY, lZ)

-- Othr Vocabulary
import Drasil.Metadata.Domains (mathematics)
import Drasil.Metadata.Concepts.Math (equation, graph, parameter, unit_)
import Data.Drasil.Citations (cartesianWiki, lineSource, pointSource)
import qualified Language.Drasil.Sentence.Combinators as S

-- | Collects all math-related concepts.
mathcon :: [ConceptChunk]
mathcon = [amplitude, angle, axis, calculation, cartesian, centre, change, component,
  constraint, direction, equation, euclidSpace, graph, laplaceTransform, law, line, matrix,
  norm, normal, number, origin, parameter, perp, positive, negative,
  point, probability, rOfChng, rate, rightHand, shape, unit_, vector,
  xAxis, xCoord, xComp, xDir, yAxis, yCoord, yComp, yDir, zAxis, zCoord, zComp, zDir, iAngle]

-- | Collects all math-related common ideas (like a concept, but with no definition).
mathcon' :: [CI]
mathcon' = [de, leftSide, ode, pde, rightSide]

-- * Mathematical Concepts

amplitude, angle, area, axis, calculation, cartesian, centre, change, component, constraint, diameter,
  direction, euclidN, euclidSpace, gradient, laplaceTransform, law, line, matrix, norm, normal, normalV,
  number, orient, origin, perp, perpV, pi_, negInf, posInf, positive, negative, point, probability,
  rOfChng, rate, rightHand, shape, surArea, surface, unitV, vector, xAxis, xCoord, xComp, xDir,
  yAxis, yCoord,  yComp, yDir, zAxis, zCoord, zComp, zDir, iAngle :: ConceptChunk

amplitude   = cncpt''' (mkUid "amplitude")   (nounPhraseSP "amplitude")  (D.S "The peak deviation of a function from zero")
angle       = cncpt''' (mkUid "angle")       (cn' "angle")               (D.S "the amount of rotation needed to bring one line or plane into coincidence with another")
area        = cncpt''' (mkUid "area")        (cn' "area")                (D.S "a part of an object or surface")
axis        = cncpt''' (mkUid "axis")        (cn' "axis")                (D.S "a fixed reference line for the measurement of coordinates")
calculation = cncpt''' (mkUid "calculation") (cn' "calculation")         (D.S "a mathematical determination of the size or number of something")
cartesian   = dccWDS "cartesian" (pn' "Cartesian coordinate system") $ D.S "a coordinate system that specifies each point uniquely in a plane by a set" `S.of_`
                                                                  D.S "numerical coordinates, which are the signed distances to the point from" +:+
                                                                  D.S "two fixed perpendicular oriented lines, measured in the same unit of length" +:+
                                                                  fromSource cartesianWiki
centre       = cncpt''' (mkUid "centre")        (cn' "centre")           (D.S "the middle point of an object")
change       = cncpt''' (mkUid "change")        (cn' "change")           (D.S "Difference between relative start and end states of an object")
component    = cncpt''' (mkUid "mathComponent") (nounPhrase "component" "components") (D.S ("The scalar quantity defining the contribution " ++
                                                                        "of a vector in one of the coordinate directions"))
constraint   = cncpt''' (mkUid "mathConstraint") (cn' "constraint")      (D.S "A condition that the solution must satisfy")
diameter     = cncpt''' (mkUid "diameter")       (cn' "diameter")        (D.S ("Any straight line segment that passes through the center of the circle" ++
                                                                         "and whose endpoints lie on the circle."))
direction    = cncpt''' (mkUid "direction")      (cn' "direction")       (D.S "'which way' a vector points, extending from the tail to the tip")
euclidSpace  = cncpt''' (mkUid "euclidSpace")    (cn' "Euclidean")       (D.S ("Denoting the system of geometry corresponding to the geometry of ordinary" ++
                                                                         "experience"))
gradient     = cncpt'''  (mkUid "gradient")      (cn' "gradient")        (D.S "degree of steepness of a graph at any point")
laplaceTransform = cncpt''' (mkUid "laplaceTransform") (cn' "laplace transform") (D.S ("An integral transform that converts a function of a real variable t "
                                                                         ++ "(often time) to a function of a complex variable s (complex frequency)"))
law          = cncpt''' (mkUid "law") (cn' "law")                        (D.S "a generalization based on a fact or event perceived to be recurrent")
line         = dccWDS "line"          (pn' "line")                       $ D.S "An interval between two points" +:+ fromSource lineSource
matrix       = cncpt''' (mkUid "matrix") (cnICES "matrix")               (D.S ("A rectangular array of quantities or expressions in rows and columns that" ++
                                                                         "is treated as a single entity and manipulated according to particular rules"))
norm         = cncpt''' (mkUid "norm")         (cn' "norm")              (D.S "the positive length or size of a vector")
normal       = cncpt''' (mkUid "normal")       (cn' "normal" )           (D.S "an object that is perpendicular to a given object")
number       = cncpt''' (mkUid "number")       (cn' "number")            (D.S "a mathematical object used to count, measure, and label")
orient       = cncpt''' (mkUid "orientation")  (cn' "orientation")       (D.S "the relative physical position or direction of something")
origin       = cncpt''' (mkUid "origin")       (cn' "origin")            (D.S "a fixed point of reference for the geometry of the surrounding space")
perp         = cncpt''' (mkUid "perp")         (cn' "perpendicular")     (D.S "At right angles")
pi_          = cncpt''' (mkUid "pi")           (cn' "ratio of circumference to diameter for any circle") (D.S "The ratio of a circle's circumference to its diameter")
posInf       = cncpt''' (mkUid "PosInf")       (cn' "Positive Infinity") (D.S "the limit of a sequence or function that eventually exceeds any prescribed bound")
negInf       = cncpt''' (mkUid "NegInf")       (cn' "Negative Infinity") (D.S "Opposite of positive infinity")
positive     = cncpt''' (mkUid "positive")     (cn' "positive")          (D.S "greater than zero")
negative     = cncpt''' (mkUid "negative")     (cn' "negative")          (D.S "less than zero")
point        = dccWDS "point"     (pn' "point")                   $ D.S "An exact location, it has no size, only position" +:+
                                                                  fromSource pointSource
probability  = cncpt''' (mkUid "probability")  (cnIES "probability")     (D.S "The likelihood of an event to occur")
rate         = cncpt''' (mkUid "rate")         (cn' "rate")              (D.S "Ratio that compares two quantities having different units of measure")
rightHand    = cncpt''' (mkUid "rightHand")    (cn' "right-handed coordinate system") (D.S "A coordinate system where the positive z-axis comes out of the screen")
shape        = cncpt''' (mkUid "shape")        (cn' "shape")             (D.S "The outline of an area or figure")
surface      = cncpt''' (mkUid "surface")      (cn' "surface")           (D.S "The outer or topmost boundary of an object")
vector       = cncpt''' (mkUid "vector")       (cn' "vector")            (D.S "Object with magnitude and direction")

xAxis = cncpt''' (mkUid "xAxis") (nounPhraseSent $ P lX :-: S "-axis")   (D.S "the primary axis of a system of coordinates")
yAxis = cncpt''' (mkUid "yAxis") (nounPhraseSent $ P lY :-: S "-axis")   (D.S "the secondary axis of a system of coordinates")
zAxis = cncpt''' (mkUid "zAxis") (nounPhraseSent $ P lZ :-: S "-axis")   (D.S "the tertiary axis of a system of coordinates")

xCoord = cncpt''' (mkUid "xCoord") (nounPhraseSent $ P lX :-: S "-coordinate") (D.S "the location of the point on the x-axis")
yCoord = cncpt''' (mkUid "yCoord") (nounPhraseSent $ P lY :-: S "-coordinate") (D.S "the location of the point on the y-axis")
zCoord = cncpt''' (mkUid "zCoord") (nounPhraseSent $ P lZ :-: S "-coordinate") (D.S "the location of the point on the z-axis")

xComp = cncpt''' (mkUid "xComp") (nounPhraseSent $ P lX :-: S "-component") (D.S "the component of a vector in the x-direction")
yComp = cncpt''' (mkUid "yComp") (nounPhraseSent $ P lY :-: S "-component") (D.S "the component of a vector in the y-direction")
zComp = cncpt''' (mkUid "zComp") (nounPhraseSent $ P lZ :-: S "-component") (D.S "the component of a vector in the z-direction")

xDir = cncpt''' (mkUid "xDir") (nounPhraseSent $ P lX :-: S "-direction") (D.S "the direction aligned with the x-axis")
yDir = cncpt''' (mkUid "yDir") (nounPhraseSent $ P lY :-: S "-direction") (D.S "the direction aligned with the y-axis")
zDir = cncpt''' (mkUid "zDir") (nounPhraseSent $ P lZ :-: S "-direction") (D.S "the direction aligned with the z-axis")
iAngle = cncpt''' (mkUid "iAngle") (cn "initial angle")                   (D.S "The initial angle where the body is being displaced")

de, leftSide, ode, pde, rightSide :: CI
--FIXME: use nounphrase instead of cn'
de  = commonIdeaWithDict (mkUid "de")  (cn' "differential equation")          "DE"  [mathematics]
ode = commonIdeaWithDict (mkUid "ode") (cn' "ordinary differential equation") "ODE" [mathematics]
pde = commonIdeaWithDict (mkUid "pde") (cn' "partial differential equation")  "PDE" [mathematics]

leftSide  = commonIdeaWithDict (mkUid "leftSide")  (nounPhrase "left-hand side"  "left-hand sides" ) "LHS" [mathematics]
rightSide = commonIdeaWithDict (mkUid "rightSide") (nounPhrase "right-hand side" "right-hand sides") "RHS" [mathematics]

--FIXME: COMBINATION HACK (all below)
-- do we really need fterms here? Would combineNINI work?
euclidN = cncpt''' (mkUid "euclidNorm")    (combineNINI euclidSpace norm) (D.S "euclidean norm")
normalV = cncpt''' (mkUid "normal vector") (combineNINI normal vector)    (D.S "unit outward normal vector for a surface")
perpV   = cncpt''' (mkUid "perp_vect")     (combineNINI perp vector)      (D.S "vector perpendicular or 90 degrees to another vector")
rOfChng = cncpt''' (mkUid "rOfChng")       (rate `of_` change) (D.S "ratio between a change in one variable relative to a corresponding change in another")
surArea = cncpt''' (mkUid "surArea")       (combineNINI surface area)     (D.S "a measure of the total area that the surface of the object occupies")
unitV   = cncpt''' (mkUid "unit_vect")     (combineNINI unit_ vector)     (D.S "a vector that has a magnitude of one")
