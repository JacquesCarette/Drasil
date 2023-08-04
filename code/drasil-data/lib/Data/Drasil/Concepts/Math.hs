-- | Defines concepts used in the field of math.
module Data.Drasil.Concepts.Math where

import Language.Drasil hiding (number, norm, matrix)
import Language.Drasil.ShortHands (lX, lY, lZ)
import Data.Drasil.Domains (mathematics)
import Data.Drasil.Citations (cartesianWiki, lineSource, pointSource)
import qualified Language.Drasil.Sentence.Combinators as S
import Language.Drasil.Chunk.Concept.NamedCombinators

-- | Collects all math-related concepts.
mathcon :: [ConceptChunk]
mathcon = [amplitude, angle, area, axis, calculation, cartesian, centre, change, component,
  constraint, diameter, direction, equation, euclidN, euclidSpace, gradient, graph, laplaceTransform, law, line, matrix,
  norm, normal, normalV, number, orient, origin, parameter, perp, perpV, pi_, negInf, posInf, positive, negative,
  point, probability, rOfChng, rate, rightHand, shape, surArea, surface, unitV, unit_, vector, 
  xAxis, xCoord, xComp, xDir, yAxis, yCoord, yComp, yDir, zAxis, zCoord, zComp, zDir, iAngle]

-- | Collects all math-related common ideas (like a concept, but with no definition).
mathcon' :: [CI]
mathcon' = [de, leftSide, ode, pde, rightSide]

-- * Mathematical Concepts

amplitude, angle, area, axis, calculation, cartesian, centre, change, component, constraint, diameter,
  direction, equation, euclidN, euclidSpace, gradient, graph, laplaceTransform, law, line, matrix, norm, normal, normalV, 
  number, orient, origin, parameter, perp, perpV, pi_, negInf, posInf, positive, negative, point, probability,
  rOfChng, rate, rightHand, shape, surArea, surface, unitV, unit_, vector, xAxis, xCoord, xComp, xDir,
  yAxis, yCoord,  yComp, yDir, zAxis, zCoord, zComp, zDir, iAngle :: ConceptChunk

  
amplitude   = dcc "amplitude"    (nounPhraseSP "amplitude")      "The peak deviation of a function from zero"
angle       = dcc "angle"        (cn' "angle")                   "the amount of rotation needed to bring one line or plane into coincidence with another"
area        = dcc "area"         (cn' "area")                    "a part of an object or surface"
axis        = dcc "axis"         (cn' "axis")                    "a fixed reference line for the measurement of coordinates" 
calculation = dcc "calculation"  (cn' "calculation")             "a mathematical determination of the size or number of something"
cartesian   = dccWDS "cartesian" (pn' "Cartesian coordinate system") $ S "a coordinate system that specifies each point uniquely in a plane by a set" `S.of_`
                                                                  S "numerical coordinates, which are the signed distances to the point from" +:+
                                                                  S "two fixed perpendicular oriented lines, measured in the same unit of length" +:+
                                                                  fromSource cartesianWiki
centre       = dcc "centre"       (cn' "centre")                  "the middle point of an object"
change       = dcc "change"       (cn' "change")                  "Difference between relative start and end states of an object"
component    = dcc "component"    (nounPhrase "component" "components") ("The scalar quantity defining the contribution " ++
                                                                  "of a vector in one of the coordinate directions")
constraint   = dcc "constraint"   (cn' "constraint")              "A condition that the solution must satisfy"
diameter     = dcc "diameter"     (cn' "diameter")                ("Any straight line segment that passes through the center of the circle" ++
                                                                  "and whose endpoints lie on the circle.")
direction    = dcc "direction"    (cn' "direction")               "'which way' a vector points, extending from the tail to the tip"
equation     = dcc "equation"     (cn' "equation")                "A statement that the values of two mathematical expressions are equal "
euclidSpace  = dcc "euclidSpace"  (cn' "Euclidean")               ("Denoting the system of geometry corresponding to the geometry of ordinary" ++
                                                                  "experience")
gradient     = dcc "gradient"     (cn' "gradient")                "degree of steepness of a graph at any point"
graph        = dcc "graph"        (cn' "graph")                   "A diagram showing the relation between variable quantities"
laplaceTransform = dcc "laplaceTransform" (cn' "laplace transform") ("An integral transform that converts a function of a real variable t " ++
                                                                     "(often time) to a function of a complex variable s (complex frequency)")
law          = dcc "law"          (cn' "law")                     "a generalization based on a fact or event perceived to be recurrent"
line         = dccWDS "line"      (pn' "line")                    $ S "An interval between two points" +:+
                                                                  fromSource lineSource
matrix       = dcc "matrix"       (cnICES "matrix")               ("A rectangular array of quantities or expressions in rows and columns that" ++
                                                                 "is treated as a single entity and manipulated according to particular rules")
norm        = dcc "norm"         (cn' "norm")                    "the positive length or size of a vector"
normal      = dcc "normal"       (cn' "normal" )                 "an object that is perpendicular to a given object"
number      = dcc "number"       (cn' "number")                  "a mathematical object used to count, measure, and label"
orient      = dcc "orientation"  (cn' "orientation")             "the relative physical position or direction of something"
origin      = dcc "origin"       (cn' "origin")                  "a fixed point of reference for the geometry of the surrounding space"
parameter   = dcc "parameter"    (cn' "parameter")               "a quantity whose value is selected depending on particular circumstances"
--FIXME: Should "parameter" be in math?
perp         = dcc "perp"         (cn' "perpendicular")          "At right angles"
pi_          = dcc "pi"           (cn' "ratio of circumference to diameter for any circle") "The ratio of a circle's circumference to its diameter"
posInf       = dcc "PosInf"       (cn' "Positive Infinity")      "the limit of a sequence or function that eventually exceeds any prescribed bound"
negInf       = dcc "NegInf"       (cn' "Negative Infinity")      "Opposite of positive infinity"
positive     = dcc "positive"     (cn' "positive")               "greater than zero"
negative     = dcc "negative"     (cn' "negative")               "less than zero"
point        = dccWDS "point"     (pn' "point")                   $ S "An exact location, it has no size, only position" +:+
                                                                  fromSource pointSource
probability  = dcc "probability"  (cnIES "probability")          "The likelihood of an event to occur"
rate         = dcc "rate"         (cn' "rate")                   "Ratio that compares two quantities having different units of measure"
rightHand    = dcc "rightHand"    (cn' "right-handed coordinate system")  "A coordinate system where the positive z-axis comes out of the screen."
shape        = dcc "shape"        (cn' "shape")                  "The outline of an area or figure"
surface      = dcc "surface"      (cn' "surface")                "The outer or topmost boundary of an object"
unit_        = dcc "unit"         (cn' "unit")                   "Identity element"
vector       = dcc "vector"       (cn' "vector")                 "Object with magnitude and direction"

xAxis = dcc "xAxis" (nounPhraseSent $ P lX :+: S "-axis") "the primary axis of a system of coordinates"
yAxis = dcc "yAxis" (nounPhraseSent $ P lY :+: S "-axis") "the secondary axis of a system of coordinates"
zAxis = dcc "zAxis" (nounPhraseSent $ P lZ :+: S "-axis") "the tertiary axis of a system of coordinates"

xCoord = dcc "xCoord" (nounPhraseSent $ P lX :+: S "-coordinate") "the location of the point on the x-axis"
yCoord = dcc "yCoord" (nounPhraseSent $ P lY :+: S "-coordinate") "the location of the point on the y-axis"
zCoord = dcc "zCoord" (nounPhraseSent $ P lZ :+: S "-coordinate") "the location of the point on the z-axis"

xComp = dcc "xComp" (nounPhraseSent $ P lX :+: S "-component") "the component of a vector in the x-direction"
yComp = dcc "yComp" (nounPhraseSent $ P lY :+: S "-component") "the component of a vector in the y-direction"
zComp = dcc "zComp" (nounPhraseSent $ P lZ :+: S "-component") "the component of a vector in the z-direction"

xDir = dcc "xDir" (nounPhraseSent $ P lX :+: S "-direction") "the direction aligned with the x-axis"
yDir = dcc "yDir" (nounPhraseSent $ P lY :+: S "-direction") "the direction aligned with the y-axis"
zDir = dcc "zDir" (nounPhraseSent $ P lZ :+: S "-direction") "the direction aligned with the z-axis"
iAngle = dcc "iAngle" (cn "initial angle")                      "The initial angle where the body is being displaced"

de, leftSide, ode, pde, rightSide :: CI
--FIXME: use nounphrase instead of cn'
de  = commonIdeaWithDict "de"  (cn' "differential equation")          "DE"  [mathematics]
ode = commonIdeaWithDict "ode" (cn' "ordinary differential equation") "ODE" [mathematics]
pde = commonIdeaWithDict "pde" (cn' "partial differential equation")  "PDE" [mathematics]

leftSide  = commonIdeaWithDict "leftSide"  (nounPhrase "left-hand side"  "left-hand sides" ) "LHS" [mathematics]
rightSide = commonIdeaWithDict "rightSide" (nounPhrase "right-hand side" "right-hand sides") "RHS" [mathematics]

--FIXME: COMBINATION HACK (all below)
-- do we really need fterms here? Would combineNINI work?
euclidN = dcc "euclidNorm"    (combineNINI euclidSpace norm) "euclidean norm"
normalV = dcc "normal vector" (combineNINI normal vector) "unit outward normal vector for a surface"
perpV   = dcc "perp_vect"     (combineNINI perp vector) "vector perpendicular or 90 degrees to another vector"
rOfChng = dcc "rOfChng"       (rate `of_` change) "ratio between a change in one variable relative to a corresponding change in another"
surArea = dcc "surArea"       (combineNINI surface area) "a measure of the total area that the surface of the object occupies"
unitV   = dcc "unit_vect"     (combineNINI unit_ vector) "a vector that has a magnitude of one"
