{-# LANGUAGE PackageImports #-}

-- | Defines concepts used in the field of math.
module Data.Drasil.Concepts.Math where

import Language.Drasil hiding (number, norm, matrix)
import Data.Drasil.Domains (mathematics)
import Data.Drasil.Citations (cartesianWiki, lineSource, pointSource)
import qualified Language.Drasil.Sentence.Combinators as S

import qualified "drasil-metadata" Data.Drasil.Concepts.Math as M

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

  
amplitude   = cc M.amplitude   "The peak deviation of a function from zero"
angle       = cc M.angle       "the amount of rotation needed to bring one line or plane into coincidence with another"
area        = cc M.area        "a part of an object or surface"
axis        = cc M.axis        "a fixed reference line for the measurement of coordinates" 
calculation = cc M.calculation "a mathematical determination of the size or number of something"
cartesian   = cc' M.cartesian  (S "a coordinate system that specifies each point uniquely in a plane by a set" `S.of_`
                                                                  S "numerical coordinates, which are the signed distances to the point from" +:+
                                                                  S "two fixed perpendicular oriented lines, measured in the same unit of length" +:+
                                                                  fromSource cartesianWiki)
centre       = cc M.centre    "the middle point of an object"
change       = cc M.change    "Difference between relative start and end states of an object"
component    = cc M.component ("The scalar quantity defining the contribution " ++
                                                                  "of a vector in one of the coordinate directions")
constraint   = cc M.constraint "A condition that the solution must satisfy"
diameter     = cc M.diameter   ("Any straight line segment that passes through the center of the circle" ++
                                                                  "and whose endpoints lie on the circle.")
direction    = cc M.direction  "'which way' a vector points, extending from the tail to the tip"
equation     = cc M.equation   "A statement that the values of two mathematical expressions are equal "
euclidSpace  = cc M.euclidSpace ("Denoting the system of geometry corresponding to the geometry of ordinary" ++
                                                                  "experience")
gradient     = cc M.gradient             "degree of steepness of a graph at any point"
graph        = cc M.graph                "A diagram showing the relation between variable quantities"
laplaceTransform = cc M.laplaceTransform ("An integral transform that converts a function of a real variable t " ++
                                                                     "(often time) to a function of a complex variable s (complex frequency)")
law          = cc M.law   "a generalization based on a fact or event perceived to be recurrent"
line         = cc' M.line $ S "An interval between two points" +:+
                                                                  fromSource lineSource
matrix       = cc M.matrix ("A rectangular array of quantities or expressions in rows and columns that" ++
                                                                 "is treated as a single entity and manipulated according to particular rules")
norm        = cc M.norm     "the positive length or size of a vector"
normal      = cc M.normal   "an object that is perpendicular to a given object"
number      = cc M.number   "a mathematical object used to count, measure, and label"
orient      = cc M.orient   "the relative physical position or direction of something"
origin      = cc M.origin   "a fixed point of reference for the geometry of the surrounding space"
parameter   = cc M.parameter "a quantity whose value is selected depending on particular circumstances"
--FIXME: Should "parameter" be in math?
perp         = cc M.perp     "At right angles"
pi_          = cc M.pi_      "The ratio of a circle's circumference to its diameter"
posInf       = cc M.posInf   "the limit of a sequence or function that eventually exceeds any prescribed bound"
negInf       = cc M.negInf   "Opposite of positive infinity"
positive     = cc M.positive "greater than zero"
negative     = cc M.negative "less than zero"
point        = cc' M.point   $ S "An exact location, it has no size, only position" +:+
                                                                  fromSource pointSource
probability  = cc M.probability "The likelihood of an event to occur"
rate         = cc M.rate "Ratio that compares two quantities having different units of measure"
rightHand    = cc M.rightHand "A coordinate system where the positive z-axis comes out of the screen"
shape        = cc M.shape "The outline of an area or figure"
surface      = cc M.surface "The outer or topmost boundary of an object"
unit_        = cc M.unit_ "Identity element"
vector       = cc M.vector "Object with magnitude and direction"

xAxis = cc M.xAxis "the primary axis of a system of coordinates"
yAxis = cc M.yAxis "the secondary axis of a system of coordinates"
zAxis = cc M.zAxis "the tertiary axis of a system of coordinates"

xCoord = cc M.xCoord "the location of the point on the x-axis"
yCoord = cc M.yCoord "the location of the point on the y-axis"
zCoord = cc M.zCoord "the location of the point on the z-axis"

xComp = cc M.xComp "the component of a vector in the x-direction"
yComp = cc M.yComp "the component of a vector in the y-direction"
zComp = cc M.zComp "the component of a vector in the z-direction"

yDir = cc M.yDir "the direction aligned with the y-axis"
zDir = cc M.zDir "the direction aligned with the z-axis"
xDir = cc M.xDir "the direction aligned with the x-axis"
iAngle = cc M.iAngle "The initial angle where the body is being displaced"

de, leftSide, ode, pde, rightSide :: CI
--FIXME: use nounphrase instead of cn'
de  = commonIdeaWithDict "de"  (cn' "differential equation")          "DE"  [mathematics]
ode = commonIdeaWithDict "ode" (cn' "ordinary differential equation") "ODE" [mathematics]
pde = commonIdeaWithDict "pde" (cn' "partial differential equation")  "PDE" [mathematics]

leftSide  = commonIdeaWithDict "leftSide"  (nounPhrase "left-hand side"  "left-hand sides" ) "LHS" [mathematics]
rightSide = commonIdeaWithDict "rightSide" (nounPhrase "right-hand side" "right-hand sides") "RHS" [mathematics]

--FIXME: COMBINATION HACK (all below)
-- do we really need fterms here? Would combineNINI work?
euclidN = cc M.euclidN "euclidean norm"
normalV = cc M.normalV "unit outward normal vector for a surface"
perpV   = cc M.perpV "vector perpendicular or 90 degrees to another vector"
rOfChng = cc M.rOfChng "ratio between a change in one variable relative to a corresponding change in another"
surArea = cc M.surArea "a measure of the total area that the surface of the object occupies"
unitV   = cc M.unitV "a vector that has a magnitude of one"
