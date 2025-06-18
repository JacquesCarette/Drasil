-- | Defines concepts used in the field of math.
module Data.Drasil.Concepts.Math where

import Language.Drasil hiding (number, norm, matrix)
import Language.Drasil.ShortHands (lX, lY, lZ)
import Data.Drasil.Domains (mathematics)
import Language.Drasil.Chunk.Concept.NamedCombinators

-- | Collects all math-related concepts.
mathcon :: [IdeaDict]
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
  yAxis, yCoord,  yComp, yDir, zAxis, zCoord, zComp, zDir, iAngle :: IdeaDict

  
amplitude   = nc "amplitude"    (nounPhraseSP "amplitude")
angle       = nc "angle"        (cn' "angle")
area        = nc "area"         (cn' "area")
axis        = nc "axis"         (cn' "axis") 
calculation = nc "calculation"  (cn' "calculation")
cartesian   = nc "cartesian" (pn' "Cartesian coordinate system")
centre       = nc "centre"       (cn' "centre")
change       = nc "change"       (cn' "change")
component    = nc "mathComponent"    (nounPhrase "component" "components")
constraint   = nc "mathConstraint" (cn' "constraint")
diameter     = nc "diameter"     (cn' "diameter")
direction    = nc "direction"    (cn' "direction")
equation     = nc "equation"     (cn' "equation")
euclidSpace  = nc "euclidSpace"  (cn' "Euclidean")
gradient     = nc "gradient"     (cn' "gradient")
graph        = nc "graph"        (cn' "graph")
laplaceTransform = nc "laplaceTransform" (cn' "laplace transform")
law          = nc "law"          (cn' "law")
line         = nc "line"      (pn' "line")
matrix       = nc "matrix"       (cnICES "matrix")
norm        = nc "norm"         (cn' "norm")
normal      = nc "normal"       (cn' "normal")
number      = nc "number"       (cn' "number")
orient      = nc "orientation"  (cn' "orientation")
origin      = nc "origin"       (cn' "origin")
parameter   = nc "parameter"    (cn' "parameter")
--FIXME: Should "parameter" be in math?
perp         = nc "perp"         (cn' "perpendicular")
pi_          = nc "pi"           (cn' "ratio of circumference to diameter for any circle")
posInf       = nc "PosInf"       (cn' "Positive Infinity")
negInf       = nc "NegInf"       (cn' "Negative Infinity")
positive     = nc "positive"     (cn' "positive")
negative     = nc "negative"     (cn' "negative")
point        = nc "point"     (pn' "point")
probability  = nc "probability"  (cnIES "probability")
rate         = nc "rate"         (cn' "rate")
rightHand    = nc "rightHand"    (cn' "right-handed coordinate system")
shape        = nc "shape"        (cn' "shape")
surface      = nc "surface"      (cn' "surface")
unit_        = nc "unit"         (cn' "unit")
vector       = nc "vector"       (cn' "vector")

xAxis = nc "xAxis" (nounPhraseSent $ P lX :+: S "-axis")
yAxis = nc "yAxis" (nounPhraseSent $ P lY :+: S "-axis")
zAxis = nc "zAxis" (nounPhraseSent $ P lZ :+: S "-axis")

xCoord = nc "xCoord" (nounPhraseSent $ P lX :+: S "-coordinate")
yCoord = nc "yCoord" (nounPhraseSent $ P lY :+: S "-coordinate")
zCoord = nc "zCoord" (nounPhraseSent $ P lZ :+: S "-coordinate")

xComp = nc "xComp" (nounPhraseSent $ P lX :+: S "-component")
yComp = nc "yComp" (nounPhraseSent $ P lY :+: S "-component")
zComp = nc "zComp" (nounPhraseSent $ P lZ :+: S "-component")

xDir = nc "xDir" (nounPhraseSent $ P lX :+: S "-direction")
yDir = nc "yDir" (nounPhraseSent $ P lY :+: S "-direction")
zDir = nc "zDir" (nounPhraseSent $ P lZ :+: S "-direction")
iAngle = nc "iAngle" (cn "initial angle")

de, leftSide, ode, pde, rightSide :: CI
--FIXME: use nounphrase instead of cn'
de  = commonIdeaWithDict "de"  (cn' "differential equation")          "DE"  [mathematics]
ode = commonIdeaWithDict "ode" (cn' "ordinary differential equation") "ODE" [mathematics]
pde = commonIdeaWithDict "pde" (cn' "partial differential equation")  "PDE" [mathematics]

leftSide  = commonIdeaWithDict "leftSide"  (nounPhrase "left-hand side"  "left-hand sides" ) "LHS" [mathematics]
rightSide = commonIdeaWithDict "rightSide" (nounPhrase "right-hand side" "right-hand sides") "RHS" [mathematics]

--FIXME: COMBINATION HACK (all below)
-- do we really need fterms here? Would combineNINI work?
euclidN = nc "euclidNorm"    (combineNINI euclidSpace norm)
normalV = nc "normal vector" (combineNINI normal vector)
perpV   = nc "perp_vect"     (combineNINI perp vector)
rOfChng = nc "rOfChng"       (rate `of_` change)
surArea = nc "surArea"       (combineNINI surface area)
unitV   = nc "unit_vect"     (combineNINI unit_ vector)
