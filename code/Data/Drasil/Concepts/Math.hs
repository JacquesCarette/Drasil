module Data.Drasil.Concepts.Math where

import Language.Drasil
import Control.Lens ((^.))

area, calculation, diameter, equation, euclidN, euclidSpace, gradient, 
  graph, matrix, norm, normal, normalV, number, orient, parameter, perp, perpV, 
  probability, surface, unit_, unitV, vector, rate, change, rOfChng, 
  constraint :: ConceptChunk

ode :: CINP

{--algorithm    = dcc "algorithm"    (cn' "algorithm")               "A series of steps to be followed in calculations and problem-solving operations" --}
area         = dcc "area"         (cn' "area")                    "A part of an object or surface"
calculation  = dcc "calculation"  (cn' "calculation")             "A mathematical determination of the size or number of something"
change       = dcc "change"       (cn' "change")                  "Difference between relative start and end states of an object"
constraint   = dcc "constraint"   (cn' "constraint")              "A condition that the solution must satisfy"
diameter     = dcc "diameter"     (cn' "diameter")                ("Any straight line segment that passes through the center of the circle" ++
                                                                  "and whose endpoints lie on the circle.")
equation     = dcc "equation"     (cn' "equation")                "A statement that the values of two mathematical expressions are equal "
euclidSpace  = dcc "euclidSpace"  (cn' "Euclidean")               ("Denoting the system of geometry corresponding to the geometry of ordinary" ++
                                                                  "experience")
gradient     = dcc "gradient"     (cn' "gradient")                "degree of steepness of a graph at any point"
graph        = dcc "graph"        (cn' "graph")                   "A diagram showing the relation between variable quantities"
matrix       = dcc "matrix"       (cnICES "matrix")               ("A rectangular array of quantities or expressions in rows and columns that" ++
                                                                  "is treated as a single entity and manipulated according to particular rules")
norm         = dcc "norm"         (cn' "norm")                    "Positive length or size of a vector"
normal       = dcc "normal"       (cn' "normal" )                 "Object that is perpendicular to a given object"
number       = dcc "number"       (cn' "number")                  "A mathematical object used to count, measure, and label"
parameter    = dcc "parameter"    (cn' "parameter")               "A quantity whose value is selected depending on particular circumstances"
perp         = dcc "perp"         (cn' "perpendicular")           "At right angles"
probability  = dcc "probability"  (cnIES "probability")           "The likelihood of an event to occur"
rate         = dcc "rate"         (cn' "rate")                    "Ratio that compares two quantities having different units of measure"
surface      = dcc "surface"      (cn' "surface")                 "The outer or topmost boundary of an object"
unit_        = dcc "unit"         (cn' "unit")                    "Identity element"
vector       = dcc "vector"       (cn' "vector")                  "Object with magnitude and direction"
orient       = dcc "orientation"  (cn' "orientation")             "The relative physical position or direction of something"

--FIXME: use nounphrase instead of cn'
ode          = commonINP "ode"    (cn' "Ordinary Differential Equation") "ODE"


--FIXME: COMBINATION HACK (all below)
euclidN      = dcc "euclidNorm"     (compoundPhrase' (euclidSpace ^. term)
                (norm ^. term)) "Euclidean norm"
normalV      = dcc "normal vector"  (compoundPhrase' (normal ^. term)
                (vector ^. term))
                "unit outward normal vector for a surface"
perpV        = dcc "perp_vect"      (compoundPhrase' (perp ^. term)
                (vector ^. term))
                "Vector perpendicular or 90 degrees to another vector"
rOfChng      = dcc "rOfChng" ((rate) `of_` (change))
                "Ratio between a change in one variable relative to a corresponding change in another"
surArea      = dcc "surArea"  (compoundPhrase' (surface ^. term) (area ^. term))
                "A measure of the total area that the surface of the object occupies"
unitV        = dcc "unit_vect"      (compoundPhrase' (unit_ ^. term)
                (vector ^. term)) "A vector that has a magnitude of one"
