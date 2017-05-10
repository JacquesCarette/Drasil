module Data.Drasil.Concepts.Math where

import Language.Drasil
import Language.Drasil.NounPhrase

calculation, euclidSpace, gradient, graph, matrix, norm, normal, orient, perp, 
  traceyMatrix, unit, vector :: ConceptChunk

ode :: CINP

--FIXME: Make some of these definitions better.
calculation  = dcc "calculation"  (cn' "calculation")             "calculation"
euclidSpace  = dcc "euclidSpace"  (cn' "Euclidean")               "Euclidean space"
gradient     = dcc "gradient"     (cn' "gradient")                "gradient operator"
graph        = dcc "graph"        (cn' "graph")                   "graph"
matrix       = dcc "matrix"       (cnICES "matrix")               "matrix"
norm         = dcc "norm"         (cn' "norm")                    "positive length or size of a vector"
normal       = dcc "normal"       (cn' "normal" )                 "object that is perpendicular to a given object"
perp         = dcc "perp"         (cn' "perpendicular")           "at right angles"
traceyMatrix = dcc "traceyMatrix" (cnICES "traceability matrix")  "traceability matrix" -- CINP type, correct?
unit         = dcc "unit"         (cn' "unit")                    "identity element"
vector       = dcc "vector"       (cn' "vector")                  "object with magnitude and direction"
orient       = dcc "orientation"  (cn' "orientation")             "orientation"
--FIXME: use nounphrase instead of cn'
ode          = commonINP "ode"    (cn' "Ordinary Differential Equation") "ODE"

