module Data.Drasil.Concepts.Math where

import Language.Drasil
import Language.Drasil.NounPhrase
import Control.Lens ((^.))

calculation, diameter, euclidN, euclidSpace, gradient, graph, matrix, norm, normal, normalV, orient, perp, perpV,
  unit, unitV, vector :: ConceptChunk

ode :: CINP

--FIXME: Make some of these definitions better.
calculation  = dcc "calculation"  (cn' "calculation")             "calculation"
diameter     = dcc "diameter"     (cn' "diameter")                "any straight line segment that passes through the center of the circle and whose endpoints lie on the circle."
euclidSpace  = dcc "euclidSpace"  (cn' "Euclidean")               "Euclidean space"
gradient     = dcc "gradient"     (cn' "gradient")                "gradient operator"
graph        = dcc "graph"        (cn' "graph")                   "graph"
matrix       = dcc "matrix"       (cnICES "matrix")               "matrix"
norm         = dcc "norm"         (cn' "norm")                    "positive length or size of a vector"
normal       = dcc "normal"       (cn' "normal" )                 "object that is perpendicular to a given object"
perp         = dcc "perp"         (cn' "perpendicular")           "at right angles"
unit_         = dcc "unit"         (cn' "unit")                    "identity element"
vector       = dcc "vector"       (cn' "vector")                  "object with magnitude and direction"
orient       = dcc "orientation"  (cn' "orientation")             "orientation"
--FIXME: use nounphrase instead of cn'
ode          = commonINP "ode"    (cn' "Ordinary Differential Equation") "ODE"


--FIXME: COMBINATION HACK (all below)
euclidN      = dcc "euclidNorm"     (compoundPhrase' (euclidSpace ^. term)
                (norm ^. term))"Euclidean norm"
normalV      = dcc "normal vector"  (compoundPhrase' (normal ^. term)
                (vector ^. term))
                "unit outward normal vector for a surface"
perpV        = dcc "perp_vect"      (compoundPhrase' (perp ^. term)
                (vector ^. term))
                "vector perpendicular or 90 degrees to another vector"
unitV        = dcc "unit_vect"      (compoundPhrase' (unit_ ^. term)
                (vector ^. term)) "unit vector"
