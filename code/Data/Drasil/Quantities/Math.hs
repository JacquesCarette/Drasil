module Data.Drasil.Quantities.Math where

import Language.Drasil
import Data.Drasil.Concepts.Math as CM
import Control.Lens((^.)) --need for parametrization hack

gradient, normalVect, orientation, unitVect, euclidNorm, perpVect :: ConVar

gradient  = cvR CM.gradient (Greek Nabla)
normalVect = cvR normalV (vec $ lN)
  where normalV = dcc "normal vector" (compoundPhrase' (CM.normal ^. term)
                  (CM.vector ^. term))
                  "unit outward normal vector for a surface"
--FIXME: COMBINATION HACK
unitVect = cvR unitV (vec $ hat lI)
  where unitV = dcc "unit_vect" (compoundPhrase' (CM.unit ^. term)
                (CM.vector ^. term))
                "unit vector"
--FIXME: COMBINATION HACK
perpVect = cvR perpV (vec $ lN)
  where perpV = dcc "perp_vect" (compoundPhrase' (CM.perp ^. term)
                (CM.vector ^. term))
                "vector perpendicular or 90 degrees to another vector"
orientation = cvR CM.orient (Greek Phi_L)
--FIXME: COMBINATION HACK
euclidNorm = cvR euclidN (Concat [Atomic "||", (vec lR), Atomic "||"])
  where euclidN = dcc "euclidNorm" (compoundPhrase' (CM.euclidSpace ^. term)
                  (CM.norm ^. term))"Euclidean norm"