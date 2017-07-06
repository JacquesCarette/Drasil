module Data.Drasil.Concepts.Education where

import Language.Drasil
import Data.Drasil.Concepts.Documentation

calculus, civil, degree_, engineering, structural, mechanics,
  undergraduate :: NamedChunk

calculus        = npnc "calculus"       (cn "calculus")
civil           = npnc "civil"          (cn' "civil") --FIXME: Adjective
degree_         = npnc "degree"         (cn' "degree")
engineering     = npnc "engineering"    (cn' "engineering")
mechanics       = npnc "mechanics"      (cn "mechanics")
structural      = npnc "structural"     (cn' "structural") --FIXME: Adjective
undergraduate   = npnc "undergraduate"  (cn' "undergraduate")


undergradDegree, scndYrCalculus, secondYear, structuralEng,
  structuralMechanics, civilEng :: NamedChunk

civilEng                     = compoundNC civil engineering
scndYrCalculus               = compoundNC secondYear calculus
secondYear                   = compoundNC second_ year
structuralEng                = compoundNC structural engineering
structuralMechanics          = compoundNC structural mechanics
undergradDegree              = compoundNC undergraduate degree_
