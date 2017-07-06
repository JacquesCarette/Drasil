module Data.Drasil.Concepts.Education where

import Language.Drasil
import Data.Drasil.Concepts.Documentation

civil, degree_, engineering, structural, undergraduate :: NamedChunk

civil           = npnc "civil"          (cn' "civil") --FIXME: Adjective
degree_         = npnc "degree"         (cn' "degree")
engineering     = npnc "engineering"    (cn' "engineering")
structural      = npnc "structural"     (cn' "structural") --FIXME: Adjective
undergraduate   = npnc "undergraduate"  (cn' "undergraduate")


undergradDegree, secondYear, structuralEng, civilEng :: NamedChunk

civilEng                     = compoundNC civil engineering
secondYear                   = compoundNC second_ year
structuralEng                = compoundNC structural engineering
undergradDegree              = compoundNC undergraduate degree_