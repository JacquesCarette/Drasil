-- | Defines concepts used to describe levels of education.
module Data.Drasil.Concepts.Education where

import Drasil.Database (mkUid)
import Language.Drasil hiding (year)
import Language.Drasil.Chunk.Concept.NamedCombinators

import Data.Drasil.Concepts.Documentation (first, physics, physical,
  second_, year)
import Data.Drasil.Concepts.PhysicalProperties (solid)

-- | Collects all education-related concepts.
educon :: [IdeaDict]
educon = [calculus, civil, degree_, engineering, structural, mechanics,
  undergraduate, highSchool, chemistry, undergradDegree,
  scndYrCalculus, solidMechanics, secondYear, structuralEng,
  structuralMechanics, civilEng, highSchoolCalculus, highSchoolPhysics,
  frstYr, physChem]

-- * Educational Concepts

calculus, civil, degree_, engineering, structural, mechanics,
  undergraduate, highSchool, chemistry :: IdeaDict

calculus      = idea' (mkUid "calculus")       (cn   "calculus"     )
civil         = idea' (mkUid "civil")          (cn'  "civil"        )--FIXME: Adjective
degree_       = idea' (mkUid "edu_degree")     (cn'  "degree"       )
engineering   = idea' (mkUid "engineering")    (cn'  "engineering"  )
mechanics     = idea' (mkUid "mechanics")      (cn   "mechanics"    )
structural    = idea' (mkUid "structural")     (cn'  "structural"   )--FIXME: Adjective
undergraduate = idea' (mkUid "undergraduate")  (cn'  "undergraduate")--FIXME: Functions as adjective
highSchool    = idea' (mkUid "highSchool")     (cn'  "high school"  )--FIXME: Functions as adjective
chemistry     = idea' (mkUid "chemistry")      (cn'  "chemistry"    )

undergradDegree, scndYrCalculus, solidMechanics, secondYear, structuralEng,
  structuralMechanics, civilEng, highSchoolCalculus, highSchoolPhysics,
  frstYr, physChem :: IdeaDict

civilEng            = compoundNC civil engineering
physChem            = compoundNC physical chemistry
highSchoolCalculus  = compoundNC highSchool calculus
highSchoolPhysics   = compoundNC highSchool physics
scndYrCalculus      = compoundNC secondYear calculus
frstYr              = compoundNC first year
secondYear          = compoundNC second_ year
solidMechanics      = compoundNC solid mechanics
structuralEng       = compoundNC structural engineering
structuralMechanics = compoundNC structural mechanics
undergradDegree     = compoundNC undergraduate degree_
