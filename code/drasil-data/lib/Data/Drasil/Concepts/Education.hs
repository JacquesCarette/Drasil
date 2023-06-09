-- | Defines concepts used to describe levels of education.
module Data.Drasil.Concepts.Education where

import Language.Drasil hiding (year)
import Language.Drasil.Chunk.Concept.NamedCombinators

import Data.Drasil.Concepts.Documentation (first, physics, second_, year)
import Data.Drasil.Concepts.PhysicalProperties (solid)

-- | Collects all education-related concepts.
educon :: [IdeaDict]
educon = [calculus, civil, degree_, engineering, structural, mechanics,
  undergraduate, highSchool, physical_, chemistry, undergradDegree,
  scndYrCalculus, solidMechanics, secondYear, structuralEng,
  structuralMechanics, civilEng, highSchoolCalculus, highSchoolPhysics,
  frstYr, physChem]

-- * Educational Concepts

calculus, civil, degree_, engineering, structural, mechanics,
  undergraduate, highSchool, physical_, chemistry :: IdeaDict

calculus      = nc "calculus"       (cn   "calculus"     ) Nothing 
civil         = nc "civil"          (cn'  "civil"        ) Nothing --FIXME: Adjective
degree_       = nc "degree"         (cn'  "degree"       ) Nothing 
engineering   = nc "engineering"    (cn'  "engineering"  ) Nothing 
mechanics     = nc "mechanics"      (cn   "mechanics"    ) Nothing 
structural    = nc "structural"     (cn'  "structural"   ) Nothing --FIXME: Adjective
undergraduate = nc "undergraduate"  (cn'  "undergraduate") Nothing --FIXME: Functions as adjective
highSchool    = nc "highSchool"     (cn'  "high school"  ) Nothing --FIXME: Functions as adjective
chemistry     = nc "chemistry"      (cn'  "chemistry"    ) Nothing 
physical_     = nc "physical"       (cn'  "physical"     ) Nothing --FIXME: Adjective

undergradDegree, scndYrCalculus, solidMechanics, secondYear, structuralEng,
  structuralMechanics, civilEng, highSchoolCalculus, highSchoolPhysics,
  frstYr, physChem :: IdeaDict

civilEng            = compoundNC civil engineering
physChem            = compoundNC physical_ chemistry
highSchoolCalculus  = compoundNC highSchool calculus
highSchoolPhysics   = compoundNC highSchool physics
scndYrCalculus      = compoundNC secondYear calculus
frstYr              = compoundNC first year
secondYear          = compoundNC second_ year
solidMechanics      = compoundNC solid mechanics
structuralEng       = compoundNC structural engineering
structuralMechanics = compoundNC structural mechanics
undergradDegree     = compoundNC undergraduate degree_
