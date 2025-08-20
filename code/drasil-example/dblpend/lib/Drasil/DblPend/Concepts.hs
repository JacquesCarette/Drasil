module Drasil.DblPend.Concepts where

import Language.Drasil
import Data.Drasil.Concepts.Documentation (first, second_, object)
import Language.Drasil.Chunk.Concept.NamedCombinators (compoundNC)
import Data.Drasil.Concepts.Physics (pendulum, motion, position, velocity, force, acceleration)


concepts :: [IdeaDict]
concepts = map nw [rod, horizontal, vertical,
  pendMotion, horizontalPos, verticalPos, horizontalVel,horizontalAccel, verticalAccel,
  verticalVel, horizontalForce, verticalForce, firstRod, secondRod, firstObject, secondObject,
  multivector, cliffordAlgebra, geometricProduct, basisVector] 
  ++ map nw defs

rod, horizontal, vertical :: IdeaDict
rod = nc "rod" (cn' "rod")
horizontal = nc "horizontal" (cn "horizontal") 
vertical = nc "vertical" (cn "vertical") 

pendMotion, horizontalPos, verticalPos, horizontalVel, verticalVel, horizontalForce, verticalForce,
  horizontalAccel, verticalAccel, firstRod, secondRod, firstObject, secondObject,
  multivector, cliffordAlgebra, geometricProduct, basisVector:: IdeaDict
pendMotion      = compoundNC pendulum motion
horizontalPos   = compoundNC horizontal position
verticalPos     = compoundNC vertical position
horizontalVel   = compoundNC horizontal velocity
verticalVel     = compoundNC vertical velocity
horizontalAccel = compoundNC horizontal acceleration
verticalAccel   = compoundNC vertical acceleration
horizontalForce = compoundNC horizontal force
verticalForce   = compoundNC vertical force
firstRod        = compoundNC first rod
secondRod       = compoundNC second_ rod
firstObject     = compoundNC first object
secondObject    = compoundNC second_ object
multivector     = nc "multivector" (cn' "multivector")
cliffordAlgebra = nc "cliffordAlgebra" (cn' "Clifford algebra")
geometricProduct = nc "geometricProduct" (cn' "geometric product")
basisVector     = nc "basisVector" (cn' "basis vector")

defs :: [ConceptChunk]
defs = [arcLen, multivectorDef, cliffordAlgebraDef, geometricProductDef, basisVectorDef, cliffordSpace, bivectorDef]

arcLen :: ConceptChunk
arcLen = dcc "arc length" (nounPhraseSP "arc length") "the distance between two points on a curve"

multivectorDef :: ConceptChunk
multivectorDef = dcc "multivector" (nounPhraseSP "multivector") 
  "a generalization of scalars, vectors, and higher-grade elements in Clifford algebra that can represent rotations and reflections"

cliffordAlgebraDef :: ConceptChunk
cliffordAlgebraDef = dcc "Clifford algebra" (nounPhraseSP "Clifford algebra")
  "a unification of real numbers, complex numbers, quaternions, and several other hypercomplex number systems into a single mathematical framework"

geometricProductDef :: ConceptChunk
geometricProductDef = dcc "geometric product" (nounPhraseSP "geometric product")
  "the fundamental operation in Clifford algebra that combines the dot product and wedge product of vectors"

basisVectorDef :: ConceptChunk
basisVectorDef = dcc "basis vector" (nounPhraseSP "basis vector")
  "fundamental unit vectors (e₁, e₂) that span the 2D Clifford space and satisfy the relations e₁² = e₂² = 1"

cliffordSpace :: ConceptChunk  
cliffordSpace = dcc "Clifford space" (nounPhraseSP "Clifford space")
  "the geometric algebra space Cl(2,0) where multivectors exist, characterized by basis vectors e₁, e₂ with signature (+,+)"

bivectorDef :: ConceptChunk
bivectorDef = dcc "bivector" (nounPhraseSP "bivector")
  "a grade-2 multivector element e₁∧e₂ representing oriented area and rotations in the plane"
