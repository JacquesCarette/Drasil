-- | Example demonstrating how to define multivectors and specify grades in Drasil
-- This file shows the various ways to work with Clifford algebra objects

module MultivectorExample where

import Language.Drasil
import Prelude hiding (cos, sin)

-- ========================================
-- 1. DEFINING MULTIVECTORS
-- ========================================

-- Basic multivector definitions using predefined helper functions
myVector2D :: Expr
myVector2D = vect2DS Real  -- Creates a 2D vector space over Real numbers

myVector3D :: Expr  
myVector3D = vect3DS Real  -- Creates a 3D vector space over Real numbers

myBivector2D :: Expr
myBivector2D = bivector2DS Real  -- Creates a 2D bivector (oriented area)

myBivector3D :: Expr
myBivector3D = bivector3DS Real  -- Creates a 3D bivector (oriented plane)

myMultivector2D :: Expr
myMultivector2D = multivector2DS Real  -- Creates a 2D multivector (scalar + vector + bivector)

myMultivector3D :: Expr
myMultivector3D = multivector3DS Real  -- Creates a 3D multivector (scalar + vector + bivector + trivector)

-- General multivector with arbitrary dimension
myMultivectorND :: Natural -> Expr
myMultivectorND n = multivectorS n Real  -- Creates an n-dimensional multivector

-- Variable-dimension multivector (for symbolic dimensions)
myVariableMultivector :: String -> Expr
myVariableMultivector dimName = multivectorNDS dimName Real


-- ========================================
-- 2. CREATING CONCRETE MULTIVECTORS
-- ========================================

-- Create a multivector with specific component values
-- Using the Clif constructor with basis blades
create2DMultivector :: Expr -> Expr -> Expr -> Expr -> Expr
create2DMultivector scalar vecX vecY bivector = 
  let dim = Fixed 2
      -- Basis blades for 2D: scalar (1), e1, e2, e1e2
      basisBlades = fromList [
          (scalarKey 2,    scalar),   -- Grade 0 (scalar)
          (vectorKey 0 2,  vecX),     -- Grade 1 (e1 component)
          (vectorKey 1 2,  vecY),     -- Grade 1 (e2 component)  
          (bivectorKey 0 1 2, bivector) -- Grade 2 (e1e2 component)
          -- 0 and 1 are the indices of the basis vectors e1 and e2
          -- 2 is the dimension of the space
        ]
  in Clif dim basisBlades

-- Create a 3D multivector
create3DMultivector :: Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr
create3DMultivector scalar vecX vecY vecZ bivXY bivXZ bivYZ trivector =
  let dim = Fixed 3
      basisBlades = fromList [
          (scalarKey 3,       scalar),    -- Grade 0
          (vectorKey 0 3,     vecX),      -- Grade 1 (e1)
          (vectorKey 1 3,     vecY),      -- Grade 1 (e2)
          (vectorKey 2 3,     vecZ),      -- Grade 1 (e3)
          (bivectorKey 0 1 3, bivXY),     -- Grade 2 (e1e2)
          (bivectorKey 0 2 3, bivXZ),     -- Grade 2 (e1e3)
          (bivectorKey 1 2 3, bivYZ),     -- Grade 2 (e2e3)
          (trivectorKey 0 1 2 3, trivector) -- Grade 3 (e1e2e3)
        ]
  in Clif dim basisBlades


-- ========================================
-- 3. GRADE SELECTION
-- ========================================

-- Extract specific grades from a multivector
gradeSelect :: Natural -> Expr -> Expr
gradeSelect grade multivector = NatCCBinaryOp GradeSelect grade multivector

-- Helper functions for common grade extractions
scalar_part :: Expr -> Expr
scalar_part mv = gradeSelect 0 mv  -- Extract grade-0 part (scalar)

vector_part :: Expr -> Expr  
vector_part mv = gradeSelect 1 mv  -- Extract grade-1 part (vector)

bivector_part :: Expr -> Expr
bivector_part mv = gradeSelect 2 mv  -- Extract grade-2 part (bivector)

trivector_part :: Expr -> Expr
trivector_part mv = gradeSelect 3 mv  -- Extract grade-3 part (trivector)


-- ========================================
-- 4. PRACTICAL EXAMPLES
-- ========================================

-- Example: Working with a 2D physics vector
velocity2D :: Expr
velocity2D = create2DMultivector 
  (Lit $ Dbl 0.0)   -- no scalar part
  (Lit $ Dbl 5.0)   -- x-component: 5 m/s
  (Lit $ Dbl 3.0)   -- y-component: 3 m/s  
  (Lit $ Dbl 0.0)   -- no bivector part

-- Extract just the vector components
velocity_vector :: Expr
velocity_vector = vector_part velocity2D

-- Example: 3D rotation using bivectors
rotation3D :: Expr
rotation3D = create3DMultivector
  (cos (Lit $ Dbl 0.5))  -- scalar part (cos(θ/2))
  (Lit $ Dbl 0.0)        -- no x-vector
  (Lit $ Dbl 0.0)        -- no y-vector
  (Lit $ Dbl 0.0)        -- no z-vector
  (sin (Lit $ Dbl 0.5))  -- xy-bivector (sin(θ/2))
  (Lit $ Dbl 0.0)        -- no xz-bivector
  (Lit $ Dbl 0.0)        -- no yz-bivector
  (Lit $ Dbl 0.0)        -- no trivector

-- Extract only the bivector part (the rotation plane)
rotation_bivector :: Expr
rotation_bivector = bivector_part rotation3D


-- ========================================
-- 5. MULTIVECTOR OPERATIONS
-- ========================================

-- Multivector addition
add_multivectors :: Expr -> Expr -> Expr
add_multivectors mv1 mv2 = CCCBinaryOp ClifAdd mv1 mv2

-- Geometric product (Clifford product)
geometric_product :: Expr -> Expr -> Expr
geometric_product mv1 mv2 = CCCBinaryOp GeomProd mv1 mv2

-- Wedge product (exterior product)
wedge_product :: Expr -> Expr -> Expr  
wedge_product mv1 mv2 = CCCBinaryOp WedgeProd mv1 mv2

-- Dot product (gives scalar result)
dot_product :: Expr -> Expr -> Expr
dot_product mv1 mv2 = CCNBinaryOp ClifDot mv1 mv2

-- Scalar multiplication
scale_multivector :: Expr -> Expr -> Expr
scale_multivector scalar mv = NCCBinaryOp ScalarMult scalar mv


-- ========================================
-- 6. EXAMPLE USAGE PATTERNS
-- ========================================

-- Example: Creating a pure vector in 3D
pure_vector_3d :: Expr -> Expr -> Expr -> Expr
pure_vector_3d x y z = create3DMultivector
  (Lit $ Dbl 0.0) x y z  -- only vector components
  (Lit $ Dbl 0.0) (Lit $ Dbl 0.0) (Lit $ Dbl 0.0)  -- no bivector
  (Lit $ Dbl 0.0)  -- no trivector

-- Example: Creating a pure bivector (rotation plane)
pure_bivector_3d :: Expr -> Expr -> Expr -> Expr
pure_bivector_3d xy xz yz = create3DMultivector
  (Lit $ Dbl 0.0) (Lit $ Dbl 0.0) (Lit $ Dbl 0.0) (Lit $ Dbl 0.0)  -- no scalar or vector
  xy xz yz  -- bivector components
  (Lit $ Dbl 0.0)  -- no trivector

-- Example: Grade decomposition of a multivector
decompose_multivector :: Expr -> (Expr, Expr, Expr, Expr)
decompose_multivector mv = 
  ( scalar_part mv
  , vector_part mv  
  , bivector_part mv
  , trivector_part mv
  )

-- Example: Check if a multivector is pure (single grade)
is_pure_scalar :: Expr -> Expr
is_pure_scalar mv = 
  -- This would need to be implemented with appropriate comparison operators
  -- For now, this shows the concept
  BoolBinaryOp Eq (vector_part mv) (Lit $ Dbl 0.0)


-- ========================================
-- 7. ADVANCED USAGE
-- ========================================

-- Working with arbitrary dimensions
create_vector_nd :: Natural -> [Expr] -> Expr
create_vector_nd dim components =
  let basisBlades = fromList $ 
        (scalarKey dim, Lit $ Dbl 0.0) :  -- no scalar part
        zipWith (\i comp -> (vectorKey i dim, comp)) [0..] components
  in Clif (Fixed dim) basisBlades

-- Grade-selective operations
-- Apply operation only to specific grades
apply_to_grade :: Natural -> (Expr -> Expr) -> Expr -> Expr
apply_to_grade grade operation mv =
  let selected = gradeSelect grade mv
      transformed = operation selected
      -- This would need a way to combine back with other grades
  in transformed  -- Simplified for example


-- ========================================
-- 8. HELPER FUNCTIONS FOR COMMON PATTERNS  
-- ========================================

-- Create unit vectors
unit_x_2d, unit_y_2d :: Expr
unit_x_2d = pure_vector_3d (Lit $ Dbl 1.0) (Lit $ Dbl 0.0) (Lit $ Dbl 0.0)
unit_y_2d = pure_vector_3d (Lit $ Dbl 0.0) (Lit $ Dbl 1.0) (Lit $ Dbl 0.0)

unit_x_3d, unit_y_3d, unit_z_3d :: Expr
unit_x_3d = pure_vector_3d (Lit $ Dbl 1.0) (Lit $ Dbl 0.0) (Lit $ Dbl 0.0)
unit_y_3d = pure_vector_3d (Lit $ Dbl 0.0) (Lit $ Dbl 1.0) (Lit $ Dbl 0.0)
unit_z_3d = pure_vector_3d (Lit $ Dbl 0.0) (Lit $ Dbl 0.0) (Lit $ Dbl 1.0)

-- Create unit bivectors (for rotations)
unit_xy_bivector, unit_xz_bivector, unit_yz_bivector :: Expr
unit_xy_bivector = pure_bivector_3d (Lit $ Dbl 1.0) (Lit $ Dbl 0.0) (Lit $ Dbl 0.0)
unit_xz_bivector = pure_bivector_3d (Lit $ Dbl 0.0) (Lit $ Dbl 1.0) (Lit $ Dbl 0.0)
unit_yz_bivector = pure_bivector_3d (Lit $ Dbl 0.0) (Lit $ Dbl 0.0) (Lit $ Dbl 1.0)

-- Create a rotor (for 3D rotations)
create_rotor :: Expr -> Expr -> Expr
create_rotor angle bivector =
  add_multivectors 
    (scale_multivector (cos (ArithBinaryOp Frac angle (Lit $ Dbl 2.0))) (scalar_part unit_x_3d))
    (scale_multivector (sin (ArithBinaryOp Frac angle (Lit $ Dbl 2.0))) bivector)


-- Note: Some helper functions like `fromList` would need to be imported 
-- from the appropriate modules (likely Data.Map or similar for OrderedMap)
