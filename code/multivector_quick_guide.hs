-- Quick Guide: Defining Multivectors and Specifying Grades in Drasil
-- ================================================================

module MultivectorQuickGuide where

import Language.Drasil
import qualified Data.Map as Map
import Data.Map (fromList)

-- ========================================
-- STEP 1: BASIC MULTIVECTOR TYPES
-- ========================================

-- These helper functions create multivector *types* (Space objects):
my2DVector     = vect2DS Real        -- 2D vector space
my3DVector     = vect3DS Real        -- 3D vector space  
my2DBivector   = bivector2DS Real    -- 2D bivector space
my3DBivector   = bivector3DS Real    -- 3D bivector space
my2DMultivector = multivector2DS Real -- 2D multivector space (scalar + vector + bivector)
my3DMultivector = multivector3DS Real -- 3D multivector space (scalar + vector + bivector + trivector)

-- For arbitrary dimensions:
myNDMultivector n = multivectorS n Real  -- n-dimensional multivector


-- ========================================
-- HELPER FUNCTIONS FOR BASIS BLADE KEYS
-- ========================================

-- Helper functions to create basis blade keys for multivectors
-- These generate the appropriate keys for storing basis blade coefficients

-- Scalar (grade-0) key for dimension n
scalarKey :: Int -> String
scalarKey n = "scalar_" ++ show n

-- Vector (grade-1) key for basis vector i in dimension n  
vectorKey :: Int -> Int -> String
vectorKey i n = "e" ++ show i ++ "_" ++ show n

-- Bivector (grade-2) key for basis bivector eij in dimension n
bivectorKey :: Int -> Int -> Int -> String  
bivectorKey i j n = "e" ++ show i ++ "e" ++ show j ++ "_" ++ show n

-- Trivector (grade-3) key for basis trivector eijk in dimension n
trivectorKey :: Int -> Int -> Int -> Int -> String
trivectorKey i j k n = "e" ++ show i ++ "e" ++ show j ++ "e" ++ show k ++ "_" ++ show n


-- ========================================
-- STEP 2: CREATING ACTUAL MULTIVECTOR VALUES
-- ========================================

-- To create actual multivector values with specific components, use the Clif constructor:

-- Example: 2D multivector with all components
example2DMultivector :: Expr
example2DMultivector = Clif (Fixed 2) basisBlades
  where
    basisBlades = fromList [
      (scalarKey 2,       Lit $ Dbl 3.0),   -- scalar part: 3
      (vectorKey 0 2,     Lit $ Dbl 4.0),   -- e1 component: 4  
      (vectorKey 1 2,     Lit $ Dbl 5.0),   -- e2 component: 5
      (bivectorKey 0 1 2, Lit $ Dbl 2.0)    -- e1e2 component: 2
    ]
    -- This represents: 3 + 4e1 + 5e2 + 2e1e2

-- Example: Pure vector (only grade-1 components)
pureVector2D :: Expr -> Expr -> Expr
pureVector2D x y = Clif (Fixed 2) basisBlades
  where
    basisBlades = fromList [
      (scalarKey 2,    Lit $ Dbl 0.0),  -- no scalar
      (vectorKey 0 2,  x),              -- x component
      (vectorKey 1 2,  y),              -- y component  
      (bivectorKey 0 1 2, Lit $ Dbl 0.0) -- no bivector
    ]

-- Example: Pure bivector (only grade-2 components)
pureBivector2D :: Expr -> Expr
pureBivector2D bivectorValue = Clif (Fixed 2) basisBlades
  where
    basisBlades = fromList [
      (scalarKey 2,       Lit $ Dbl 0.0),  -- no scalar
      (vectorKey 0 2,     Lit $ Dbl 0.0),  -- no x vector
      (vectorKey 1 2,     Lit $ Dbl 0.0),  -- no y vector  
      (bivectorKey 0 1 2, bivectorValue)   -- bivector component
    ]


-- ========================================
-- STEP 3: GRADE SELECTION
-- ========================================

-- To extract specific grades from a multivector, use gradeSelect:
gradeSelect :: Natural -> Expr -> Expr  
gradeSelect grade multivector = NatCCBinaryOp GradeSelect grade multivector

-- Common grade extractions:
getScalar     mv = gradeSelect 0 mv  -- Extract grade-0 (scalar part)
getVector     mv = gradeSelect 1 mv  -- Extract grade-1 (vector part)  
getBivector   mv = gradeSelect 2 mv  -- Extract grade-2 (bivector part)
getTrivector  mv = gradeSelect 3 mv  -- Extract grade-3 (trivector part)

-- Example usage:
-- Let's say you have a 2D multivector and want just the vector part:
myMultivector = example2DMultivector  -- 3 + 4e1 + 5e2 + 2e1e2
vectorPart = getVector myMultivector  -- Results in: 4e1 + 5e2


-- ========================================
-- STEP 4: PRACTICAL EXAMPLE 
-- ========================================

-- Create a 3D position vector
position3D :: Expr -> Expr -> Expr -> Expr
position3D x y z = Clif (Fixed 3) basisBlades
  where
    basisBlades = fromList [
      (scalarKey 3,    Lit $ Dbl 0.0),  -- no scalar part
      (vectorKey 0 3,  x),              -- x component
      (vectorKey 1 3,  y),              -- y component
      (vectorKey 2 3,  z),              -- z component
      -- All bivector and trivector components are 0
      (bivectorKey 0 1 3, Lit $ Dbl 0.0),
      (bivectorKey 0 2 3, Lit $ Dbl 0.0),  
      (bivectorKey 1 2 3, Lit $ Dbl 0.0),
      (trivectorKey 0 1 2 3, Lit $ Dbl 0.0)
    ]

-- Create a rotor for 3D rotations (scalar + bivector)  
rotor3D :: Expr -> Expr -> Expr
rotor3D angle bivectorComponent = Clif (Fixed 3) basisBlades
  where
    halfAngle = ArithBinaryOp Frac angle (Lit $ Dbl 2.0)
    basisBlades = fromList [
      (scalarKey 3,       cos halfAngle),        -- cos(θ/2)
      (vectorKey 0 3,     Lit $ Dbl 0.0),        -- no vector components
      (vectorKey 1 3,     Lit $ Dbl 0.0),
      (vectorKey 2 3,     Lit $ Dbl 0.0),
      (bivectorKey 0 1 3, ArithBinaryOp Mul (sin halfAngle) bivectorComponent), -- sin(θ/2) * bivector
      (bivectorKey 0 2 3, Lit $ Dbl 0.0),
      (bivectorKey 1 2 3, Lit $ Dbl 0.0),
      (trivectorKey 0 1 2 3, Lit $ Dbl 0.0)     -- no trivector
    ]

-- Example: Extract only the rotation plane (bivector part)
extractRotationPlane rotor = getBivector rotor


-- ========================================
-- SUMMARY
-- ========================================

{-
To define multivectors and specify grades:

1. Use helper functions like multivector2DS, multivector3DS for type definitions
2. Use the Clif constructor with basis blade maps for actual values
3. Use gradeSelect with NatCCBinaryOp GradeSelect to extract specific grades
4. Use basis key functions (scalarKey, vectorKey, bivectorKey, etc.) to specify components

Key functions:
- scalarKey, vectorKey, bivectorKey, trivectorKey: Create basis element keys
- Clif: Constructor for multivector expressions  
- NatCCBinaryOp GradeSelect: Extract specific grades
- gradeSelect: Convenient wrapper for grade extraction
-}


-- ========================================
-- WORKING EXAMPLE
-- ========================================

-- Let's create a complete working example:
exampleUsage :: IO ()
exampleUsage = do
  let 
    -- Create a 2D multivector: 1 + 2e1 + 3e2 + 4e1e2
    mv = Clif (Fixed 2) $ fromList [
      (scalarKey 2,       Lit $ Dbl 1.0),  -- scalar: 1
      (vectorKey 0 2,     Lit $ Dbl 2.0),  -- e1: 2
      (vectorKey 1 2,     Lit $ Dbl 3.0),  -- e2: 3  
      (bivectorKey 0 1 2, Lit $ Dbl 4.0)   -- e1e2: 4
    ]
    
    -- Extract different grades
    scalar_part   = gradeSelect 0 mv  -- Gets: 1
    vector_part   = gradeSelect 1 mv  -- Gets: 2e1 + 3e2  
    bivector_part = gradeSelect 2 mv  -- Gets: 4e1e2
    
  -- You can now use these parts in further calculations
  putStrLn "Multivector created and grades extracted successfully!"
