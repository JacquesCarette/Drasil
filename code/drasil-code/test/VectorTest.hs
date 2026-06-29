-- | GProc test program exercising each 'NativeVector' operation once, so we can
-- eyeball the generated MATLAB for vector construction, scaling, addition,
-- indexing, and dot products.
module VectorTest (vectorTestProc) where

import Drasil.GProc (SMethod, ProcProg, bodyStatements, TypeSym(..),
  VariableSym(..), Literal(..), VariableValue(..), (&=), MethodSym(..),
  VisibilitySym(..), ParameterSym(..), ControlStatement(..), NativeVector(..),
  List(..))
import qualified Drasil.GProc as GProc (GSProgram, ProgramSym(..), FileSym(..),
  ModuleSym(..))
import Drasil.Metadata (watermark)

import Prelude hiding (return,print,log,exp,sin,cos,tan)

-- | A program with one function that applies each vector operation.
vectorTestProc :: (ProcProg r) => GProc.GSProgram r
vectorTestProc = GProc.prog "VectorTest" ""
  [GProc.docMod "Tests native vector operations." watermark ["Drasil"] "" $
    GProc.fileDoc (GProc.buildModule "VectorTest" [] [vectorOps])]

-- | Takes two vectors and stores each vector operation's result, returning
-- their dot product.
vectorOps :: (ProcProg r) => SMethod r
vectorOps =
  function "vectorOps" public double [param (var "a" double), param (var "b" double)]
  (bodyStatements
    [ var "made" (listType double) &= litList double [litDouble 1.0, litDouble 2.0, litDouble 3.0]  -- [1.0, 2.0, 3.0]
    , var "scaled" double &= vecScale (litDouble 2.0) a  -- 2.0 * a
    , var "summed" double &= vecAdd a b                  -- a + b
    , var "elem"   double &= vecIndex a (litInt 0)       -- a(1)
    , var "dotted" double &= vecDot a b                  -- dot(a, b)
    , var "dim"    double &= listSize a                  -- length(a)
    , var "mag"    double &= vecMag a                    -- norm(a)
    , var "unit"   (listType double) &= vecUnit a        -- a / norm(a)
    , returnStmt (valueOf (var "dotted" double))
    ])
  where a = valueOf (var "a" double)
        b = valueOf (var "b" double)
