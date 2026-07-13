-- | GProc test program exercising each 'NativeVector' operation once, so we can
-- eyeball the generated MATLAB for vector construction, scaling, addition,
-- indexing, and dot products.
module VectorTest (vectorTestProc) where

import Drasil.GProc (ProcProg, MS, bodyStatements, TypeSym(..), VariableSym(..),
  Literal(..), VariableValue(..), DeclStatement(..), ScopeSym(..), MethodSym(..),
  VisibilitySym(..), ParameterSym(..), ControlStatement(..), NativeVector(..),
  List(..))
import qualified Drasil.GProc as GProc (GSProgram, ProgramSym(..), FileSym(..),
  ModuleSym(..))
import Drasil.Metadata (watermark)

import Prelude hiding (return,print,log,exp,sin,cos,tan)

-- | A program with one function that applies each vector operation.
vectorTestProc :: (ProcProg r vis smt) => GProc.GSProgram r
vectorTestProc = GProc.prog "VectorTest" ""
  [GProc.docMod "Tests native vector operations." watermark ["Drasil"] "" $
    GProc.fileDoc (GProc.buildModule "VectorTest" [] [vectorOps])]

-- | Takes two vectors and stores each vector operation's result, returning
-- their dot product.
vectorOps :: (ProcProg r vis smt) => MS (r (Method r))
vectorOps =
  function "vectorOps" public double [param (var "a" double), param (var "b" double)]
  (bodyStatements
    [ varDecDef (var "made" (vecType double)) local (litVec double [litDouble 1.0, litDouble 2.0, litDouble 3.0])  -- [1.0, 2.0, 3.0]
    , varDecDef (var "scaled" double) local (vecScale (litDouble 2.0) a)  -- 2.0 * a
    , varDecDef (var "summed" double) local (vecAdd a b)                  -- a + b
    , varDecDef (var "elem"   double) local (vecIndex a (litInt 0))       -- a(1)
    , varDecDef (var "dotted" double) local (vecDot a b)                  -- dot(a, b)
    , varDecDef (var "dim"    double) local (listSize a)                  -- length(a)
    , varDecDef (var "mag"    double) local (vecMag a)                    -- norm(a)
    , varDecDef (var "unit"   (vecType double)) local (vecUnit a)         -- a / norm(a)
    -- Composed operations: check precedence/parenthesization.
    , varDecDef (var "combo1" (vecType double)) local (vecAdd (vecScale (litDouble 2.0) a) b)  -- 2.0 * a + b
    , varDecDef (var "combo2" (vecType double)) local (vecScale (litDouble 2.0) (vecAdd a b))  -- 2.0 * (a + b)
    , returnStmt (valueOf (var "dotted" double))
    ])
  where a = valueOf (var "a" double)
        b = valueOf (var "b" double)
