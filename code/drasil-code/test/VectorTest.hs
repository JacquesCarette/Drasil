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
vectorTestProc
  ::
    ( NativeVector r
    , ProcProg r vis stmt mthd prg file mod
    )
  => GProc.GSProgram r prg
vectorTestProc = GProc.prog "VectorTest" ""
  [GProc.docMod "Tests native vector operations." watermark ["Drasil"] "" $
    GProc.fileDoc (GProc.buildModule "VectorTest" [] [vectorOps])]

-- | Takes two vectors and stores each vector operation's result, returning
-- their dot product.
vectorOps
  ::
    ( NativeVector r
    , ProcProg r vis stmt mthd prg file mod
    )
  => MS (r mthd)
vectorOps =
  function "vectorOps" public double [param (var "a" vt), param (var "b" vt)]
  (bodyStatements
    [ varDecDef (var "made" vt) local (litVec double [litDouble 1.0, litDouble 2.0, litDouble 3.0])  -- [1.0, 2.0, 3.0]
    , varDecDef (var "scaled" vt) local (vecScale (litDouble 2.0) a)  -- 2.0 * a
    , varDecDef (var "summed" vt) local (vecAdd a b)                  -- a + b
    , varDecDef (var "elem"   double) local (vecIndex a (litInt 0))   -- a(1)
    , varDecDef (var "dotted" double) local (vecDot a b)              -- dot(a, b)
    , varDecDef (var "dim"    double) local (listSize a)              -- length(a)
    , varDecDef (var "mag"    double) local (vecMag a)                -- norm(a)
    , varDecDef (var "unit"   vt) local (vecUnit a)                   -- a / norm(a)
    -- Composed operations: check precedence/parenthesization.
    , varDecDef (var "combo1" vt) local (vecAdd (vecScale (litDouble 2.0) a) b)  -- 2.0 * a + b
    , varDecDef (var "combo2" vt) local (vecScale (litDouble 2.0) (vecAdd a b))  -- 2.0 * (a + b)
    , returnStmt (valueOf (var "dotted" double))
    ])
  where vt = vecType double
        a  = valueOf (var "a" vt)
        b  = valueOf (var "b" vt)
