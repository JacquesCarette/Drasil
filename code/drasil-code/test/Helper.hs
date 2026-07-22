-- | Makes the helper file for the GOOL HelloWorld tests.
module Helper (helperOO, helperProc) where

import Drasil.GOOL (SharedProg, OOProg, File, FS, MS, bodyStatements,
  TypeSym(..), DeclStatement(..), ControlStatement(..), (&=), VariableSym(var),
  Literal(..), VariableValue(..), NumericExpression(..), VisibilitySym(..),
  ParameterSym(..), MethodSym(..), ScopeSym(local))
import qualified Drasil.GOOL as OO (FileSym(..), ModuleSym(..))
import Drasil.GProc (ProcProg)
import qualified Drasil.GProc as GProc (FileSym(..), ModuleSym(..))

import Prelude hiding (return,print,log,exp,sin,cos,tan)

-- | Creates Helper module that contains an addition function.
helperOO :: (OOProg r vis stmt mthd svr att prg) => FS (r File)
helperOO = OO.fileDoc (OO.buildModule "Helper" [] [doubleAndAdd] [])

-- | Creates Helper module that contains an addition function.
helperProc :: (ProcProg r vis stmt mthd prg) => FS (r File)
helperProc = GProc.fileDoc (GProc.buildModule "Helper" [] [doubleAndAdd])

-- | Creates a function that doubles the arguments and adds them together.
doubleAndAdd :: (SharedProg r vis stmt mthd) => MS (r mthd)
doubleAndAdd = docFunc "This function adds two numbers"
  ["First number to add", "Second number to add"] (Just "Sum") $
  function "doubleAndAdd"  public double
  [param $ var "num1" double, param $ var "num2" double]
  (bodyStatements [
    varDec (var "doubledSum" double) local,
    var "doubledSum" double &=
      (litDouble 2.0 #* valueOf (var "num1" double)) #+
      (litDouble 2.0 #* valueOf (var "num2" double)),
    returnStmt (valueOf (var "doubledSum" double))])
