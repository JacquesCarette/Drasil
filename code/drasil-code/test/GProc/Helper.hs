-- | Makes the helper file for the GOOL HelloWorld tests.
module GProc.Helper (helper) where

import Drasil.GProc (SFile, SMethod, ProcProg, FileSym(..), bodyStatements,
  TypeSym(..), DeclStatement(..), ControlStatement(..), (&=), locVar,
  Literal(..), VariableValue(..), NumericExpression(..), VisibilitySym(..),
  ParameterSym(..), MethodSym(..), ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

-- | Creates Helper module that contains an addition function.
helper :: (ProcProg r) => SFile r
helper = fileDoc (buildModule "Helper" [] [doubleAndAdd])

-- | Creates a function that doubles the arguments and adds them together.
doubleAndAdd :: (ProcProg r) => SMethod r
doubleAndAdd = docFunc "This function adds two numbers"
  ["First number to add", "Second number to add"] (Just "Sum") $
  function "doubleAndAdd"  public double
  [param $ locVar "num1" double, param $ locVar "num2" double]
  (bodyStatements [
    varDec $ locVar "doubledSum" double,
    locVar "doubledSum" double &= ((litDouble 2.0 #*
      valueOf (locVar "num1" double)) #+
      (litDouble 2.0 #* valueOf (locVar "num2" double))),
    returnStmt (valueOf (locVar "doubledSum" double))])
