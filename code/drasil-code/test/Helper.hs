-- | Makes the helper file for the GOOL HelloWorld tests.
module Helper (helper) where

import GOOL.Drasil (SFile, SMethod,
  OOProg, FileSym(..), bodyStatements, TypeSym(..),
  DeclStatement(..), ControlStatement(..), (&=), var,
  ScopeSym(..), Literal(..), VariableValue(..), NumericExpression(..),
  VisibilitySym(..), ParameterSym(..), MethodSym(..), ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

-- | Creates Helper module that contains an addition function.
helper :: (OOProg r) => SFile r
helper = fileDoc (buildModule "Helper" [] [doubleAndAdd] [])

-- | Creates a function that doubles the arguments and adds them together.
doubleAndAdd :: (OOProg r) => SMethod r
doubleAndAdd = docFunc "This function adds two numbers"
  ["First number to add", "Second number to add"] (Just "Sum") $
  function "doubleAndAdd"  public double
  [param $ var "num1" double local, param $ var "num2" double local]
  (bodyStatements [
    varDec $ var "doubledSum" double local,
    var "doubledSum" double local &= ((litDouble 2.0 #* valueOf (var "num1" double local)) #+
      (litDouble 2.0 #* valueOf (var "num2" double local))),
    returnStmt (valueOf (var "doubledSum" double local))])
