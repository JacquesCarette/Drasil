module Test.Helper (helper) where

import GOOL.Drasil (
  RenderSym, FileSym(..), PermanenceSym(..), BodySym(..), TypeSym(..), 
  StatementSym(..), VariableSym(..), ValueSym(..), NumericExpression(..), 
  ScopeSym(..), ParameterSym(..), MethodSym(..), ModuleSym(..), FS, MS)
import Prelude hiding (return,print,log,exp,sin,cos,tan)

helper :: (RenderSym repr) => FS (repr (RenderFile repr))
helper = fileDoc (buildModule "Helper" [] [doubleAndAdd] [])

doubleAndAdd :: (RenderSym repr) => MS (repr (Method repr))
doubleAndAdd = docFunc "This function adds two numbers" 
  ["First number to add", "Second number to add"] (Just "Sum") $ 
  function "doubleAndAdd"  public static_ float
  [param $ var "num1" float, param $ var "num2" float]
  (bodyStatements [
    varDec $ var "doubledSum" float, 
    var "doubledSum" float &= ((litFloat 2.0 #* valueOf (var "num1" float)) #+ 
      (litFloat 2.0 #* valueOf (var "num2" float))),
    returnState (valueOf (var "doubledSum" float))])