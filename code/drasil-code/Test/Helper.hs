module Test.Helper (helper) where

import Language.Drasil.Code.Imperative.Symantics (
  RenderSym(..), PermanenceSym(..), BodySym(..), StateTypeSym(..), 
  StatementSym(..), VariableSym(..), ValueSym(..), NumericExpression(..), 
  ScopeSym(..), MethodTypeSym(..), ParameterSym(..), MethodSym(..), 
  ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

helper :: (RenderSym repr) => repr (RenderFile repr)
helper = fileDoc (buildModule "Helper" [] [doubleAndAdd] [])

doubleAndAdd :: (RenderSym repr) => repr (Method repr)
doubleAndAdd = docFunc "This function adds two numbers" 
  ["First number to add", "Second number to add"] (Just "Sum") $ 
  function "doubleAndAdd"  public static_ (mState float) 
  [stateParam $ var "num1" float, stateParam $ var "num2" float]
  (bodyStatements [
    varDec $ var "doubledSum" float, 
    var "doubledSum" float &= ((litFloat 2.0 #* valueOf (var "num1" float)) #+ 
      (litFloat 2.0 #* valueOf (var "num2" float))),
    returnState (valueOf (var "doubledSum" float))])