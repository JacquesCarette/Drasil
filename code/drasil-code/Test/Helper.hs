module Test.Helper (helper) where

import Language.Drasil.Code.Imperative.Symantics (
  RenderSym(..), PermanenceSym(..), BodySym(..), StateTypeSym(..), 
  StatementSym(..),  ValueSym(..), NumericExpression(..), ScopeSym(..), 
  MethodTypeSym(..), ParameterSym(..), MethodSym(..), ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

helper :: (RenderSym repr) => repr (RenderFile repr)
helper = fileDoc (buildModule "Helper" [] [doubleAndAdd] [])

doubleAndAdd :: (RenderSym repr) => repr (Method repr)
doubleAndAdd = docFunc "This function adds two numbers" 
  ["First number to add", "Second number to add"] $ 
  function "doubleAndAdd"  public static_ (mState float) 
  [stateParam $ varVal "num1" float, stateParam $ varVal "num2" float]
  (bodyStatements [
    varDec $ varVal "doubledSum" float, 
    varVal "doubledSum" float &= ((litFloat 2.0 #* varVal "num1" float) #+ 
      (litFloat 2.0 #* varVal "num2" float)),
    returnState (varVal "doubledSum" float)])