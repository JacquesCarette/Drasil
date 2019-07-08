module Test.Helper (helper) where

import Language.Drasil.Code.Imperative.Symantics (
  RenderSym(..), PermanenceSym(..), BodySym(..), StateTypeSym(..), 
  StatementSym(..),  ValueSym(..), NumericExpression(..), ScopeSym(..), 
  MethodTypeSym(..), ParameterSym(..), MethodSym(..), ModuleSym(..),
  BlockCommentSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

helper :: (RenderSym repr) => repr (RenderFile repr)
helper = fileDoc (buildModule "Helper" [] [doubleAndAdd] [])

doubleAndAdd :: (RenderSym repr) => repr (Method repr)
doubleAndAdd = docFunc "doubleAndAdd" "This function adds two numbers" public 
  static_ (mState float) 
  [(stateParam $ var "num1" float, "First number to add"), 
  (stateParam $ var "num2" float, "Second number to add")]
  (bodyStatements [
    varDec $ var "doubledSum" float, 
    var "doubledSum" float &= ((litFloat 2.0 #* var "num1" float) #+ 
      (litFloat 2.0 #* var "num2" float)),
    returnState (var "doubledSum" float)])