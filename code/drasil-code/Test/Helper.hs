module Test.Helper (helper) where

import Language.Drasil.Code.Imperative.Symantics (
  RenderSym(..), PermanenceSym(..), BodySym(..), StateTypeSym(..), 
  StatementSym(..),  ValueSym(..), NumericExpression(..), ScopeSym(..), 
  MethodTypeSym(..), ParameterSym(..), MethodSym(..), ModuleSym(..),
  BlockCommentSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

helper :: (RenderSym repr) => repr (RenderFile repr)
helper = fileDoc (buildModule "Helper" [] [commentedFunc funcD doubleAndAdd] [])

funcD :: (RenderSym repr) => repr (BlockComment repr)
funcD = docComment ["\\brief This function adds two numbers",
                    "\\param num1 First number to add",
                    "\\param num2 Second number to add"]

doubleAndAdd :: (RenderSym repr) => repr (Method repr)
doubleAndAdd = function "doubleAndAdd" public static_ (mState float) 
  [stateParam $ var "num1" float, stateParam $ var "num2" float]
  (bodyStatements [
    varDec $ var "doubledSum" float, 
    var "doubledSum" float &= ((litFloat 2.0 #* var "num1" float) #+ 
      (litFloat 2.0 #* var "num2" float)),
    returnVar "doubledSum" float])