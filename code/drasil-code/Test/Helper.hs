module Test.Helper (helper) where

import Language.Drasil.Code.Imperative.New (
  RenderSym(..), PermanenceSym(..), BodySym(..), StateTypeSym(..), 
  StatementSym(..),  ValueSym(..), NumericExpression(..), ScopeSym(..), 
  MethodTypeSym(..), ParameterSym(..), MethodSym(..), ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

helper :: (RenderSym repr) => repr (RenderFile repr)
helper = fileDoc (buildModule "Helper" [] [] [doubleAndAdd] [])

doubleAndAdd :: (RenderSym repr) => repr (Method repr)
doubleAndAdd = function "doubleAndAdd" public static_ (mState float) 
  [stateParam "num1" float, stateParam "num2" float]
  (bodyStatements [
    varDec "doubledSum" float, 
    "doubledSum" &.= ((litFloat 2.0 #* var "num1") #+ 
      (litFloat 2.0 #* var "num2")),
    returnVar "doubledSum"])