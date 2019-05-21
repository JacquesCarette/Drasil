module Example.Helper (helper) where

import New (
  PackageSym(..), RenderSym(..), PermanenceSym(..), BodySym(..), StateTypeSym(..), 
    StatementSym(..),  ValueSym(..), NumericExpression(..), ScopeSym(..), 
    MethodTypeSym(..), ParameterSym(..), MethodSym(..), ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

helper :: (PackageSym repr) => repr (Package repr)
helper = packMods "Helper" [fileDoc (buildModule "Helper" [] [] [doubleAndAdd] [])]

doubleAndAdd :: (RenderSym repr) => repr (Method repr)
doubleAndAdd = function "doubleAndAdd" public static (mState float) [(stateParam "num1" float), (stateParam "num2" float)]
  (bodyStatements [(varDec "doubledSum" float), ("doubledSum" &.= (((litFloat 2.0) #* (var "num1")) #+ ((litFloat 2.0) #* (var "num2")))),
    (returnVar "doubledSum")])