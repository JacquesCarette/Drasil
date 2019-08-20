module Test.Observer (observer) where

import Language.Drasil.Code.Imperative.GOOL.Symantics (
  RenderSym(..), PermanenceSym(..), BodySym(..), StateTypeSym(..), 
  StatementSym(..), VariableSym(..), ValueSym(..), ScopeSym(..), 
  MethodTypeSym(..), MethodSym(..), StateVarSym(..), ClassSym(..), 
  ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

observer :: (RenderSym repr) => repr (RenderFile repr)
observer = fileDoc (buildModule "Observer" [] [] [docClass 
  "This is an arbitrary class acting as an Observer"
  helperClass])

helperClass :: (RenderSym repr) => repr (Class repr)
helperClass = pubClass "Observer" Nothing [stateVar 0 public dynamic_ 
  (var "x" int)] [observerConstructor, printNumMethod]

observerConstructor :: (RenderSym repr) => repr (Method repr)
observerConstructor = constructor "Observer" [] (oneLiner (assign (objVarSelf "Observer" "x" int) (litInt 5)))

printNumMethod :: (RenderSym repr) => repr (Method repr)
printNumMethod = method "printNum" "Observer" public dynamic_ (mState void) [] (oneLiner (printLn (valueOf $ objVarSelf "Observer" "x" int)))