module Test.Observer (observer) where

import Language.Drasil.Code.Imperative.Symantics (
  RenderSym(..), PermanenceSym(..),
  BodySym(..), StateTypeSym(..), StatementSym(..), ValueSym(..), ScopeSym(..), 
  MethodTypeSym(..), MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

observer :: (RenderSym repr) => repr (RenderFile repr)
observer = fileDoc (buildModule "Observer" [] [] [] [helperClass])

helperClass :: (RenderSym repr) => repr (Class repr)
helperClass = pubClass "Observer" Nothing [stateVar 0 "x" public dynamic_ int] [observerConstructor, printNumMethod]

observerConstructor :: (RenderSym repr) => repr (Method repr)
observerConstructor = constructor "Observer" [] (oneLiner (assign (objVarSelf "x" int) (litInt 5)))

printNumMethod :: (RenderSym repr) => repr (Method repr)
printNumMethod = method "printNum" "Observer" public dynamic_ void [] (oneLiner (printLn int (objVarSelf "x" int)))