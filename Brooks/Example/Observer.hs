module Example.Observer (observer) where

import New (
  RenderSym(..), PermanenceSym(..),
  BodySym(..), StateTypeSym(..), StatementSym(..), ValueSym(..), ScopeSym(..), 
  MethodTypeSym(..), MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..))
import LanguageRenderer.NewJavaRenderer()
import Prelude hiding (return,print,log,exp,sin,cos,tan)

observer :: (RenderSym repr) => repr (RenderFile repr)
observer = fileDoc (buildModule "" [] [] [] [helperClass])

helperClass :: (RenderSym repr) => repr (Class repr)
helperClass = pubClass "Observer" Nothing [stateVar 0 "x" public static int] [observerConstructor, printNumMethod]

observerConstructor :: (RenderSym repr) => repr (Method repr)
observerConstructor = constructor "Observer" [] (oneLiner (assign (self $-> var "x") (litInt 5)))

printNumMethod :: (RenderSym repr) => repr (Method repr)
printNumMethod = method "printNum" public dynamic void [] (oneLiner (printLn int (self $-> (var "x"))))