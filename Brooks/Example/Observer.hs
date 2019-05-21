module Example.Observer (observer) where

import New (
  PackageSym(..), RenderSym(..), PermanenceSym(..),
  BodySym(..), StateTypeSym(..), StatementSym(..), ValueSym(..), ScopeSym(..), 
  MethodTypeSym(..), MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

observer :: (PackageSym repr) => repr (Package repr)
observer = packMods "Observer" [fileDoc (buildModule "Observer" [] [] [] [helperClass])]

helperClass :: (RenderSym repr) => repr (Class repr)
helperClass = pubClass "Observer" Nothing [stateVar 0 "x" public dynamic int] [observerConstructor, printNumMethod]

observerConstructor :: (RenderSym repr) => repr (Method repr)
observerConstructor = constructor "Observer" [] (oneLiner (assign (self $-> var "x") (litInt 5)))

printNumMethod :: (RenderSym repr) => repr (Method repr)
printNumMethod = method "printNum" "Observer" public dynamic void [] (oneLiner (printLn int (self $-> (var "x"))))