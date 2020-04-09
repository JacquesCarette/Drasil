module Test.Helper (helper) where

import GOOL.Drasil (SFile, SMethod,
  ProgramSym, FileSym(..), PermanenceSym(..), bodyStatements, TypeSym(..), 
  DeclStatement(..), ControlStatement(..), (&=), VariableSym(..), ValueSym(..), 
  NumericExpression(..), ScopeSym(..), ParameterSym(..), MethodSym(..), 
  ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

helper :: (ProgramSym repr) => SFile repr
helper = fileDoc (buildModule "Helper" [] [doubleAndAdd] [])

doubleAndAdd :: (ProgramSym repr) => SMethod repr
doubleAndAdd = docFunc "This function adds two numbers" 
  ["First number to add", "Second number to add"] (Just "Sum") $ 
  function "doubleAndAdd"  public static double
  [param $ var "num1" double, param $ var "num2" double]
  (bodyStatements [
    varDec $ var "doubledSum" double, 
    var "doubledSum" double &= ((litDouble 2.0 #* valueOf (var "num1" double)) #+ 
      (litDouble 2.0 #* valueOf (var "num2" double))),
    returnState (valueOf (var "doubledSum" double))])