{-# LANGUAGE PostfixOperators #-}
-- | Tests GOOL classes.  It **might** run without errors
module SimpleLib (simpleLib, doubleAndAdd) where

import GOOL.Drasil (OOProg, SFile, FileSym (fileDoc), ModuleSym (buildModule), 
  VariableValue (valueOf), ScopeSym (..), VariableSym (..), TypeSym (..),
  MethodSym (..), Literal (..), SMethod, ParameterSym (param),
  ControlStatement (..), (&=), DeclStatement (..), bodyStatements,
  NumericExpression (..))
import Prelude hiding (return, print, log, exp, sin, cos, tan, const)

-- | Creates Helper module that contains an addition function.
simpleLib :: (OOProg r) => SFile r
simpleLib = fileDoc (buildModule "SimpleLib" [] [doubleAndAdd] [])

-- | Creates a function that doubles the arguments and adds them together.
doubleAndAdd :: (OOProg r) => SMethod r
doubleAndAdd = docFunc "This function adds two numbers"
  ["First number to add", "Second number to add"] (Just "Sum") $
  function "doubleAndAdd"  public double
  [param $ var "num1" double, param $ var "num2" double]
  (bodyStatements [
    varDec $ var "doubledSum" double,
    var "doubledSum" double &= ((litDouble 2.0 #* valueOf (var "num1" double)) #+
      (litDouble 2.0 #* valueOf (var "num2" double))),
    returnStmt (valueOf (var "doubledSum" double))])
