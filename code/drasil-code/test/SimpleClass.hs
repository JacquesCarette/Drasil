{-# LANGUAGE PostfixOperators #-}
-- | Tests GOOL classes.  It **might** run without errors
module SimpleClass (simpleClass, simpleClassName, simpleClassType) where

import GOOL.Drasil (OOProg, SFile, FileSym (fileDoc), ModuleSym (buildModule), 
  ClassSym (docClass, buildClass), VariableValue (valueOf), SClass, 
  StateVarSym (stateVar), ScopeSym (public), PermanenceSym (dynamic), 
  VariableSym (..), SVariable, TypeSym (..), 
  MethodSym (..), Literal (..), SMethod, initializer, IOStatement (printLn), 
  oneLiner, ParameterSym (param), BodySym (body), BlockSym (block), 
  ControlStatement (..), Comparison ((?==)), (&=), DeclStatement (..), 
  bodyStatements, NumericExpression (..), VSType, newObj, objMethodCall, valStmt)
import Prelude hiding (return, print, log, exp, sin, cos, tan, const)

simpleClassName, simpleDesc :: String
-- | Class name
simpleClassName = "SimpleData"
-- | Class description
simpleDesc = "A test class for GOOL.  It **might** run without errors"

-- | Class type
simpleClassType :: (TypeSym r) => VSType r
simpleClassType = obj simpleClassName

-- | Creates the simple class
simpleClass :: (OOProg r) => SFile r
simpleClass = fileDoc (buildModule simpleClassName [] [makeSimpleData,
  updateSimpleData, doubleAndAdd] [docClass simpleDesc buildSimpleClass])

-- | Makes a variable @x@
x :: (VariableSym r) => SVariable r
x = var "x" int

-- | Makes a variable @y@
y :: (VariableSym r) => SVariable r
y = var "y" int

-- | Makes a variable @sObj
sObj :: (VariableSym r) => SVariable r
sObj = var "sObj" simpleClassType

buildSimpleClass :: (OOProg r) => SClass r
buildSimpleClass = buildClass Nothing 
  [stateVar public dynamic x, stateVar public dynamic y] [simpleConstructor] 
  [getMethod x, setMethod x, getMethod y, setMethod y, resetXMethod]

-- | Devault value for simple class is y=3
simpleConstructor :: (MethodSym r, Literal r) => SMethod r
simpleConstructor = initializer [] [(x, litInt 5), (y, litInt 3)]

-- | Create the @printNum@ method.
resetXMethod :: (OOProg r) => SMethod r
resetXMethod = method "resetXIfTrue" public dynamic void 
  [param $ var "cond" bool] $ body [block [ifCond [(valueOf (var "cond" bool), 
    body [block [objVarSelf x &= litInt 5]])] (body [])]]

-- | Creates an instance of the SimpleData class
makeSimpleData :: (OOProg r) => SMethod r
makeSimpleData = docFunc "This function creates an instance of SimpleData."
  ["Value for x, and half of value for y"] (Just "Instance") $
  function "makeSimpleData" public (obj simpleClassName)
  [param x] (bodyStatements [varDecDef sObj $ newObj simpleClassType [],
    valStmt $ objMethodCall void (valueOf sObj) "setX" [valueOf x],
    valStmt $ objMethodCall void (valueOf sObj) "setY" [valueOf x #* litInt 2],
    returnStmt $ valueOf sObj])

-- | Updates values of a SimpleData object
updateSimpleData :: (OOProg r) => SMethod r
updateSimpleData = docFunc "This function updates the values of a SimpleData object."
  ["Instance of " ++ simpleClassName, "Value for x, and half of value for y"] Nothing $
  function "updateSimpleData" public void [param sObj, param x]
  (bodyStatements [valStmt $ objMethodCall void (valueOf sObj) "setX" [valueOf x],
    valStmt $ objMethodCall void (valueOf sObj) "setY" [valueOf x #* litInt 2]])

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
