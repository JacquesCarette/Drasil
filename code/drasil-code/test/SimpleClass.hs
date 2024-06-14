{-# LANGUAGE PostfixOperators #-}
-- | Tests GOOL classes.  It **might** run without errors
module SimpleClass (simpleClass, simpleClassName, simpleClassType) where

import GOOL.Drasil (OOProg, SFile, FileSym (fileDoc), ModuleSym (buildModule), 
  ClassSym (docClass, buildClass), VariableValue (valueOf), SClass, 
  StateVarSym (stateVar), ScopeSym (..), PermanenceSym (..), 
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
simpleClass = fileDoc (buildModule simpleClassName [] [] [docClass simpleDesc buildSimpleClass])

-- | Makes a variable @x@
dynamicX :: (VariableSym r) => SVariable r
dynamicX = var "x" int

staticX :: (VariableSym r) => SVariable r
staticX = staticVar "x" int

classX :: (VariableSym r) => SVariable r
classX = classVar simpleClassType staticX

-- | Makes a variable @y@
y :: (VariableSym r) => SVariable r
y = var "y" int

-- | Makes a variable @sObj
sObj :: (VariableSym r) => SVariable r
sObj = var "sObj" simpleClassType

buildSimpleClass :: (OOProg r) => SClass r
buildSimpleClass = buildClass Nothing 
  [stateVar public dynamic dynamicX, stateVar public dynamic y] [simpleConstructor] 
  [getX, setX, getMethod y, setMethod y, resetXMethod]

-- | Devault value for simple class is y=3
simpleConstructor :: (OOProg r) => SMethod r
simpleConstructor = constructor [] [(y, litInt 3)] (oneLiner $ dynamicX &= litInt 5)

getX :: (OOProg r) => SMethod r
getX = method "getX" public static int [] (oneLiner $ returnStmt $ valueOf dynamicX)

setX :: (OOProg r) => SMethod r
setX = method "setX" public static void [param dynamicX] (oneLiner $ dynamicX &= valueOf dynamicX)

-- | Create the @printNum@ method.
resetXMethod :: (OOProg r) => SMethod r
resetXMethod = method "resetXIfTrue" public dynamic void 
  [param $ var "cond" bool] $ body [block [ifCond [(valueOf (var "cond" bool), 
    body [block [dynamicX &= litInt 5]])] (body [])]]