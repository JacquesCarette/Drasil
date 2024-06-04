{-# LANGUAGE PostfixOperators #-}
-- | Tests GOOL classes.  It **might** run without errors
module SimpleClass (simpleClass, simpleName) where

import GOOL.Drasil (OOProg, SFile, FileSym (fileDoc), ModuleSym (buildModule), ClassSym (docClass, buildClass), VariableValue (valueOf), SClass, StateVarSym (stateVar), ScopeSym (public), PermanenceSym (dynamic), VariableSym (var, objVarSelf), SVariable, TypeSym (int, void, bool), MethodSym (getMethod, method, setMethod), Literal (litInt, litTrue), SMethod, initializer, IOStatement (printLn), oneLiner, ParameterSym (param), BodySym (body), BlockSym (block), ControlStatement (ifCond), Comparison ((?==)), (&=))
import Prelude hiding (return, print, log, exp, sin, cos, tan, const)

simpleName, simpleDesc :: String
-- | Class name
simpleName = "SimpleClass"
-- | Class description
simpleDesc = "A test class for GOOL.  It **might** run without errors"

-- | Creates the simple class
simpleClass :: (OOProg r) => SFile r
simpleClass = fileDoc (buildModule simpleName [] [] [docClass simpleDesc 
  buildSimpleClass])

-- | Makes a variable @x@
x :: (VariableSym r) => SVariable r
x = var "x" int

-- | Makes a variable @y@
y :: (VariableSym r) => SVariable r
y = var "y" int

buildSimpleClass :: (OOProg r) => SClass r
buildSimpleClass = buildClass Nothing [stateVar public dynamic x, stateVar public dynamic y] [simpleConstructor, simpleConstructor2] [getMethod x, setMethod x, printXMethod]

-- | Devault value for simple class is y=3
simpleConstructor :: (MethodSym r, Literal r) => SMethod r
simpleConstructor = initializer [] [(x, litInt 5), (y, litInt 3)]

-- | Devault value for simple class is y=3
simpleConstructor2 :: (MethodSym r, Literal r, VariableValue r) => SMethod r
simpleConstructor2 = initializer [param x] [(x, valueOf $ var "x" int), (y, litInt 3)]

-- | Create the @printNum@ method.
printXMethod :: (OOProg r) => SMethod r
printXMethod = method "resetXIfTrue" public dynamic void [param $ var "cond" bool] $
  body [block [ifCond [(valueOf (var "cond" bool), body [block [objVarSelf x &= litInt 5]])] (body [])]]