{-# OPTIONS -Wall #-} 
module Language.Drasil.CCode.Helpers where

import Language.Drasil.CCode.AST

stdHeaders, testHeaders :: [Header]
stdHeaders = [Library "stdlib", Library "stdio"]
testHeaders = [Local "unity"]

getMethodName :: Method -> String
getMethodName ((MethodDecl _ name _), _) = name

-- error handling
-- 0 = no errors
-- 1 = div by zero error
errorCode :: Variable
errorCode = "err"

errorDecl :: VarDecl
errorDecl = VarDecl IntType errorCode

errorMethod :: String -> Method
errorMethod moduleName = ((MethodDecl IntType (moduleName ++ "_get_error_code") []), [Return (Var errorCode)])


-- for automated testing
-- using unity (http://www.throwtheswitch.org/unity/) for now;  can adapt to something else later
testBegin :: Method
testBegin = ((MethodDecl IntType "UNITY_BEGIN" []), [])

runTest :: Method
runTest = ((MethodDecl VoidType "RUN_TEST" [VarDecl StrType "methodName"]), [])

testEnd :: Method
testEnd = ((MethodDecl IntType "UNITY_END" []), [])

assertEquals :: Method
assertEquals = ((MethodDecl VoidType "TEST_ASSERT_EQUAL" [VarDecl DblType "expected", VarDecl DblType "actual"]), [])