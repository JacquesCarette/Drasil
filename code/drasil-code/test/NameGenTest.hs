module NameGenTest (nameGenTest) where

import Drasil.GOOL

nameGenTest :: OOProg r => GSProgram r
nameGenTest = prog "NameGenTest" "" [fileDoc $ buildModule "NameGenTest" [] [main, helper] []]

helper :: OOProg r => SMethod r
helper = function "helper" private void [param temp] $ body
  [block [listDec 2 result],
    listSlice result (valueOf temp) (Just (litInt 1)) (Just (litInt 3)) Nothing]
  where
    temp = var "temp" (listType int)
    result = var "result" (listType int)

main :: OOProg r => SMethod r
main = mainFunction $ body
  [block [listDecDef temp [litInt 1, litInt 2, litInt 3],
    listDec 2 result],
    listSlice result (valueOf temp) (Just (litInt 1)) (Just (litInt 3)) Nothing]
  where
    temp = var "temp" (listType int)
    result = var "result" (listType int)
