module NameGenExample (nameGenExample) where

import Drasil.GOOL

nameGenExample :: OOProg r => GSProgram r
nameGenExample = prog "NameGenExample" "" [fileDoc $ buildModule "NameGenExample" [] [main, helper] []]

helper :: OOProg r => SMethod r
helper = function "helper" private void [param temp] $ body
  [block [listDec 2 result],
    listSlice result (valueOf temp) (Just (litInt 1)) (Just (litInt 3)) Nothing]
  where
    temp = var "temp" (listType int) local
    result = var "result" (listType int) local

main :: OOProg r => SMethod r
main = mainFunction $ body
  [block [listDecDef temp [litInt 1, litInt 2, litInt 3],
    listDec 2 result],
    listSlice result (valueOf temp) (Just (litInt 1)) (Just (litInt 3)) Nothing,
    block [
      comment "This shadows a generated name:",
      listDecDef temp0 [litInt 1, litInt 2, litInt 3],
      comment "This shadows a user-given name:",
      listDec 2 result]]
  where
    temp = mainVar "temp" (listType int)
    result = mainVar "result" (listType int)
    temp0 = mainVar "temp0" (listType int)
