module GOOL.NameGenTest (nameGenTest) where

import Drasil.GOOL

nameGenTest :: OOProg r => GSProgram r
nameGenTest = prog "NameGenTest" "" [fileDoc $ buildModule "NameGenTest" [] [main, helper] []]

helper :: OOProg r => SMethod r
helper = function "helper" private void [param temp] $ body
  [block [listDec 2 result],
    listSlice result (valueOf temp) (Just (litInt 1)) (Just (litInt 3)) Nothing,
    block [assert (listSize (valueOf result) ?== litInt 2) (litString "Result list should have 2 elements after slicing.")]
  ]
  where
    temp = locVar "temp" (listType int)
    result = locVar "result" (listType int)

main :: OOProg r => SMethod r
main = mainFunction $ body
  [block [
    listDecDef temp [litInt 1, litInt 2, litInt 3],
    listDec 2 result],
    listSlice result (valueOf temp) (Just (litInt 1)) (Just (litInt 3)) Nothing,
    block [assert (listSize (valueOf result) ?== litInt 2) (litString "Result list should have 2 elements after slicing.")],
    block [assert (listAccess (valueOf result) (litInt 0) ?== litInt 2) (litString "First element of result should be 2.")]]
  where
    temp = mainVar "temp" (listType int)
    result = mainVar "result" (listType int)
