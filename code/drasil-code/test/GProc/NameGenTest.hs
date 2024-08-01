module GProc.NameGenTest (nameGenTest) where

import Drasil.GProc

nameGenTest :: ProcProg r => GSProgram r
nameGenTest = prog "NameGenTest" "" [fileDoc $ buildModule "NameGenTest" [] [main, helper]]

helper :: ProcProg r => SMethod r
helper = function "helper" private void [param temp] $ body
  [block [listDec 2 result],
    listSlice result (valueOf temp) (Just (litInt 1)) (Just (litInt 3)) Nothing]
  where
    temp = locVar "temp" (listType int)
    result = locVar "result" (listType int)

main :: ProcProg r => SMethod r
main = mainFunction $ body
  [block [listDecDef temp [litInt 1, litInt 2, litInt 3],
    listDec 2 result],
    listSlice result (valueOf temp) (Just (litInt 1)) (Just (litInt 3)) Nothing]
  where
    temp = mainVar "temp" (listType int)
    result = mainVar "result" (listType int)
