module NameGenTest (nameGenTestOO, nameGenTestProc) where

import Drasil.GOOL (SMethod, SharedProg, OOProg, BodySym(..), BlockSym(..),
  TypeSym(..), mainVar, locVar, Literal(..), DeclStatement(..), MethodSym(..),
  VariableValue(..), listSlice, ParameterSym(..), VisibilitySym(..))
import qualified Drasil.GOOL as OO (GSProgram, ProgramSym(..), FileSym(..),
  ModuleSym(..))
import Drasil.GProc (ProcProg)
import qualified Drasil.GProc as GProc (GSProgram, ProgramSym(..), FileSym(..),
  ModuleSym(..))

nameGenTestOO :: OOProg r => OO.GSProgram r
nameGenTestOO = OO.prog "NameGenTest" "" [OO.fileDoc $ OO.buildModule
  "NameGenTest" [] [main, helper] []]

nameGenTestProc :: ProcProg r => GProc.GSProgram r
nameGenTestProc = GProc.prog "NameGenTest" "" [GProc.fileDoc $ GProc.buildModule
  "NameGenTest" [] [main, helper]]

helper :: SharedProg r => SMethod r
helper = function "helper" private void [param temp] $ body
  [block [listDec 2 result],
    listSlice result (valueOf temp) (Just (litInt 1)) (Just (litInt 3)) Nothing]
  where
    temp = locVar "temp" (listType int)
    result = locVar "result" (listType int)

main :: SharedProg r => SMethod r
main = mainFunction $ body
  [block [listDecDef temp [litInt 1, litInt 2, litInt 3],
    listDec 2 result],
    listSlice result (valueOf temp) (Just (litInt 1)) (Just (litInt 3)) Nothing]
  where
    temp = mainVar "temp" (listType int)
    result = mainVar "result" (listType int)
