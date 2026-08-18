module NameGenTest (nameGenTestOO, nameGenTestProc) where

import Drasil.GOOL (OOProg, MS, BodySym(..), BlockSym(..), TypeSym(..),
  VariableSym(var), Literal(..), DeclStatement(..), ControlStatement(..),
  MethodSym(..), VariableValue(..), Comparison(..), listSlice, List(..),
  ParameterSym(..), VisibilitySym(..), ScopeSym(..), InternalList)
import qualified Drasil.GOOL as OO (GSProgram, ProgramSym(..), FileSym(..),
  ModuleSym(..))
import Drasil.GProc (ProcProg)
import qualified Drasil.GProc as GProc (GSProgram, ProgramSym(..), FileSym(..),
  ModuleSym(..))

nameGenTestOO :: OOProg r vis stmt mthd stvr attch prg file mod bod => OO.GSProgram r prg
nameGenTestOO = OO.prog "NameGenTest" "" [OO.fileDoc $ OO.buildModule
  "NameGenTest" [] [main, helper] []]

nameGenTestProc
  :: (ProcProg r vis stmt mthd prg file mod bod)
  => GProc.GSProgram r prg
nameGenTestProc = GProc.prog "NameGenTest" "" [GProc.fileDoc $ GProc.buildModule
  "NameGenTest" [] [main, helper]]

helper
  ::
    ( BlockSym r stmt
    , BodySym r bod
    , Literal r
    , VariableValue r
    , Comparison r
    , List r
    , InternalList r
    , DeclStatement r stmt bod
    , ControlStatement r stmt bod
    , MethodSym r vis mthd bod
    )
  => MS (r mthd)
helper = function "helper" private void [param temp] $ body
  [block [listDec 2 result local],
    listSlice result (valueOf temp) (Just (litInt 1)) (Just (litInt 3)) Nothing,
    block [assert (listSize (valueOf result) ?== litInt 2) (litString "Result list should have 2 elements after slicing.")]]
  where
    temp = var "temp" (listType int)
    result = var "result" (listType int)

main
  ::
    ( BlockSym r stmt
    , BodySym r bod
    , Literal r
    , VariableValue r
    , Comparison r
    , List r
    , InternalList r
    , DeclStatement r stmt bod
    , ControlStatement r stmt bod
    , MethodSym r vis mthd bod
    )
  => MS (r mthd)
main = mainFunction $ body
  [block [
    listDecDef temp mainFn [litInt 1, litInt 2, litInt 3],
    listDec 2 result mainFn],
    listSlice result (valueOf temp) (Just (litInt 1)) (Just (litInt 3)) Nothing,
    block [assert (listSize (valueOf result) ?== litInt 2) (litString "Result list should have 2 elements after slicing.")],
    block [assert (listAccess (valueOf result) (litInt 0) ?== litInt 2) (litString "First element of result should be 2.")]]
  where
    temp = var "temp" (listType int)
    result = var "result" (listType int)
