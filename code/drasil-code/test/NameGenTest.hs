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

nameGenTestOO
  :: OOProg r prg file mod cls stvr mthd attch vis param bod block stmt var scope val binder typ
  => OO.GSProgram r prg
nameGenTestOO = OO.prog "NameGenTest" "" [OO.fileDoc $ OO.buildModule
  "NameGenTest" [] [main, helper] []]

nameGenTestProc
  :: (ProcProg r prg file mod mthd vis param bod block stmt var scope val binder typ)
  => GProc.GSProgram r prg
nameGenTestProc = GProc.prog "NameGenTest" "" [GProc.fileDoc $ GProc.buildModule
  "NameGenTest" [] [main, helper]]

helper
  ::
    ( BlockSym r block stmt
    , BodySym r bod block
    , TypeSym r typ
    , Literal r val typ
    , ScopeSym r scope
    , VariableSym r var typ
    , VariableValue r var val
    , Comparison r val
    , List r val
    , InternalList r block var val
    , ParameterSym r param var
    , VisibilitySym r vis
    , DeclStatement r bod stmt var scope val
    , ControlStatement r bod stmt var val
    , MethodSym r mthd vis param bod var typ
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
    ( BlockSym r block stmt
    , BodySym r bod block
    , TypeSym r typ
    , Literal r val typ
    , ScopeSym r scope
    , VariableSym r var typ
    , VariableValue r var val
    , Comparison r val
    , List r val
    , InternalList r block var val
    , DeclStatement r bod stmt var scope val
    , ControlStatement r bod stmt var val
    , MethodSym r mthd vis param bod var typ
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
