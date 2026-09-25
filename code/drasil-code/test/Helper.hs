-- | Makes the helper file for the GOOL HelloWorld tests.
module Helper (helperOO, helperProc) where

import Drasil.GOOL (OOProg, FS, MS, bodyStatements, BodySym, BlockSym,
  TypeSym(..), DeclStatement(..), AssignStatement, ControlStatement(..),
  (&=), VariableSym(var), Literal(..), VariableValue(..),
  NumericExpression(..), VisibilitySym(..), ParameterSym(..), MethodSym(..),
  ScopeSym(local))
import qualified Drasil.GOOL as OO (FileSym(..), ModuleSym(..))
import Drasil.GProc (ProcProg)
import qualified Drasil.GProc as GProc (FileSym(..), ModuleSym(..))

import Prelude hiding (return,print,log,exp,sin,cos,tan)

-- | Creates Helper module that contains an addition function.
helperOO
  :: (OOProg r vis scope typ var param val stmt mthd stvr attch prg file mod bod block)
  => FS (r file)
helperOO = OO.fileDoc (OO.buildModule "Helper" [] [doubleAndAdd] [])

-- | Creates Helper module that contains an addition function.
helperProc
  :: (ProcProg r vis scope typ var param val stmt mthd prg file mod bod block)
  => FS (r file)
helperProc = GProc.fileDoc (GProc.buildModule "Helper" [] [doubleAndAdd])

-- | Creates a function that doubles the arguments and adds them together.
doubleAndAdd
  ::
    ( BlockSym r block stmt
    , BodySym r bod block
    , TypeSym r typ
    , Literal r typ val
    , ScopeSym r scope
    , VariableSym r typ var
    , VariableValue r var val
    , NumericExpression r val
    , ParameterSym r var param
    , VisibilitySym r vis
    , DeclStatement r scope var val stmt bod
    , AssignStatement r var val stmt
    , ControlStatement r var val stmt bod
    , MethodSym r vis typ var param mthd bod
    )
  => MS (r mthd)
doubleAndAdd = docFunc "This function adds two numbers"
  ["First number to add", "Second number to add"] (Just "Sum") $
  function "doubleAndAdd"  public double
  [param $ var "num1" double, param $ var "num2" double]
  (bodyStatements [
    varDec (var "doubledSum" double) local,
    var "doubledSum" double &=
      (litDouble 2.0 #* valueOf (var "num1" double)) #+
      (litDouble 2.0 #* valueOf (var "num2" double)),
    returnStmt (valueOf (var "doubledSum" double))])
