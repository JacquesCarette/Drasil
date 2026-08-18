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
  :: (OOProg r vis stmt mthd stvr attch prg file mod bod)
  => FS (r file)
helperOO = OO.fileDoc (OO.buildModule "Helper" [] [doubleAndAdd] [])

-- | Creates Helper module that contains an addition function.
helperProc
  :: (ProcProg r vis stmt mthd prg file mod bod)
  => FS (r file)
helperProc = GProc.fileDoc (GProc.buildModule "Helper" [] [doubleAndAdd])

-- | Creates a function that doubles the arguments and adds them together.
doubleAndAdd
  ::
    ( BlockSym r stmt
    , BodySym r bod
    , Literal r
    , VariableValue r
    , NumericExpression r
    , DeclStatement r stmt bod
    , AssignStatement r stmt
    , ControlStatement r stmt bod
    , MethodSym r vis mthd bod
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
