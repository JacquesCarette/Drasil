-- | Makes the helper file for the GOOL HelloWorld tests.
module Helper (helperOO, helperProc) where

import Drasil.GOOL (SMethod, SharedProg, OOProg, bodyStatements, TypeSym(..),
  DeclStatement(..), ControlStatement(..), (&=), locVar, Literal(..),
  VariableValue(..), NumericExpression(..), VisibilitySym(..), ParameterSym(..),
  MethodSym(..))
import qualified Drasil.GOOL as OO (SFile, FileSym(..), ModuleSym(..))
import Drasil.GProc (ProcProg)
import qualified Drasil.GProc as GProc (SFile, FileSym(..), ModuleSym(..))

import Prelude hiding (return,print,log,exp,sin,cos,tan)

-- | Creates Helper module that contains an addition function.
helperOO :: (OOProg r) => OO.SFile r
helperOO = OO.fileDoc (OO.buildModule "Helper" [] [doubleAndAdd] [])

-- | Creates Helper module that contains an addition function.
helperProc :: (ProcProg r) => GProc.SFile r
helperProc = GProc.fileDoc (GProc.buildModule "Helper" [] [doubleAndAdd])

-- | Creates a function that doubles the arguments and adds them together.
doubleAndAdd :: (SharedProg r) => SMethod r
doubleAndAdd = docFunc "This function adds two numbers"
  ["First number to add", "Second number to add"] (Just "Sum") $
  function "doubleAndAdd"  public double
  [param $ locVar "num1" double, param $ locVar "num2" double]
  (bodyStatements [
    varDec $ locVar "doubledSum" double,
    locVar "doubledSum" double &= ((litDouble 2.0 #*
      valueOf (locVar "num1" double)) #+
      (litDouble 2.0 #* valueOf (locVar "num2" double))),
    returnStmt (valueOf (locVar "doubledSum" double))])
