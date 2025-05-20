module Drasil.Shared.LanguageRenderer.OOHelpers (
  constructor,
  intClass,
  extraClass,
  inherit,
  implements,
  objVarSelf,
  classVar,
  stateVarDef,
  constVar,
  funcDecDef,
  setMethodCall
) where

import Drasil.Shared.InterfaceCommon (SValue, Label, MSParameter, MSBody, SMethod, SVariable, VSType, Scope, MSStatement, variableName, variableType, MethodSym(function))
import Drasil.GOOL.InterfaceGOOL (Initializers, OOTypeSym(obj), OOVariableSym(self, objVar), OOFunctionSym(..), SClass, CSStateVar, PermanenceSym(..))
import Drasil.Shared.RendererClassesCommon (variableBind, CommonRenderSym, RenderVariable(varFromData), ScopeElim(scopeData))
import qualified Drasil.Shared.RendererClassesCommon as RC (type', variable)
import Drasil.Shared.Helpers (toState, onStateValue)
import Drasil.GOOL.RendererClassesOO (OORenderSym, OORenderMethod(intMethod), ParentSpec)
import qualified Drasil.GOOL.RendererClassesOO as S (OOMethodTypeSym(construct), RenderClass(intClass, inherit))
import Control.Monad.State (modify)
import Control.Lens ((^.))
import qualified Control.Lens as L (set)
import Drasil.Shared.State (getClassName, currParameters, setClassName, lensCStoMS)
import Control.Monad (join)
import Data.List (intercalate)
import Text.PrettyPrint.HughesPJ (Doc, text, empty, (<+>), colon)
import qualified Drasil.Shared.LanguageRenderer.LanguagePolymorphic as G (
  multiBody )
import Drasil.Shared.State ( CS )
import qualified Drasil.GOOL.InterfaceGOOL as RC (stateVar, method, visibility )

-- | Helper to generate a constructor method for OO languages
constructor :: (OORenderSym r) => Label -> [MSParameter r] -> Initializers r -> MSBody r -> SMethod r
constructor fName ps is b = getClassName >>= (\c -> intMethod False fName public dynamic (S.construct c) ps (G.multiBody [initStmts is, b]))

-- | Helper to generate a class with inheritance and visibility
intClass :: (OORenderSym r, Monad r) => (Label -> Doc -> Doc -> Doc -> Doc -> Doc) -> Label -> r (Visibility r) -> r ParentSpec -> [CSStateVar r] -> [SMethod r] -> [SMethod r] -> CS (r Doc)
intClass f n s i svrs cstrs mths = do
  modify (setClassName n)
  svs <- onStateList (R.stateVarList . map S.stateVar) svrs
  ms <- onStateList (vibcat . map RC.method) (map (zoom lensCStoMS) (cstrs ++ mths))
  return $ onCodeValue (\p -> f n p (RC.visibility s) svs ms) i

-- | Helper to generate an extra class (with inheritance)
extraClass :: (OORenderSym r) => Label -> Maybe Label -> [CSStateVar r] -> [SMethod r] -> [SMethod r] -> SClass r
extraClass n = S.intClass n public . S.inherit

-- | Helper for inheritance specification
inherit :: (Monad r) => Maybe Label -> r ParentSpec
inherit n = toCode $ maybe empty ((colon <+>) . text) n

-- | Helper for interface implementation specification
implements :: (Monad r) => [Label] -> r ParentSpec
implements is = toCode $ colon <+> text (intercalate listSep is)

-- | Helper for object variable referencing self
objVarSelf :: (OORenderSym r) => SVariable r -> SVariable r
objVarSelf = objVar self

-- | Helper for class variable
classVar :: (CommonRenderSym r) => (Doc -> Doc -> Doc) -> VSType r -> SVariable r -> SVariable r
classVar f c' v' = do
  c <- c'
  v <- v'
  vr <- varFromData
    (variableBind v) (getTypeString c `access` variableName v)
    (toState $ variableType v) (f (RC.type' c) (RC.variable v))
  toState $ classVarCheckStatic vr

-- | Helper for state variable definition
stateVarDef :: (OORenderSym r, Monad r) => r (Visibility  r) -> r (Permanence r) -> SVariable r -> SValue r -> CS (r Doc)
stateVarDef s p vr vl = zoom lensCStoMS $ onStateValue (toCode . R.stateVar (RC.visibility  s) (RC.perm p) . RC.statement)
  (S.stmt $ IC.varDecDef vr IC.local vl)

-- | Helper for constant variable definition
constVar :: (CommonRenderSym r, Monad r) => Doc -> r (Visibility  r) -> SVariable r -> SValue r -> CS (r Doc)
constVar p s vr vl = zoom lensCStoMS $ onStateValue (toCode . R.stateVar (RC.visibility s) p . RC.statement) (S.stmt $ IC.constDecDef vr IC.local vl)

-- | Helper for function declaration and definition
funcDecDef :: (OORenderSym r) => SVariable r -> r (Scope r) -> [SVariable r] -> MSBody r -> MSStatement r
funcDecDef v scp ps b = do
  vr <- zoom lensMStoVS v
  modify $ useVarName $ variableName vr
  modify $ setVarScope (variableName vr) (scopeData scp)
  s <- get
  f <- function (variableName vr) private (return $ variableType vr) (map IC.param ps) b
  modify (L.set currParameters (s ^. currParameters))
  mkStmtNoEnd $ RC.method f

-- | Helper for setting a method call
setMethodCall :: (OORenderSym r) => Label -> SValue r -> SValue r -> SValue r
setMethodCall n a b = objMethodCall (listInnerType $ onStateValue valueType a) a n [b]