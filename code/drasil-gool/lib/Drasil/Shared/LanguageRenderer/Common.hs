-- | Implementations defined here are valid in some, but not all, language renderers
module Drasil.Shared.LanguageRenderer.Common (
  boolRender, bool, extVar, funcType, extFuncAppMixedArgs, listAccessFunc,
  forEach', varDecDef, listSize, increment
) where

import Prelude hiding (print, pi, (<>))
import Control.Lens.Zoom (zoom)
import Control.Monad.State (modify)
import Text.PrettyPrint.HughesPJ (text, empty, Doc)

import Drasil.Shared.CodeType (CodeType(..))
import Drasil.Shared.InterfaceCommon (Body, Variable, SVariable, MixedCall,
  Value, SValue, ValueSym, TypeSym(int), VariableElim(variableName), Label,
  Library, funcApp, getCodeType, AssignStatement, ValueExpression,
  EmptyStatement)
import Drasil.Shared.RendererClassesCommon (scopeData, call,
  RenderFunction(funcFromData), RenderVariable, RenderValue, ValueElim,
  RenderStatement, ScopeElim, InternalVarElim)
import Drasil.Shared.LanguageRenderer (access, intValue)
import qualified Drasil.Shared.LanguageRenderer as R (extVar, listAccessFunc,
  addAssign)
import Drasil.Shared.LanguageRenderer.Constructors(mkStmtNoEnd, mkStateVar,
  typeFromData)
import Drasil.Shared.State (MS, VS, lensMStoVS, useVarName, setVarScope)
import qualified Drasil.Shared.InterfaceCommon as IC
import Drasil.Shared.AST (ScopeData, TypeData, FuncData)

-- Swift and Julia --

boolRender :: String
boolRender = "Bool"

bool :: (Monad r) => VS (r TypeData)
bool = typeFromData Boolean boolRender (text boolRender)

-- Python, Java, C#, and Julia --

extVar :: (RenderVariable r) => Label -> Label -> VS (r TypeData) -> SVariable r
extVar l n t = mkStateVar (l `access` n) t (R.extVar l n)

-- Python, Java, and Julia --

funcType :: (Monad r, IC.TypeElim r) => [VS (r TypeData)] ->
  VS (r TypeData) -> VS (r TypeData)
funcType ps' r' =  do
  ps <- sequence ps'
  r <- r'
  typeFromData (Func (map getCodeType ps) (getCodeType r)) "" empty

-- Python, Java, C#, Swift, and Julia --
extFuncAppMixedArgs :: (RenderValue r) => Library -> MixedCall r
extFuncAppMixedArgs l = call (Just l) Nothing

-- Python, C#, Swift, and Julia --

listAccessFunc
  :: (RenderFunction r, IC.TypeElim r, ValueElim r, ValueSym r)
  => VS (r TypeData) -> SValue r -> VS (r FuncData)
listAccessFunc t v = intValue v >>= ((`funcFromData` t) . R.listAccessFunc)

-- Python, Swift, and Julia --

forEach' :: (RenderStatement r stmt) => (r Variable -> r Value ->
  r Body -> Doc) -> SVariable r -> SValue r -> MS (r Body) -> MS (r stmt)
forEach' f i' v' b' = do
  i <- zoom lensMStoVS i'
  v <- zoom lensMStoVS v'
  b <- b'
  mkStmtNoEnd (f i v b)

-- Python and Julia --

varDecDef
  :: (EmptyStatement r stmt, AssignStatement r stmt, ScopeElim r, VariableElim r)
  => SVariable r -> r ScopeData -> Maybe (SValue r) -> MS (r stmt)
varDecDef v scp e = do
  v' <- zoom lensMStoVS v
  modify $ useVarName (variableName v')
  modify $ setVarScope (variableName v') (scopeData scp)
  def e
  where
    def Nothing = IC.emptyStmt
    def (Just d) = IC.assign v d

-- Python and Swift --

increment
  :: (InternalVarElim r, RenderStatement r stmt, ValueElim r)
  => SVariable r -> SValue r -> MS (r stmt)
increment vr' v'= do
  vr <- zoom lensMStoVS vr'
  v <- zoom lensMStoVS v'
  mkStmtNoEnd $ R.addAssign vr v

-- Python, Julia, and MATLAB --

-- | Call to get the size of a list as a function call
listSize :: (ValueExpression r) => String -> SValue r -> SValue r
listSize fnName list = funcApp fnName int [list]
