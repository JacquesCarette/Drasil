{-# LANGUAGE FlexibleContexts #-}
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
import Drasil.Shared.InterfaceCommon (UnRepr(..), VSType, SVariable, MixedCall,
  SValue, VSFunction, ValueSym(Value), TypeSym(int), MSBody, MSStatement,
  VariableElim(variableName), VariableSym(Variable), Label, Library,
  BodySym(Body), funcApp, getCodeType)
import Drasil.Shared.RendererClassesCommon (scopeData, CommonRenderSym, call,
  RenderFunction(funcFromData))
import Drasil.Shared.LanguageRenderer (access, intValue)
import qualified Drasil.Shared.LanguageRenderer as R (extVar, listAccessFunc,
  addAssign)
import Drasil.Shared.LanguageRenderer.Constructors(mkStmtNoEnd, mkStateVar, typeFromData)
import Drasil.Shared.State (lensMStoVS, useVarName, setVarScope)
import qualified Drasil.Shared.InterfaceCommon as IC (emptyStmt, assign)
import Drasil.Shared.AST (ScopeData, TypeData)

-- Swift and Julia --

boolRender :: String
boolRender = "Bool"

bool :: (Monad r) => VSType r
bool = typeFromData Boolean boolRender (text boolRender)

-- Python, Java, C#, and Julia --

extVar :: (CommonRenderSym r) => Label -> Label -> VSType r -> SVariable r
extVar l n t = mkStateVar (l `access` n) t (R.extVar l n)

-- Python, Java, and Julia --

funcType :: (Monad r, UnRepr r TypeData) => [VSType r] ->
              VSType r -> VSType r
funcType ps' r' =  do
  ps <- sequence ps'
  r <- r'
  typeFromData (Func (map getCodeType ps) (getCodeType r)) "" empty

-- Python, Java, C#, Swift, and Julia --
extFuncAppMixedArgs :: (CommonRenderSym r) => Library -> MixedCall r
extFuncAppMixedArgs l = call (Just l) Nothing

-- Python, C#, Swift, and Julia --

listAccessFunc :: (CommonRenderSym r, UnRepr r TypeData) => VSType r -> SValue r -> VSFunction r
listAccessFunc t v = intValue v >>= ((`funcFromData` t) . R.listAccessFunc)

-- Python, Swift, and Julia --

forEach' :: (CommonRenderSym r) => (r (Variable r) -> r (Value r) -> r (Body r) -> Doc)
  -> SVariable r -> SValue r -> MSBody r -> MSStatement r
forEach' f i' v' b' = do
  i <- zoom lensMStoVS i'
  v <- zoom lensMStoVS v'
  b <- b'
  mkStmtNoEnd (f i v b)

-- Python and Julia --

varDecDef :: (CommonRenderSym r) => SVariable r -> r ScopeData -> Maybe (SValue r)
  -> MSStatement r
varDecDef v scp e = do
  v' <- zoom lensMStoVS v
  modify $ useVarName (variableName v')
  modify $ setVarScope (variableName v') (scopeData scp)
  def e
  where
    def Nothing = IC.emptyStmt
    def (Just d) = IC.assign v d

-- Python and Swift --

increment :: (CommonRenderSym r) => SVariable r -> SValue r -> MSStatement r
increment vr' v'= do
  vr <- zoom lensMStoVS vr'
  v <- zoom lensMStoVS v'
  mkStmtNoEnd $ R.addAssign vr v

-- Python, Julia, and MATLAB --

-- | Call to get the size of a list as a function call
listSize :: (CommonRenderSym r) => String -> SValue r -> SValue r
listSize fnName list = funcApp fnName int [list]
