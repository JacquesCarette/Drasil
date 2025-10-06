-- | Implementations defined here are valid in some, but not all, language renderers
module Drasil.Shared.LanguageRenderer.Common where

import Drasil.Shared.CodeType (CodeType(..))
import Drasil.Shared.InterfaceCommon
import Drasil.Shared.RendererClassesCommon (scopeData, CommonRenderSym, typeFromData, call, RenderFunction(funcFromData))
import Drasil.Shared.LanguageRenderer
import qualified Drasil.Shared.LanguageRenderer as R
import qualified Drasil.Shared.RendererClassesCommon as RC (value, functionType, function)
import Drasil.Shared.LanguageRenderer.Constructors
import Prelude hiding (print,pi,(<>))
import Text.PrettyPrint.HughesPJ (text, empty, Doc)
import Control.Lens.Zoom (zoom)
import Drasil.Shared.State
import qualified Drasil.Shared.InterfaceCommon as IC
import Control.Monad.State (modify)
import qualified Drasil.Shared.RendererClassesCommon as S



-- Swift and Julia --

boolRender :: String
boolRender = "Bool"

bool :: (CommonRenderSym r) => Type r
bool = typeFromData Boolean boolRender (text boolRender)


-- Python, Java, C#, and Julia --

extVar :: (CommonRenderSym r) => Label -> Label -> Type r -> Variable r
extVar l n t = mkStateVar (l `access` n) t (R.extVar l n)

-- Python, Java, C++, and Julia --

funcType :: (CommonRenderSym r) => [Type r] -> Type r -> Type r
funcType ps r = typeFromData (Func (map getType ps) (getType r)) "" empty

-- Python, Java, C#, Swift, and Julia --
extFuncAppMixedArgs :: (CommonRenderSym r) => Library -> MixedCall r
extFuncAppMixedArgs l = call (Just l) Nothing

-- Python, C#, Swift, and Julia --

listAccessFunc :: (CommonRenderSym r) => Type r -> Value r -> Function r
listAccessFunc t v = ((`funcFromData` t) . R.listAccessFunc) (intValue v)

listSetFunc :: (CommonRenderSym r) => (Doc -> Doc -> Doc) -> Value r -> Value r ->
  Value r -> Function r
listSetFunc f v idx = (\i toVal -> funcFromData
  (f (RC.value i) (RC.value toVal)) (valueType v)) (intValue idx)

-- Python, Swift, and Julia --

forEach' :: (CommonRenderSym r) => (Variable r -> Value r -> Body r -> Doc)
  -> Variable r -> Value r -> Body r -> Statement r
forEach' f i' v' b' = mkStmtNoEnd (f i' v' b')

-- Python and Julia --

varDecDef :: (CommonRenderSym r) => Variable r -> Scope r -> Maybe (Value r)
  -> Statement r
varDecDef v scp e = undefined {-do
  v' <- zoom lensMStoVS v
  modify $ useVarName (variableName v')
  modify $ setVarScope (variableName v') (scopeData scp)
  def e
  where
    def Nothing = IC.emptyStmt
    def (Just d) = IC.assign v d-}


-- Python, Julia, and MATLAB --

-- | Call to get the size of a list in a language where this is not a method.
listSize :: (CommonRenderSym r) => Value r -> Value r
listSize l =
  let 
    f = S.listSizeFunc l
  in mkVal (RC.functionType f) (RC.function f)