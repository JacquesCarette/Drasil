-- | Implementations defined here are valid in some, but not all, language renderers
module Drasil.Shared.LanguageRenderer.Common where

import Drasil.Shared.CodeType (CodeType(..))
import Drasil.Shared.InterfaceCommon 
import Drasil.Shared.RendererClassesCommon (CommonRenderSym, typeFromData, call, RenderFunction(funcFromData))
import Control.Monad (join)
import Drasil.Shared.LanguageRenderer
import qualified Drasil.Shared.LanguageRenderer as R
import qualified Drasil.Shared.RendererClassesCommon as RC (value)
import Drasil.Shared.LanguageRenderer.Constructors 
import Prelude hiding (print,pi,(<>))
import Drasil.Shared.Helpers
import Text.PrettyPrint.HughesPJ (text, empty, Doc)

-- Swift and Julia --

boolRender :: String
boolRender = "Bool"

bool :: (CommonRenderSym r) => VSType r
bool = typeFromData Boolean boolRender (text boolRender)


-- Python, Java, C#, and Julia --

extVar :: (CommonRenderSym r) => Label -> Label -> VSType r -> SVariable r
extVar l n t = mkStateVar (l `access` n) t (R.extVar l n)

-- Python, Java, C++, and Julia --

funcType :: (CommonRenderSym r) => [VSType r] -> VSType r -> VSType r
funcType ps' r' =  do
  ps <- sequence ps'
  r <- r'
  typeFromData (Func (map getType ps) (getType r)) "" empty

-- Python, Java, C#, Swift, and Julia --
extFuncAppMixedArgs :: (CommonRenderSym r) => Library -> MixedCall r
extFuncAppMixedArgs l = call (Just l) Nothing

-- Python, C#, Swift, and Julia --

listAccessFunc :: (CommonRenderSym r) => VSType r -> SValue r -> VSFunction r
listAccessFunc t v = intValue v >>= ((`funcFromData` t) . R.listAccessFunc)

listSetFunc :: (CommonRenderSym r) => (Doc -> Doc -> Doc) -> SValue r -> SValue r ->
  SValue r -> VSFunction r
listSetFunc f v idx setVal = join $ on2StateValues (\i toVal -> funcFromData
  (f (RC.value i) (RC.value toVal)) (onStateValue valueType v)) (intValue idx)
  setVal

