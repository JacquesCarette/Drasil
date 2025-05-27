-- | Implementations defined here are valid in some, but not all, language renderers
module Drasil.Shared.LanguageRenderer.Common where

import Drasil.Shared.CodeType (CodeType(..))
import Drasil.Shared.InterfaceCommon 
import Drasil.Shared.RendererClassesCommon (CommonRenderSym, typeFromData, call)
import Drasil.Shared.LanguageRenderer
import qualified Drasil.Shared.LanguageRenderer as R 
import Drasil.Shared.LanguageRenderer.Constructors 
import Prelude hiding (print,pi,(<>))
import Text.PrettyPrint.HughesPJ (text, empty)

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
