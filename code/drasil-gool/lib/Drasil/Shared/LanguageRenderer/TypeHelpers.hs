module Drasil.Shared.LanguageRenderer.TypeHelpers (
  int,
  double,
  float,
  string,
  string',
  bool,
  funcType,
  arrayType
) where

import Drasil.Shared.RendererClassesCommon (CommonRenderSym, typeFromData)
import Text.PrettyPrint.HughesPJ (text)
import Drasil.Shared.InterfaceCommon( VSType)
import qualified Drasil.Shared.RendererClassesCommon as RC (type', variable)



-- | Integer type
int :: (CommonRenderSym r) => VSType r
int = typeFromData Integer intRender (text intRender)

-- | Double/float type
doubleRender :: String
doubleRender = "Double"

double :: (CommonRenderSym r) => VSType r
double = typeFromData Double doubleRender (text doubleRender)

-- | Float type
floatRender :: String
floatRender = "Float"

float :: (CommonRenderSym r) => VSType r
float = typeFromData Float floatRender (text floatRender)

-- | String type
string :: (CommonRenderSym r) => VSType r
string = typeFromData String "string" (text "string")

-- | Alternate string type (if needed)
stringRender' :: String
stringRender' = "String"

string' :: (CommonRenderSym r) => VSType r
string' = typeFromData String stringRender' (text stringRender')

-- | Boolean type
boolRender :: String
boolRender = "Bool"

bool :: (CommonRenderSym r) => VSType r
bool = typeFromData Boolean boolRender (text boolRender)

-- | Function type
funcType :: (CommonRenderSym r) => [VSType r] -> VSType r -> VSType r
funcType ps' r' =  do
  ps <- sequence ps'
  r <- r'
  typeFromData (Func (map getType ps) (getType r)) "" empty

-- | Array type
arrayType :: (CommonRenderSym r) => VSType r -> VSType r
arrayType t' = do
  t <- t'
  typeFromData (Array (getType t))
    (getTypeString t ++ array) (RC.type' t <> brackets empty)