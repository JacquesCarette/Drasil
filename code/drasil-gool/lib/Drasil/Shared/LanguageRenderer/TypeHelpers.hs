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

import Drasil.Shared.RendererClassesCommon (CommonRenderSym, VSType)
import Text.PrettyPrint.HughesPJ (text)

-- | Integer type
int :: (CommonRenderSym r) => VSType r
int = typeFromData Integer "int" (text "int")

-- | Double/float type
double :: (CommonRenderSym r) => VSType r
double = typeFromData Double "double" (text "double")

-- | Float type
float :: (CommonRenderSym r) => VSType r
float = typeFromData Float "float" (text "float")

-- | String type
string :: (CommonRenderSym r) => VSType r
string = typeFromData String "string" (text "string")

-- | Alternate string type (if needed)
string' :: (CommonRenderSym r) => VSType r
string' = typeFromData String "String" (text "String")

-- | Boolean type
bool :: (CommonRenderSym r) => VSType r
bool = typeFromData Boolean "bool" (text "bool")

-- | Function type
funcType :: (CommonRenderSym r) => [VSType r] -> VSType r -> VSType r
funcType args ret = typeFromData (Function args ret) "func" (text "func")

-- | Array type
arrayType :: (CommonRenderSym r) => VSType r -> VSType r
arrayType t = typeFromData (Array t) (show t ++ "[]") (text (show t ++ "[]"))