module Drasil.Shared.LanguageRenderer.CollectionHelpers (
  listAddFunc,
  listAdd,
  listAppend,
  listSizeFunc,
  listSize,
  listAccessFunc,
  listAccessFunc',
  listSetFunc,
  litArray,
  litSet,
  litSetFunc,
  listDecDef,
  setDecDef,
  setDec,
  arrayType,
  arrayDec,
  arrayDecDef
) where

import Drasil.Shared.RendererClassesCommon (CommonRenderSym, SValue, VSFunction, VSType, MSStatement)
import Text.PrettyPrint.HughesPJ (Doc)
import Drasil.Shared.InterfaceCommon (SVariable)
import qualified Drasil.Shared.InterfaceCommon as IC

-- | Add an element to a list (function)
listAddFunc :: (CommonRenderSym r) => SValue r -> SValue r -> VSFunction r
listAddFunc l v = funcFromData (RC.type' $ valueType l) (RC.value l <> ".add(" <> RC.value v <> ")")

-- | Add an element to a list
listAdd :: (CommonRenderSym r) => SValue r -> SValue r -> MSStatement r
listAdd l v = stmtFromData (RC.value l <> ".add(" <> RC.value v <> ")")

-- | Append a list to another list
listAppend :: (CommonRenderSym r) => SValue r -> SValue r -> MSStatement r
listAppend l1 l2 = stmtFromData (RC.value l1 <> ".extend(" <> RC.value l2 <> ")")

-- | Get the size of a list (function)
listSizeFunc :: (CommonRenderSym r) => SValue r -> VSFunction r
listSizeFunc l = funcFromData (RC.type' $ valueType l) (RC.value l <> ".size()")

-- | Get the size of a list
listSize :: (CommonRenderSym r) => SValue r -> SValue r
listSize l = valueFromData (RC.type' $ valueType l) (RC.value l <> ".size()")

-- | Access an element in a list (function)
listAccessFunc :: (CommonRenderSym r) => VSType r -> SValue r -> VSFunction r
listAccessFunc t l = funcFromData (RC.type' t) (RC.value l <> "[i]")

-- | Access an element in a list (alternate)
listAccessFunc' :: (CommonRenderSym r) => SValue r -> VSFunction r
listAccessFunc' l = funcFromData (RC.type' $ valueType l) (RC.value l <> "[i]")

-- | Set an element in a list
listSetFunc :: (CommonRenderSym r) => (Doc -> Doc -> Doc) -> SValue r -> SValue r -> SValue r -> VSFunction r
listSetFunc f l idx setVal = funcFromData (RC.type' $ valueType l) (f (RC.value l) (RC.value idx) <> " = " <> RC.value setVal)

-- | Literal array
litArray :: (CommonRenderSym r) => [SValue r] -> SValue r
litArray vals = valueFromData (RC.type' $ valueType $ head vals) ("[" <> mconcat (intersperse ", " (map RC.value vals)) <> "]")

-- | Literal set
litSet :: (CommonRenderSym r) => [SValue r] -> SValue r
litSet vals = valueFromData (RC.type' $ valueType $ head vals) ("{" <> mconcat (intersperse ", " (map RC.value vals)) <> "}")

-- | Literal set (function)
litSetFunc :: (CommonRenderSym r) => [SValue r] -> VSFunction r
litSetFunc vals = funcFromData (RC.type' $ valueType $ head vals) ("{" <> mconcat (intersperse ", " (map RC.value vals)) <> "}")

-- | List declaration with definition
listDecDef :: (CommonRenderSym r) => SVariable r -> SValue r -> MSStatement r
listDecDef v val = stmtFromData (RC.variable v <> " = " <> RC.value val)

-- | Set declaration with definition
setDecDef :: (CommonRenderSym r) => SVariable r -> SValue r -> MSStatement r
setDecDef v val = stmtFromData (RC.variable v <> " = " <> RC.value val)

-- | Set declaration
setDec :: (CommonRenderSym r) => SVariable r -> MSStatement r
setDec v = stmtFromData (RC.variable v <> " = set()")

-- | Array type
arrayType :: (CommonRenderSym r) => VSType r -> VSType r
arrayType t = typeFromData (RC.type' t) (RC.type' t <> "[]")

-- | Array declaration
arrayDec :: (CommonRenderSym r) => SVariable r -> MSStatement r
arrayDec v = stmtFromData (RC.variable v <> " = []")

-- | Array declaration with definition
arrayDecDef :: (CommonRenderSym r) => SVariable r -> SValue r -> MSStatement r
arrayDecDef v val = stmtFromData (RC.variable v <> " = " <> RC.value val)