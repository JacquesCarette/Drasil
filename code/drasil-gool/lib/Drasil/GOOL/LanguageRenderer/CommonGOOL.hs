-- | Contains common implementations specific to GOOL
module Drasil.GOOL.LanguageRenderer.CommonGOOL (
  classMethodCall, listAppend, listAdd, innerType
) where

import Drasil.Shared.InterfaceCommon (UnRepr(..), TypeElim(..), NamedArgs,
  TypeSym(void), IndexTranslator(..), getCodeType,
  ValueStatement(valStmt))
import Drasil.GOOL.InterfaceGOOL (objMethodCall, convTypeOO, InternalValueExp,
  OOTypeSym)
import Drasil.Shared.RendererClassesCommon (RenderValue(..))
import Drasil.Shared.LanguageRenderer (dot)
import Drasil.GOOL.Renderers (renderType)
import Drasil.Shared.AST (TypeData)
import Drasil.Shared.State (MS, VS)
import Drasil.Shared.Helpers (getInnerType)

classMethodCall
  :: (RenderValue r var val TypeData, UnRepr r TypeData)
  => String
  -> VS (r TypeData)
  -> VS (r TypeData)
  -> [VS (r val)]
  -> NamedArgs r var val
  -> VS (r val)
classMethodCall f t cls vs ns = do
  c <- cls
  call Nothing (Just $ renderType c <> dot) f t vs ns

listAppend
  :: (TypeSym r typ, InternalValueExp r var val typ, ValueStatement r stmt val)
  => String -> VS (r val) -> VS (r val) -> MS (r stmt)
listAppend fnName list val = valStmt $ objMethodCall void list fnName [val]

listAdd
  ::
    ( TypeSym r typ
    , IndexTranslator r val
    , InternalValueExp r var val typ
    , ValueStatement r stmt val
    )
  => String -> VS (r val) -> VS (r val) -> VS (r val) -> MS (r stmt)
listAdd fnName list idx val = valStmt $ objMethodCall void list fnName [intToIndex idx, val]

innerType
  :: (TypeElim r typ, TypeSym r typ, OOTypeSym r typ)
  => VS (r typ) -> VS (r typ)
innerType t = t >>= (convTypeOO . getInnerType . getCodeType)
