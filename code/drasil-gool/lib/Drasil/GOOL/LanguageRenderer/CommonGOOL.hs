-- | Contains common implementations specific to GOOL
module Drasil.GOOL.LanguageRenderer.CommonGOOL (
  constDecDef, classMethodCall, listAppend, listAdd, innerType
) where

import Drasil.Shared.InterfaceCommon (UnRepr(..), TypeElim(..), SVariable,
  SValue, NamedArgs, VariableElim(..), TypeSym(void), IndexTranslator(..),
  getCodeType, ValueStatement(valStmt))
import Drasil.GOOL.InterfaceGOOL (objMethodCall, convTypeOO, InternalValueExp,
  OOTypeSym)
import Drasil.Shared.RendererClassesCommon (ScopeElim(..), RenderValue(..),
  InternalVarElim, RenderStatement, ValueElim)
import Drasil.Shared.LanguageRenderer.Constructors (mkStmt)
import Drasil.Shared.LanguageRenderer (dot)
import Drasil.GOOL.Renderers (renderType, renderConstDecDef)
import Drasil.Shared.AST (TypeData, ScopeData)
import Drasil.Shared.State (MS, VS, lensMStoVS, useVarName, setVarScope)
import Drasil.Shared.Helpers (getInnerType)

import Control.Lens.Zoom (zoom)
import Control.Monad.State (modify)

constDecDef
  :: ( InternalVarElim r
     , RenderStatement r stmt
     , ScopeElim r
     , UnRepr r TypeData
     , ValueElim r
     , VariableElim r TypeData
     )
  => SVariable r -> r ScopeData -> SValue r -> MS (r stmt)
constDecDef vr' scp v'= do
  vr <- zoom lensMStoVS vr'
  v <- zoom lensMStoVS v'
  modify $ useVarName $ variableName vr
  modify $ setVarScope (variableName vr) (scopeData scp)
  mkStmt (renderConstDecDef vr v)

classMethodCall
  :: (RenderValue r TypeData, UnRepr r TypeData)
  => String
  -> VS (r TypeData)
  -> VS (r TypeData)
  -> [SValue r]
  -> NamedArgs r
  -> SValue r
classMethodCall f t cls vs ns = do
  c <- cls
  call Nothing (Just $ renderType c <> dot) f t vs ns

listAppend
  :: (TypeSym r typ, InternalValueExp r typ, ValueStatement r stmt)
  => String -> SValue r -> SValue r -> MS (r stmt)
listAppend fnName list val = valStmt $ objMethodCall void list fnName [val]

listAdd
  ::
    ( TypeSym r typ
    , IndexTranslator r
    , InternalValueExp r typ
    , ValueStatement r stmt
    )
  => String -> SValue r -> SValue r -> SValue r -> MS (r stmt)
listAdd fnName list idx val = valStmt $ objMethodCall void list fnName [intToIndex idx, val]

innerType
  :: (TypeElim r typ, TypeSym r typ, OOTypeSym r typ)
  => VS (r typ) -> VS (r typ)
innerType t = t >>= (convTypeOO . getInnerType . getCodeType)
