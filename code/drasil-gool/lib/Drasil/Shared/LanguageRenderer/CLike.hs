{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Implementations for C-like renderers are defined here.
module Drasil.Shared.LanguageRenderer.CLike (charRender, float, double, char,
  listType, setType, void, notOp, andOp, orOp, self, litTrue, litFalse, litFloat,
  inlineIf, libFuncAppMixedArgs, libNewObjMixedArgs, listSize, increment,
  increment1, decrement1, varDec, varDecDef, setDecDef, listDec, extObjDecNew,
  switch, for, while, intFunc, multiAssignError, multiReturnError, multiTypeError
) where

import Drasil.FileHandling.Legacy (indent)

import Drasil.Shared.CodeType (CodeType(..))
import Drasil.Shared.InterfaceCommon (UnRepr(..), Label, Library, MSBody, VSType,
  SVariable, SValue, MSStatement, MSParameter, SMethod, MixedCall, MixedCtorCall,
  VariableElim(..), ValueSym(Value, valueType), VisibilitySym(..), getCodeType,
  getTypeString)
import qualified Drasil.Shared.InterfaceCommon as IC (TypeSym(bool, float),
  ValueExpression(funcAppMixedArgs), DeclStatement(varDec, setDec, varDecDef))
import Drasil.GOOL.InterfaceGOOL (AttachmentSym(..), extNewObj, ($.))
import qualified Drasil.GOOL.InterfaceGOOL as IG (OOTypeSym(obj),
  OOValueExpression(newObjMixedArgs))
import Drasil.Shared.RendererClassesCommon (MSMthdType, CommonRenderSym,
  InternalVarElim(variableBind), RenderValue(valFromData), ValueElim(valuePrec),
  ScopeElim(scopeData))
import qualified Drasil.Shared.RendererClassesCommon as S (
  InternalListFunc(listSizeFunc), RenderStatement(stmt, loopStmt))
import qualified Drasil.Shared.RendererClassesCommon as RC (BodyElim(..),
  InternalVarElim(variable), ValueElim(value), StatementElim(statement))
import Drasil.GOOL.RendererClassesOO (OORenderSym,
  OORenderMethod(intMethod))
import qualified Drasil.GOOL.RendererClassesOO as RC (PermElim(..))
import Drasil.GOOL.Renderers (renderType)
import Drasil.Shared.AST (AttachmentTag(..), Terminator(..), ScopeData,
  TypeData)
import Drasil.Shared.Helpers (angles, toState, onStateValue)
import Drasil.Shared.LanguageRenderer (forLabel, whileLabel, containing)
import qualified Drasil.Shared.LanguageRenderer as R (switch, addAssign,
  increment, decrement, this', this)
import Drasil.Shared.LanguageRenderer.Constructors (typeFromData, mkStmt,
  mkStmtNoEnd, mkStateVal, mkStateVar, VSOp, unOpPrec, andPrec, orPrec)
import Drasil.Shared.State (lensMStoVS, lensVStoMS, addLibImportVS, getClassName,
  useVarName, setVarScope)

import Prelude hiding (break,(<>))
import Control.Applicative ((<|>))
import Control.Monad.State (modify)
import Control.Lens.Zoom (zoom)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), parens, vcat, semi,
  equals, empty)
import qualified Text.PrettyPrint.HughesPJ as D (float)

-- Types --

floatRender, doubleRender, charRender, voidRender :: String
floatRender = "float"
doubleRender = "double"
charRender = "char"
voidRender = "void"

float :: (Monad r) => VSType r
float = typeFromData Float floatRender (text floatRender)

double :: (Monad r) => VSType r
double = typeFromData Double doubleRender (text doubleRender)

char :: (Monad r) => VSType r
char = typeFromData Char charRender (text charRender)

listType :: (Monad r, UnRepr r TypeData) => String ->
  VSType r -> VSType r
listType lst t' = do
  t <- t'
  typeFromData (List (getCodeType t)) (lst
    `containing` getTypeString t) $ text lst <> angles (renderType t)

setType :: (Monad r, UnRepr r TypeData) => String -> VSType r ->
  VSType r
setType lst t' = do
  t <- t'
  typeFromData (Set (getCodeType t)) (lst
    `containing` getTypeString t) $ text lst <> angles (renderType t)

void :: (Monad r) => VSType r
void = typeFromData Void voidRender (text voidRender)

-- Unary Operators --

notOp :: (Monad r) => VSOp r
notOp = unOpPrec "!"

-- Binary Operators --

andOp :: (Monad r) => VSOp r
andOp = andPrec "&&"

orOp :: (Monad r) => VSOp r
orOp = orPrec "||"
-- Variables --

self :: (OORenderSym r) => SVariable r
self = do
  l <- zoom lensVStoMS getClassName
  mkStateVar R.this (IG.obj l) R.this'

-- Values --

litTrue :: (CommonRenderSym r) => SValue r
litTrue = mkStateVal IC.bool (text "true")

litFalse :: (CommonRenderSym r) => SValue r
litFalse = mkStateVal IC.bool (text "false")

litFloat :: (CommonRenderSym r) => Float -> SValue r
litFloat f = mkStateVal IC.float (D.float f <> text "f")

inlineIf :: (CommonRenderSym r) => SValue r -> SValue r -> SValue r -> SValue r
inlineIf c' v1' v2' = do
  c <- c'
  v1 <- v1'
  v2 <- v2'
  valFromData (prec c) Nothing (toState $ valueType v1)
    (RC.value c <+> text "?" <+> RC.value v1 <+> text ":" <+> RC.value v2)
  where prec cd = valuePrec cd <|> Just 0

libFuncAppMixedArgs :: (CommonRenderSym r) => Library -> MixedCall r
libFuncAppMixedArgs l n t vs ns = modify (addLibImportVS l) >>
  IC.funcAppMixedArgs n t vs ns

libNewObjMixedArgs :: (OORenderSym r) => Library -> MixedCtorCall r
libNewObjMixedArgs l tp vs ns = modify (addLibImportVS l) >>
  IG.newObjMixedArgs tp vs ns

-- Functions --

listSize :: (OORenderSym r) => SValue r -> SValue r
listSize v = v $. S.listSizeFunc v

-- Statements --

increment :: (CommonRenderSym r) => SVariable r -> SValue r -> MSStatement r
increment vr' v'= do
  vr <- zoom lensMStoVS vr'
  v <- zoom lensMStoVS v'
  mkStmt $ R.addAssign vr v

increment1 :: (CommonRenderSym r) => SVariable r -> MSStatement r
increment1 vr' = do
  vr <- zoom lensMStoVS vr'
  (mkStmt . R.increment) vr

decrement1 :: (CommonRenderSym r) => SVariable r -> MSStatement r
decrement1 vr' = do
  vr <- zoom lensMStoVS vr'
  (mkStmt . R.decrement) vr

varDec :: (OORenderSym r, UnRepr r TypeData) => r (Attachment r) ->
  r (Attachment r) -> Doc -> SVariable r -> r ScopeData -> MSStatement r
varDec s d pdoc v' scp = do
  v <- zoom lensMStoVS v'
  modify $ useVarName (variableName v)
  modify $ setVarScope (variableName v) (scopeData scp)
  mkStmt (RC.perm (bind $ variableBind v)
    <+> renderType (variableType v) <+> (ptrdoc (getCodeType (variableType v)) <>
    RC.variable v))
  where bind ClassLevel = s
        bind InstanceLevel = d
        ptrdoc (List _) = pdoc
        ptrdoc (Set _) = pdoc
        ptrdoc _ = empty

varDecDef :: (CommonRenderSym r) => Terminator -> SVariable r -> r ScopeData ->
  SValue r -> MSStatement r
varDecDef t vr scp vl' = do
  vd <- IC.varDec vr scp
  vl <- zoom lensMStoVS vl'
  let stmtCtor Empty = mkStmtNoEnd
      stmtCtor Semi = mkStmt
  stmtCtor t (RC.statement vd <+> equals <+> RC.value vl)

setDecDef :: (CommonRenderSym r) => Terminator -> SVariable r -> r ScopeData -> SValue r ->
  MSStatement r
setDecDef t vr scp vl' = do
  vd <- IC.setDec vr scp
  vl <- zoom lensMStoVS vl'
  let stmtCtor Empty = mkStmtNoEnd
      stmtCtor Semi = mkStmt
  stmtCtor t (RC.statement vd <+> equals <+> RC.value vl)

listDec :: (CommonRenderSym r) => (r (Value r) -> Doc) -> SValue r ->
  SVariable r -> r ScopeData -> MSStatement r
listDec f vl v scp = do
  sz <- zoom lensMStoVS vl
  vd <- IC.varDec v scp
  mkStmt (RC.statement vd <> f sz)

extObjDecNew :: (OORenderSym r) => Library -> SVariable r -> r ScopeData ->
  [SValue r] -> MSStatement r
extObjDecNew l v scp vs = IC.varDecDef v scp
  (extNewObj l (onStateValue variableType v) vs)

-- 1st parameter is a Doc function to apply to the render of the control value (i.e. parens)
-- 2nd parameter is a statement to end every case with
switch :: (CommonRenderSym r) => (Doc -> Doc) -> MSStatement r -> SValue r ->
  [(SValue r, MSBody r)] -> MSBody r -> MSStatement r
switch f st v cs bod = do
  s <- S.stmt st
  val <- zoom lensMStoVS v
  vals <- mapM (zoom lensMStoVS . fst) cs
  bods <- mapM snd cs
  dflt <- bod
  mkStmt $ R.switch f s val dflt (zip vals bods)

for :: (CommonRenderSym r) => Doc -> Doc -> MSStatement r -> SValue r ->
  MSStatement r -> MSBody r -> MSStatement r
for bStart bEnd sInit vGuard sUpdate b = do
  initl <- S.loopStmt sInit
  guard <- zoom lensMStoVS vGuard
  upd <- S.loopStmt sUpdate
  bod <- b
  mkStmtNoEnd $ vcat [
    forLabel <+> parens (RC.statement initl <> semi <+> RC.value guard <>
      semi <+> RC.statement upd) <+> bStart,
    indent $ RC.body bod,
    bEnd]

-- Doc function parameter is applied to the render of the while-condition
while :: (CommonRenderSym r) => (Doc -> Doc) -> Doc -> Doc -> SValue r -> MSBody r ->
  MSStatement r
while f bStart bEnd v' b'= do
  v <- zoom lensMStoVS v'
  b <- b'
  mkStmtNoEnd (vcat [whileLabel <+> f (RC.value v) <+> bStart,
    indent $ RC.body b,
    bEnd])

-- Methods --

intFunc :: (OORenderSym r) => Bool -> Label -> r (Visibility  r) ->
  r (Attachment r) -> MSMthdType r -> [MSParameter r] -> MSBody r -> SMethod r
intFunc = intMethod

-- Error Messages --

multiAssignError :: String -> String
multiAssignError l = "No multiple assignment statements in " ++ l

multiReturnError :: String -> String
multiReturnError l = "Cannot return multiple values in " ++ l

multiTypeError :: String -> String
multiTypeError l = "Multi-types not supported in " ++ l
