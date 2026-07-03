{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Implementations for C-like renderers are defined here.
module Drasil.Shared.LanguageRenderer.CLike (charRender, float, double, char,
  listType, setType, void, notOp, andOp, orOp, self, litTrue, litFalse, litFloat,
  inlineIf, libFuncAppMixedArgs, libNewObjMixedArgs, listSize, listSize',
  increment, increment1, decrement1, varDec, varDecDef, setDecDef, listDec,
  extObjDecNew, switch, for, while, intFunc, multiAssignError, multiReturnError,
  multiTypeError
) where

import Drasil.FileHandling.Legacy (indent)

import Drasil.Shared.CodeType (CodeType(..))
import Drasil.Shared.InterfaceCommon (UnRepr(..), Label, Library, MSBody,
  TypeElim(..), SVariable, SValue, MSParameter, SMethod, MixedCall,
  MixedCtorCall, VariableSym(..), VariableValue(..), VariableElim(..),
  ValueSym(Value, valueType), getCodeType, getTypeString)
import qualified Drasil.Shared.InterfaceCommon as IC (TypeSym(bool, float, int),
  ValueExpression(funcAppMixedArgs), DeclStatement(varDec, setDec, varDecDef))
import Drasil.GOOL.InterfaceGOOL (AttachmentSym(..), extNewObj,
  objMethodCallNoParams, ($->))
import qualified Drasil.GOOL.InterfaceGOOL as IG (OOTypeSym(obj),
  OOValueExpression(newObjMixedArgs))
import Drasil.Shared.RendererClassesCommon (MSMthdType, CommonRenderSym,
  InternalVarElim(variableBind), RenderValue(valFromData), ValueElim(valuePrec),
  ScopeElim(scopeData))
import qualified Drasil.Shared.RendererClassesCommon as S (
  RenderStatement(stmt, loopStmt))
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
import Drasil.Shared.State (MS, VS, lensMStoVS, lensVStoMS, addLibImportVS,
  getClassName, useVarName, setVarScope)

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

float :: (Monad r) => VS (r TypeData)
float = typeFromData Float floatRender (text floatRender)

double :: (Monad r) => VS (r TypeData)
double = typeFromData Double doubleRender (text doubleRender)

char :: (Monad r) => VS (r TypeData)
char = typeFromData Char charRender (text charRender)

listType :: (Monad r, TypeElim r TypeData, UnRepr r TypeData) => String ->
  VS (r TypeData) -> VS (r TypeData)
listType lst t' = do
  t <- t'
  typeFromData (List (getCodeType t)) (lst
    `containing` getTypeString t) $ text lst <> angles (renderType t)

setType :: (Monad r, TypeElim r TypeData, UnRepr r TypeData) => String ->
  VS (r TypeData) -> VS (r TypeData)
setType lst t' = do
  t <- t'
  typeFromData (Set (getCodeType t)) (lst
    `containing` getTypeString t) $ text lst <> angles (renderType t)

void :: (Monad r) => VS (r TypeData)
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

self :: (OORenderSym r tp vis smt) => SVariable r
self = do
  l <- zoom lensVStoMS getClassName
  mkStateVar R.this (IG.obj l) R.this'

-- Values --

litTrue :: (CommonRenderSym r tp vis smt) => SValue r
litTrue = mkStateVal IC.bool (text "true")

litFalse :: (CommonRenderSym r tp vis smt) => SValue r
litFalse = mkStateVal IC.bool (text "false")

litFloat :: (CommonRenderSym r tp vis smt) => Float -> SValue r
litFloat f = mkStateVal IC.float (D.float f <> text "f")

inlineIf :: (CommonRenderSym r tp vis smt) => SValue r -> SValue r -> SValue r -> SValue r
inlineIf c' v1' v2' = do
  c <- c'
  v1 <- v1'
  v2 <- v2'
  valFromData (prec c) Nothing (toState $ valueType v1)
    (RC.value c <+> text "?" <+> RC.value v1 <+> text ":" <+> RC.value v2)
  where prec cd = valuePrec cd <|> Just 0

libFuncAppMixedArgs :: (CommonRenderSym r tp vis smt) => Library -> MixedCall r tp
libFuncAppMixedArgs l n t vs ns = modify (addLibImportVS l) >>
  IC.funcAppMixedArgs n t vs ns

libNewObjMixedArgs :: (OORenderSym r tp vis smt) => Library -> MixedCtorCall r tp
libNewObjMixedArgs l tp vs ns = modify (addLibImportVS l) >>
  IG.newObjMixedArgs tp vs ns

-- Functions --

listSize :: (OORenderSym r tp vis smt) => String -> SValue r -> SValue r
listSize fnName list = objMethodCallNoParams IC.int list fnName

listSize' :: (OORenderSym r tp vis smt) => String -> SValue r -> SValue r
listSize' lengthName list = valueOf $ list $-> var lengthName IC.int

-- Statements --

increment :: (CommonRenderSym r tp vis smt) => SVariable r -> SValue r -> MS (r smt)
increment vr' v'= do
  vr <- zoom lensMStoVS vr'
  v <- zoom lensMStoVS v'
  mkStmt $ R.addAssign vr v

increment1 :: (CommonRenderSym r tp vis smt) => SVariable r -> MS (r smt)
increment1 vr' = do
  vr <- zoom lensMStoVS vr'
  (mkStmt . R.increment) vr

decrement1 :: (CommonRenderSym r tp vis smt) => SVariable r -> MS (r smt)
decrement1 vr' = do
  vr <- zoom lensMStoVS vr'
  (mkStmt . R.decrement) vr

varDec :: (OORenderSym r TypeData vis smt, UnRepr r TypeData, TypeElim r TypeData) =>
  r (Attachment r) -> r (Attachment r) -> Doc -> SVariable r -> r ScopeData -> MS (r smt)
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

varDecDef :: (CommonRenderSym r tp vis smt) => Terminator -> SVariable r ->
  r ScopeData -> SValue r -> MS (r smt)
varDecDef t vr scp vl' = do
  vd <- IC.varDec vr scp
  vl <- zoom lensMStoVS vl'
  let stmtCtor Empty = mkStmtNoEnd
      stmtCtor Semi = mkStmt
  stmtCtor t (RC.statement vd <+> equals <+> RC.value vl)

setDecDef :: (CommonRenderSym r tp vis smt) => Terminator -> SVariable r ->
  r ScopeData -> SValue r -> MS (r smt)
setDecDef t vr scp vl' = do
  vd <- IC.setDec vr scp
  vl <- zoom lensMStoVS vl'
  let stmtCtor Empty = mkStmtNoEnd
      stmtCtor Semi = mkStmt
  stmtCtor t (RC.statement vd <+> equals <+> RC.value vl)

listDec :: (CommonRenderSym r tp vis smt) => (r (Value r) -> Doc) -> SValue r ->
  SVariable r -> r ScopeData -> MS (r smt)
listDec f vl v scp = do
  sz <- zoom lensMStoVS vl
  vd <- IC.varDec v scp
  mkStmt (RC.statement vd <> f sz)

extObjDecNew :: (OORenderSym r tp vis smt) => Library -> SVariable r ->
  r ScopeData -> [SValue r] -> MS (r smt)
extObjDecNew l v scp vs = IC.varDecDef v scp
  (extNewObj l (onStateValue variableType v) vs)

-- 1st parameter is a Doc function to apply to the render of the control value (i.e. parens)
-- 2nd parameter is a statement to end every case with
switch :: (CommonRenderSym r tp vis smt) => (Doc -> Doc) -> MS (r smt) ->
  SValue r -> [(SValue r, MSBody r)] -> MSBody r -> MS (r smt)
switch f st v cs bod = do
  s <- S.stmt st
  val <- zoom lensMStoVS v
  vals <- mapM (zoom lensMStoVS . fst) cs
  bods <- mapM snd cs
  dflt <- bod
  mkStmt $ R.switch f s val dflt (zip vals bods)

for :: (CommonRenderSym r tp vis smt) => Doc -> Doc -> MS (r smt) -> SValue r ->
  MS (r smt) -> MSBody r -> MS (r smt)
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
while :: (CommonRenderSym r tp vis smt) => (Doc -> Doc) -> Doc -> Doc -> SValue r ->
  MSBody r -> MS (r smt)
while f bStart bEnd v' b'= do
  v <- zoom lensMStoVS v'
  b <- b'
  mkStmtNoEnd (vcat [whileLabel <+> f (RC.value v) <+> bStart,
    indent $ RC.body b,
    bEnd])

-- Methods --

intFunc :: (OORenderSym r tp vis smt) => Bool -> Label -> r vis ->
  r (Attachment r) -> MSMthdType r -> [MSParameter r] -> MSBody r -> SMethod r
intFunc = intMethod

-- Error Messages --

multiAssignError :: String -> String
multiAssignError l = "No multiple assignment statements in " ++ l

multiReturnError :: String -> String
multiReturnError l = "Cannot return multiple values in " ++ l

multiTypeError :: String -> String
multiTypeError l = "Multi-types not supported in " ++ l
