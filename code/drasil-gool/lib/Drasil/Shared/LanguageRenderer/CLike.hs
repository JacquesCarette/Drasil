{-# LANGUAGE PostfixOperators #-}

-- | Implementations for C-like renderers are defined here.
module Drasil.Shared.LanguageRenderer.CLike (charRender, float, double, char,
  listType, setType, void, notOp, andOp, orOp, self, litTrue, litFalse, litFloat,
  inlineIf, libFuncAppMixedArgs, libNewObjMixedArgs, listSize, increment1,
  decrement1, varDec, varDecDef, setDecDef, listDec, extObjDecNew, switch, for, while,
  intFunc, multiAssignError, multiReturnError, multiTypeError
) where

import Utils.Drasil (indent)

import Drasil.Shared.CodeType (CodeType(..))
import Drasil.Shared.InterfaceCommon (Label, Library, MixedCall, MixedCtorCall,
  TypeElim(getType, getTypeString), ScopeSym(..), TypeSym(Type), VariableSym(..),
  StatementSym(..), VariableElim(..), ValueSym(Value, valueType), BodySym(..),
  VisibilitySym(..), ParameterSym (..), MethodSym (..))
import qualified Drasil.Shared.InterfaceCommon as IC (TypeSym(bool, float),
  ValueExpression(funcAppMixedArgs), DeclStatement(varDec, setDec, varDecDef))
import Drasil.GOOL.InterfaceGOOL (PermanenceSym(..), extNewObj, ($.))
import qualified Drasil.GOOL.InterfaceGOOL as IG (OOTypeSym(obj),
  OOValueExpression(newObjMixedArgs))
import Drasil.Shared.RendererClassesCommon (CommonRenderSym, UnaryOpSym(UnaryOp),
  BinaryOpSym(BinaryOp), RenderType(..), InternalVarElim(variableBind),
  RenderValue(valFromData),
  ValueElim(valuePrec), ScopeElim(scopeData), MethodTypeSym (..))
import qualified Drasil.Shared.RendererClassesCommon as S (
  InternalListFunc(listSizeFunc), RenderStatement(loopStmt))
import qualified Drasil.Shared.RendererClassesCommon as RC (BodyElim(..),
  InternalTypeElim(..), InternalVarElim(variable), ValueElim(value),
  StatementElim(statement))
import Drasil.GOOL.RendererClassesOO (OORenderSym,
  OORenderMethod(intMethod))
import qualified Drasil.GOOL.RendererClassesOO as RC (PermElim(..))
import Drasil.Shared.AST (Binding(..), Terminator(..))
import Drasil.Shared.Helpers (angles, onStateValue)
import Drasil.Shared.LanguageRenderer (forLabel, whileLabel, containing)
import qualified Drasil.Shared.LanguageRenderer as R (switch, increment,
  decrement, this', this)
import Drasil.Shared.LanguageRenderer.Constructors (mkStmt, mkStmtNoEnd,
  mkStateVal, mkStateVar, unOpPrec, andPrec, orPrec)
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

float :: (CommonRenderSym r) => Type r
float = typeFromData Float floatRender (text floatRender)

double :: (CommonRenderSym r) => Type r
double = typeFromData Double doubleRender (text doubleRender)

char :: (CommonRenderSym r) => Type r
char = typeFromData Char charRender (text charRender)

listType :: (CommonRenderSym r) => String -> Type r -> Type r
listType lst t' = typeFromData (List (getType t')) (lst
    `containing` getTypeString t') $ text lst <> angles (RC.type' t')

setType :: (OORenderSym r) => String -> Type r -> Type r
setType lst t' = typeFromData (Set (getType t')) (lst
    `containing` getTypeString t') $ text lst <> angles (RC.type' t')

void :: (CommonRenderSym r) => Type r
void = typeFromData Void voidRender (text voidRender)

-- Unary Operators --

notOp :: UnaryOp r
notOp = unOpPrec "!"

-- Binary Operators --

andOp :: BinaryOp r
andOp = andPrec "&&"

orOp :: BinaryOp r
orOp = orPrec "||"
-- Variables --

self :: (OORenderSym r) => Variable r
self = do
  l <- zoom lensVStoMS getClassName
  mkStateVar R.this (IG.obj l) R.this'

-- Values --

litTrue :: (CommonRenderSym r) => Value r
litTrue = mkStateVal IC.bool (text "true")

litFalse :: (CommonRenderSym r) => Value r
litFalse = mkStateVal IC.bool (text "false")

litFloat :: (CommonRenderSym r) => Float -> Value r
litFloat f = mkStateVal IC.float (D.float f <> text "f")

inlineIf :: (CommonRenderSym r) => Value r -> Value r -> Value r -> Value r
inlineIf c' v1' v2' = valFromData (prec c') Nothing (valueType v1')
    (RC.value c' <+> text "?" <+> RC.value v1' <+> text ":" <+> RC.value v2')
  where prec cd = valuePrec cd <|> Just 0

libFuncAppMixedArgs :: (CommonRenderSym r) => Library -> MixedCall r
libFuncAppMixedArgs l n t vs ns = modify (addLibImportVS l) >>
  IC.funcAppMixedArgs n t vs ns

libNewObjMixedArgs :: (OORenderSym r) => Library -> MixedCtorCall r
libNewObjMixedArgs l tp vs ns = modify (addLibImportVS l) >>
  IG.newObjMixedArgs tp vs ns

-- Functions --

listSize :: (OORenderSym r) => Value r -> Value r
listSize v = v $. S.listSizeFunc v

-- Statements --

increment1 :: (CommonRenderSym r) => Variable r -> Statement r
increment1 = mkStmt . R.increment

decrement1 :: (CommonRenderSym r) => Variable r -> Statement r
decrement1 = mkStmt . R.decrement

varDec :: (OORenderSym r) => Permanence r -> Permanence r -> Doc ->
  Variable r -> Scope r -> Statement r
varDec s d pdoc v' scp = do
  v <- zoom lensMStoVS v'
  modify $ useVarName (variableName v)
  modify $ setVarScope (variableName v) (scopeData scp)
  mkStmt (RC.perm (bind $ variableBind v)
    <+> RC.type' (variableType v) <+> (ptrdoc (getType (variableType v)) <>
    RC.variable v))
  where bind Static = s
        bind Dynamic = d
        ptrdoc (List _) = pdoc
        ptrdoc (Set _) = pdoc
        ptrdoc _ = empty

varDecDef :: (CommonRenderSym r) => Terminator -> Variable r -> Scope r ->
  Value r -> Statement r
varDecDef t vr scp vl' = do
  vd <- IC.varDec vr scp
  vl <- zoom lensMStoVS vl'
  let stmtCtor Empty = mkStmtNoEnd
      stmtCtor Semi = mkStmt
  stmtCtor t (RC.statement vd <+> equals <+> RC.value vl)

setDecDef :: (CommonRenderSym r) => Terminator -> Variable r -> Scope r -> Value r ->
  Statement r
setDecDef t vr scp vl' = do
  vd <- IC.setDec vr scp
  vl <- zoom lensMStoVS vl'
  let stmtCtor Empty = mkStmtNoEnd
      stmtCtor Semi = mkStmt
  stmtCtor t (RC.statement vd <+> equals <+> RC.value vl)

listDec :: (CommonRenderSym r) => (Value r -> Doc) -> Value r ->
  Variable r -> Scope r -> Statement r
listDec f vl v scp = do
  sz <- zoom lensMStoVS vl
  vd <- IC.varDec v scp
  mkStmt (RC.statement vd <> f sz)

extObjDecNew :: (OORenderSym r) => Library -> Variable r -> Scope r ->
  [Value r] -> Statement r
extObjDecNew l v scp vs = IC.varDecDef v scp
  (extNewObj l (onStateValue variableType v) vs)

-- 1st parameter is a Doc function to apply to the render of the control value (i.e. parens)
-- 2nd parameter is a statement to end every case with
switch :: (CommonRenderSym r) => (Doc -> Doc) -> Statement r -> Value r ->
  [(Value r, Body r)] -> Body r -> Statement r
switch f st v cs bod =
  let 
    vals = map fst cs
    bods = map snd cs
  in mkStmt $ R.switch f st v bod (zip vals bods)

for :: (CommonRenderSym r) => Doc -> Doc -> Statement r -> Value r ->
  Statement r -> Body r -> Statement r
for bStart bEnd sInit vGuard sUpdate b =
  let
    initl = S.loopStmt sInit
    upd = S.loopStmt sUpdate
  in mkStmtNoEnd $ vcat [
    forLabel <+> parens (RC.statement initl <> semi <+> RC.value vGuard <>
      semi <+> RC.statement upd) <+> bStart,
    indent $ RC.body b,
    bEnd]

-- Doc function parameter is applied to the render of the while-condition
while :: (CommonRenderSym r) => (Doc -> Doc) -> Doc -> Doc -> Value r -> Body r ->
  Statement r
while f bStart bEnd v' b'= mkStmtNoEnd (
  vcat [whileLabel <+> f (RC.value v') <+> bStart,
  indent $ RC.body b',
  bEnd])

-- Methods --

intFunc :: (OORenderSym r) => Bool -> Label -> Visibility  r ->
  Permanence r -> MethodType r -> [Parameter r] -> Body r -> Method r
intFunc = intMethod

-- Error Messages --

multiAssignError :: String -> String
multiAssignError l = "No multiple assignment statements in " ++ l

multiReturnError :: String -> String
multiReturnError l = "Cannot return multiple values in " ++ l

multiTypeError :: String -> String
multiTypeError l = "Multi-types not supported in " ++ l
