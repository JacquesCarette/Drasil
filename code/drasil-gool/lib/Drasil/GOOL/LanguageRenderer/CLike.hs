{-# LANGUAGE PostfixOperators #-}

-- | Implementations for C-like renderers are defined here.
module Drasil.GOOL.LanguageRenderer.CLike (charRender, float, double, char, 
  listType, setType, void, notOp, andOp, orOp, inOp, self, litTrue, litFalse, litFloat, 
  inlineIf, libFuncAppMixedArgs, libNewObjMixedArgs, listSize, increment1, 
  decrement1, varDec, varDecDef, listDec, extObjDecNew, switch, for, while, 
  intFunc, multiAssignError, multiReturnError, multiTypeError
) where

import Utils.Drasil (indent)

import Drasil.GOOL.CodeType (CodeType(..))
import Drasil.GOOL.InterfaceCommon (Label, Library, MSBody, VSType, SVariable, 
  SValue, MSStatement, MSParameter, SMethod, MixedCall, MixedCtorCall, 
  TypeElim(getType, getTypeString), 
  VariableElim(..), ValueSym(Value, valueType), VisibilitySym(..))
import qualified Drasil.GOOL.InterfaceCommon as IC (TypeSym(bool, float),
  ValueExpression(funcAppMixedArgs), DeclStatement(varDec, varDecDef))
import Drasil.GOOL.InterfaceGOOL (PermanenceSym(..), extNewObj, ($.))
import qualified Drasil.GOOL.InterfaceGOOL as IG (OOTypeSym(obj),
  OOValueExpression(newObjMixedArgs))
import Drasil.GOOL.RendererClassesCommon (MSMthdType, CommonRenderSym,
  RenderType(..), InternalVarElim(variableBind), RenderValue(valFromData), 
  ValueElim(valuePrec))
import qualified Drasil.GOOL.RendererClassesCommon as S (
  InternalListFunc(listSizeFunc), RenderStatement(stmt, loopStmt))
import qualified Drasil.GOOL.RendererClassesCommon as RC (BodyElim(..),
  InternalTypeElim(..), InternalVarElim(variable), ValueElim(value),
  StatementElim(statement))
import Drasil.GOOL.RendererClassesOO (OORenderSym,
  OORenderMethod(intMethod))
import qualified Drasil.GOOL.RendererClassesOO as RC (PermElim(..))
import Drasil.GOOL.AST (Binding(..), Terminator(..))
import Drasil.GOOL.Helpers (angles, toState, onStateValue)
import Drasil.GOOL.LanguageRenderer (forLabel, whileLabel, containing)
import qualified Drasil.GOOL.LanguageRenderer as R (switch, increment, 
  decrement, this', this)
import Drasil.GOOL.LanguageRenderer.Constructors (mkStmt, mkStmtNoEnd, 
  mkStateVal, mkStateVar, VSOp, unOpPrec, andPrec, orPrec, inPrec)
import Drasil.GOOL.State (lensMStoVS, lensVStoMS, addLibImportVS, getClassName,
  useVarName)

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

float :: (CommonRenderSym r) => VSType r
float = typeFromData Float floatRender (text floatRender)

double :: (CommonRenderSym r) => VSType r
double = typeFromData Double doubleRender (text doubleRender)

char :: (CommonRenderSym r) => VSType r
char = typeFromData Char charRender (text charRender)

listType :: (CommonRenderSym r) => String -> VSType r -> VSType r
listType lst t' = do 
  t <- t'
  typeFromData (List (getType t)) (lst 
    `containing` getTypeString t) $ text lst <> angles (RC.type' t) 

setType :: (OORenderSym r) => String -> VSType r -> VSType r
setType lst t' = do 
  t <- t'
  typeFromData (Set (getType t)) (lst 
    `containing` getTypeString t) $ text lst <> angles (RC.type' t) 

void :: (CommonRenderSym r) => VSType r
void = typeFromData Void voidRender (text voidRender)

-- Unary Operators --

notOp :: (Monad r) => VSOp r
notOp = unOpPrec "!"

-- Binary Operators --

andOp :: (Monad r) => VSOp r
andOp = andPrec "&&"

orOp :: (Monad r) => VSOp r
orOp = orPrec "||"

inOp :: (Monad r) => VSOp r
inOp = inPrec ""
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

increment1 :: (CommonRenderSym r) => SVariable r -> MSStatement r
increment1 vr' = do 
  vr <- zoom lensMStoVS vr'
  (mkStmt . R.increment) vr

decrement1 :: (CommonRenderSym r) => SVariable r -> MSStatement r
decrement1 vr' = do 
  vr <- zoom lensMStoVS vr'
  (mkStmt . R.decrement) vr

varDec :: (OORenderSym r) => r (Permanence r) -> r (Permanence r) -> Doc -> 
  SVariable r -> MSStatement r
varDec s d pdoc v' = do 
  v <- zoom lensMStoVS v' 
  modify $ useVarName (variableName v)
  mkStmt (RC.perm (bind $ variableBind v)
    <+> RC.type' (variableType v) <+> (ptrdoc (getType (variableType v)) <> 
    RC.variable v))
  where bind Static = s
        bind Dynamic = d
        ptrdoc (List _) = pdoc
        ptrdoc _ = empty

varDecDef :: (CommonRenderSym r) => Terminator -> SVariable r -> SValue r -> 
  MSStatement r
varDecDef t vr vl' = do 
  vd <- IC.varDec vr
  vl <- zoom lensMStoVS vl'
  let stmtCtor Empty = mkStmtNoEnd
      stmtCtor Semi = mkStmt
  stmtCtor t (RC.statement vd <+> equals <+> RC.value vl)

listDec :: (CommonRenderSym r) => (r (Value r) -> Doc) -> SValue r -> SVariable r -> 
  MSStatement r
listDec f vl v = do 
  sz <- zoom lensMStoVS vl
  vd <- IC.varDec v
  mkStmt (RC.statement vd <> f sz)
  
extObjDecNew :: (OORenderSym r) => Library -> SVariable r -> [SValue r] -> 
  MSStatement r
extObjDecNew l v vs = IC.varDecDef v (extNewObj l (onStateValue variableType v)
  vs)

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
  r (Permanence r) -> MSMthdType r -> [MSParameter r] -> MSBody r -> SMethod r
intFunc = intMethod

-- Error Messages --

multiAssignError :: String -> String
multiAssignError l = "No multiple assignment statements in " ++ l

multiReturnError :: String -> String
multiReturnError l = "Cannot return multiple values in " ++ l

multiTypeError :: String -> String
multiTypeError l = "Multi-types not supported in " ++ l
