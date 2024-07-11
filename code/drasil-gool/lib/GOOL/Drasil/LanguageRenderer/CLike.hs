{-# LANGUAGE PostfixOperators #-}

-- | Implementations for C-like renderers are defined here.
module GOOL.Drasil.LanguageRenderer.CLike (charRender, float, double, char, 
  listType, void, notOp, andOp, orOp, self, litTrue, litFalse, litFloat, 
  inlineIf, libFuncAppMixedArgs, libNewObjMixedArgs, listSize, increment1, 
  decrement1, varDec, varDecDef, listDec, extObjDecNew, switch, for, while, 
  intFunc, multiAssignError, multiReturnError, multiTypeError
) where

import Utils.Drasil (indent)

import GOOL.Drasil.CodeType (CodeType(..))
import GOOL.Drasil.InterfaceCommon (Label, Library, MSBody, VSType, SVariable, 
  SValue, MSStatement, MSParameter, SMethod, MixedCall, MixedCtorCall, 
  PermanenceSym(..), TypeElim(getType, getTypeString), 
  VariableElim(..), ValueSym(Value, valueType), ScopeSym(..))
import qualified GOOL.Drasil.InterfaceCommon as IC (TypeSym(bool, float),
  ValueExpression(funcAppMixedArgs), DeclStatement(varDec, varDecDef))
import GOOL.Drasil.InterfaceGOOL (extNewObj, ($.))
import qualified GOOL.Drasil.InterfaceGOOL as IG (OOTypeSym(obj),
  OOValueExpression(newObjMixedArgs))
import GOOL.Drasil.RendererClasses (MSMthdType, RenderSym, RenderType(..),
  InternalVarElim(variableBind), RenderValue(valFromData), 
  ValueElim(valuePrec), RenderMethod(intMethod))
import qualified GOOL.Drasil.RendererClasses as S (
  InternalListFunc(listSizeFunc), RenderStatement(stmt, loopStmt))
import qualified GOOL.Drasil.RendererClasses as RC (PermElim(..), BodyElim(..), 
  InternalTypeElim(..), InternalVarElim(variable), ValueElim(value), 
  StatementElim(statement))
import GOOL.Drasil.AST (Binding(..), Terminator(..))
import GOOL.Drasil.Helpers (angles, toState, onStateValue)
import GOOL.Drasil.LanguageRenderer (forLabel, whileLabel, containing)
import qualified GOOL.Drasil.LanguageRenderer as R (switch, increment, 
  decrement, this', this)
import GOOL.Drasil.LanguageRenderer.Constructors (mkStmt, mkStmtNoEnd, 
  mkStateVal, mkStateVar, VSOp, unOpPrec, andPrec, orPrec)
import GOOL.Drasil.State (lensMStoVS, lensVStoMS, addLibImportVS, getClassName,
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

float :: (RenderSym r) => VSType r
float = typeFromData Float floatRender (text floatRender)

double :: (RenderSym r) => VSType r
double = typeFromData Double doubleRender (text doubleRender)

char :: (RenderSym r) => VSType r
char = typeFromData Char charRender (text charRender)

listType :: (RenderSym r) => String -> VSType r -> VSType r
listType lst t' = do 
  t <- t'
  typeFromData (List (getType t)) (lst 
    `containing` getTypeString t) $ text lst <> angles (RC.type' t) 

void :: (RenderSym r) => VSType r
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

self :: (RenderSym r) => SVariable r
self = do 
  l <- zoom lensVStoMS getClassName 
  mkStateVar R.this (IG.obj l) R.this'

-- Values --

litTrue :: (RenderSym r) => SValue r
litTrue = mkStateVal IC.bool (text "true")

litFalse :: (RenderSym r) => SValue r
litFalse = mkStateVal IC.bool (text "false")

litFloat :: (RenderSym r) => Float -> SValue r
litFloat f = mkStateVal IC.float (D.float f <> text "f")

inlineIf :: (RenderSym r) => SValue r -> SValue r -> SValue r -> SValue r
inlineIf c' v1' v2' = do
  c <- c'
  v1 <- v1'
  v2 <- v2' 
  valFromData (prec c) Nothing (toState $ valueType v1) 
    (RC.value c <+> text "?" <+> RC.value v1 <+> text ":" <+> RC.value v2) 
  where prec cd = valuePrec cd <|> Just 0

libFuncAppMixedArgs :: (RenderSym r) => Library -> MixedCall r
libFuncAppMixedArgs l n t vs ns = modify (addLibImportVS l) >> 
  IC.funcAppMixedArgs n t vs ns
  
libNewObjMixedArgs :: (RenderSym r) => Library -> MixedCtorCall r
libNewObjMixedArgs l tp vs ns = modify (addLibImportVS l) >> 
  IG.newObjMixedArgs tp vs ns

-- Functions --

listSize :: (RenderSym r) => SValue r -> SValue r
listSize v = v $. S.listSizeFunc v

-- Statements --

increment1 :: (RenderSym r) => SVariable r -> MSStatement r
increment1 vr' = do 
  vr <- zoom lensMStoVS vr'
  (mkStmt . R.increment) vr

decrement1 :: (RenderSym r) => SVariable r -> MSStatement r
decrement1 vr' = do 
  vr <- zoom lensMStoVS vr'
  (mkStmt . R.decrement) vr

varDec :: (RenderSym r) => r (Permanence r) -> r (Permanence r) -> Doc -> 
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

varDecDef :: (RenderSym r) => Terminator -> SVariable r -> SValue r -> 
  MSStatement r
varDecDef t vr vl' = do 
  vd <- IC.varDec vr
  vl <- zoom lensMStoVS vl'
  let stmtCtor Empty = mkStmtNoEnd
      stmtCtor Semi = mkStmt
  stmtCtor t (RC.statement vd <+> equals <+> RC.value vl)

listDec :: (RenderSym r) => (r (Value r) -> Doc) -> SValue r -> SVariable r -> 
  MSStatement r
listDec f vl v = do 
  sz <- zoom lensMStoVS vl
  vd <- IC.varDec v
  mkStmt (RC.statement vd <> f sz)
  
extObjDecNew :: (RenderSym r) => Library -> SVariable r -> [SValue r] -> 
  MSStatement r
extObjDecNew l v vs = IC.varDecDef v (extNewObj l (onStateValue variableType v)
  vs)

-- 1st parameter is a Doc function to apply to the render of the control value (i.e. parens)
-- 2nd parameter is a statement to end every case with
switch :: (RenderSym r) => (Doc -> Doc) -> MSStatement r -> SValue r -> 
  [(SValue r, MSBody r)] -> MSBody r -> MSStatement r
switch f st v cs bod = do
  s <- S.stmt st
  val <- zoom lensMStoVS v
  vals <- mapM (zoom lensMStoVS . fst) cs
  bods <- mapM snd cs
  dflt <- bod
  mkStmt $ R.switch f s val dflt (zip vals bods)

for :: (RenderSym r) => Doc -> Doc -> MSStatement r -> SValue r -> 
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
while :: (RenderSym r) => (Doc -> Doc) -> Doc -> Doc -> SValue r -> MSBody r -> 
  MSStatement r
while f bStart bEnd v' b'= do 
  v <- zoom lensMStoVS v'
  b <- b'
  mkStmtNoEnd (vcat [whileLabel <+> f (RC.value v) <+> bStart, 
    indent $ RC.body b, 
    bEnd]) 

-- Methods --

intFunc :: (RenderSym r) => Bool -> Label -> r (Scope r) -> r (Permanence r) -> 
  MSMthdType r -> [MSParameter r] -> MSBody r -> SMethod r
intFunc = intMethod

-- Error Messages --

multiAssignError :: String -> String
multiAssignError l = "No multiple assignment statements in " ++ l

multiReturnError :: String -> String
multiReturnError l = "Cannot return multiple values in " ++ l

multiTypeError :: String -> String
multiTypeError l = "Multi-types not supported in " ++ l
