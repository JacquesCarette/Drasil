{-# LANGUAGE PostfixOperators #-}

-- | Implementations for C-like renderers are defined here.
module GOOL.Drasil.LanguageRenderer.CLike (float, double, char, listType, void, 
  notOp, andOp, orOp, self, litTrue, litFalse, litFloat, inlineIf, 
  libFuncAppMixedArgs, libNewObjMixedArgs, listSize, increment1, varDec, 
  varDecDef, listDec, extObjDecNew, discardInput, switch, for, while, 
  intFunc, multiAssignError, multiReturnError
) where

import Utils.Drasil (indent)

import GOOL.Drasil.CodeType (CodeType(..))
import GOOL.Drasil.ClassInterface (Label, Library, MSBody, VSType, SVariable, 
  SValue, MSStatement, MSParameter, SMethod, MixedCall, MixedCtorCall, 
  PermanenceSym(..), TypeElim(getType, getTypeString), 
  VariableElim(variableType), ValueSym(Value, valueType), extNewObj, ($.), 
  ControlStatement(break), ScopeSym(..))
import qualified GOOL.Drasil.ClassInterface as S (TypeSym(bool, float, obj),
  ValueExpression(funcAppMixedArgs, newObjMixedArgs), 
  DeclStatement(varDec, varDecDef))
import GOOL.Drasil.RendererClasses (MSMthdType, RenderSym, RenderType(..),
  InternalVarElim(variableBind), RenderValue(inputFunc, valFromData), 
  ValueElim(valuePrec), RenderMethod(intMethod))
import qualified GOOL.Drasil.RendererClasses as S (
  InternalListFunc(listSizeFunc), RenderStatement(stmt, loopStmt))
import qualified GOOL.Drasil.RendererClasses as RC (PermElim(..), BodyElim(..), 
  InternalTypeElim(..), InternalVarElim(variable), ValueElim(value), 
  StatementElim(statement))
import GOOL.Drasil.AST (Binding(..))
import GOOL.Drasil.Helpers (angles, toState, onStateValue, on2StateValues, 
  on3StateValues)
import GOOL.Drasil.LanguageRenderer (forLabel)
import qualified GOOL.Drasil.LanguageRenderer as R (switch, increment, self)
import GOOL.Drasil.LanguageRenderer.Constructors (mkStmt, mkStmtNoEnd, 
  mkStateVal, mkStateVar, VSOp, unOpPrec, andPrec, orPrec)
import GOOL.Drasil.State (lensMStoVS, lensVStoMS, addLibImportVS, getClassName)

import Prelude hiding (break,(<>))
import Control.Applicative ((<|>))
import Control.Monad.State (modify)
import Control.Lens.Zoom (zoom)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), parens, vcat, semi, 
  equals, empty)
import qualified Text.PrettyPrint.HughesPJ as D (float)

-- Types --

float :: (RenderSym r) => VSType r
float = toState $ typeFromData Float "float" (text "float")

double :: (RenderSym r) => VSType r
double = toState $ typeFromData Double "double" (text "double")

char :: (RenderSym r) => VSType r
char = toState $ typeFromData Char "char" (text "char")

listType :: (RenderSym r) => String -> VSType r -> VSType r
listType lst = onStateValue (\t -> typeFromData (List (getType t)) (lst ++ "<" 
  ++ getTypeString t ++ ">") (text lst <> angles (RC.type' t)))
  
void :: (RenderSym r) => VSType r
void = toState $ typeFromData Void "void" (text "void")

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
self = zoom lensVStoMS getClassName >>= (\l -> mkStateVar "this" (S.obj l) R.self)

-- Values --

litTrue :: (RenderSym r) => SValue r
litTrue = mkStateVal S.bool (text "true")

litFalse :: (RenderSym r) => SValue r
litFalse = mkStateVal S.bool (text "false")

litFloat :: (RenderSym r) => Float -> SValue r
litFloat f = mkStateVal S.float (D.float f <> text "f")

inlineIf :: (RenderSym r) => SValue r -> SValue r -> SValue r -> SValue r
inlineIf = on3StateValues (\c v1 v2 -> valFromData (prec c) (valueType v1) 
  (RC.value c <+> text "?" <+> RC.value v1 <+> text ":" <+> RC.value v2)) 
  where prec cd = valuePrec cd <|> Just 0

libFuncAppMixedArgs :: (RenderSym r) => Library -> MixedCall r
libFuncAppMixedArgs l n t vs ns = modify (addLibImportVS l) >> 
  S.funcAppMixedArgs n t vs ns
  
libNewObjMixedArgs :: (RenderSym r) => Library -> MixedCtorCall r
libNewObjMixedArgs l tp vs ns = modify (addLibImportVS l) >> 
  S.newObjMixedArgs tp vs ns

-- Functions --

listSize :: (RenderSym r) => SValue r -> SValue r
listSize v = v $. S.listSizeFunc

-- Statements --

increment1 :: (RenderSym r) => SVariable r -> MSStatement r
increment1 vr = zoom lensMStoVS $ onStateValue (mkStmt . R.increment) vr

varDec :: (RenderSym r) => r (Permanence r) -> r (Permanence r) -> Doc -> 
  SVariable r -> MSStatement r
varDec s d pdoc v' = onStateValue (\v -> mkStmt (RC.perm (bind $ variableBind v)
  <+> RC.type' (variableType v) <+> (ptrdoc (getType (variableType v)) <> 
  RC.variable v))) (zoom lensMStoVS v')
  where bind Static = s
        bind Dynamic = d
        ptrdoc (List _) = pdoc
        ptrdoc _ = empty

varDecDef :: (RenderSym r) => SVariable r -> SValue r -> MSStatement r
varDecDef vr vl' = on2StateValues (\vd vl -> mkStmt (RC.statement vd <+> equals 
  <+> RC.value vl)) (S.varDec vr) (zoom lensMStoVS vl')

listDec :: (RenderSym r) => (r (Value r) -> Doc) -> SValue r -> SVariable r -> 
  MSStatement r
listDec f vl v = on2StateValues (\sz vd -> mkStmt (RC.statement vd <> f 
  sz)) (zoom lensMStoVS vl) (S.varDec v)
  
extObjDecNew :: (RenderSym r) => Library -> SVariable r -> [SValue r] -> 
  MSStatement r
extObjDecNew l v vs = S.varDecDef v (extNewObj l (onStateValue variableType v)
  vs)
  
discardInput :: (RenderSym r) => (r (Value r) -> Doc) -> MSStatement r
discardInput f = zoom lensMStoVS $ onStateValue (mkStmt . f) inputFunc

switch :: (RenderSym r) => SValue r -> [(SValue r, MSBody r)] -> MSBody r -> 
  MSStatement r
switch v cs bod = do
  brk <- S.stmt break
  val <- zoom lensMStoVS v
  vals <- mapM (zoom lensMStoVS . fst) cs
  bods <- mapM snd cs
  dflt <- bod
  return $ mkStmt $ R.switch brk val dflt (zip vals bods)

for :: (RenderSym r) => Doc -> Doc -> MSStatement r -> SValue r -> 
  MSStatement r -> MSBody r -> MSStatement r
for bStart bEnd sInit vGuard sUpdate b = do
  initl <- S.loopStmt sInit
  guard <- zoom lensMStoVS vGuard
  upd <- S.loopStmt sUpdate
  bod <- b
  return $ mkStmtNoEnd $ vcat [
    forLabel <+> parens (RC.statement initl <> semi <+> RC.value guard <> 
      semi <+> RC.statement upd) <+> bStart,
    indent $ RC.body bod,
    bEnd]
  
while :: (RenderSym r) => Doc -> Doc -> SValue r -> MSBody r -> MSStatement r
while bStart bEnd v' = on2StateValues (\v b -> mkStmtNoEnd (vcat [
  text "while" <+> parens (RC.value v) <+> bStart,
  indent $ RC.body b,
  bEnd])) (zoom lensMStoVS v')

-- Methods --

intFunc :: (RenderSym r) => Bool -> Label -> r (Scope r) -> r (Permanence r) -> 
  MSMthdType r -> [MSParameter r] -> MSBody r -> SMethod r
intFunc = intMethod

-- Error Messages --

multiAssignError :: String -> String
multiAssignError l = "No multiple assignment statements in " ++ l

multiReturnError :: String -> String
multiReturnError l = "Cannot return multiple values in " ++ l