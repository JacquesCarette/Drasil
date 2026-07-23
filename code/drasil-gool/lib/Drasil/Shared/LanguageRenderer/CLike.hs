{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Implementations for C-like renderers are defined here.
module Drasil.Shared.LanguageRenderer.CLike (charRender, float, double, char,
  listType, setType, void, notOp, andOp, orOp, self, litTrue, litFalse, litFloat,
  inlineIf, libFuncAppMixedArgs, libNewObjMixedArgs, listSize, listSize',
  increment, increment1, decrement1, varDec, varDecDef, setDecDef, listDec,
  extObjDecNew, switch, for, while, multiAssignError, multiReturnError,
  multiTypeError
) where

import Drasil.FileHandling.Legacy (indent)

import Drasil.Shared.CodeType (CodeType(..))
import Drasil.Shared.InterfaceCommon (UnRepr(..), Library, Body, TypeElim(..),
  SVariable, Value, SValue, MixedCall, MixedCtorCall, VariableSym(..),
  VariableValue(..), VariableElim(..), ValueSym(valueType), getCodeType,
  getTypeString)
import qualified Drasil.Shared.InterfaceCommon as IC
import Drasil.GOOL.InterfaceGOOL (extNewObj, objMethodCallNoParams, ($->))
import qualified Drasil.GOOL.InterfaceGOOL as IG
import Drasil.Shared.RendererClassesCommon (InternalVarElim(variableBind),
  RenderValue(valFromData), ValueElim(valuePrec), ScopeElim(scopeData))
import qualified Drasil.Shared.RendererClassesCommon as RC
import Drasil.GOOL.Renderers (renderType)
import qualified Drasil.GOOL.RendererClassesOO as RO
import Drasil.Shared.AST (AttachmentTag(..), Terminator(..), ScopeData,
  TypeData)
import Drasil.Shared.Helpers (angles, toState, onStateValue)
import Drasil.Shared.LanguageRenderer (forLabel, whileLabel, containing)
import qualified Drasil.Shared.LanguageRenderer as R
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
import qualified Text.PrettyPrint.HughesPJ as D

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

listType :: (Monad r, TypeElim r, UnRepr r TypeData) => String ->
  VS (r TypeData) -> VS (r TypeData)
listType lst t' = do
  t <- t'
  typeFromData (List (getCodeType t)) (lst
    `containing` getTypeString t) $ text lst <> angles (renderType t)

setType :: (Monad r, TypeElim r, UnRepr r TypeData) => String ->
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

self :: (IG.OOTypeSym r, RC.RenderVariable r) => SVariable r
self = do
  l <- zoom lensVStoMS getClassName
  mkStateVar R.this (IG.obj l) R.this'

-- Values --

litTrue :: (RenderValue r, IC.TypeSym r) => SValue r
litTrue = mkStateVal IC.bool (text "true")

litFalse :: (RenderValue r, IC.TypeSym r) => SValue r
litFalse = mkStateVal IC.bool (text "false")

litFloat :: (RenderValue r, IC.TypeSym r) => Float -> SValue r
litFloat f = mkStateVal IC.float (D.float f <> text "f")

inlineIf
  :: (RenderValue r, ValueElim r, ValueSym r)
  => SValue r -> SValue r -> SValue r -> SValue r
inlineIf c' v1' v2' = do
  c <- c'
  v1 <- v1'
  v2 <- v2'
  valFromData (prec c) Nothing (toState $ valueType v1)
    (RC.value c <+> text "?" <+> RC.value v1 <+> text ":" <+> RC.value v2)
  where prec cd = valuePrec cd <|> Just 0

libFuncAppMixedArgs :: (IC.ValueExpression r) => Library -> MixedCall r
libFuncAppMixedArgs l n t vs ns = modify (addLibImportVS l) >>
  IC.funcAppMixedArgs n t vs ns

libNewObjMixedArgs :: (IG.OOValueExpression r) => Library -> MixedCtorCall r
libNewObjMixedArgs l tp vs ns = modify (addLibImportVS l) >>
  IG.newObjMixedArgs tp vs ns

-- Functions --

listSize :: (IG.InternalValueExp r) => String -> SValue r -> SValue r
listSize fnName list = objMethodCallNoParams IC.int list fnName

listSize' :: (IG.OOVariableSym r, VariableValue r) => String -> SValue r -> SValue r
listSize' lengthName list = valueOf $ list $-> var lengthName IC.int

-- Statements --

increment
  :: (InternalVarElim r, RC.RenderStatement r stmt, ValueElim r)
  => SVariable r -> SValue r -> MS (r stmt)
increment vr' v'= do
  vr <- zoom lensMStoVS vr'
  v <- zoom lensMStoVS v'
  mkStmt $ R.addAssign vr v

increment1 :: (InternalVarElim r, RC.RenderStatement r stmt) => SVariable r -> MS (r stmt)
increment1 vr' = do
  vr <- zoom lensMStoVS vr'
  (mkStmt . R.increment) vr

decrement1 :: (InternalVarElim r, RC.RenderStatement r stmt) => SVariable r -> MS (r stmt)
decrement1 vr' = do
  vr <- zoom lensMStoVS vr'
  (mkStmt . R.decrement) vr

varDec
  :: ( InternalVarElim r
     , RO.PermElim r attch
     , RC.RenderStatement r stmt
     , ScopeElim r
     , UnRepr r TypeData
     , TypeElim r
     , VariableElim r
     )
  => r attch -> r attch -> Doc -> SVariable r -> r ScopeData -> MS (r stmt)
varDec s d pdoc v' scp = do
  v <- zoom lensMStoVS v'
  modify $ useVarName (variableName v)
  modify $ setVarScope (variableName v) (scopeData scp)
  mkStmt (RO.perm (bind $ variableBind v)
    <+> renderType (variableType v) <+> (ptrdoc (getCodeType (variableType v)) <>
    RC.variable v))
  where bind ClassLevel = s
        bind InstanceLevel = d
        ptrdoc (List _) = pdoc
        ptrdoc (Set _) = pdoc
        ptrdoc _ = empty

varDecDef
  :: ( IC.DeclStatement r stmt
     , RC.RenderStatement r stmt
     , RC.StatementElim r stmt
     , ValueElim r
     )
  => Terminator -> SVariable r -> r ScopeData -> SValue r -> MS (r stmt)
varDecDef t vr scp vl' = do
  vd <- IC.varDec vr scp
  vl <- zoom lensMStoVS vl'
  let stmtCtor Empty = mkStmtNoEnd
      stmtCtor Semi = mkStmt
  stmtCtor t (RC.statement vd <+> equals <+> RC.value vl)

setDecDef
  :: ( IC.DeclStatement r stmt
     , RC.RenderStatement r stmt
     , RC.StatementElim r stmt
     , ValueElim r
     )
  => Terminator -> SVariable r -> r ScopeData -> SValue r -> MS (r stmt)
setDecDef t vr scp vl' = do
  vd <- IC.setDec vr scp
  vl <- zoom lensMStoVS vl'
  let stmtCtor Empty = mkStmtNoEnd
      stmtCtor Semi = mkStmt
  stmtCtor t (RC.statement vd <+> equals <+> RC.value vl)

listDec
  :: (IC.DeclStatement r stmt, RC.RenderStatement r stmt, RC.StatementElim r stmt)
  => (r Value -> Doc) -> SValue r -> SVariable r -> r ScopeData -> MS (r stmt)
listDec f vl v scp = do
  sz <- zoom lensMStoVS vl
  vd <- IC.varDec v scp
  mkStmt (RC.statement vd <> f sz)

extObjDecNew
  :: (IC.DeclStatement r stmt, IG.OOValueExpression r, VariableElim r)
  => Library -> SVariable r -> r ScopeData -> [SValue r] -> MS (r stmt)
extObjDecNew l v scp vs = IC.varDecDef v scp
  (extNewObj l (onStateValue variableType v) vs)

-- 1st parameter is a Doc function to apply to the render of the control value (i.e. parens)
-- 2nd parameter is a statement to end every case with
switch
  :: ( RC.BodyElim r
     , RC.RenderStatement r stmt
     , RC.StatementElim r stmt
     , ValueElim r
     )
  => (Doc -> Doc)
  -> MS (r stmt)
  -> SValue r
  -> [(SValue r, MS (r Body))]
  -> MS (r Body)
  -> MS (r stmt)
switch f st v cs bod = do
  s <- RC.stmt st
  val <- zoom lensMStoVS v
  vals <- mapM (zoom lensMStoVS . fst) cs
  bods <- mapM snd cs
  dflt <- bod
  mkStmt $ R.switch f s val dflt (zip vals bods)

for
  :: ( RC.BodyElim r
     , RC.RenderStatement r stmt
     , RC.StatementElim r stmt
     , ValueElim r
     )
  => Doc
  -> Doc
  -> MS (r stmt)
  -> SValue r
  -> MS (r stmt)
  -> MS (r Body)
  -> MS (r stmt)
for bStart bEnd sInit vGuard sUpdate b = do
  initl <- RC.loopStmt sInit
  guard <- zoom lensMStoVS vGuard
  upd <- RC.loopStmt sUpdate
  bod <- b
  mkStmtNoEnd $ vcat [
    forLabel <+> parens (RC.statement initl <> semi <+> RC.value guard <>
      semi <+> RC.statement upd) <+> bStart,
    indent $ RC.body bod,
    bEnd]

-- Doc function parameter is applied to the render of the while-condition
while
  :: (RC.BodyElim r, RC.RenderStatement r stmt, ValueElim r)
  => (Doc -> Doc) -> Doc -> Doc -> SValue r -> MS (r Body) -> MS (r stmt)
while f bStart bEnd v' b'= do
  v <- zoom lensMStoVS v'
  b <- b'
  mkStmtNoEnd (vcat [whileLabel <+> f (RC.value v) <+> bStart,
    indent $ RC.body b,
    bEnd])

-- Error Messages --

multiAssignError :: String -> String
multiAssignError l = "No multiple assignment statements in " ++ l

multiReturnError :: String -> String
multiReturnError l = "Cannot return multiple values in " ++ l

multiTypeError :: String -> String
multiTypeError l = "Multi-types not supported in " ++ l
