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
import Drasil.Shared.InterfaceCommon (UnRepr(..), Library, TypeElim(..),
  MixedCall, MixedCtorCall, VariableSym(..), VariableValue(..), VariableElim(..),
  ValueSym(valueType), getCodeType, getTypeString)
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

listType
  :: (Monad r, TypeElim r TypeData, UnRepr r TypeData)
  => String -> VS (r TypeData) -> VS (r TypeData)
listType lst t' = do
  t <- t'
  typeFromData (List (getCodeType t)) (lst
    `containing` getTypeString t) $ text lst <> angles (renderType t)

setType
  :: (Monad r, TypeElim r TypeData, UnRepr r TypeData)
  => String -> VS (r TypeData) -> VS (r TypeData)
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

self :: (IG.OOTypeSym r typ, RC.RenderVariable r typ var) => VS (r var)
self = do
  l <- zoom lensVStoMS getClassName
  mkStateVar R.this (IG.obj l) R.this'

-- Values --

litTrue :: (RenderValue r typ var val, IC.TypeSym r typ) => VS (r val)
litTrue = mkStateVal IC.bool (text "true")

litFalse :: (RenderValue r typ var val, IC.TypeSym r typ) => VS (r val)
litFalse = mkStateVal IC.bool (text "false")

litFloat :: (RenderValue r typ var val, IC.TypeSym r typ) => Float -> VS (r val)
litFloat f = mkStateVal IC.float (D.float f <> text "f")

inlineIf
  :: (RenderValue r typ var val, ValueElim r val, ValueSym r typ val)
  => VS (r val) -> VS (r val) -> VS (r val) -> VS (r val)
inlineIf c' v1' v2' = do
  c <- c'
  v1 <- v1'
  v2 <- v2'
  valFromData (prec c) Nothing (toState $ valueType v1)
    (RC.value c <+> text "?" <+> RC.value v1 <+> text ":" <+> RC.value v2)
  where prec cd = valuePrec cd <|> Just 0

libFuncAppMixedArgs
  :: (IC.ValueExpression r typ var val)
  => Library -> MixedCall r typ var val
libFuncAppMixedArgs l n t vs ns = modify (addLibImportVS l) >>
  IC.funcAppMixedArgs n t vs ns

libNewObjMixedArgs
  :: (IG.OOValueExpression r typ var val)
  => Library -> MixedCtorCall r typ var val
libNewObjMixedArgs l tp vs ns = modify (addLibImportVS l) >>
  IG.newObjMixedArgs tp vs ns

-- Functions --

listSize
  :: (IC.TypeSym r typ, IG.InternalValueExp r typ var val)
  => String -> VS (r val) -> VS (r val)
listSize fnName list = objMethodCallNoParams IC.int list fnName

listSize'
  ::
    ( IC.TypeSym r typ
    , VariableSym r typ var
    , IG.OOVariableSym r typ var val
    , VariableValue r var val
    )
  => String -> VS (r val) -> VS (r val)
listSize' lengthName list = valueOf $ list $-> var lengthName IC.int

-- Statements --

increment
  :: (InternalVarElim r var, RC.RenderStatement r stmt, ValueElim r val)
  => VS (r var) -> VS (r val) -> MS (r stmt)
increment vr' v'= do
  vr <- zoom lensMStoVS vr'
  v <- zoom lensMStoVS v'
  mkStmt $ R.addAssign vr v

increment1
  :: (InternalVarElim r var, RC.RenderStatement r stmt)
  => VS (r var) -> MS (r stmt)
increment1 vr' = do
  vr <- zoom lensMStoVS vr'
  (mkStmt . R.increment) vr

decrement1
  :: (InternalVarElim r var, RC.RenderStatement r stmt)
  => VS (r var) -> MS (r stmt)
decrement1 vr' = do
  vr <- zoom lensMStoVS vr'
  (mkStmt . R.decrement) vr

varDec
  :: ( InternalVarElim r var
     , RO.PermElim r attch
     , RC.RenderStatement r stmt
     , ScopeElim r ScopeData
     , UnRepr r TypeData
     , TypeElim r TypeData
     , VariableElim r TypeData var
     )
  => r attch -> r attch -> Doc -> VS (r var) -> r ScopeData -> MS (r stmt)
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
  :: ( IC.DeclStatement r scope var val stmt bod
     , RC.RenderStatement r stmt
     , RC.StatementElim r stmt
     , ValueElim r val
     )
  => Terminator -> VS (r var) -> r scope -> VS (r val) -> MS (r stmt)
varDecDef t vr scp vl' = do
  vd <- IC.varDec vr scp
  vl <- zoom lensMStoVS vl'
  let stmtCtor Empty = mkStmtNoEnd
      stmtCtor Semi = mkStmt
  stmtCtor t (RC.statement vd <+> equals <+> RC.value vl)

setDecDef
  :: ( IC.DeclStatement r scope var val stmt bod
     , RC.RenderStatement r stmt
     , RC.StatementElim r stmt
     , ValueElim r val
     )
  => Terminator -> VS (r var) -> r scope -> VS (r val) -> MS (r stmt)
setDecDef t vr scp vl' = do
  vd <- IC.setDec vr scp
  vl <- zoom lensMStoVS vl'
  let stmtCtor Empty = mkStmtNoEnd
      stmtCtor Semi = mkStmt
  stmtCtor t (RC.statement vd <+> equals <+> RC.value vl)

listDec
  ::
    ( IC.DeclStatement r scope var val stmt bod
    , RC.RenderStatement r stmt
    , RC.StatementElim r stmt
    )
  => (r val -> Doc) -> VS (r val) -> VS (r var) -> r scope -> MS (r stmt)
listDec f vl v scp = do
  sz <- zoom lensMStoVS vl
  vd <- IC.varDec v scp
  mkStmt (RC.statement vd <> f sz)

extObjDecNew
  ::
    ( IC.DeclStatement r scope var val stmt bod
    , IG.OOValueExpression r typ var val
    , VariableElim r typ var
    )
  => Library -> VS (r var) -> r scope -> [VS (r val)] -> MS (r stmt)
extObjDecNew l v scp vs = IC.varDecDef v scp
  (extNewObj l (onStateValue variableType v) vs)

-- 1st parameter is a Doc function to apply to the render of the control value (i.e. parens)
-- 2nd parameter is a statement to end every case with
switch
  :: ( RC.BodyElim r bod
     , RC.RenderStatement r stmt
     , RC.StatementElim r stmt
     , ValueElim r val
     )
  => (Doc -> Doc)
  -> MS (r stmt)
  -> VS (r val)
  -> [(VS (r val), MS (r bod))]
  -> MS (r bod)
  -> MS (r stmt)
switch f st v cs bod = do
  s <- RC.stmt st
  val <- zoom lensMStoVS v
  vals <- mapM (zoom lensMStoVS . fst) cs
  bods <- mapM snd cs
  dflt <- bod
  mkStmt $ R.switch f s val dflt (zip vals bods)

for
  :: ( RC.BodyElim r bod
     , RC.RenderStatement r stmt
     , RC.StatementElim r stmt
     , ValueElim r val
     )
  => Doc
  -> Doc
  -> MS (r stmt)
  -> VS (r val)
  -> MS (r stmt)
  -> MS (r bod)
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
  :: (RC.BodyElim r bod, RC.RenderStatement r stmt, ValueElim r val)
  => (Doc -> Doc) -> Doc -> Doc -> VS (r val) -> MS (r bod) -> MS (r stmt)
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
