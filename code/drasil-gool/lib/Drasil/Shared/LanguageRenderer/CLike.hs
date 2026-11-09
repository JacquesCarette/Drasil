-- | Implementations for C-like renderers are defined here.
module Drasil.Shared.LanguageRenderer.CLike (charRender, float, double, char,
  listType, setType, void, notOp, andOp, orOp, self, litTrue, litFalse, litFloat,
  inlineIf, libFuncAppMixedArgs, libNewObjMixedArgs, listSize, listSize',
  statement, varDec, varDecDef, setDecDef, listDec, extObjDecNew, for, while,
  multiAssignError, multiReturnError, multiTypeError
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
  RenderValue(valFromData), ValueElim(valuePrec), ScopeElim(scopeData),
  stmtFromData)
import qualified Drasil.Shared.RendererClassesCommon as RC
import Drasil.GOOL.Renderers (renderType)
import qualified Drasil.GOOL.RendererClassesOO as RO
import Drasil.Shared.AST (AttachmentTag(..), Terminator(..), ScopeData,
  TypeData)
import Drasil.Shared.Helpers (angles, toState, onStateValue)
import Drasil.Shared.LanguageRenderer (forLabel, whileLabel, containing, getTerm)
import qualified Drasil.Shared.LanguageRenderer as R
import Drasil.Shared.LanguageRenderer.Constructors (typeFromData, mkStateVal,
  mkStateVar, VSOp, unOpPrec, andPrec, orPrec)
import Drasil.Shared.State (MS, VS, lensMStoVS, lensVStoMS, addLibImportVS,
  getClassName, useVarName, setVarScope)

import Prelude hiding (break,(<>))
import qualified Prelude as P ((<>))
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

self :: (IG.OOTypeSym r typ, RC.RenderVariable r var typ) => VS (r var)
self = do
  l <- zoom lensVStoMS getClassName
  mkStateVar R.this (IG.obj l) R.this'

-- Values --

litTrue :: (RenderValue r var val typ, IC.TypeSym r typ) => VS (r val)
litTrue = mkStateVal IC.bool (text "true")

litFalse :: (RenderValue r var val typ, IC.TypeSym r typ) => VS (r val)
litFalse = mkStateVal IC.bool (text "false")

litFloat :: (RenderValue r var val typ, IC.TypeSym r typ) => Float -> VS (r val)
litFloat f = mkStateVal IC.float (D.float f <> text "f")

inlineIf
  :: (RenderValue r var val typ, ValueElim r val, ValueSym r val typ)
  => VS (r val) -> VS (r val) -> VS (r val) -> VS (r val)
inlineIf c' v1' v2' = do
  c <- c'
  v1 <- v1'
  v2 <- v2'
  valFromData (prec c) Nothing (toState $ valueType v1)
    (RC.value c <+> text "?" <+> RC.value v1 <+> text ":" <+> RC.value v2)
  where prec cd = valuePrec cd <|> Just 0

libFuncAppMixedArgs
  :: (IC.ValueExpression r var val binder typ)
  => Library -> MixedCall r var val typ
libFuncAppMixedArgs l n t vs ns = modify (addLibImportVS l) >>
  IC.funcAppMixedArgs n t vs ns

libNewObjMixedArgs
  :: (IG.OOValueExpression r var val typ)
  => Library -> MixedCtorCall r var val typ
libNewObjMixedArgs l tp vs ns = modify (addLibImportVS l) >>
  IG.newObjMixedArgs tp vs ns

-- Functions --

listSize
  :: (IC.TypeSym r typ, IG.InternalValueExp r var val typ)
  => String -> VS (r val) -> VS (r val)
listSize fnName list = objMethodCallNoParams IC.int list fnName

listSize'
  ::
    ( IC.TypeSym r typ
    , VariableSym r var typ
    , IG.OOVariableSym r var val typ
    , VariableValue r var val
    )
  => String -> VS (r val) -> VS (r val)
listSize' lengthName list = valueOf $ list $-> var lengthName IC.int

-- Statements --

statement :: (RC.StatementElim r stmt, RC.StatementTerm r stmt) => r stmt -> Doc
statement st = RC.preStatement st <> getTerm (RC.statementTerm st)

-- TODO [Brandon Bosman, 11/04/2026]: Split this up
varDec
  :: ( InternalVarElim r var
     , RO.PermElim r attch
     , RC.RenderStatement r stmt
     , ScopeElim r ScopeData
     , UnRepr r TypeData
     , TypeElim r TypeData
     , VariableElim r var TypeData
     )
  => r attch -> r attch -> Doc -> VS (r var) -> r ScopeData -> MS (r stmt)
varDec s d pdoc v' scp = do
  v <- zoom lensMStoVS v'
  modify $ useVarName (variableName v)
  modify $ setVarScope (variableName v) (scopeData scp)
  stmtFromData (RO.perm (bind $ variableBind v)
    <+> renderType (variableType v) <+> (ptrdoc (getCodeType (variableType v)) <>
    RC.variable v)) Semi
  where bind ClassLevel = s
        bind InstanceLevel = d
        ptrdoc (List _) = pdoc
        ptrdoc (Set _) = pdoc
        ptrdoc _ = empty

-- TODO [Brandon Bosman, 11/04/2026]: Split this up
varDecDef
  :: ( IC.DeclStatement r bod stmt var scope val
     , RC.RenderStatement r stmt
     , RC.StatementElim r stmt
     , ValueElim r val
     )
  => Terminator -> VS (r var) -> r scope -> VS (r val) -> MS (r stmt)
varDecDef t vr scp vl' = do
  vd <- IC.varDec vr scp
  vl <- zoom lensMStoVS vl'
  stmtFromData (RC.preStatement vd <+> equals <+> RC.value vl) t

-- TODO [Brandon Bosman, 11/04/2026]: Split this up
setDecDef
  :: ( IC.DeclStatement r bod stmt var scope val
     , RC.RenderStatement r stmt
     , RC.StatementElim r stmt
     , ValueElim r val
     )
  => Terminator -> VS (r var) -> r scope -> VS (r val) -> MS (r stmt)
setDecDef t vr scp vl' = do
  vd <- IC.setDec vr scp
  vl <- zoom lensMStoVS vl'
  stmtFromData (RC.preStatement vd <+> equals <+> RC.value vl) t

-- TODO [Brandon Bosman, 11/04/2026]: Split this up
listDec
  ::
    ( IC.DeclStatement r bod stmt var scope val
    , RC.RenderStatement r stmt
    , RC.StatementElim r stmt
    )
  => (r val -> Doc) -> VS (r val) -> VS (r var) -> r scope -> MS (r stmt)
listDec f vl v scp = do
  sz <- zoom lensMStoVS vl
  vd <- IC.varDec v scp
  stmtFromData (RC.preStatement vd <> f sz) Semi

extObjDecNew
  ::
    ( IC.DeclStatement r bod stmt var scope val
    , IG.OOValueExpression r var val typ
    , VariableElim r var typ
    )
  => Library -> VS (r var) -> r scope -> [VS (r val)] -> MS (r stmt)
extObjDecNew l v scp vs = IC.varDecDef v scp
  (extNewObj l (onStateValue variableType v) vs)

for :: Doc -> Doc -> Doc -> Doc -> Doc
for initial guard update bod = vcat
  [ forLabel <+> parens (initial <> semi <+> guard <> semi <+> update) <+> R.bodyStart
  , indent bod
  , R.bodyEnd
  ]

while :: (Doc -> Doc) -> Doc -> Doc -> Doc -> Doc -> Doc
while f bStart bEnd val bod = vcat
  [ whileLabel <+> f val <+> bStart
  , indent bod
  , bEnd
  ]

-- Error Messages --

multiAssignError :: String -> String
multiAssignError l = "No multiple assignment statements in " P.<> l

multiReturnError :: String -> String
multiReturnError l = "Cannot return multiple values in " P.<> l

multiTypeError :: String -> String
multiTypeError l = "Multi-types not supported in " P.<> l
