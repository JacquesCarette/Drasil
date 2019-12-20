{-# LANGUAGE PostfixOperators #-}

-- | The structure for a class of renderers is defined here.
module GOOL.Drasil.LanguageRenderer.LanguagePolymorphic (fileFromData, oneLiner,
  block, bool, int, float, double, char, string, fileType, listType, 
  listInnerType, obj, enumType, void, runStrategy, listSlice, unOpPrec, 
  notOp, notOp', negateOp, sqrtOp, sqrtOp', absOp, absOp', expOp, expOp', sinOp,
  sinOp', cosOp, cosOp', tanOp, tanOp', asinOp, asinOp', acosOp, acosOp', 
  atanOp, atanOp', unExpr, unExpr', typeUnExpr, powerPrec, multPrec, andPrec, 
  orPrec, equalOp, notEqualOp, greaterOp, greaterEqualOp, lessOp, lessEqualOp, 
  plusOp, minusOp, multOp, divideOp, moduloOp, powerOp, andOp, orOp, binExpr, 
  binExpr', typeBinExpr, addmathImport, var, staticVar, extVar, self, enumVar, 
  classVar, objVar, objVarSelf, listVar, listOf, iterVar, litTrue, litFalse, 
  litChar, litFloat, litInt, litString, pi, valueOf, arg, enumElement, argsList,
  inlineIf, funcApp, selfFuncApp, extFuncApp, newObj, notNull, objAccess, 
  objMethodCall, objMethodCallNoParams, selfAccess, listIndexExists, indexOf, 
  func, get, set, listSize, listAdd, listAppend, iterBegin, iterEnd, listAccess,
  listSet, getFunc, setFunc, listSizeFunc, listAddFunc, listAppendFunc, 
  iterBeginError, iterEndError, listAccessFunc, listAccessFunc', listSetFunc, 
  printSt, state, loopState, emptyState, assign, assignToListIndex, 
  multiAssignError, decrement, increment, increment', increment1, increment1', 
  decrement1, varDec, varDecDef, listDec, listDecDef, listDecDef', objDecNew, 
  objDecNewNoParams, constDecDef, discardInput, discardFileInput, openFileR, 
  openFileW, openFileA, closeFile, discardFileLine, stringListVals, 
  stringListLists, returnState, multiReturnError, valState, comment, freeError, 
  throw, initState, changeState, initObserverList, addObserver, ifCond, 
  ifNoElse, switch, switchAsIf, ifExists, for, forRange, forEach, while, 
  tryCatch, checkState, notifyObservers, construct, param, method, getMethod, 
  setMethod,privMethod, pubMethod, constructor, docMain, function, mainFunction,
  docFunc, docInOutFunc, intFunc, stateVar,stateVarDef, constVar, privMVar, 
  pubMVar, pubGVar, buildClass, enum, privClass, pubClass, docClass, 
  commentedClass, buildModule, buildModule', modFromData, fileDoc, docMod
) where

import Utils.Drasil (indent)

import GOOL.Drasil.CodeType (CodeType(..), isObject)
import GOOL.Drasil.Symantics (Label, Library, KeywordSym(..), RenderSym,
  FileSym(RenderFile, commentedMod), BlockSym(Block), InternalBlock(..), 
  BodySym(Body, body, bodyStatements, bodyDoc), ImportSym(..), 
  PermanenceSym(..), InternalPerm(..), 
  TypeSym(Type, infile, outfile, iterator, getType, getTypeDoc, getTypeString), 
  InternalType(..), UnaryOpSym(UnaryOp), BinaryOpSym(BinaryOp), InternalOp(..),
  VariableSym(Variable, variableBind, variableName, variableType, variableDoc), 
  InternalVariable(varFromData), ValueSym(Value, valueDoc, valueType), 
  NumericExpression(..), BooleanExpression(..), InternalValue(..), 
  Selector(($.)), FunctionSym(Function), SelectorFunction(at), 
  InternalFunction(iterBeginFunc, iterEndFunc, functionDoc, functionType, 
    funcFromData),
  InternalStatement(statementDoc, statementTerm, stateFromData), 
  StatementSym(Statement, (&=), (&+=), (&++), break, multi), ScopeSym(..),
  InternalScope(..), MethodTypeSym(MethodType, mType), ParameterSym(Parameter), 
  InternalParam(paramFromData), MethodSym(Method), 
  InternalMethod(intMethod, commentedFunc, methodDoc), StateVarSym(StateVar), 
  InternalStateVar(..), ClassSym(Class), InternalClass(..), ModuleSym(Module), 
  InternalMod(moduleDoc, updateModuleDoc), BlockComment(..))
import qualified GOOL.Drasil.Symantics as S (InternalFile(fileFromData), 
  BodySym(oneLiner), BlockSym(block), 
  TypeSym(bool, int, float, char, string, listType, listInnerType, void), 
  VariableSym(var, self, objVar, objVarSelf, listVar, listOf),
  ValueSym(litTrue, litFalse, litInt, litString, valueOf),
  ValueExpression(funcApp, newObj, notNull), Selector(objAccess), objMethodCall,
  objMethodCallNoParams, FunctionSym(func, listSize, listAdd, listAppend),
  SelectorFunction(listAccess, listSet),
  InternalFunction(getFunc, setFunc, listSizeFunc, listAddFunc, listAppendFunc, 
    listAccessFunc, listSetFunc),
  InternalStatement(state, loopState, emptyState), 
  StatementSym(assign, varDec, varDecDef, listDec, listDecDef, objDecNew, 
    constDecDef, valState, returnState),
  ControlStatementSym(ifCond, for, forRange, switch), MethodTypeSym(construct), 
  ParameterSym(param), MethodSym(method, mainFunction), InternalMethod(intFunc),
  StateVarSym(stateVar), ClassSym(buildClass, commentedClass), 
  InternalMod(modFromData))
import GOOL.Drasil.Data (Binding(..), Terminator(..), FileType)
import GOOL.Drasil.Helpers (angles, doubleQuotedText, vibcat, emptyIfEmpty, 
  toState, onStateValue, on2StateValues, on3StateValues, on4StateValues, 
  onStateList, on2StateLists, on1StateValue1List, getInnerType, convType)
import GOOL.Drasil.LanguageRenderer (forLabel, new, observerListName, addExt, 
  moduleDocD, blockDocD, assignDocD, plusEqualsDocD, plusPlusDocD, mkStateVal, 
  mkVal, mkStateVar, mkVar, mkStaticVar, varDocD, extVarDocD, selfDocD, argDocD,
  enumElemDocD, classVarCheckStatic, objVarDocD, funcAppDocD, objAccessDocD, 
  funcDocD, listAccessFuncDocD, constDecDefDocD, printDoc, returnDocD, 
  getTermDoc, switchDocD, stateVarDocD, stateVarListDocD, enumDocD, 
  enumElementsDocD, fileDoc', docFuncRepr, commentDocD, commentedItem, 
  functionDox, classDox, moduleDox, getterName, setterName, valueList, intValue)
import GOOL.Drasil.State (FS, MS, lensFStoGS, lensFStoMS, lensMStoFS, 
  currMain, putAfter, getPutReturnFunc, getPutReturnFunc2, addFile, setMainMod, 
  addLangImport, getLangImports, getModuleImports, setFilePath, getFilePath, 
  setModuleName, getModuleName, addParameter)

import Prelude hiding (break,print,last,mod,pi,(<>))
import Data.Bifunctor (first)
import Data.Map as Map (lookup, fromList)
import Data.Maybe (fromMaybe, maybeToList)
import Control.Applicative ((<|>))
import Control.Monad (join)
import Control.Monad.State (modify)
import Control.Lens ((^.), over)
import Control.Lens.Zoom (zoom)
import Text.PrettyPrint.HughesPJ (Doc, text, empty, render, (<>), (<+>), parens,
  braces, quotes, integer, vcat, semi, comma, equals, isEmpty)
import qualified Text.PrettyPrint.HughesPJ as D (char, double)

-- Bodies --

oneLiner :: (RenderSym repr) => FS (repr (Statement repr)) -> 
  FS (repr (Body repr))
oneLiner s = bodyStatements [s]

-- Blocks --

block :: (RenderSym repr) => repr (Keyword repr) -> [FS (repr (Statement repr))]
  -> FS (repr (Block repr))
block end sts = docBlock $ onStateList (blockDocD (keyDoc end) . map 
  statementDoc) (map S.state sts)

-- Types --

bool :: (RenderSym repr) => FS (repr (Type repr))
bool = toState $ typeFromData Boolean "Boolean" (text "Boolean")

int :: (RenderSym repr) => FS (repr (Type repr))
int = toState $ typeFromData Integer "int" (text "int")

float :: (RenderSym repr) => FS (repr (Type repr))
float = toState $ typeFromData Float "float" (text "float")

double :: (RenderSym repr) => FS (repr (Type repr))
double = toState $ typeFromData Float "double" (text "double")

char :: (RenderSym repr) => FS (repr (Type repr))
char = toState $ typeFromData Char "char" (text "char")

string :: (RenderSym repr) => FS (repr (Type repr))
string = toState $ typeFromData String "string" (text "string")

fileType :: (RenderSym repr) => FS (repr (Type repr))
fileType = toState $ typeFromData File "File" (text "File")

listType :: (RenderSym repr) => repr (Permanence repr) -> FS (repr (Type repr)) 
  -> FS (repr (Type repr))
listType p = onStateValue (\t -> typeFromData (List (getType t)) (render 
  (keyDoc $ list p) ++ "<" ++ getTypeString t ++ ">") (keyDoc (list p) <> 
  angles (getTypeDoc t)))

listInnerType :: (RenderSym repr) => FS (repr (Type repr)) -> 
  FS (repr (Type repr))
listInnerType t = t >>= (convType . getInnerType . getType)

obj :: (RenderSym repr) => Label -> FS (repr (Type repr))
obj n = toState $ typeFromData (Object n) n (text n)

enumType :: (RenderSym repr) => Label -> FS (repr (Type repr))
enumType e = toState $ typeFromData (Enum e) e (text e)

void :: (RenderSym repr) => FS (repr (Type repr))
void = toState $ typeFromData Void "void" (text "void")

-- ControlBlock --

strat :: (RenderSym repr) => FS (repr (Statement repr)) -> FS (repr (Body repr))
  -> FS (repr (Block repr))
strat r bd = docBlock $ on2StateValues (\result b -> vcat [bodyDoc b, 
  statementDoc result]) r bd

runStrategy :: (RenderSym repr) => String -> [(Label, FS (repr (Body repr)))] 
  -> Maybe (FS (repr (Value repr))) -> Maybe (FS (repr (Variable repr))) -> 
  FS (repr (Block repr))
runStrategy l strats rv av = maybe
  (strError l "RunStrategy called on non-existent strategy") 
  (strat (S.state resultState)) (Map.lookup l (Map.fromList strats))
  where resultState = maybe S.emptyState asgState av
        asgState v = maybe (strError l 
          "Attempt to assign null return to a Value") (v &=) rv
        strError n s = error $ "Strategy '" ++ n ++ "': " ++ s ++ "."

listSlice :: (RenderSym repr) => Maybe (FS (repr (Value repr))) -> 
  Maybe (FS (repr (Value repr))) -> Maybe (FS (repr (Value repr))) -> 
  FS (repr (Variable repr)) -> FS (repr (Value repr)) -> FS (repr (Block repr))
listSlice b e s vnew vold = 
  let l_temp = "temp"
      var_temp = S.var l_temp (onStateValue variableType vnew)
      v_temp = S.valueOf var_temp
      l_i = "i_temp"
      var_i = S.var l_i S.int
      v_i = S.valueOf var_i
  in
    S.block [
      S.listDec 0 var_temp,
      S.for (S.varDecDef var_i (fromMaybe (S.litInt 0) b)) 
        (v_i ?< fromMaybe (S.listSize vold) e) (maybe (var_i &++) (var_i &+=) s)
        (S.oneLiner $ S.valState $ S.listAppend v_temp (S.listAccess vold v_i)),
      vnew &= v_temp]

-- Unary Operators --

unOpPrec :: (RenderSym repr) => String -> FS (repr (UnaryOp repr))
unOpPrec = uOpFromData 9 . text

notOp :: (RenderSym repr) => FS (repr (UnaryOp repr))
notOp = unOpPrec "!"

notOp' :: (RenderSym repr) => FS (repr (UnaryOp repr))
notOp' = unOpPrec "not"

negateOp :: (RenderSym repr) => FS (repr (UnaryOp repr))
negateOp = unOpPrec "-"

sqrtOp :: (RenderSym repr) => FS (repr (UnaryOp repr))
sqrtOp = unOpPrec "sqrt"

sqrtOp' :: (RenderSym repr) => FS (repr (UnaryOp repr))
sqrtOp' = addmathImport $ unOpPrec "math.sqrt"

absOp :: (RenderSym repr) => FS (repr (UnaryOp repr))
absOp = unOpPrec "fabs"

absOp' :: (RenderSym repr) => FS (repr (UnaryOp repr))
absOp' = addmathImport $ unOpPrec "math.fabs"

expOp :: (RenderSym repr) => FS (repr (UnaryOp repr))
expOp = unOpPrec "exp"

expOp' :: (RenderSym repr) => FS (repr (UnaryOp repr))
expOp' = addmathImport $ unOpPrec "math.exp"

sinOp :: (RenderSym repr) => FS (repr (UnaryOp repr))
sinOp = unOpPrec "sin"

sinOp' :: (RenderSym repr) => FS (repr (UnaryOp repr))
sinOp' = addmathImport $ unOpPrec "math.sin"

cosOp :: (RenderSym repr) => FS (repr (UnaryOp repr))
cosOp = unOpPrec "cos"

cosOp' :: (RenderSym repr) => FS (repr (UnaryOp repr))
cosOp' = addmathImport $ unOpPrec "math.cos"

tanOp :: (RenderSym repr) => FS (repr (UnaryOp repr))
tanOp = unOpPrec "tan"

tanOp' :: (RenderSym repr) => FS (repr (UnaryOp repr))
tanOp' = addmathImport $ unOpPrec "math.tan"

asinOp :: (RenderSym repr) => FS (repr (UnaryOp repr))
asinOp = unOpPrec "asin"

asinOp' :: (RenderSym repr) => FS (repr (UnaryOp repr))
asinOp' = addmathImport $ unOpPrec "math.asin"

acosOp :: (RenderSym repr) => FS (repr (UnaryOp repr))
acosOp = unOpPrec "acos"

acosOp' :: (RenderSym repr) => FS (repr (UnaryOp repr))
acosOp' = addmathImport $ unOpPrec "math.acos"

atanOp :: (RenderSym repr) => FS (repr (UnaryOp repr))
atanOp = unOpPrec "atan"

atanOp' :: (RenderSym repr) => FS (repr (UnaryOp repr))
atanOp' = addmathImport $ unOpPrec "math.atan"

unOpDocD :: Doc -> Doc -> Doc
unOpDocD op v = op <> parens v

unOpDocD' :: Doc -> Doc -> Doc
unOpDocD' op v = op <> v

unExpr :: (RenderSym repr) => FS (repr (UnaryOp repr)) -> FS (repr (Value repr))
  -> FS (repr (Value repr))
unExpr = on2StateValues (\u v -> mkExpr (uOpPrec u) (valueType v) (unOpDocD 
  (uOpDoc u) (valueDoc v)))

unExpr' :: (RenderSym repr) => FS (repr (UnaryOp repr)) -> 
  FS (repr (Value repr)) -> FS (repr (Value repr))
unExpr' = on2StateValues (\u v -> mkExpr (uOpPrec u) (valueType v) (unOpDocD' 
  (uOpDoc u) (valueDoc v)))

typeUnExpr :: (RenderSym repr) => FS (repr (UnaryOp repr)) -> 
  FS (repr (Type repr)) -> FS (repr (Value repr)) -> FS (repr (Value repr))
typeUnExpr = on3StateValues (\u t -> mkExpr (uOpPrec u) t . unOpDocD (uOpDoc u) 
  . valueDoc)

-- Binary Operators --

compEqualPrec :: (RenderSym repr) => String -> FS (repr (BinaryOp repr))
compEqualPrec = bOpFromData 4 . text

compPrec :: (RenderSym repr) => String -> FS (repr (BinaryOp repr))
compPrec = bOpFromData 5 . text

addPrec :: (RenderSym repr) => String -> FS (repr (BinaryOp repr))
addPrec = bOpFromData 6 . text

multPrec :: (RenderSym repr) => String -> FS (repr (BinaryOp repr))
multPrec = bOpFromData 7 . text

powerPrec :: (RenderSym repr) => String -> FS (repr (BinaryOp repr))
powerPrec = bOpFromData 8 . text

andPrec :: (RenderSym repr) => String -> FS (repr (BinaryOp repr)) 
andPrec = bOpFromData 3 . text

orPrec :: (RenderSym repr) => String -> FS (repr (BinaryOp repr))
orPrec = bOpFromData 2 . text

equalOp :: (RenderSym repr) => FS (repr (BinaryOp repr))
equalOp = compEqualPrec "=="

notEqualOp :: (RenderSym repr) => FS (repr (BinaryOp repr))
notEqualOp = compEqualPrec "!="

greaterOp :: (RenderSym repr) => FS (repr (BinaryOp repr))
greaterOp = compPrec ">"

greaterEqualOp :: (RenderSym repr) => FS (repr (BinaryOp repr))
greaterEqualOp = compPrec ">="

lessOp :: (RenderSym repr) => FS (repr (BinaryOp repr))
lessOp = compPrec "<"

lessEqualOp :: (RenderSym repr) => FS (repr (BinaryOp repr))
lessEqualOp = compPrec "<="

plusOp :: (RenderSym repr) => FS (repr (BinaryOp repr))
plusOp = addPrec "+"

minusOp :: (RenderSym repr) => FS (repr (BinaryOp repr))
minusOp = addPrec "-"

multOp :: (RenderSym repr) => FS (repr (BinaryOp repr))
multOp = multPrec "*"

divideOp :: (RenderSym repr) => FS (repr (BinaryOp repr))
divideOp = multPrec "/"

moduloOp :: (RenderSym repr) => FS (repr (BinaryOp repr))
moduloOp = multPrec "%"

powerOp :: (RenderSym repr) => FS (repr (BinaryOp repr))
powerOp = powerPrec "pow"

andOp :: (RenderSym repr) => FS (repr (BinaryOp repr))
andOp = andPrec "&&"

orOp :: (RenderSym repr) => FS (repr (BinaryOp repr))
orOp = orPrec "||"

binOpDocD :: Doc -> Doc -> Doc -> Doc
binOpDocD op v1 v2 = v1 <+> op <+> v2

binOpDocD' :: Doc -> Doc -> Doc -> Doc
binOpDocD' op v1 v2 = op <> parens (v1 <> comma <+> v2)

binExpr :: (RenderSym repr) => FS (repr (BinaryOp repr)) -> 
  FS (repr (Value repr)) -> FS (repr (Value repr)) -> FS (repr (Value repr))
binExpr = on3StateValues (\b v1 v2 -> mkExpr (bOpPrec b) (numType (valueType v1)
  (valueType v2)) (binOpDocD (bOpDoc b) (exprParensL b v1 $ valueDoc v1) 
  (exprParensR b v2 $ valueDoc v2)))

binExpr' :: (RenderSym repr) => FS (repr (BinaryOp repr)) -> 
  FS (repr (Value repr)) -> FS (repr (Value repr)) -> FS (repr (Value repr))
binExpr' = on3StateValues (\b v1 v2 -> mkExpr 9 (numType (valueType v1) 
  (valueType v2)) (binOpDocD' (bOpDoc b) (valueDoc v1) (valueDoc v2)))

typeBinExpr :: (RenderSym repr) => FS (repr (BinaryOp repr)) -> 
  FS (repr (Type repr)) -> FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
  FS (repr (Value repr))
typeBinExpr bod tp vl1 vl2 = (\b t v1 v2 -> mkExpr (bOpPrec b) t (binOpDocD 
  (bOpDoc b) (exprParensL b v1 $ valueDoc v1) (exprParensR b v2 $ valueDoc v2)))
  <$> bod <*> tp <*> vl1 <*> vl2 

addmathImport :: FS a -> FS a
addmathImport = (>>) $ modify (addLangImport "math")

-- Variables --

var :: (RenderSym repr) => Label -> FS (repr (Type repr)) -> 
  FS (repr (Variable repr))
var n t = mkStateVar n t (varDocD n)

staticVar :: (RenderSym repr) => Label -> FS (repr (Type repr)) -> 
  FS (repr (Variable repr))
staticVar n t = mkStaticVar n t (varDocD n)

extVar :: (RenderSym repr) => Label -> Label -> FS (repr (Type repr)) -> 
  FS (repr (Variable repr))
extVar l n t = mkStateVar (l ++ "." ++ n) t (extVarDocD l n)

self :: (RenderSym repr) => Label -> FS (repr (Variable repr))
self l = mkStateVar "this" (obj l) selfDocD

enumVar :: (RenderSym repr) => Label -> Label -> FS (repr (Variable repr))
enumVar e en = S.var e (enumType en)

classVar :: (RenderSym repr) => (Doc -> Doc -> Doc) -> FS (repr (Type repr)) -> 
  FS (repr (Variable repr)) -> FS (repr (Variable repr))
classVar f = on2StateValues (\c v -> classVarCheckStatic $ varFromData 
  (variableBind v) (getTypeString c ++ "." ++ variableName v) 
  (variableType v) (f (getTypeDoc c) (variableDoc v)))

objVar :: (RenderSym repr) => FS (repr (Variable repr)) -> 
  FS (repr (Variable repr)) -> FS (repr (Variable repr))
objVar = on2StateValues (\o v -> mkVar (variableName o ++ "." ++ variableName 
  v) (variableType v) (objVarDocD (variableDoc o) (variableDoc v)))

objVarSelf :: (RenderSym repr) => Label -> FS (repr (Variable repr)) -> 
  FS (repr (Variable repr))
objVarSelf l = S.objVar (S.self l)

listVar :: (RenderSym repr) => Label -> repr (Permanence repr) -> 
  FS (repr (Type repr)) -> FS (repr (Variable repr))
listVar n p t = S.var n (listType p t)

listOf :: (RenderSym repr) => Label -> FS (repr (Type repr)) -> 
  FS (repr (Variable repr))
listOf n = S.listVar n static_

iterVar :: (RenderSym repr) => Label -> FS (repr (Type repr)) -> 
  FS (repr (Variable repr))
iterVar n t = S.var n (iterator t)

-- Values --

litTrue :: (RenderSym repr) => FS (repr (Value repr))
litTrue = mkStateVal S.bool (text "true")

litFalse :: (RenderSym repr) => FS (repr (Value repr))
litFalse = mkStateVal S.bool (text "false")

litChar :: (RenderSym repr) => Char -> FS (repr (Value repr))
litChar c = mkStateVal S.char (quotes $ D.char c)

litFloat :: (RenderSym repr) => Double -> FS (repr (Value repr))
litFloat f = mkStateVal S.float (D.double f)

litInt :: (RenderSym repr) => Integer -> FS (repr (Value repr))
litInt i = mkStateVal S.int (integer i)

litString :: (RenderSym repr) => String -> FS (repr (Value repr))
litString s = mkStateVal S.string (doubleQuotedText s)

pi :: (RenderSym repr) => FS (repr (Value repr))
pi = mkStateVal S.float (text "Math.PI")

valueOf :: (RenderSym repr) => FS (repr (Variable repr)) -> 
  FS (repr (Value repr))
valueOf = onStateValue (\v -> mkVal (variableType v) (variableDoc v))

arg :: (RenderSym repr) => FS (repr (Value repr)) -> FS (repr (Value repr)) ->
  FS (repr (Value repr))
arg = on3StateValues (\s n args -> mkVal s (argDocD n args)) S.string

enumElement :: (RenderSym repr) => Label -> Label -> FS (repr (Value repr))
enumElement en e = mkStateVal (enumType en) (enumElemDocD en e)

argsList :: (RenderSym repr) => String -> FS (repr (Value repr))
argsList l = mkStateVal (listType static_ S.string) (text l)

inlineIf :: (RenderSym repr) => FS (repr (Value repr)) -> FS (repr (Value repr))
  -> FS (repr (Value repr)) -> FS (repr (Value repr))
inlineIf = on3StateValues (\c v1 v2 -> valFromData (prec c) (valueType v1) 
  (valueDoc c <+> text "?" <+> valueDoc v1 <+> text ":" <+> valueDoc v2)) 
  where prec cd = valuePrec cd <|> Just 0

funcApp :: (RenderSym repr) => Label -> FS (repr (Type repr)) -> 
  [FS (repr (Value repr))] -> FS (repr (Value repr))
funcApp n = on1StateValue1List (\t -> mkVal t . funcAppDocD n)

selfFuncApp :: (RenderSym repr) => FS (repr (Variable repr)) -> Label -> 
  FS (repr (Type repr)) -> [FS (repr (Value repr))] -> FS (repr (Value repr))
selfFuncApp s n t vs = s >>= 
  (\slf -> S.funcApp (variableName slf ++ "." ++ n) t vs)

extFuncApp :: (RenderSym repr) => Library -> Label -> FS (repr (Type repr)) -> 
  [FS (repr (Value repr))] -> FS (repr (Value repr))
extFuncApp l n = S.funcApp (l ++ "." ++ n)

newObj :: (RenderSym repr) => (repr (Type repr) -> Doc -> Doc) -> 
  FS (repr (Type repr)) -> [FS (repr (Value repr))] -> FS (repr (Value repr))
newObj f = on1StateValue1List (\t -> mkVal t . f t . valueList)

notNull :: (RenderSym repr) => FS (repr (Value repr)) -> FS (repr (Value repr))
notNull v = v ?!= S.valueOf (S.var "null" $ onStateValue valueType v)

objAccess :: (RenderSym repr) => FS (repr (Value repr)) ->
  FS (repr (Function repr)) -> FS (repr (Value repr))
objAccess = on2StateValues (\v f -> mkVal (functionType f) (objAccessDocD 
  (valueDoc v) (functionDoc f)))

objMethodCall :: (RenderSym repr) => Label -> FS (repr (Type repr)) -> 
  FS (repr (Value repr)) -> [FS (repr (Value repr))] -> FS (repr (Value repr))
objMethodCall f t o ps = S.objAccess o (S.func f t ps)

objMethodCallNoParams :: (RenderSym repr) => Label -> FS (repr (Type repr)) -> 
  FS (repr (Value repr)) -> FS (repr (Value repr))
objMethodCallNoParams f t o = S.objMethodCall t o f []

selfAccess :: (RenderSym repr) => Label -> FS (repr (Function repr)) -> 
  FS (repr (Value repr))
selfAccess l = S.objAccess (S.valueOf $ S.self l)

listIndexExists :: (RenderSym repr) => FS (repr (Value repr)) -> 
  FS (repr (Value repr)) -> FS (repr (Value repr))
listIndexExists lst index = S.listSize lst ?> index

indexOf :: (RenderSym repr) => Label -> FS (repr (Value repr)) -> 
  FS (repr (Value repr)) -> FS (repr (Value repr))
indexOf f l v = S.objAccess l (S.func f S.int [v])

-- Functions --

func :: (RenderSym repr) => Label -> FS (repr (Type repr)) -> 
  [FS (repr (Value repr))] -> FS (repr (Function repr))
func l t vs = S.funcApp l t vs >>= ((`funcFromData` t) . funcDocD . valueDoc)

get :: (RenderSym repr) => FS (repr (Value repr)) -> FS (repr (Variable repr)) 
  -> FS (repr (Value repr))
get v vToGet = v $. S.getFunc vToGet

set :: (RenderSym repr) => FS (repr (Value repr)) -> FS (repr (Variable repr)) 
  -> FS (repr (Value repr)) -> FS (repr (Value repr))
set v vToSet toVal = v $. S.setFunc (onStateValue valueType v) vToSet toVal

listSize :: (RenderSym repr) => FS (repr (Value repr)) -> FS (repr (Value repr))
listSize v = v $. S.listSizeFunc

listAdd :: (RenderSym repr) => FS (repr (Value repr)) -> FS (repr (Value repr)) 
  -> FS (repr (Value repr)) -> FS (repr (Value repr))
listAdd v i vToAdd = v $. S.listAddFunc v i vToAdd

listAppend :: (RenderSym repr) => FS (repr (Value repr)) -> 
  FS (repr (Value repr)) -> FS (repr (Value repr))
listAppend v vToApp = v $. S.listAppendFunc vToApp

iterBegin :: (RenderSym repr) => FS (repr (Value repr)) -> 
  FS (repr (Value repr))
iterBegin v = v $. iterBeginFunc (S.listInnerType $ onStateValue valueType v)

iterEnd :: (RenderSym repr) => FS (repr (Value repr)) -> FS (repr (Value repr))
iterEnd v = v $. iterEndFunc (S.listInnerType $ onStateValue valueType v)

listAccess :: (RenderSym repr) => FS (repr (Value repr)) -> 
  FS (repr (Value repr)) -> FS (repr (Value repr))
listAccess v i = v $. S.listAccessFunc (S.listInnerType $ onStateValue 
  valueType v) i

listSet :: (RenderSym repr) => FS (repr (Value repr)) -> FS (repr (Value repr)) 
  -> FS (repr (Value repr)) -> FS (repr (Value repr))
listSet v i toVal = v $. S.listSetFunc v i toVal

getFunc :: (RenderSym repr) => FS (repr (Variable repr)) -> 
  FS (repr (Function repr))
getFunc v = v >>= (\vr -> S.func (getterName $ variableName vr) 
  (toState $ variableType vr) [])

setFunc :: (RenderSym repr) => FS (repr (Type repr)) ->
  FS (repr (Variable repr)) -> FS (repr (Value repr)) -> 
  FS (repr (Function repr))
setFunc t v toVal = v >>= (\vr -> S.func (setterName $ variableName vr) t 
  [toVal])

listSizeFunc :: (RenderSym repr) => FS (repr (Function repr))
listSizeFunc = S.func "size" S.int []

listAddFunc :: (RenderSym repr) => Label -> FS (repr (Value repr)) -> 
  FS (repr (Value repr)) -> FS (repr (Function repr))
listAddFunc f i v = S.func f (listType static_ $ onStateValue valueType v) 
  [i, v]

listAppendFunc :: (RenderSym repr) => Label -> FS (repr (Value repr)) -> 
  FS (repr (Function repr))
listAppendFunc f v = S.func f (listType static_ $ onStateValue valueType v) [v]

iterBeginError :: String -> String
iterBeginError l = "Attempt to use iterBeginFunc in " ++ l ++ ", but " ++ l ++ 
  " has no iterators"

iterEndError :: String -> String
iterEndError l = "Attempt to use iterEndFunc in " ++ l ++ ", but " ++ l ++ 
  " has no iterators"

listAccessFunc :: (RenderSym repr) => FS (repr (Type repr)) ->
  FS (repr (Value repr)) -> FS (repr (Function repr))
listAccessFunc t v = intValue v >>= ((`funcFromData` t) . listAccessFuncDocD)

listAccessFunc' :: (RenderSym repr) => Label -> FS (repr (Type repr)) -> 
  FS (repr (Value repr)) -> FS (repr (Function repr))
listAccessFunc' f t i = S.func f t [intValue i]

listSetFunc :: (RenderSym repr) => (Doc -> Doc -> Doc) -> 
  FS (repr (Value repr)) -> FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
  FS (repr (Function repr))
listSetFunc f v idx setVal = join $ on2StateValues (\i toVal -> funcFromData 
  (f (valueDoc i) (valueDoc toVal)) (onStateValue valueType v)) (intValue idx) 
  setVal

-- Statements --

printSt :: (RenderSym repr) => FS (repr (Value repr)) -> FS (repr (Value repr))
  -> FS (repr (Statement repr))
printSt = on2StateValues (\p v -> stateFromData (printDoc p v) Semi)

state :: (RenderSym repr) => FS (repr (Statement repr)) -> 
  FS (repr (Statement repr))
state = onStateValue (\s -> stateFromData (statementDoc s <> getTermDoc 
  (statementTerm s)) Empty)
  
loopState :: (RenderSym repr) => FS (repr (Statement repr)) -> 
  FS (repr (Statement repr))
loopState = S.state . setEmpty

emptyState :: (RenderSym repr) => FS (repr (Statement repr))
emptyState = toState $ stateFromData empty Empty

assign :: (RenderSym repr) => Terminator -> FS (repr (Variable repr)) -> 
  FS (repr (Value repr)) -> FS (repr (Statement repr))
assign t = on2StateValues (\vr vl -> stateFromData (assignDocD vr vl) t)

assignToListIndex :: (RenderSym repr) => FS (repr (Variable repr)) -> 
  FS (repr (Value repr)) -> FS (repr (Value repr)) -> FS (repr (Statement repr))
assignToListIndex lst index v = S.valState $ S.listSet (S.valueOf lst) index v

multiAssignError :: String -> String
multiAssignError l = "No multiple assignment statements in " ++ l

decrement :: (RenderSym repr) => FS (repr (Variable repr)) -> 
  FS (repr (Value repr)) -> FS (repr (Statement repr))
decrement vr vl = vr &= (S.valueOf vr #- vl)

increment :: (RenderSym repr) => FS (repr (Variable repr)) -> 
  FS (repr (Value repr)) -> FS (repr (Statement repr))
increment = on2StateValues (\vr vl -> stateFromData (plusEqualsDocD vr vl) Semi)

increment1 :: (RenderSym repr) => FS (repr (Variable repr)) -> 
  FS (repr (Statement repr))
increment1 = onStateValue (\v -> stateFromData (plusPlusDocD v) Semi)

increment' :: (RenderSym repr) => FS (repr (Variable repr)) -> 
  FS (repr (Value repr)) -> FS (repr (Statement repr))
increment' vr vl = vr &= S.valueOf vr #+ vl

increment1' :: (RenderSym repr) => FS (repr (Variable repr)) -> 
  FS (repr (Statement repr))
increment1' vr = vr &= S.valueOf vr #+ S.litInt 1

decrement1 :: (RenderSym repr) => FS (repr (Variable repr)) -> 
  FS (repr (Statement repr))
decrement1 v = v &= (S.valueOf v #- S.litInt 1)

varDec :: (RenderSym repr) => repr (Permanence repr) -> repr (Permanence repr) 
  -> FS (repr (Variable repr)) -> FS (repr (Statement repr))
varDec s d = onStateValue (\v -> stateFromData (permDoc (bind $ variableBind v) 
  <+> getTypeDoc (variableType v) <+> variableDoc v) Semi)
  where bind Static = s
        bind Dynamic = d

varDecDef :: (RenderSym repr) => FS (repr (Variable repr)) -> 
  FS (repr (Value repr)) -> FS (repr (Statement repr))
varDecDef vr = on2StateValues (\vd vl -> stateFromData (statementDoc vd <+> 
  equals <+> valueDoc vl) Semi) (S.varDec vr)

listDec :: (RenderSym repr) => (repr (Value repr) -> Doc) -> 
  FS (repr (Value repr)) -> FS (repr (Variable repr)) -> 
  FS (repr (Statement repr))
listDec f vl v = on2StateValues (\sz vd -> stateFromData (statementDoc vd <> f 
  sz) Semi) vl (S.varDec v)

listDecDef :: (RenderSym repr) => ([repr (Value repr)] -> Doc) -> 
  FS (repr (Variable repr)) -> [FS (repr (Value repr))] -> 
  FS (repr (Statement repr))
listDecDef f v = on1StateValue1List (\vd vs -> stateFromData (statementDoc vd 
  <> f vs) Semi) (S.varDec v)

listDecDef' :: (RenderSym repr) => FS (repr (Variable repr)) -> 
  [FS (repr (Value repr))] -> FS (repr (Statement repr))
listDecDef' v vals = on3StateValues (\vd vr vs -> stateFromData (statementDoc
  vd <+> equals <+> new <+> getTypeDoc (variableType vr) <+> braces 
  (valueList vs)) Semi) (S.varDec v) v (sequence vals)

objDecNew :: (RenderSym repr) => FS (repr (Variable repr)) -> 
  [FS (repr (Value repr))] -> FS (repr (Statement repr))
objDecNew v vs = S.varDecDef v (S.newObj (onStateValue variableType v) vs)

objDecNewNoParams :: (RenderSym repr) => FS (repr (Variable repr)) -> 
  FS (repr (Statement repr))
objDecNewNoParams v = S.objDecNew v []

constDecDef :: (RenderSym repr) => FS (repr (Variable repr)) -> 
  FS (repr (Value repr)) -> FS (repr (Statement repr))
constDecDef = on2StateValues (\v def -> stateFromData (constDecDefDocD v def) 
  Semi)

discardInput :: (RenderSym repr) => (repr (Value repr) -> Doc) ->
  FS (repr (Statement repr))
discardInput f = onStateValue (\infn -> stateFromData (f infn) Semi) inputFunc

discardFileInput :: (RenderSym repr) => (repr (Value repr) -> Doc) -> 
  FS (repr (Value repr)) -> FS (repr (Statement repr))
discardFileInput f = onStateValue (\v -> stateFromData (f v) Semi)

openFileR :: (RenderSym repr) => (FS (repr (Value repr)) -> 
  FS (repr (Type repr)) -> FS (repr (Value repr))) -> FS (repr (Variable repr)) 
  -> FS (repr (Value repr)) -> FS (repr (Statement repr))
openFileR f vr vl = vr &= f vl infile

openFileW :: (RenderSym repr) => (FS (repr (Value repr)) -> 
  FS (repr (Type repr)) -> FS (repr (Value repr)) -> FS (repr (Value repr))) -> 
  FS (repr (Variable repr)) -> FS (repr (Value repr)) ->
  FS (repr (Statement repr))
openFileW f vr vl = vr &= f vl outfile S.litFalse

openFileA :: (RenderSym repr) => (FS (repr (Value repr)) -> 
  FS (repr (Type repr)) -> FS (repr (Value repr)) -> FS (repr (Value repr))) -> 
  FS (repr (Variable repr)) -> FS (repr (Value repr)) -> 
  FS (repr (Statement repr))
openFileA f vr vl = vr &= f vl outfile S.litTrue

closeFile :: (RenderSym repr) => Label -> FS (repr (Value repr)) -> 
  FS (repr (Statement repr))
closeFile n f = S.valState $ S.objMethodCallNoParams S.void f n

discardFileLine :: (RenderSym repr) => Label -> FS (repr (Value repr)) -> 
  FS (repr (Statement repr))
discardFileLine n f = S.valState $ S.objMethodCallNoParams S.string f n 

stringListVals :: (RenderSym repr) => [FS (repr (Variable repr))] -> 
  FS (repr (Value repr)) -> FS (repr (Statement repr))
stringListVals vars sl = sl >>= (\slst -> multi $ checkList (getType $ 
  valueType slst))
  where checkList (List String) = assignVals vars 0
        checkList _ = error 
          "Value passed to stringListVals must be a list of strings"
        assignVals [] _ = []
        assignVals (v:vs) n = S.assign v (cast (onStateValue variableType v) 
          (S.listAccess sl (S.litInt n))) : assignVals vs (n+1)

stringListLists :: (RenderSym repr) => [FS (repr (Variable repr))] -> 
  FS (repr (Value repr)) -> FS (repr (Statement repr))
stringListLists lsts sl = sl >>= (\slst -> checkList (getType $ valueType slst))
  where checkList (List String) = sequence lsts >>= listVals . map (getType . 
          variableType)
        checkList _ = error 
          "Value passed to stringListLists must be a list of strings"
        listVals [] = loop
        listVals (List _:vs) = listVals vs
        listVals _ = error 
          "All values passed to stringListLists must have list types"
        loop = S.forRange var_i (S.litInt 0) (S.listSize sl #/ numLists) 
          (S.litInt 1) (bodyStatements $ appendLists (map S.valueOf lsts) 0)
        appendLists [] _ = []
        appendLists (v:vs) n = S.valState (S.listAppend v (cast 
          (S.listInnerType $ onStateValue valueType v)
          (S.listAccess sl ((v_i #* numLists) #+ S.litInt n)))) 
          : appendLists vs (n+1)
        numLists = S.litInt (toInteger $ length lsts)
        var_i = S.var "stringlist_i" S.int
        v_i = S.valueOf var_i

returnState :: (RenderSym repr) => Terminator -> FS (repr (Value repr)) -> 
  FS (repr (Statement repr))
returnState t = onStateValue (\v -> stateFromData (returnDocD [v]) t)

multiReturnError :: String -> String
multiReturnError l = "Cannot return multiple values in " ++ l

valState :: (RenderSym repr) => Terminator -> FS (repr (Value repr)) ->
  FS (repr (Statement repr))
valState t = onStateValue (\v -> stateFromData (valueDoc v) t)

comment :: (RenderSym repr) => repr (Keyword repr) -> Label -> 
  FS (repr (Statement repr))
comment cs c = toState $ stateFromData (commentDocD c (keyDoc cs)) Empty

freeError :: String -> String
freeError l = "Cannot free variables in " ++ l

throw :: (RenderSym repr) => (repr (Value repr) -> Doc) -> Terminator -> 
  Label -> FS (repr (Statement repr))
throw f t = onStateValue (\msg -> stateFromData (f msg) t) . S.litString

initState :: (RenderSym repr) => Label -> Label -> FS (repr (Statement repr))
initState fsmName initialState = S.varDecDef (S.var fsmName S.string) 
  (S.litString initialState)

changeState :: (RenderSym repr) => Label -> Label -> FS (repr (Statement repr))
changeState fsmName tState = S.var fsmName S.string &= S.litString tState

initObserverList :: (RenderSym repr) => FS (repr (Type repr)) -> 
  [FS (repr (Value repr))] -> FS (repr (Statement repr))
initObserverList t = S.listDecDef (S.var observerListName (S.listType static_ t))

addObserver :: (RenderSym repr) => FS (repr (Value repr)) -> 
  FS (repr (Statement repr))
addObserver o = S.valState $ S.listAdd obsList lastelem o
  where obsList = S.valueOf $ observerListName `S.listOf` onStateValue 
          valueType o
        lastelem = S.listSize obsList

-- ControlStatements --

ifCond :: (RenderSym repr) => repr (Keyword repr) -> repr (Keyword repr) -> 
  repr (Keyword repr) -> [(FS (repr (Value repr)), FS (repr (Body repr)))] -> 
  FS (repr (Body repr)) -> FS (repr (Statement repr))
ifCond _ _ _ [] _ = error "if condition created with no cases"
ifCond ifst elseif blEnd (c:cs) eBody = 
    let ifStart = keyDoc ifst
        elif = keyDoc elseif
        bEnd = keyDoc blEnd
        ifSect (v, b) = on2StateValues (\val bd -> vcat [
          text "if" <+> parens (valueDoc val) <+> ifStart,
          indent $ bodyDoc bd,
          bEnd]) v b
        elseIfSect (v, b) = on2StateValues (\val bd -> vcat [
          elif <+> parens (valueDoc val) <+> ifStart,
          indent $ bodyDoc bd,
          bEnd]) v b
        elseSect = onStateValue (\bd -> emptyIfEmpty (bodyDoc bd) $ vcat [
          text "else" <+> ifStart,
          indent $ bodyDoc bd,
          bEnd]) eBody
    in onStateList (\l -> stateFromData (vcat l) Empty)
      (ifSect c : map elseIfSect cs ++ [elseSect])

ifNoElse :: (RenderSym repr) => [(FS (repr (Value repr)), 
  FS (repr (Body repr)))] -> FS (repr (Statement repr))
ifNoElse bs = S.ifCond bs $ body []

switch :: (RenderSym repr) => FS (repr (Value repr)) -> 
  [(FS (repr (Value repr)), FS (repr (Body repr)))] -> FS (repr (Body repr)) -> 
  FS (repr (Statement repr))
switch v cs = on4StateValues (\b val css de -> stateFromData (switchDocD b val 
  de css) Semi) (S.state break) v (on2StateLists zip (map fst cs) (map snd cs))

switchAsIf :: (RenderSym repr) => FS (repr (Value repr)) -> 
  [(FS (repr (Value repr)), FS (repr (Body repr)))] -> FS (repr (Body repr)) -> 
  FS (repr (Statement repr))
switchAsIf v cs = S.ifCond cases
  where cases = map (first (v ?==)) cs

ifExists :: (RenderSym repr) => FS (repr (Value repr)) -> FS (repr (Body repr)) 
  -> FS (repr (Body repr)) -> FS (repr (Statement repr))
ifExists v ifBody = S.ifCond [(S.notNull v, ifBody)]

for :: (RenderSym repr) => repr (Keyword repr) -> repr (Keyword repr) -> 
  FS (repr (Statement repr)) -> FS (repr (Value repr)) -> 
  FS (repr (Statement repr)) -> FS (repr (Body repr)) -> 
  FS (repr (Statement repr))
for bStart bEnd sInit vGuard sUpdate = on4StateValues (\initl guard upd bod -> 
  stateFromData (vcat [
  forLabel <+> parens (statementDoc initl <> semi <+> valueDoc guard <> semi 
    <+> statementDoc upd) <+> keyDoc bStart,
  indent $ bodyDoc bod,
  keyDoc bEnd]) Empty) (S.loopState sInit) vGuard (S.loopState sUpdate)

forRange :: (RenderSym repr) => FS (repr (Variable repr)) -> 
  FS (repr (Value repr)) -> FS (repr (Value repr)) -> FS (repr (Value repr)) -> 
  FS (repr (Body repr)) -> FS (repr (Statement repr))
forRange i initv finalv stepv = S.for (S.varDecDef i initv) (S.valueOf i ?< 
  finalv) (i &+= stepv)

forEach :: (RenderSym repr) => repr (Keyword repr) -> repr (Keyword repr) -> 
  repr (Keyword repr) -> repr (Keyword repr) -> FS (repr (Variable repr)) -> 
  FS (repr (Value repr)) -> FS (repr (Body repr)) -> FS (repr (Statement repr))
forEach bStart bEnd forEachLabel inLbl = on3StateValues (\e v b -> stateFromData
  (vcat [keyDoc forEachLabel <+> parens (getTypeDoc (variableType e) <+> 
    variableDoc e <+> keyDoc inLbl <+> valueDoc v) <+> keyDoc bStart,
  indent $ bodyDoc b,
  keyDoc bEnd]) Empty) 

while :: (RenderSym repr) => repr (Keyword repr) -> repr (Keyword repr) -> 
  FS (repr (Value repr)) -> FS (repr (Body repr)) -> FS (repr (Statement repr))
while bStart bEnd = on2StateValues (\v b -> stateFromData (vcat [
  text "while" <+> parens (valueDoc v) <+> keyDoc bStart,
  indent $ bodyDoc b,
  keyDoc bEnd]) Empty)

tryCatch :: (RenderSym repr) => (repr (Body repr) -> repr (Body repr) -> Doc) ->
  FS (repr (Body repr)) -> FS (repr (Body repr)) -> FS (repr (Statement repr))
tryCatch f = on2StateValues (\tb cb -> stateFromData (f tb cb) Empty)

checkState :: (RenderSym repr) => Label -> [(FS (repr (Value repr)), 
  FS (repr (Body repr)))] -> FS (repr (Body repr)) -> FS (repr (Statement repr))
checkState l = S.switch (S.valueOf $ S.var l S.string)

notifyObservers :: (RenderSym repr) => FS (repr (Function repr)) -> 
  FS (repr (Type repr)) -> FS (repr (Statement repr))
notifyObservers f t = S.for initv (v_index ?< S.listSize obsList) 
  (var_index &++) notify
  where obsList = S.valueOf $ observerListName `S.listOf` t 
        var_index = S.var "observerIndex" S.int
        v_index = S.valueOf var_index
        initv = S.varDecDef var_index $ S.litInt 0
        notify = S.oneLiner $ S.valState $ at obsList v_index $. f

-- Methods --

construct :: (RenderSym repr) => Label -> FS (repr (Type repr))
construct n = toState $ typeFromData (Object n) n empty

param :: (RenderSym repr) => (repr (Variable repr) -> Doc) -> 
  FS (repr (Variable repr)) -> MS (repr (Parameter repr))
param f = getPutReturnFunc (\s v -> addParameter (variableName v) s) 
  (\v -> paramFromData v (f v)) . zoom lensMStoFS

method :: (RenderSym repr) => Label -> Label -> repr (Scope repr) -> 
  repr (Permanence repr) -> FS (repr (Type repr)) -> 
  [MS (repr (Parameter repr))] -> FS (repr (Body repr)) -> 
  MS (repr (Method repr))
method n c s p t = intMethod False n c s p (mType t)

getMethod :: (RenderSym repr) => Label -> FS (repr (Variable repr)) -> 
  MS (repr (Method repr))
getMethod c v = zoom lensMStoFS v >>= (\vr -> S.method (getterName $ 
    variableName vr) c public dynamic_ (toState $ variableType vr) [] getBody)
    where getBody = S.oneLiner $ S.returnState (S.valueOf $ S.objVarSelf c v)

setMethod :: (RenderSym repr) => Label -> FS (repr (Variable repr)) -> 
  MS (repr (Method repr))
setMethod c v = zoom lensMStoFS v >>= (\vr -> S.method (setterName $ 
  variableName vr) c public dynamic_ S.void [S.param v] setBody)
  where setBody = S.oneLiner $ S.objVarSelf c v &= S.valueOf v

privMethod :: (RenderSym repr) => Label -> Label -> FS (repr (Type repr)) -> 
  [MS (repr (Parameter repr))] -> FS (repr (Body repr)) -> 
  MS (repr (Method repr))
privMethod n c = S.method n c private dynamic_

pubMethod :: (RenderSym repr) => Label -> Label -> FS (repr (Type repr)) -> 
  [MS (repr (Parameter repr))] -> FS (repr (Body repr)) -> 
  MS (repr (Method repr))
pubMethod n c = S.method n c public dynamic_

constructor :: (RenderSym repr) => Label -> Label -> 
  [MS (repr (Parameter repr))] -> FS (repr (Body repr)) -> 
  MS (repr (Method repr))
constructor fName n = intMethod False fName n public dynamic_ (S.construct n)

docMain :: (RenderSym repr) => FS (repr (Body repr)) -> MS (repr (Method repr))
docMain b = commentedFunc (docComment $ toState $ functionDox 
  "Controls the flow of the program" 
  [("args", "List of command-line arguments")] []) (S.mainFunction b)

function :: (RenderSym repr) => Label -> repr (Scope repr) -> 
  repr (Permanence repr) -> FS (repr (Type repr)) -> 
  [MS (repr (Parameter repr))] -> FS (repr (Body repr)) -> 
  MS (repr (Method repr))
function n s p t = S.intFunc False n s p (mType t)

mainFunction :: (RenderSym repr) => FS (repr (Type repr)) -> Label -> 
  FS (repr (Body repr)) -> MS (repr (Method repr))
mainFunction s n = S.intFunc True n public static_ (mType S.void)
  [S.param (S.var "args" (onStateValue (\argT -> typeFromData (List String) 
  (render (getTypeDoc argT) ++ "[]") (getTypeDoc argT <> text "[]")) s))]

docFunc :: (RenderSym repr) => String -> [String] -> Maybe String -> 
  MS (repr (Method repr)) -> MS (repr (Method repr))
docFunc desc pComms rComm = docFuncRepr desc pComms (maybeToList rComm)

docInOutFunc :: (RenderSym repr) => (repr (Scope repr) -> repr (Permanence repr)
    -> [FS (repr (Variable repr))] -> [FS (repr (Variable repr))] -> 
    [FS (repr (Variable repr))] -> FS (repr (Body repr)) -> 
    MS (repr (Method repr)))
  -> repr (Scope repr) -> repr (Permanence repr) -> String -> 
  [(String, FS (repr (Variable repr)))] -> [(String, FS (repr (Variable repr)))]
  -> [(String, FS (repr (Variable repr)))] -> FS (repr (Body repr)) -> 
  MS (repr (Method repr))
docInOutFunc f s p desc is [o] [] b = docFuncRepr desc (map fst is) [fst o] 
  (f s p (map snd is) [snd o] [] b)
docInOutFunc f s p desc is [] [both] b = zoom lensMStoFS (snd both) >>= (\bth 
  -> docFuncRepr desc (map fst $ both : is) [fst both | not ((isObject . 
  getType . variableType) bth)] (f s p (map snd is) [] [toState bth] b))
docInOutFunc f s p desc is os bs b = docFuncRepr desc (map fst $ bs ++ is ++ os)
  [] (f s p (map snd is) (map snd os) (map snd bs) b)

intFunc :: (RenderSym repr) => Bool -> Label -> repr (Scope repr) -> 
  repr (Permanence repr) -> FS (repr (MethodType repr)) -> 
  [MS (repr (Parameter repr))] -> FS (repr (Body repr)) -> 
  MS (repr (Method repr))
intFunc m n = intMethod m n ""

-- State Variables --

stateVar :: (RenderSym repr) => repr (Scope repr) -> repr (Permanence repr) ->
  FS (repr (Variable repr)) -> FS (repr (StateVar repr))
stateVar s p v = stateVarFromData (onStateValue (stateVarDocD 
  (scopeDoc s) (permDoc p) . statementDoc) (S.state $ S.varDec v))

stateVarDef :: (RenderSym repr) => repr (Scope repr) -> repr (Permanence repr) 
  -> FS (repr (Variable repr)) -> FS (repr (Value repr)) -> 
  FS (repr (StateVar repr))
stateVarDef s p vr vl = stateVarFromData (onStateValue (stateVarDocD 
  (scopeDoc s) (permDoc p) . statementDoc) (S.state $ S.varDecDef vr vl))

constVar :: (RenderSym repr) => Doc -> repr (Scope repr) ->
  FS (repr (Variable repr)) -> FS (repr (Value repr)) -> FS (repr (StateVar repr))
constVar p s vr vl = stateVarFromData (onStateValue (stateVarDocD (scopeDoc s) 
  p . statementDoc) (S.state $ S.constDecDef vr vl))

privMVar :: (RenderSym repr) => FS (repr (Variable repr)) -> 
  FS (repr (StateVar repr))
privMVar = S.stateVar private dynamic_

pubMVar :: (RenderSym repr) => FS (repr (Variable repr)) -> 
  FS (repr (StateVar repr))
pubMVar = S.stateVar public dynamic_

pubGVar :: (RenderSym repr) => FS (repr (Variable repr)) -> 
  FS (repr (StateVar repr))
pubGVar = S.stateVar public static_

-- Classes --

buildClass :: (RenderSym repr) => (Label -> Doc -> Doc -> Doc -> Doc -> Doc) -> 
  (Label -> repr (Keyword repr)) -> Label -> Maybe Label -> repr (Scope repr) 
  -> [FS (repr (StateVar repr))] -> [MS (repr (Method repr))] -> 
  FS (repr (Class repr))
buildClass f i n p s vs fs = classFromData (on2StateValues (f n parent 
  (scopeDoc s)) (onStateList (stateVarListDocD . map stateVarDoc) vs)
  (onStateList (vibcat . map methodDoc) (map (zoom lensFStoMS) fs)))
  where parent = case p of Nothing -> empty
                           Just pn -> keyDoc $ i pn

enum :: (RenderSym repr) => Label -> [Label] -> repr (Scope repr) -> 
  FS (repr (Class repr))
enum n es s = classFromData (toState $ enumDocD n (enumElementsDocD es False) 
  (scopeDoc s))

privClass :: (RenderSym repr) => Label -> Maybe Label -> 
  [FS (repr (StateVar repr))] -> 
  [MS (repr (Method repr))] -> FS (repr (Class repr))
privClass n p = S.buildClass n p private

pubClass :: (RenderSym repr) => Label -> Maybe Label -> 
  [FS (repr (StateVar repr))] -> [MS (repr (Method repr))] -> 
  FS (repr (Class repr))
pubClass n p = S.buildClass n p public

docClass :: (RenderSym repr) => String -> FS (repr (Class repr))
  -> FS (repr (Class repr))
docClass d = S.commentedClass (docComment $ toState $ classDox d)

commentedClass :: (RenderSym repr) => FS (repr (BlockComment repr))
  -> FS (repr (Class repr)) -> FS (repr (Class repr))
commentedClass cmt cs = classFromData (on2StateValues (\cmt' cs' -> 
  commentedItem (blockCommentDoc cmt') (classDoc cs')) cmt cs)

-- Modules --

buildModule :: (RenderSym repr) => Label -> FS Doc -> [MS (repr (Method repr))] 
  -> [FS (repr (Class repr))] -> FS (repr (Module repr))
buildModule n imps ms cs = S.modFromData n ((\cls fs is -> moduleDocD is 
  (vibcat (map classDoc cls)) (vibcat (map methodDoc fs))) <$> sequence cs <*> 
  mapM (zoom lensFStoMS) ms <*> imps)

buildModule' :: (RenderSym repr) => Label -> (String -> repr (Import repr)) -> 
  [MS (repr (Method repr))] -> [FS (repr (Class repr))] -> 
  FS (repr (Module repr))
buildModule' n inc ms cs = S.modFromData n (on3StateValues (\cls lis mis -> 
  vibcat [
    vcat (map (importDoc . inc) lis), 
    vcat (map (importDoc . inc) mis), 
    vibcat (map classDoc cls)])
  (sequence $ if null ms then cs else pubClass n Nothing [] ms : cs)
  getLangImports getModuleImports)

modFromData :: Label -> (Doc -> repr (Module repr)) -> FS Doc -> 
  FS (repr (Module repr))
modFromData n f d = putAfter (setModuleName n) (onStateValue f d)

-- Files --

fileDoc :: (RenderSym repr) => FileType -> String -> (repr (Module repr) -> 
  repr (Block repr)) -> repr (Block repr) -> FS (repr (Module repr)) -> 
  FS (repr (RenderFile repr))
fileDoc ft ext topb botb = S.fileFromData ft (onStateValue (addExt ext) 
  getModuleName) . onStateValue (\m -> updateModuleDoc (\d -> emptyIfEmpty d 
  (fileDoc' (blockDoc $ topb m) d (blockDoc botb))) m)

docMod :: (RenderSym repr) => String -> [String] -> String -> 
  FS (repr (RenderFile repr)) -> FS (repr (RenderFile repr))
docMod d a dt = commentedMod (docComment $ moduleDox d a dt <$> getFilePath)

fileFromData :: (RenderSym repr) => (repr (Module repr) -> FilePath -> 
  repr (RenderFile repr)) -> FileType -> FS FilePath -> 
  FS (repr (Module repr)) -> FS (repr (RenderFile repr))
fileFromData f ft fp m = getPutReturnFunc2 (\s mdl fpath -> (if isEmpty 
  (moduleDoc mdl) then id else (if snd s ^. currMain then over lensFStoGS 
  (setMainMod fpath) else id) . over lensFStoGS (addFile ft fpath) . 
  setFilePath fpath) s) f m fp 

-- Helper functions

setEmpty :: (RenderSym repr) => FS (repr (Statement repr)) -> 
  FS (repr (Statement repr))
setEmpty = onStateValue (\s -> stateFromData (statementDoc s) Empty)

numType :: (RenderSym repr) => repr (Type repr) -> repr (Type repr) -> 
  repr (Type repr)
numType t1 t2 = numericType (getType t1) (getType t2)
  where numericType Integer Integer = t1
        numericType Float _ = t1
        numericType _ Float = t2
        numericType _ _ = error "Numeric types required for numeric expression"

mkExpr :: (RenderSym repr) => Int -> repr (Type repr) -> Doc -> 
  repr (Value repr)
mkExpr p = valFromData (Just p)

exprParensL :: (RenderSym repr) => repr (BinaryOp repr) -> repr (Value repr) -> 
  (Doc -> Doc)
exprParensL o v = if maybe False (< bOpPrec o) (valuePrec v) then parens else id

exprParensR :: (RenderSym repr) => repr (BinaryOp repr) -> repr (Value repr) -> 
  (Doc -> Doc)
exprParensR o v = if maybe False (<= bOpPrec o) (valuePrec v) then parens else 
  id