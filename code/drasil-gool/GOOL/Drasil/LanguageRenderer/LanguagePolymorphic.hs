{-# LANGUAGE PostfixOperators #-}

-- | The structure for a class of renderers is defined here.
module GOOL.Drasil.LanguageRenderer.LanguagePolymorphic (fileFromData, oneLiner,
  block, bool, int, float, double, char, string, fileType, listType, 
  listInnerType, obj, enumType, void, runStrategy, listSlice, var, staticVar, 
  extVar, self, enumVar, classVar, objVar, objVarSelf, listVar, listOf, iterVar,
  litTrue, litFalse, litChar, litFloat, litInt, litString, pi, valueOf, arg, 
  enumElement, argsList, inlineIf, funcApp, selfFuncApp, extFuncApp, newObj, 
  notNull, objAccess, objMethodCall, objMethodCallNoParams, selfAccess, 
  listIndexExists, indexOf, func, get, set, listSize, listAdd, listAppend, 
  iterBegin, iterEnd, listAccess, listSet, getFunc, setFunc, listSizeFunc, 
  listAddFunc, listAppendFunc, iterBeginError, iterEndError, listAccessFunc, 
  listAccessFunc', listSetFunc, printSt, state, loopState, emptyState, assign, 
  assignToListIndex, multiAssignError, decrement, increment, increment', 
  increment1, increment1', decrement1, varDec, varDecDef, listDec, listDecDef, 
  listDecDef', objDecNew, objDecNewNoParams, constDecDef, discardInput, 
  discardFileInput, openFileR, openFileW, openFileA, closeFile, discardFileLine,
  stringListVals, stringListLists, returnState, multiReturnError, valState, 
  comment, freeError, throw, initState, changeState, initObserverList, 
  addObserver, ifCond, ifNoElse, switch, switchAsIf, ifExists, for, forRange, 
  forEach, while, tryCatch, checkState, notifyObservers, construct, param, 
  method, getMethod, setMethod,privMethod, pubMethod, constructor, docMain, 
  function, mainFunction, docFunc, docInOutFunc, intFunc, stateVar,stateVarDef, 
  constVar, privMVar, pubMVar, pubGVar, buildClass, enum, privClass, pubClass, 
  docClass, commentedClass, buildModule, buildModule', modFromData, fileDoc, 
  docMod
) where

import Utils.Drasil (indent)

import GOOL.Drasil.CodeType (CodeType(..), isObject)
import GOOL.Drasil.Symantics (Label, Library, KeywordSym(..), RenderSym,
  FileSym(RenderFile, commentedMod), BlockSym(Block), InternalBlock(..), 
  BodySym(Body, body, bodyStatements, bodyDoc), PermanenceSym(..), 
  InternalPerm(..), 
  TypeSym(Type, infile, outfile, iterator, getType, getTypeDoc, getTypeString), 
  InternalType(..), VariableSym(Variable, variableBind, variableName, 
    variableType, variableDoc), 
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
import GOOL.Drasil.LanguageRenderer (forLabel, observerListName, addExt, 
  blockDocD, assignDocD, plusEqualsDocD, plusPlusDocD, mkStateVal, mkVal, 
  mkStateVar, mkVar, mkStaticVar, varDocD, extVarDocD, selfDocD, argDocD, 
  enumElemDocD, classVarCheckStatic, objVarDocD, funcAppDocD, objAccessDocD, 
  funcDocD, listAccessFuncDocD, constDecDefDocD, printDoc, returnDocD, 
  getTermDoc, switchDocD, stateVarDocD, stateVarListDocD, methodListDocD, 
  enumDocD, enumElementsDocD, moduleDocD, fileDoc', docFuncRepr, commentDocD, 
  commentedItem, functionDox, classDox, moduleDox, getterName, setterName, 
  valueList, intValue)
import GOOL.Drasil.State (GS, FS, MS, lensFStoGS, lensMStoGS, lensFStoMS, 
  currMain, putAfter, getPutReturnFunc, getPutReturnFunc2, addFile, setMainMod, 
  setFilePath, getFilePath, setModuleName, getModuleName, getCurrMain, 
  addParameter)

import Prelude hiding (break,print,last,mod,pi,(<>))
import Data.Bifunctor (first)
import Data.Map as Map (lookup, fromList)
import Data.Maybe (fromMaybe, maybeToList)
import Control.Applicative ((<|>))
import Control.Monad (join)
import Control.Lens ((^.), over)
import Control.Lens.Zoom (zoom)
import Text.PrettyPrint.HughesPJ (Doc, text, empty, render, (<>), (<+>), parens,
  quotes, integer, vcat, semi, equals, isEmpty)
import qualified Text.PrettyPrint.HughesPJ as D (char, double)

oneLiner :: (RenderSym repr) => GS (repr (Statement repr)) -> 
  GS (repr (Body repr))
oneLiner s = bodyStatements [s]

block :: (RenderSym repr) => repr (Keyword repr) -> [GS (repr (Statement repr))]
  -> GS (repr (Block repr))
block end sts = docBlock $ onStateList (blockDocD (keyDoc end) . map 
  statementDoc) (map S.state sts)

bool :: (RenderSym repr) => GS (repr (Type repr))
bool = toState $ typeFromData Boolean "Boolean" (text "Boolean")

int :: (RenderSym repr) => GS (repr (Type repr))
int = toState $ typeFromData Integer "int" (text "int")

float :: (RenderSym repr) => GS (repr (Type repr))
float = toState $ typeFromData Float "float" (text "float")

double :: (RenderSym repr) => GS (repr (Type repr))
double = toState $ typeFromData Float "double" (text "double")

char :: (RenderSym repr) => GS (repr (Type repr))
char = toState $ typeFromData Char "char" (text "char")

string :: (RenderSym repr) => GS (repr (Type repr))
string = toState $ typeFromData String "string" (text "string")

fileType :: (RenderSym repr) => GS (repr (Type repr))
fileType = toState $ typeFromData File "File" (text "File")

listType :: (RenderSym repr) => repr (Permanence repr) -> GS (repr (Type repr)) 
  -> GS (repr (Type repr))
listType p = onStateValue (\t -> typeFromData (List (getType t)) (render 
  (keyDoc $ list p) ++ "<" ++ getTypeString t ++ ">") (keyDoc (list p) <> 
  angles (getTypeDoc t)))

listInnerType :: (RenderSym repr) => GS (repr (Type repr)) -> 
  GS (repr (Type repr))
listInnerType t = t >>= (convType . getInnerType . getType)

obj :: (RenderSym repr) => Label -> GS (repr (Type repr))
obj n = toState $ typeFromData (Object n) n (text n)

enumType :: (RenderSym repr) => Label -> GS (repr (Type repr))
enumType e = toState $ typeFromData (Enum e) e (text e)

void :: (RenderSym repr) => GS (repr (Type repr))
void = toState $ typeFromData Void "void" (text "void")

strat :: (RenderSym repr) => GS (repr (Statement repr)) -> GS (repr (Body repr))
  -> GS (repr (Block repr))
strat r bd = docBlock $ on2StateValues (\result b -> vcat [bodyDoc b, 
  statementDoc result]) r bd

runStrategy :: (RenderSym repr) => String -> [(Label, GS (repr (Body repr)))] 
  -> Maybe (GS (repr (Value repr))) -> Maybe (GS (repr (Variable repr))) -> 
  GS (repr (Block repr))
runStrategy l strats rv av = maybe
  (strError l "RunStrategy called on non-existent strategy") 
  (strat (S.state resultState)) (Map.lookup l (Map.fromList strats))
  where resultState = maybe S.emptyState asgState av
        asgState v = maybe (strError l 
          "Attempt to assign null return to a Value") (v &=) rv
        strError n s = error $ "Strategy '" ++ n ++ "': " ++ s ++ "."

listSlice :: (RenderSym repr) => Maybe (GS (repr (Value repr))) -> 
  Maybe (GS (repr (Value repr))) -> Maybe (GS (repr (Value repr))) -> 
  GS (repr (Variable repr)) -> GS (repr (Value repr)) -> GS (repr (Block repr))
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

var :: (RenderSym repr) => Label -> GS (repr (Type repr)) -> 
  GS (repr (Variable repr))
var n t = mkStateVar n t (varDocD n)

staticVar :: (RenderSym repr) => Label -> GS (repr (Type repr)) -> 
  GS (repr (Variable repr))
staticVar n t = mkStaticVar n t (varDocD n)

extVar :: (RenderSym repr) => Label -> Label -> GS (repr (Type repr)) -> 
  GS (repr (Variable repr))
extVar l n t = mkStateVar (l ++ "." ++ n) t (extVarDocD l n)

self :: (RenderSym repr) => Label -> GS (repr (Variable repr))
self l = mkStateVar "this" (obj l) selfDocD

enumVar :: (RenderSym repr) => Label -> Label -> GS (repr (Variable repr))
enumVar e en = S.var e (enumType en)

classVar :: (RenderSym repr) => (Doc -> Doc -> Doc) -> GS (repr (Type repr)) -> 
  GS (repr (Variable repr)) -> GS (repr (Variable repr))
classVar f = on2StateValues (\c v -> classVarCheckStatic $ varFromData 
  (variableBind v) (getTypeString c ++ "." ++ variableName v) 
  (variableType v) (f (getTypeDoc c) (variableDoc v)))

objVar :: (RenderSym repr) => GS (repr (Variable repr)) -> 
  GS (repr (Variable repr)) -> GS (repr (Variable repr))
objVar = on2StateValues (\o v -> mkVar (variableName o ++ "." ++ variableName 
  v) (variableType v) (objVarDocD (variableDoc o) (variableDoc v)))

objVarSelf :: (RenderSym repr) => Label -> GS (repr (Variable repr)) -> 
  GS (repr (Variable repr))
objVarSelf l = S.objVar (S.self l)

listVar :: (RenderSym repr) => Label -> repr (Permanence repr) -> 
  GS (repr (Type repr)) -> GS (repr (Variable repr))
listVar n p t = S.var n (listType p t)

listOf :: (RenderSym repr) => Label -> GS (repr (Type repr)) -> 
  GS (repr (Variable repr))
listOf n = S.listVar n static_

iterVar :: (RenderSym repr) => Label -> GS (repr (Type repr)) -> 
  GS (repr (Variable repr))
iterVar n t = S.var n (iterator t)

litTrue :: (RenderSym repr) => GS (repr (Value repr))
litTrue = mkStateVal S.bool (text "true")

litFalse :: (RenderSym repr) => GS (repr (Value repr))
litFalse = mkStateVal S.bool (text "false")

litChar :: (RenderSym repr) => Char -> GS (repr (Value repr))
litChar c = mkStateVal S.char (quotes $ D.char c)

litFloat :: (RenderSym repr) => Double -> GS (repr (Value repr))
litFloat f = mkStateVal S.float (D.double f)

litInt :: (RenderSym repr) => Integer -> GS (repr (Value repr))
litInt i = mkStateVal S.int (integer i)

litString :: (RenderSym repr) => String -> GS (repr (Value repr))
litString s = mkStateVal S.string (doubleQuotedText s)

pi :: (RenderSym repr) => GS (repr (Value repr))
pi = mkStateVal S.float (text "Math.PI")

valueOf :: (RenderSym repr) => GS (repr (Variable repr)) -> 
  GS (repr (Value repr))
valueOf = onStateValue (\v -> mkVal (variableType v) (variableDoc v))

arg :: (RenderSym repr) => GS (repr (Value repr)) -> GS (repr (Value repr)) ->
  GS (repr (Value repr))
arg = on3StateValues (\s n args -> mkVal s (argDocD n args)) S.string

enumElement :: (RenderSym repr) => Label -> Label -> GS (repr (Value repr))
enumElement en e = mkStateVal (enumType en) (enumElemDocD en e)

argsList :: (RenderSym repr) => String -> GS (repr (Value repr))
argsList l = mkStateVal (listType static_ S.string) (text l)

inlineIf :: (RenderSym repr) => GS (repr (Value repr)) -> GS (repr (Value repr))
  -> GS (repr (Value repr)) -> GS (repr (Value repr))
inlineIf = on3StateValues (\c v1 v2 -> valFromData (prec c) (valueType v1) 
  (valueDoc c <+> text "?" <+> valueDoc v1 <+> text ":" <+> valueDoc v2)) 
  where prec cd = valuePrec cd <|> Just 0

funcApp :: (RenderSym repr) => Label -> GS (repr (Type repr)) -> 
  [GS (repr (Value repr))] -> GS (repr (Value repr))
funcApp n = on1StateValue1List (\t -> mkVal t . funcAppDocD n)

selfFuncApp :: (RenderSym repr) => GS (repr (Variable repr)) -> Label -> 
  GS (repr (Type repr)) -> [GS (repr (Value repr))] -> GS (repr (Value repr))
selfFuncApp s n t vs = s >>= 
  (\slf -> S.funcApp (variableName slf ++ "." ++ n) t vs)

extFuncApp :: (RenderSym repr) => Library -> Label -> GS (repr (Type repr)) -> 
  [GS (repr (Value repr))] -> GS (repr (Value repr))
extFuncApp l n = S.funcApp (l ++ "." ++ n)

newObj :: (RenderSym repr) => (repr (Type repr) -> Doc -> Doc) -> 
  GS (repr (Type repr)) -> [GS (repr (Value repr))] -> GS (repr (Value repr))
newObj f = on1StateValue1List (\t -> mkVal t . f t . valueList)

notNull :: (RenderSym repr) => GS (repr (Value repr)) -> GS (repr (Value repr))
notNull v = v ?!= S.valueOf (S.var "null" $ onStateValue valueType v)

objAccess :: (RenderSym repr) => GS (repr (Value repr)) ->
  GS (repr (Function repr)) -> GS (repr (Value repr))
objAccess = on2StateValues (\v f -> mkVal (functionType f) (objAccessDocD 
  (valueDoc v) (functionDoc f)))

objMethodCall :: (RenderSym repr) => Label -> GS (repr (Type repr)) -> 
  GS (repr (Value repr)) -> [GS (repr (Value repr))] -> GS (repr (Value repr))
objMethodCall f t o ps = S.objAccess o (S.func f t ps)

objMethodCallNoParams :: (RenderSym repr) => Label -> GS (repr (Type repr)) -> 
  GS (repr (Value repr)) -> GS (repr (Value repr))
objMethodCallNoParams f t o = S.objMethodCall t o f []

selfAccess :: (RenderSym repr) => Label -> GS (repr (Function repr)) -> 
  GS (repr (Value repr))
selfAccess l = S.objAccess (S.valueOf $ S.self l)

listIndexExists :: (RenderSym repr) => GS (repr (Value repr)) -> 
  GS (repr (Value repr)) -> GS (repr (Value repr))
listIndexExists lst index = S.listSize lst ?> index

indexOf :: (RenderSym repr) => Label -> GS (repr (Value repr)) -> 
  GS (repr (Value repr)) -> GS (repr (Value repr))
indexOf f l v = S.objAccess l (S.func f S.int [v])

func :: (RenderSym repr) => Label -> GS (repr (Type repr)) -> 
  [GS (repr (Value repr))] -> GS (repr (Function repr))
func l t vs = S.funcApp l t vs >>= ((`funcFromData` t) . funcDocD . valueDoc)

get :: (RenderSym repr) => GS (repr (Value repr)) -> GS (repr (Variable repr)) 
  -> GS (repr (Value repr))
get v vToGet = v $. S.getFunc vToGet

set :: (RenderSym repr) => GS (repr (Value repr)) -> GS (repr (Variable repr)) 
  -> GS (repr (Value repr)) -> GS (repr (Value repr))
set v vToSet toVal = v $. S.setFunc (onStateValue valueType v) vToSet toVal

listSize :: (RenderSym repr) => GS (repr (Value repr)) -> GS (repr (Value repr))
listSize v = v $. S.listSizeFunc

listAdd :: (RenderSym repr) => GS (repr (Value repr)) -> GS (repr (Value repr)) 
  -> GS (repr (Value repr)) -> GS (repr (Value repr))
listAdd v i vToAdd = v $. S.listAddFunc v i vToAdd

listAppend :: (RenderSym repr) => GS (repr (Value repr)) -> 
  GS (repr (Value repr)) -> GS (repr (Value repr))
listAppend v vToApp = v $. S.listAppendFunc vToApp

iterBegin :: (RenderSym repr) => GS (repr (Value repr)) -> 
  GS (repr (Value repr))
iterBegin v = v $. iterBeginFunc (S.listInnerType $ onStateValue valueType v)

iterEnd :: (RenderSym repr) => GS (repr (Value repr)) -> GS (repr (Value repr))
iterEnd v = v $. iterEndFunc (S.listInnerType $ onStateValue valueType v)

listAccess :: (RenderSym repr) => GS (repr (Value repr)) -> 
  GS (repr (Value repr)) -> GS (repr (Value repr))
listAccess v i = v $. S.listAccessFunc (S.listInnerType $ onStateValue 
  valueType v) i

listSet :: (RenderSym repr) => GS (repr (Value repr)) -> GS (repr (Value repr)) 
  -> GS (repr (Value repr)) -> GS (repr (Value repr))
listSet v i toVal = v $. S.listSetFunc v i toVal

getFunc :: (RenderSym repr) => GS (repr (Variable repr)) -> 
  GS (repr (Function repr))
getFunc v = v >>= (\vr -> S.func (getterName $ variableName vr) 
  (toState $ variableType vr) [])

setFunc :: (RenderSym repr) => GS (repr (Type repr)) ->
  GS (repr (Variable repr)) -> GS (repr (Value repr)) -> 
  GS (repr (Function repr))
setFunc t v toVal = v >>= (\vr -> S.func (setterName $ variableName vr) t 
  [toVal])

listSizeFunc :: (RenderSym repr) => GS (repr (Function repr))
listSizeFunc = S.func "size" S.int []

listAddFunc :: (RenderSym repr) => Label -> GS (repr (Value repr)) -> 
  GS (repr (Value repr)) -> GS (repr (Function repr))
listAddFunc f i v = S.func f (listType static_ $ onStateValue valueType v) 
  [i, v]

listAppendFunc :: (RenderSym repr) => Label -> GS (repr (Value repr)) -> 
  GS (repr (Function repr))
listAppendFunc f v = S.func f (listType static_ $ onStateValue valueType v) [v]

iterBeginError :: String -> String
iterBeginError l = "Attempt to use iterBeginFunc in " ++ l ++ ", but " ++ l ++ 
  " has no iterators"

iterEndError :: String -> String
iterEndError l = "Attempt to use iterEndFunc in " ++ l ++ ", but " ++ l ++ 
  " has no iterators"

listAccessFunc :: (RenderSym repr) => GS (repr (Type repr)) ->
  GS (repr (Value repr)) -> GS (repr (Function repr))
listAccessFunc t v = intValue v >>= ((`funcFromData` t) . listAccessFuncDocD)

listAccessFunc' :: (RenderSym repr) => Label -> GS (repr (Type repr)) -> 
  GS (repr (Value repr)) -> GS (repr (Function repr))
listAccessFunc' f t i = S.func f t [intValue i]

listSetFunc :: (RenderSym repr) => (Doc -> Doc -> Doc) -> 
  GS (repr (Value repr)) -> GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
  GS (repr (Function repr))
listSetFunc f v idx setVal = join $ on2StateValues (\i toVal -> funcFromData 
  (f (valueDoc i) (valueDoc toVal)) (onStateValue valueType v)) (intValue idx) 
  setVal

printSt :: (RenderSym repr) => GS (repr (Value repr)) -> GS (repr (Value repr))
  -> GS (repr (Statement repr))
printSt = on2StateValues (\p v -> stateFromData (printDoc p v) Semi)

state :: (RenderSym repr) => GS (repr (Statement repr)) -> 
  GS (repr (Statement repr))
state = onStateValue (\s -> stateFromData (statementDoc s <> getTermDoc 
  (statementTerm s)) Empty)
  
loopState :: (RenderSym repr) => GS (repr (Statement repr)) -> 
  GS (repr (Statement repr))
loopState = S.state . setEmpty

emptyState :: (RenderSym repr) => GS (repr (Statement repr))
emptyState = toState $ stateFromData empty Empty

assign :: (RenderSym repr) => Terminator -> GS (repr (Variable repr)) -> 
  GS (repr (Value repr)) -> GS (repr (Statement repr))
assign t = on2StateValues (\vr vl -> stateFromData (assignDocD vr vl) t)

assignToListIndex :: (RenderSym repr) => GS (repr (Variable repr)) -> 
  GS (repr (Value repr)) -> GS (repr (Value repr)) -> GS (repr (Statement repr))
assignToListIndex lst index v = S.valState $ S.listSet (S.valueOf lst) index v

multiAssignError :: String -> String
multiAssignError l = "No multiple assignment statements in " ++ l

decrement :: (RenderSym repr) => GS (repr (Variable repr)) -> 
  GS (repr (Value repr)) -> GS (repr (Statement repr))
decrement vr vl = vr &= (S.valueOf vr #- vl)

increment :: (RenderSym repr) => GS (repr (Variable repr)) -> 
  GS (repr (Value repr)) -> GS (repr (Statement repr))
increment = on2StateValues (\vr vl -> stateFromData (plusEqualsDocD vr vl) Semi)

increment1 :: (RenderSym repr) => GS (repr (Variable repr)) -> 
  GS (repr (Statement repr))
increment1 = onStateValue (\v -> stateFromData (plusPlusDocD v) Semi)

increment' :: (RenderSym repr) => GS (repr (Variable repr)) -> 
  GS (repr (Value repr)) -> GS (repr (Statement repr))
increment' vr vl = vr &= S.valueOf vr #+ vl

increment1' :: (RenderSym repr) => GS (repr (Variable repr)) -> 
  GS (repr (Statement repr))
increment1' vr = vr &= S.valueOf vr #+ S.litInt 1

decrement1 :: (RenderSym repr) => GS (repr (Variable repr)) -> 
  GS (repr (Statement repr))
decrement1 v = v &= (S.valueOf v #- S.litInt 1)

varDec :: (RenderSym repr) => repr (Permanence repr) -> repr (Permanence repr) 
  -> GS (repr (Variable repr)) -> GS (repr (Statement repr))
varDec s d = onStateValue (\v -> stateFromData (permDoc (bind $ variableBind v) 
  <+> getTypeDoc (variableType v) <+> variableDoc v) Semi)
  where bind Static = s
        bind Dynamic = d

varDecDef :: (RenderSym repr) => GS (repr (Variable repr)) -> 
  GS (repr (Value repr)) -> GS (repr (Statement repr))
varDecDef vr = on2StateValues (\vd vl -> stateFromData (statementDoc vd <+> 
  equals <+> valueDoc vl) Semi) (S.varDec vr)

listDec :: (RenderSym repr) => (repr (Value repr) -> Doc) -> 
  GS (repr (Value repr)) -> GS (repr (Variable repr)) -> 
  GS (repr (Statement repr))
listDec f vl v = on2StateValues (\sz vd -> stateFromData (statementDoc vd <> f 
  sz) Semi) vl (S.varDec v)

listDecDef :: (RenderSym repr) => ([repr (Value repr)] -> Doc) -> 
  GS (repr (Variable repr)) -> [GS (repr (Value repr))] -> 
  GS (repr (Statement repr))
listDecDef f v = on1StateValue1List (\vd vs -> stateFromData (statementDoc vd 
  <> f vs) Semi) (S.varDec v)

listDecDef' :: (RenderSym repr) => (repr (Variable repr)-> [repr (Value repr)] 
  -> Doc) -> GS (repr (Variable repr)) -> [GS (repr (Value repr))] -> 
  GS (repr (Statement repr))
listDecDef' f v vals = on3StateValues (\vd vr vs -> stateFromData (statementDoc
  vd <> f vr vs) Semi) (S.varDec v) v (sequence vals)

objDecNew :: (RenderSym repr) => GS (repr (Variable repr)) -> 
  [GS (repr (Value repr))] -> GS (repr (Statement repr))
objDecNew v vs = S.varDecDef v (S.newObj (onStateValue variableType v) vs)

objDecNewNoParams :: (RenderSym repr) => GS (repr (Variable repr)) -> 
  GS (repr (Statement repr))
objDecNewNoParams v = S.objDecNew v []

constDecDef :: (RenderSym repr) => GS (repr (Variable repr)) -> 
  GS (repr (Value repr)) -> GS (repr (Statement repr))
constDecDef = on2StateValues (\v def -> stateFromData (constDecDefDocD v def) 
  Semi)

discardInput :: (RenderSym repr) => (repr (Value repr) -> Doc) ->
  GS (repr (Statement repr))
discardInput f = onStateValue (\infn -> stateFromData (f infn) Semi) inputFunc

discardFileInput :: (RenderSym repr) => (repr (Value repr) -> Doc) -> 
  GS (repr (Value repr)) -> GS (repr (Statement repr))
discardFileInput f = onStateValue (\v -> stateFromData (f v) Semi)

openFileR :: (RenderSym repr) => (GS (repr (Value repr)) -> 
  GS (repr (Type repr)) -> GS (repr (Value repr))) -> GS (repr (Variable repr)) 
  -> GS (repr (Value repr)) -> GS (repr (Statement repr))
openFileR f vr vl = vr &= f vl infile

openFileW :: (RenderSym repr) => (GS (repr (Value repr)) -> 
  GS (repr (Type repr)) -> GS (repr (Value repr)) -> GS (repr (Value repr))) -> 
  GS (repr (Variable repr)) -> GS (repr (Value repr)) ->
  GS (repr (Statement repr))
openFileW f vr vl = vr &= f vl outfile S.litFalse

openFileA :: (RenderSym repr) => (GS (repr (Value repr)) -> 
  GS (repr (Type repr)) -> GS (repr (Value repr)) -> GS (repr (Value repr))) -> 
  GS (repr (Variable repr)) -> GS (repr (Value repr)) -> 
  GS (repr (Statement repr))
openFileA f vr vl = vr &= f vl outfile S.litTrue

closeFile :: (RenderSym repr) => Label -> GS (repr (Value repr)) -> 
  GS (repr (Statement repr))
closeFile n f = S.valState $ S.objMethodCallNoParams S.void f n

discardFileLine :: (RenderSym repr) => Label -> GS (repr (Value repr)) -> 
  GS (repr (Statement repr))
discardFileLine n f = S.valState $ S.objMethodCallNoParams S.string f n 

stringListVals :: (RenderSym repr) => [GS (repr (Variable repr))] -> 
  GS (repr (Value repr)) -> GS (repr (Statement repr))
stringListVals vars sl = sl >>= (\slst -> multi $ checkList (getType $ 
  valueType slst))
  where checkList (List String) = assignVals vars 0
        checkList _ = error 
          "Value passed to stringListVals must be a list of strings"
        assignVals [] _ = []
        assignVals (v:vs) n = S.assign v (cast (onStateValue variableType v) 
          (S.listAccess sl (S.litInt n))) : assignVals vs (n+1)

stringListLists :: (RenderSym repr) => [GS (repr (Variable repr))] -> 
  GS (repr (Value repr)) -> GS (repr (Statement repr))
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

returnState :: (RenderSym repr) => Terminator -> GS (repr (Value repr)) -> 
  GS (repr (Statement repr))
returnState t = onStateValue (\v -> stateFromData (returnDocD [v]) t)

multiReturnError :: String -> String
multiReturnError l = "Cannot return multiple values in " ++ l

valState :: (RenderSym repr) => Terminator -> GS (repr (Value repr)) ->
  GS (repr (Statement repr))
valState t = onStateValue (\v -> stateFromData (valueDoc v) t)

comment :: (RenderSym repr) => repr (Keyword repr) -> Label -> 
  GS (repr (Statement repr))
comment cs c = toState $ stateFromData (commentDocD c (keyDoc cs)) Empty

freeError :: String -> String
freeError l = "Cannot free variables in " ++ l

throw :: (RenderSym repr) => (repr (Value repr) -> Doc) -> Terminator -> 
  Label -> GS (repr (Statement repr))
throw f t = onStateValue (\msg -> stateFromData (f msg) t) . S.litString

initState :: (RenderSym repr) => Label -> Label -> GS (repr (Statement repr))
initState fsmName initialState = S.varDecDef (S.var fsmName S.string) 
  (S.litString initialState)

changeState :: (RenderSym repr) => Label -> Label -> GS (repr (Statement repr))
changeState fsmName tState = S.var fsmName S.string &= S.litString tState

initObserverList :: (RenderSym repr) => GS (repr (Type repr)) -> 
  [GS (repr (Value repr))] -> GS (repr (Statement repr))
initObserverList t = S.listDecDef (S.var observerListName (S.listType static_ t))

addObserver :: (RenderSym repr) => GS (repr (Value repr)) -> 
  GS (repr (Statement repr))
addObserver o = S.valState $ S.listAdd obsList lastelem o
  where obsList = S.valueOf $ observerListName `S.listOf` onStateValue 
          valueType o
        lastelem = S.listSize obsList

ifCond :: (RenderSym repr) => repr (Keyword repr) -> repr (Keyword repr) -> 
  repr (Keyword repr) -> [(GS (repr (Value repr)), GS (repr (Body repr)))] -> 
  GS (repr (Body repr)) -> GS (repr (Statement repr))
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

ifNoElse :: (RenderSym repr) => [(GS (repr (Value repr)), 
  GS (repr (Body repr)))] -> GS (repr (Statement repr))
ifNoElse bs = S.ifCond bs $ body []

switch :: (RenderSym repr) => GS (repr (Value repr)) -> 
  [(GS (repr (Value repr)), GS (repr (Body repr)))] -> GS (repr (Body repr)) -> 
  GS (repr (Statement repr))
switch v cs = on4StateValues (\b val css de -> stateFromData (switchDocD b val 
  de css) Semi) (S.state break) v (on2StateLists zip (map fst cs) (map snd cs))

switchAsIf :: (RenderSym repr) => GS (repr (Value repr)) -> 
  [(GS (repr (Value repr)), GS (repr (Body repr)))] -> GS (repr (Body repr)) -> 
  GS (repr (Statement repr))
switchAsIf v cs = S.ifCond cases
  where cases = map (first (v ?==)) cs

ifExists :: (RenderSym repr) => GS (repr (Value repr)) -> GS (repr (Body repr)) 
  -> GS (repr (Body repr)) -> GS (repr (Statement repr))
ifExists v ifBody = S.ifCond [(S.notNull v, ifBody)]

for :: (RenderSym repr) => repr (Keyword repr) -> repr (Keyword repr) -> 
  GS (repr (Statement repr)) -> GS (repr (Value repr)) -> 
  GS (repr (Statement repr)) -> GS (repr (Body repr)) -> 
  GS (repr (Statement repr))
for bStart bEnd sInit vGuard sUpdate = on4StateValues (\initl guard upd bod -> 
  stateFromData (vcat [
  forLabel <+> parens (statementDoc initl <> semi <+> valueDoc guard <> semi 
    <+> statementDoc upd) <+> keyDoc bStart,
  indent $ bodyDoc bod,
  keyDoc bEnd]) Empty) (S.loopState sInit) vGuard (S.loopState sUpdate)

forRange :: (RenderSym repr) => GS (repr (Variable repr)) -> 
  GS (repr (Value repr)) -> GS (repr (Value repr)) -> GS (repr (Value repr)) -> 
  GS (repr (Body repr)) -> GS (repr (Statement repr))
forRange i initv finalv stepv = S.for (S.varDecDef i initv) (S.valueOf i ?< 
  finalv) (i &+= stepv)

forEach :: (RenderSym repr) => repr (Keyword repr) -> repr (Keyword repr) -> 
  repr (Keyword repr) -> repr (Keyword repr) -> GS (repr (Variable repr)) -> 
  GS (repr (Value repr)) -> GS (repr (Body repr)) -> GS (repr (Statement repr))
forEach bStart bEnd forEachLabel inLbl = on3StateValues (\e v b -> stateFromData
  (vcat [keyDoc forEachLabel <+> parens (getTypeDoc (variableType e) <+> 
    variableDoc e <+> keyDoc inLbl <+> valueDoc v) <+> keyDoc bStart,
  indent $ bodyDoc b,
  keyDoc bEnd]) Empty) 

while :: (RenderSym repr) => repr (Keyword repr) -> repr (Keyword repr) -> 
  GS (repr (Value repr)) -> GS (repr (Body repr)) -> GS (repr (Statement repr))
while bStart bEnd = on2StateValues (\v b -> stateFromData (vcat [
  text "while" <+> parens (valueDoc v) <+> keyDoc bStart,
  indent $ bodyDoc b,
  keyDoc bEnd]) Empty)

tryCatch :: (RenderSym repr) => (repr (Body repr) -> repr (Body repr) -> Doc) ->
  GS (repr (Body repr)) -> GS (repr (Body repr)) -> GS (repr (Statement repr))
tryCatch f = on2StateValues (\tb cb -> stateFromData (f tb cb) Empty)

checkState :: (RenderSym repr) => Label -> [(GS (repr (Value repr)), 
  GS (repr (Body repr)))] -> GS (repr (Body repr)) -> GS (repr (Statement repr))
checkState l = S.switch (S.valueOf $ S.var l S.string)

notifyObservers :: (RenderSym repr) => GS (repr (Function repr)) -> 
  GS (repr (Type repr)) -> GS (repr (Statement repr))
notifyObservers f t = S.for initv (v_index ?< S.listSize obsList) 
  (var_index &++) notify
  where obsList = S.valueOf $ observerListName `S.listOf` t 
        var_index = S.var "observerIndex" S.int
        v_index = S.valueOf var_index
        initv = S.varDecDef var_index $ S.litInt 0
        notify = S.oneLiner $ S.valState $ at obsList v_index $. f

construct :: (RenderSym repr) => Label -> GS (repr (Type repr))
construct n = toState $ typeFromData (Object n) n empty

param :: (RenderSym repr) => (repr (Variable repr) -> Doc) -> 
  GS (repr (Variable repr)) -> MS (repr (Parameter repr))
param f = getPutReturnFunc (\s v -> addParameter (variableName v) s) 
  (\v -> paramFromData v (f v)) . zoom lensMStoGS

method :: (RenderSym repr) => Label -> Label -> repr (Scope repr) -> 
  repr (Permanence repr) -> GS (repr (Type repr)) -> 
  [MS (repr (Parameter repr))] -> GS (repr (Body repr)) -> 
  MS (repr (Method repr))
method n c s p t = intMethod False n c s p (mType t)

getMethod :: (RenderSym repr) => Label -> GS (repr (Variable repr)) -> 
  MS (repr (Method repr))
getMethod c v = zoom lensMStoGS v >>= (\vr -> S.method (getterName $ 
    variableName vr) c public dynamic_ (toState $ variableType vr) [] getBody)
    where getBody = S.oneLiner $ S.returnState (S.valueOf $ S.objVarSelf c v)

setMethod :: (RenderSym repr) => Label -> GS (repr (Variable repr)) -> 
  MS (repr (Method repr))
setMethod c v = zoom lensMStoGS v >>= (\vr -> S.method (setterName $ 
  variableName vr) c public dynamic_ S.void [S.param v] setBody)
  where setBody = S.oneLiner $ S.objVarSelf c v &= S.valueOf v

privMethod :: (RenderSym repr) => Label -> Label -> GS (repr (Type repr)) -> 
  [MS (repr (Parameter repr))] -> GS (repr (Body repr)) -> 
  MS (repr (Method repr))
privMethod n c = S.method n c private dynamic_

pubMethod :: (RenderSym repr) => Label -> Label -> GS (repr (Type repr)) -> 
  [MS (repr (Parameter repr))] -> GS (repr (Body repr)) -> 
  MS (repr (Method repr))
pubMethod n c = S.method n c public dynamic_

constructor :: (RenderSym repr) => Label -> Label -> 
  [MS (repr (Parameter repr))] -> GS (repr (Body repr)) -> 
  MS (repr (Method repr))
constructor fName n = intMethod False fName n public dynamic_ (S.construct n)

docMain :: (RenderSym repr) => GS (repr (Body repr)) -> MS (repr (Method repr))
docMain b = commentedFunc (docComment $ toState $ functionDox 
  "Controls the flow of the program" 
  [("args", "List of command-line arguments")] []) (S.mainFunction b)

function :: (RenderSym repr) => Label -> repr (Scope repr) -> 
  repr (Permanence repr) -> GS (repr (Type repr)) -> 
  [MS (repr (Parameter repr))] -> GS (repr (Body repr)) -> 
  MS (repr (Method repr))
function n s p t = S.intFunc False n s p (mType t)

mainFunction :: (RenderSym repr) => GS (repr (Type repr)) -> Label -> 
  GS (repr (Body repr)) -> MS (repr (Method repr))
mainFunction s n = S.intFunc True n public static_ (mType S.void)
  [S.param (S.var "args" (onStateValue (\argT -> typeFromData (List String) 
  (render (getTypeDoc argT) ++ "[]") (getTypeDoc argT <> text "[]")) s))]

docFunc :: (RenderSym repr) => String -> [String] -> Maybe String -> 
  MS (repr (Method repr)) -> MS (repr (Method repr))
docFunc desc pComms rComm = docFuncRepr desc pComms (maybeToList rComm)

docInOutFunc :: (RenderSym repr) => (repr (Scope repr) -> repr (Permanence repr)
    -> [GS (repr (Variable repr))] -> [GS (repr (Variable repr))] -> 
    [GS (repr (Variable repr))] -> GS (repr (Body repr)) -> 
    MS (repr (Method repr)))
  -> repr (Scope repr) -> repr (Permanence repr) -> String -> 
  [(String, GS (repr (Variable repr)))] -> [(String, GS (repr (Variable repr)))]
  -> [(String, GS (repr (Variable repr)))] -> GS (repr (Body repr)) -> 
  MS (repr (Method repr))
docInOutFunc f s p desc is [o] [] b = docFuncRepr desc (map fst is) [fst o] 
  (f s p (map snd is) [snd o] [] b)
docInOutFunc f s p desc is [] [both] b = zoom lensMStoGS (snd both) >>= (\bth 
  -> docFuncRepr desc (map fst $ both : is) [fst both | not ((isObject . 
  getType . variableType) bth)] (f s p (map snd is) [] [toState bth] b))
docInOutFunc f s p desc is os bs b = docFuncRepr desc (map fst $ bs ++ is ++ os)
  [] (f s p (map snd is) (map snd os) (map snd bs) b)

intFunc :: (RenderSym repr) => Bool -> Label -> repr (Scope repr) -> 
  repr (Permanence repr) -> GS (repr (MethodType repr)) -> 
  [MS (repr (Parameter repr))] -> GS (repr (Body repr)) -> 
  MS (repr (Method repr))
intFunc m n = intMethod m n ""

stateVar :: (RenderSym repr) => repr (Scope repr) -> repr (Permanence repr) ->
  GS (repr (Variable repr)) -> GS (repr (StateVar repr))
stateVar s p v = stateVarFromData (onStateValue (stateVarDocD 
  (scopeDoc s) (permDoc p) . statementDoc) (S.state $ S.varDec v))

stateVarDef :: (RenderSym repr) => repr (Scope repr) -> repr (Permanence repr) 
  -> GS (repr (Variable repr)) -> GS (repr (Value repr)) -> 
  GS (repr (StateVar repr))
stateVarDef s p vr vl = stateVarFromData (onStateValue (stateVarDocD 
  (scopeDoc s) (permDoc p) . statementDoc) (S.state $ S.varDecDef vr vl))

constVar :: (RenderSym repr) => Doc -> repr (Scope repr) ->
  GS (repr (Variable repr)) -> GS (repr (Value repr)) -> GS (repr (StateVar repr))
constVar p s vr vl = stateVarFromData (onStateValue (stateVarDocD (scopeDoc s) 
  p . statementDoc) (S.state $ S.constDecDef vr vl))

privMVar :: (RenderSym repr) => GS (repr (Variable repr)) -> 
  GS (repr (StateVar repr))
privMVar = S.stateVar private dynamic_

pubMVar :: (RenderSym repr) => GS (repr (Variable repr)) -> 
  GS (repr (StateVar repr))
pubMVar = S.stateVar public dynamic_

pubGVar :: (RenderSym repr) => GS (repr (Variable repr)) -> 
  GS (repr (StateVar repr))
pubGVar = S.stateVar public static_

buildClass :: (RenderSym repr) => (Label -> Doc -> Doc -> Doc -> Doc -> Doc) -> 
  (Label -> repr (Keyword repr)) -> Label -> Maybe Label -> repr (Scope repr) 
  -> [GS (repr (StateVar repr))] -> 
  [MS (repr (Method repr))] -> FS (repr (Class repr))
buildClass f i n p s vs fs = classFromData (on2StateValues (f n parent 
  (scopeDoc s)) (onStateList (stateVarListDocD . map stateVarDoc) 
  (map (zoom lensFStoGS) vs)) (onStateList (methodListDocD . map methodDoc) 
  (map (zoom lensFStoMS) fs)))
  where parent = case p of Nothing -> empty
                           Just pn -> keyDoc $ i pn

enum :: (RenderSym repr) => Label -> [Label] -> repr (Scope repr) -> 
  FS (repr (Class repr))
enum n es s = classFromData (toState $ enumDocD n (enumElementsDocD es False) 
  (scopeDoc s))

privClass :: (RenderSym repr) => Label -> Maybe Label -> 
  [GS (repr (StateVar repr))] -> 
  [MS (repr (Method repr))] -> FS (repr (Class repr))
privClass n p = S.buildClass n p private

pubClass :: (RenderSym repr) => Label -> Maybe Label -> 
  [GS (repr (StateVar repr))] -> [MS (repr (Method repr))] -> 
  FS (repr (Class repr))
pubClass n p = S.buildClass n p public

docClass :: (RenderSym repr) => String -> FS (repr (Class repr))
  -> FS (repr (Class repr))
docClass d = S.commentedClass (docComment $ toState $ classDox d)

commentedClass :: (RenderSym repr) => FS (repr (BlockComment repr))
  -> FS (repr (Class repr)) -> FS (repr (Class repr))
commentedClass cmt cs = classFromData (on2StateValues (\cmt' cs' -> 
  commentedItem (blockCommentDoc cmt') (classDoc cs')) cmt cs)

buildModule :: (RenderSym repr) => Label -> [repr (Keyword repr)] -> 
  [MS (repr (Method repr))] -> [FS (repr (Class repr))] -> 
  FS (repr (Module repr))
buildModule n ls ms cs = S.modFromData n getCurrMain (on2StateValues 
  (moduleDocD (vcat $ map keyDoc ls)) (onStateList (vibcat . map classDoc) cs) 
  (onStateList (methodListDocD . map methodDoc) (map (zoom lensFStoMS) ms)))

buildModule' :: (RenderSym repr) => Label -> [MS (repr (Method repr))] -> 
  [FS (repr (Class repr))] -> FS (repr (Module repr))
buildModule' n ms cs = S.modFromData n getCurrMain (onStateList (vibcat . map 
  classDoc) (if null ms then cs else pubClass n Nothing [] ms : cs))

modFromData :: Label -> (Doc -> Bool -> repr (Module repr)) -> FS Bool -> 
  FS Doc -> FS (repr (Module repr))
modFromData n f m d = putAfter (setModuleName n) (on2StateValues f d m)

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

setEmpty :: (RenderSym repr) => GS (repr (Statement repr)) -> 
  GS (repr (Statement repr))
setEmpty = onStateValue (\s -> stateFromData (statementDoc s) Empty)