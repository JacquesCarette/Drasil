{-# LANGUAGE PostfixOperators #-}

-- | The structure for a class of renderers is defined here.
module GOOL.Drasil.LanguageRenderer.LanguagePolymorphic (fileFromData, oneLiner,
  multiBody, block, multiBlock, bool, int, float, double, char, string, 
  fileType, listType, arrayType, listInnerType, obj, funcType, void, 
  runStrategy, listSlice, unOpPrec, notOp, notOp', negateOp, sqrtOp, sqrtOp', 
  absOp, absOp', expOp, expOp', sinOp, sinOp', cosOp, cosOp', tanOp, tanOp', 
  asinOp, asinOp', acosOp, acosOp', atanOp, atanOp', csc, sec, cot, unExpr, 
  unExpr', unExprNumDbl, typeUnExpr, powerPrec, multPrec, andPrec, orPrec, 
  equalOp, notEqualOp, greaterOp, greaterEqualOp, lessOp, lessEqualOp, plusOp, 
  minusOp, multOp, divideOp, moduloOp, powerOp, andOp, orOp, binExpr, binExpr', 
  binExprNumDbl', typeBinExpr, addmathImport, var, staticVar, extVar, self, 
  classVar, objVar, objVarSelf, listVar, listOf, arrayElem, iterVar, 
  litTrue, litFalse, litChar, litDouble, litFloat, litInt, litString, litArray, 
  litList, pi, valueOf, arg, argsList, inlineIf, call', call, 
  funcAppMixedArgs, namedArgError, selfFuncAppMixedArgs, 
  extFuncAppMixedArgs, libFuncAppMixedArgs, 
  newObjMixedArgs, extNewObjMixedArgs, 
  libNewObjMixedArgs, lambda, notNull, objAccess, objMethodCall, 
  objMethodCallNoParams, indexOf, func, get, set, 
  listSize, listAdd, listAppend, iterBegin, iterEnd, listAccess, listSet, 
  getFunc, setFunc, listSizeFunc, listAddFunc, listAppendFunc, iterBeginError, 
  iterEndError, listAccessFunc, listAccessFunc', listSetFunc, printSt, state, 
  loopState, emptyState, assign, multiAssignError, 
  decrement, increment, increment', increment1, increment1', decrement1, 
  varDec, varDecDef, listDec, listDecDef, listDecDef', arrayDec, arrayDecDef, 
  objDecNew, objDecNewNoParams, extObjDecNew, extObjDecNewNoParams, 
  constDecDef, funcDecDef, discardInput, discardFileInput, openFileR, 
  openFileW, openFileA, closeFile, discardFileLine, stringListVals, 
  stringListLists, returnState, multiReturnError, valState, comment, freeError, 
  throw, ifCond, switch, ifExists, for, forRange, forEach, while, 
  tryCatch, checkState, notifyObservers, construct, param, method, getMethod, 
  setMethod, constructor, docMain, function, mainFunction, docFunc, 
  docInOutFunc, intFunc, stateVar, stateVarDef, constVar, buildClass, 
  extraClass, implementingClass, docClass, commentedClass, intClass, 
  buildModule, buildModule', modFromData, fileDoc, docMod
) where

import Utils.Drasil (indent)

import GOOL.Drasil.CodeType (CodeType(..), ClassName)
import GOOL.Drasil.ClassInterface (Label, Library, 
  FileSym(RenderFile, commentedMod), BodySym(Body, bodyStatements), 
  BlockSym(Block), PermanenceSym(..), 
  TypeSym(Type, infile, outfile, iterator, getType, getTypeDoc, getTypeString), 
  VariableSym(Variable, variableName, variableType), 
  ValueSym(Value, valueDoc, valueType), 
  NumericExpression((#+), (#-), (#*), (#/), sin, cos, tan), 
  BooleanExpression(..), funcApp, newObj, extNewObj, ($.), FunctionSym(Function), at, 
  StatementSym(Statement, (&+=), (&++), break, multi), (&=), observerListName, ScopeSym(..),
  ParameterSym(Parameter), MethodSym(Method), StateVarSym(StateVar), 
  ClassSym(Class), ModuleSym(Module), BlockComment(..), convType)
import qualified GOOL.Drasil.ClassInterface as S (
  BodySym(oneLiner), BlockSym(block), 
  TypeSym(bool, int, float, double, char, string, listType, arrayType, 
    listInnerType, void), 
  VariableSym(var, self, objVar, objVarSelf, listVar, listOf),
  ValueSym(litTrue, litFalse, litInt, litString, litList, valueOf),
  ValueExpression(funcAppMixedArgs, newObjMixedArgs, notNull, lambda), 
  Selector(objAccess), objMethodCall, objMethodCallNoParams, 
  FunctionSym(func, listSize, listAppend), SelectorFunction(listAccess),
  StatementSym(assign, varDec, varDecDef, listDec, objDecNew, extObjDecNew, 
    constDecDef, valState, returnState),
  ControlStatementSym(ifCond, for, forRange, switch),
  ParameterSym(param), MethodSym(method, mainFunction),
  ClassSym(buildClass, commentedClass))
import GOOL.Drasil.RendererClasses (KeywordSym(..), RenderSym, 
  InternalBody(bodyDoc, docBody), InternalBlock(docBlock, blockDoc), 
  ImportSym(..), InternalPerm(..), InternalType(..), UnaryOpSym(UnaryOp), 
  BinaryOpSym(BinaryOp), InternalOp(..), 
  InternalVariable(variableBind, variableDoc, varFromData), 
  InternalValue(inputFunc, cast, valuePrec, valFromData),
  InternalFunction(iterBeginFunc, iterEndFunc, functionDoc, functionType, 
    funcFromData),
  InternalStatement(statementDoc, statementTerm, stateFromData), 
  InternalScope(..), MethodTypeSym(MethodType, mType), 
  InternalParam(paramFromData), 
  InternalMethod(intMethod, commentedFunc, methodDoc), InternalStateVar(..), 
  InternalClass(classDoc, classFromData), 
  InternalMod(moduleDoc, updateModuleDoc))
import qualified GOOL.Drasil.RendererClasses as S (InternalFile(fileFromData), 
  InternalValue(call), InternalFunction(getFunc, setFunc, listSizeFunc, 
    listAddFunc, listAppendFunc, listAccessFunc, listSetFunc),
  InternalStatement(state, loopState, emptyState), MethodTypeSym(construct), 
  InternalMethod(intFunc), InternalClass(intClass), InternalMod(modFromData))
import GOOL.Drasil.AST (Binding(..), ScopeTag(..), Terminator(..), isSource)
import GOOL.Drasil.Helpers (angles, doubleQuotedText, vibcat, emptyIfEmpty, 
  toState, onStateValue, on2StateValues, on3StateValues, onStateList, 
  on2StateLists, on1StateValue1List, getInnerType)
import GOOL.Drasil.LanguageRenderer (dot, forLabel, new, 
  addExt, moduleDocD, blockDocD, assignDocD, plusEqualsDocD, plusPlusDocD, 
  mkSt, mkStNoEnd, mkStateVal, mkVal, mkStateVar, mkVar, mkStaticVar, varDocD, 
  extVarDocD, selfDocD, argDocD, classVarCheckStatic, objVarDocD, 
  objAccessDocD, funcDocD, listAccessFuncDocD, constDecDefDocD, printDoc, 
  returnDocD, getTermDoc, switchDocD, stateVarDocD, stateVarListDocD, 
  fileDoc', docFuncRepr, commentDocD, commentedItem,
  functionDox, classDox, moduleDox, getterName, setterName, valueList, intValue)
import GOOL.Drasil.State (FS, CS, MS, VS, lensFStoGS, lensFStoCS, lensFStoMS, 
  lensCStoMS, lensMStoVS, lensVStoMS, currMain, currFileType, modifyReturnFunc, 
  modifyReturnFunc2, addFile, setMainMod, addLangImportVS, getLangImports, 
  addLibImportVS, getLibImports, getModuleImports, setModuleName, 
  getModuleName, setClassName, getClassName, addParameter)

import Prelude hiding (break,print,last,mod,pi,sin,cos,tan,(<>))
import Data.List (sort, intersperse)
import Data.Map as Map (lookup, fromList)
import Data.Maybe (fromMaybe, maybeToList)
import Control.Applicative ((<|>))
import Control.Monad (join)
import Control.Monad.State (modify)
import Control.Lens ((^.), over)
import Control.Lens.Zoom (zoom)
import Text.PrettyPrint.HughesPJ (Doc, text, empty, render, (<>), (<+>), parens,
  brackets, braces, quotes, integer, hcat, vcat, semi, comma, equals, isEmpty)
import qualified Text.PrettyPrint.HughesPJ as D (char, double, float)

-- Bodies --

oneLiner :: (RenderSym repr) => MS (repr (Statement repr)) -> 
  MS (repr (Body repr))
oneLiner s = bodyStatements [s]

multiBody :: (RenderSym repr) => [MS (repr (Body repr))] -> 
  MS (repr (Body repr))
multiBody bs = docBody $ onStateList vibcat $ map (onStateValue bodyDoc) bs

-- Blocks --

block :: (RenderSym repr) => repr (Keyword repr) -> [MS (repr (Statement repr))]
  -> MS (repr (Block repr))
block end sts = docBlock $ onStateList (blockDocD (keyDoc end) . map 
  statementDoc) (map S.state sts)

multiBlock :: (RenderSym repr) => [MS (repr (Block repr))] -> 
  MS (repr (Block repr))
multiBlock bs = docBlock $ onStateList vibcat $ map (onStateValue blockDoc) bs

-- Types --

bool :: (RenderSym repr) => VS (repr (Type repr))
bool = toState $ typeFromData Boolean "Boolean" (text "Boolean")

int :: (RenderSym repr) => VS (repr (Type repr))
int = toState $ typeFromData Integer "int" (text "int")

float :: (RenderSym repr) => VS (repr (Type repr))
float = toState $ typeFromData Float "float" (text "float")

double :: (RenderSym repr) => VS (repr (Type repr))
double = toState $ typeFromData Double "double" (text "double")

char :: (RenderSym repr) => VS (repr (Type repr))
char = toState $ typeFromData Char "char" (text "char")

string :: (RenderSym repr) => VS (repr (Type repr))
string = toState $ typeFromData String "string" (text "string")

fileType :: (RenderSym repr) => VS (repr (Type repr))
fileType = toState $ typeFromData File "File" (text "File")

listType :: (RenderSym repr) => repr (Keyword repr) -> VS (repr (Type repr)) -> 
  VS (repr (Type repr))
listType lst = onStateValue (\t -> typeFromData (List (getType t)) (render 
  (keyDoc lst) ++ "<" ++ getTypeString t ++ ">") (keyDoc lst <> 
  angles (getTypeDoc t)))

arrayType :: (RenderSym repr) => VS (repr (Type repr)) -> VS (repr (Type repr))
arrayType = onStateValue (\t -> typeFromData (Array (getType t)) 
  (getTypeString t ++ "[]") (getTypeDoc t <> brackets empty)) 

listInnerType :: (RenderSym repr) => VS (repr (Type repr)) -> 
  VS (repr (Type repr))
listInnerType t = t >>= (convType . getInnerType . getType)

obj :: (RenderSym repr) => ClassName -> VS (repr (Type repr))
obj n = toState $ typeFromData (Object n) n (text n)

-- enumType :: (RenderSym repr) => Label -> VS (repr (Type repr))
-- enumType e = toState $ typeFromData (Enum e) e (text e)

funcType :: (RenderSym repr) => [VS (repr (Type repr))] -> VS (repr (Type repr))
  -> VS (repr (Type repr))
funcType ps' = on2StateValues (\ps r -> typeFromData (Func (map getType ps) 
  (getType r)) "" empty) (sequence ps')

void :: (RenderSym repr) => VS (repr (Type repr))
void = toState $ typeFromData Void "void" (text "void")

-- ControlBlock --

strat :: (RenderSym repr) => MS (repr (Statement repr)) -> MS (repr (Body repr))
  -> MS (repr (Block repr))
strat r bd = docBlock $ on2StateValues (\result b -> vcat [bodyDoc b, 
  statementDoc result]) r bd

runStrategy :: (RenderSym repr) => String -> [(Label, MS (repr (Body repr)))] 
  -> Maybe (VS (repr (Value repr))) -> Maybe (VS (repr (Variable repr))) -> 
  MS (repr (Block repr))
runStrategy l strats rv av = maybe
  (strError l "RunStrategy called on non-existent strategy") 
  (strat (S.state resultState)) (Map.lookup l (Map.fromList strats))
  where resultState = maybe S.emptyState asgState av
        asgState v = maybe (strError l 
          "Attempt to assign null return to a Value") (v &=) rv
        strError n s = error $ "Strategy '" ++ n ++ "': " ++ s ++ "."

listSlice :: (RenderSym repr) => Maybe (VS (repr (Value repr))) -> 
  Maybe (VS (repr (Value repr))) -> Maybe (VS (repr (Value repr))) -> 
  VS (repr (Variable repr)) -> VS (repr (Value repr)) -> MS (repr (Block repr))
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

unOpPrec :: (RenderSym repr) => String -> VS (repr (UnaryOp repr))
unOpPrec = uOpFromData 9 . text

notOp :: (RenderSym repr) => VS (repr (UnaryOp repr))
notOp = unOpPrec "!"

notOp' :: (RenderSym repr) => VS (repr (UnaryOp repr))
notOp' = unOpPrec "not"

negateOp :: (RenderSym repr) => VS (repr (UnaryOp repr))
negateOp = unOpPrec "-"

sqrtOp :: (RenderSym repr) => VS (repr (UnaryOp repr))
sqrtOp = unOpPrec "sqrt"

sqrtOp' :: (RenderSym repr) => VS (repr (UnaryOp repr))
sqrtOp' = addmathImport $ unOpPrec "math.sqrt"

absOp :: (RenderSym repr) => VS (repr (UnaryOp repr))
absOp = unOpPrec "fabs"

absOp' :: (RenderSym repr) => VS (repr (UnaryOp repr))
absOp' = addmathImport $ unOpPrec "math.fabs"

expOp :: (RenderSym repr) => VS (repr (UnaryOp repr))
expOp = unOpPrec "exp"

expOp' :: (RenderSym repr) => VS (repr (UnaryOp repr))
expOp' = addmathImport $ unOpPrec "math.exp"

sinOp :: (RenderSym repr) => VS (repr (UnaryOp repr))
sinOp = unOpPrec "sin"

sinOp' :: (RenderSym repr) => VS (repr (UnaryOp repr))
sinOp' = addmathImport $ unOpPrec "math.sin"

cosOp :: (RenderSym repr) => VS (repr (UnaryOp repr))
cosOp = unOpPrec "cos"

cosOp' :: (RenderSym repr) => VS (repr (UnaryOp repr))
cosOp' = addmathImport $ unOpPrec "math.cos"

tanOp :: (RenderSym repr) => VS (repr (UnaryOp repr))
tanOp = unOpPrec "tan"

tanOp' :: (RenderSym repr) => VS (repr (UnaryOp repr))
tanOp' = addmathImport $ unOpPrec "math.tan"

asinOp :: (RenderSym repr) => VS (repr (UnaryOp repr))
asinOp = unOpPrec "asin"

asinOp' :: (RenderSym repr) => VS (repr (UnaryOp repr))
asinOp' = addmathImport $ unOpPrec "math.asin"

acosOp :: (RenderSym repr) => VS (repr (UnaryOp repr))
acosOp = unOpPrec "acos"

acosOp' :: (RenderSym repr) => VS (repr (UnaryOp repr))
acosOp' = addmathImport $ unOpPrec "math.acos"

atanOp :: (RenderSym repr) => VS (repr (UnaryOp repr))
atanOp = unOpPrec "atan"

atanOp' :: (RenderSym repr) => VS (repr (UnaryOp repr))
atanOp' = addmathImport $ unOpPrec "math.atan"

csc :: (RenderSym repr) => VS (repr (Value repr)) -> VS (repr (Value repr))
csc v = valOfOne (fmap valueType v) #/ sin v

sec :: (RenderSym repr) => VS (repr (Value repr)) -> VS (repr (Value repr))
sec v = valOfOne (fmap valueType v) #/ cos v

cot :: (RenderSym repr) => VS (repr (Value repr)) -> VS (repr (Value repr))
cot v = valOfOne (fmap valueType v) #/ tan v

valOfOne :: (RenderSym repr) => VS (repr (Type repr)) -> VS (repr (Value repr))
valOfOne t = t >>= (getVal . getType)
  where getVal Float = litFloat 1.0
        getVal _ = litDouble 1.0

unOpDocD :: Doc -> Doc -> Doc
unOpDocD op v = op <> parens v

unOpDocD' :: Doc -> Doc -> Doc
unOpDocD' op v = op <> v

unExpr :: (RenderSym repr) => VS (repr (UnaryOp repr)) -> VS (repr (Value repr))
  -> VS (repr (Value repr))
unExpr = on2StateValues (mkUnExpr unOpDocD)

unExpr' :: (RenderSym repr) => VS (repr (UnaryOp repr)) -> 
  VS (repr (Value repr)) -> VS (repr (Value repr))
unExpr' = on2StateValues (mkUnExpr unOpDocD')

mkUnExpr :: (RenderSym repr) => (Doc -> Doc -> Doc) -> repr (UnaryOp repr) -> 
  repr (Value repr) -> repr (Value repr)
mkUnExpr d u v = mkExpr (uOpPrec u) (valueType v) (d (uOpDoc u) (valueDoc v))

unExprNumDbl :: (RenderSym repr) => VS (repr (UnaryOp repr)) -> 
  VS (repr (Value repr)) -> VS (repr (Value repr))
unExprNumDbl u' v' = u' >>= (\u -> v' >>= (\v -> 
  unExprCastFloat (valueType v) $ return $ mkUnExpr unOpDocD u v))

unExprCastFloat :: (RenderSym repr) => repr (Type repr) -> 
  (VS (repr (Value repr)) -> VS (repr (Value repr)))
unExprCastFloat t = castType $ getType t
  where castType Float = cast float
        castType _ = id
  
typeUnExpr :: (RenderSym repr) => VS (repr (UnaryOp repr)) -> 
  VS (repr (Type repr)) -> VS (repr (Value repr)) -> VS (repr (Value repr))
typeUnExpr = on3StateValues (\u t -> mkExpr (uOpPrec u) t . unOpDocD (uOpDoc u) 
  . valueDoc)

-- Binary Operators --

compEqualPrec :: (RenderSym repr) => String -> VS (repr (BinaryOp repr))
compEqualPrec = bOpFromData 4 . text

compPrec :: (RenderSym repr) => String -> VS (repr (BinaryOp repr))
compPrec = bOpFromData 5 . text

addPrec :: (RenderSym repr) => String -> VS (repr (BinaryOp repr))
addPrec = bOpFromData 6 . text

multPrec :: (RenderSym repr) => String -> VS (repr (BinaryOp repr))
multPrec = bOpFromData 7 . text

powerPrec :: (RenderSym repr) => String -> VS (repr (BinaryOp repr))
powerPrec = bOpFromData 8 . text

andPrec :: (RenderSym repr) => String -> VS (repr (BinaryOp repr)) 
andPrec = bOpFromData 3 . text

orPrec :: (RenderSym repr) => String -> VS (repr (BinaryOp repr))
orPrec = bOpFromData 2 . text

equalOp :: (RenderSym repr) => VS (repr (BinaryOp repr))
equalOp = compEqualPrec "=="

notEqualOp :: (RenderSym repr) => VS (repr (BinaryOp repr))
notEqualOp = compEqualPrec "!="

greaterOp :: (RenderSym repr) => VS (repr (BinaryOp repr))
greaterOp = compPrec ">"

greaterEqualOp :: (RenderSym repr) => VS (repr (BinaryOp repr))
greaterEqualOp = compPrec ">="

lessOp :: (RenderSym repr) => VS (repr (BinaryOp repr))
lessOp = compPrec "<"

lessEqualOp :: (RenderSym repr) => VS (repr (BinaryOp repr))
lessEqualOp = compPrec "<="

plusOp :: (RenderSym repr) => VS (repr (BinaryOp repr))
plusOp = addPrec "+"

minusOp :: (RenderSym repr) => VS (repr (BinaryOp repr))
minusOp = addPrec "-"

multOp :: (RenderSym repr) => VS (repr (BinaryOp repr))
multOp = multPrec "*"

divideOp :: (RenderSym repr) => VS (repr (BinaryOp repr))
divideOp = multPrec "/"

moduloOp :: (RenderSym repr) => VS (repr (BinaryOp repr))
moduloOp = multPrec "%"

powerOp :: (RenderSym repr) => VS (repr (BinaryOp repr))
powerOp = powerPrec "pow"

andOp :: (RenderSym repr) => VS (repr (BinaryOp repr))
andOp = andPrec "&&"

orOp :: (RenderSym repr) => VS (repr (BinaryOp repr))
orOp = orPrec "||"

binOpDocD :: Doc -> Doc -> Doc -> Doc
binOpDocD op v1 v2 = v1 <+> op <+> v2

binOpDocD' :: Doc -> Doc -> Doc -> Doc
binOpDocD' op v1 v2 = op <> parens (v1 <> comma <+> v2)

binExpr :: (RenderSym repr) => VS (repr (BinaryOp repr)) -> 
  VS (repr (Value repr)) -> VS (repr (Value repr)) -> VS (repr (Value repr))
binExpr = on3StateValues (\b v1 v2 -> mkExpr (bOpPrec b) (numType (valueType v1)
  (valueType v2)) (binOpDocD (bOpDoc b) (exprParensL b v1 $ valueDoc v1) 
  (exprParensR b v2 $ valueDoc v2)))

binExpr' :: (RenderSym repr) => VS (repr (BinaryOp repr)) -> 
  VS (repr (Value repr)) -> VS (repr (Value repr)) -> VS (repr (Value repr))
binExpr' = on3StateValues (\b v1 v2 -> mkExpr 9 (numType (valueType v1) 
  (valueType v2)) (binOpDocD' (bOpDoc b) (valueDoc v1) (valueDoc v2)))

binExprNumDbl' :: (RenderSym repr) => VS (repr (BinaryOp repr)) -> 
  VS (repr (Value repr)) -> VS (repr (Value repr)) -> VS (repr (Value repr))
binExprNumDbl' b' v1' v2' = b' >>= (\b -> v1' >>= (\v1 -> v2' >>= (\v2 -> 
  let t1 = valueType v1
      t2 = valueType v2
  in binExprCastFloat t1 t2 $ return $ mkExpr 9 (numType t1 t2) 
  (binOpDocD' (bOpDoc b) (valueDoc v1) (valueDoc v2)))))

binExprCastFloat :: (RenderSym repr) => repr (Type repr) -> repr (Type repr) ->
  (VS (repr (Value repr)) -> VS (repr (Value repr)))
binExprCastFloat t1 t2 = castType (getType t1) (getType t2)
  where castType Float _ = cast float
        castType _ Float = cast float
        castType _ _ = id

typeBinExpr :: (RenderSym repr) => VS (repr (BinaryOp repr)) -> 
  VS (repr (Type repr)) -> VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
  VS (repr (Value repr))
typeBinExpr bod tp vl1 vl2 = (\b t v1 v2 -> mkExpr (bOpPrec b) t (binOpDocD 
  (bOpDoc b) (exprParensL b v1 $ valueDoc v1) (exprParensR b v2 $ valueDoc v2)))
  <$> bod <*> tp <*> vl1 <*> vl2 

addmathImport :: VS a -> VS a
addmathImport = (>>) $ modify (addLangImportVS "math")

-- Variables --

var :: (RenderSym repr) => Label -> VS (repr (Type repr)) -> 
  VS (repr (Variable repr))
var n t = mkStateVar n t (varDocD n)

staticVar :: (RenderSym repr) => Label -> VS (repr (Type repr)) -> 
  VS (repr (Variable repr))
staticVar n t = mkStaticVar n t (varDocD n)

extVar :: (RenderSym repr) => Label -> Label -> VS (repr (Type repr)) -> 
  VS (repr (Variable repr))
extVar l n t = mkStateVar (l ++ "." ++ n) t (extVarDocD l n)

self :: (RenderSym repr) => VS (repr (Variable repr))
self = zoom lensVStoMS getClassName >>= (\l -> mkStateVar "this" (obj l) selfDocD)

-- enumVar :: (RenderSym repr) => Label -> Label -> VS (repr (Variable repr))
-- enumVar e en = S.var e (enumType en)

classVar :: (RenderSym repr) => (Doc -> Doc -> Doc) -> VS (repr (Type repr)) -> 
  VS (repr (Variable repr)) -> VS (repr (Variable repr))
classVar f = on2StateValues (\c v -> classVarCheckStatic $ varFromData 
  (variableBind v) (getTypeString c ++ "." ++ variableName v) 
  (variableType v) (f (getTypeDoc c) (variableDoc v)))

objVar :: (RenderSym repr) => VS (repr (Variable repr)) -> 
  VS (repr (Variable repr)) -> VS (repr (Variable repr))
objVar = on2StateValues (\o v -> mkVar (variableName o ++ "." ++ variableName 
  v) (variableType v) (objVarDocD (variableDoc o) (variableDoc v)))

objVarSelf :: (RenderSym repr) => VS (repr (Variable repr)) -> 
  VS (repr (Variable repr))
objVarSelf = S.objVar S.self

listVar :: (RenderSym repr) => Label -> VS (repr (Type repr)) -> 
  VS (repr (Variable repr))
listVar n t = S.var n (S.listType t)

listOf :: (RenderSym repr) => Label -> VS (repr (Type repr)) -> 
  VS (repr (Variable repr))
listOf = S.listVar

arrayElem :: (RenderSym repr) => VS (repr (Value repr)) -> 
  VS (repr (Variable repr)) -> VS (repr (Variable repr))
arrayElem i' v' = join $ on2StateValues (\i v -> mkStateVar (variableName v ++ 
  "[" ++ render (valueDoc i) ++ "]") (listInnerType $ toState $ variableType v) 
  (variableDoc v <> brackets (valueDoc i))) i' v'

iterVar :: (RenderSym repr) => Label -> VS (repr (Type repr)) -> 
  VS (repr (Variable repr))
iterVar n t = S.var n (iterator t)

-- Values --

litTrue :: (RenderSym repr) => VS (repr (Value repr))
litTrue = mkStateVal S.bool (text "true")

litFalse :: (RenderSym repr) => VS (repr (Value repr))
litFalse = mkStateVal S.bool (text "false")

litChar :: (RenderSym repr) => Char -> VS (repr (Value repr))
litChar c = mkStateVal S.char (quotes $ D.char c)

litDouble :: (RenderSym repr) => Double -> VS (repr (Value repr))
litDouble d = mkStateVal S.double (D.double d)

litFloat :: (RenderSym repr) => Float -> VS (repr (Value repr))
litFloat f = mkStateVal S.float (D.float f <> text "f")

litInt :: (RenderSym repr) => Integer -> VS (repr (Value repr))
litInt i = mkStateVal S.int (integer i)

litString :: (RenderSym repr) => String -> VS (repr (Value repr))
litString s = mkStateVal S.string (doubleQuotedText s)

litArray :: (RenderSym repr) => VS (repr (Type repr)) -> 
  [VS (repr (Value repr))] -> VS (repr (Value repr))
litArray t es = sequence es >>= (\elems -> mkStateVal (S.arrayType t) 
  (braces $ valueList elems))

litList :: (RenderSym repr) => (VS (repr (Type repr)) -> VS (repr (Type repr)))
  -> VS (repr (Type repr)) -> [VS (repr (Value repr))] -> VS (repr (Value repr))
litList f t = on1StateValue1List (\lt es -> mkVal lt (new <+> getTypeDoc lt <+> 
  braces (valueList es))) (f t)

pi :: (RenderSym repr) => VS (repr (Value repr))
pi = mkStateVal S.double (text "Math.PI")

valueOf :: (RenderSym repr) => VS (repr (Variable repr)) -> 
  VS (repr (Value repr))
valueOf = onStateValue (\v -> mkVal (variableType v) (variableDoc v))

arg :: (RenderSym repr) => VS (repr (Value repr)) -> VS (repr (Value repr)) ->
  VS (repr (Value repr))
arg = on3StateValues (\s n args -> mkVal s (argDocD n args)) S.string

-- enumElement :: (RenderSym repr) => Label -> Label -> VS (repr (Value repr))
-- enumElement en e = mkStateVal (enumType en) (enumElemDocD en e)

argsList :: (RenderSym repr) => String -> VS (repr (Value repr))
argsList l = mkStateVal (S.arrayType S.string) (text l)

inlineIf :: (RenderSym repr) => VS (repr (Value repr)) -> VS (repr (Value repr))
  -> VS (repr (Value repr)) -> VS (repr (Value repr))
inlineIf = on3StateValues (\c v1 v2 -> valFromData (prec c) (valueType v1) 
  (valueDoc c <+> text "?" <+> valueDoc v1 <+> text ":" <+> valueDoc v2)) 
  where prec cd = valuePrec cd <|> Just 0

call' :: (RenderSym repr) => String -> Maybe Library -> Label -> 
  VS (repr (Type repr)) -> Maybe Doc -> [VS (repr (Value repr))] -> 
  [(VS (repr (Variable repr)), VS (repr (Value repr)))] -> 
  VS (repr (Value repr))
call' l _ _ _ _ _ (_:_) = error $ namedArgError l
call' _ l n t o ps ns = call empty l n t o ps ns

call :: (RenderSym repr) => Doc -> Maybe Library -> Label -> 
  VS (repr (Type repr)) -> Maybe Doc -> [VS (repr (Value repr))] -> 
  [(VS (repr (Variable repr)), VS (repr (Value repr)))] -> 
  VS (repr (Value repr))
call sep lib n t o pas nas = (\tp pargs nms nargs -> mkVal tp $ obDoc <> libDoc 
  <> text n <> parens (valueList pargs <+> (if null pas || null nas then empty 
  else comma) <+> hcat (intersperse (text ", ") (zipWith (\nm a -> 
  variableDoc nm <> sep <> valueDoc a) nms nargs)))) <$> t <*> sequence pas <*> 
  mapM fst nas <*> mapM snd nas
  where libDoc = maybe empty (text . (++ ".")) lib
        obDoc = fromMaybe empty o

funcAppMixedArgs :: (RenderSym repr) => Label -> VS (repr (Type repr)) 
  -> [VS (repr (Value repr))] -> 
  [(VS (repr (Variable repr)), VS (repr (Value repr)))] -> 
  VS (repr (Value repr))
funcAppMixedArgs n t = S.call Nothing n t Nothing

namedArgError :: String -> String
namedArgError l = "Named arguments not supported in " ++ l 

selfFuncAppMixedArgs :: (RenderSym repr) => Doc -> VS (repr (Variable repr)) -> 
  Label -> VS (repr (Type repr)) -> [VS (repr (Value repr))] -> 
  [(VS (repr (Variable repr)), VS (repr (Value repr)))] -> 
  VS (repr (Value repr))
selfFuncAppMixedArgs d slf n t vs ns = slf >>= (\s -> S.call Nothing n t 
  (Just $ variableDoc s <> d) vs ns)

extFuncAppMixedArgs :: (RenderSym repr) => Library -> Label ->
  VS (repr (Type repr)) -> [VS (repr (Value repr))] ->
  [(VS (repr (Variable repr)), VS (repr (Value repr)))] -> 
  VS (repr (Value repr))
extFuncAppMixedArgs l n t = S.call (Just l) n t Nothing

libFuncAppMixedArgs :: (RenderSym repr) => Library -> Label ->
  VS (repr (Type repr)) -> [VS (repr (Value repr))] ->
  [(VS (repr (Variable repr)), VS (repr (Value repr)))] -> 
  VS (repr (Value repr))
libFuncAppMixedArgs l n t vs ns = modify (addLibImportVS l) >> 
  S.funcAppMixedArgs n t vs ns

newObjMixedArgs :: (RenderSym repr) => String -> VS (repr (Type repr)) -> 
  [VS (repr (Value repr))] -> 
  [(VS (repr (Variable repr)), VS (repr (Value repr)))] -> 
  VS (repr (Value repr))
newObjMixedArgs s tp vs ns = tp >>= 
  (\t -> S.call Nothing (s ++ getTypeString t) (return t) Nothing vs ns)

extNewObjMixedArgs :: (RenderSym repr) => Library -> VS (repr (Type repr)) -> 
  [VS (repr (Value repr))] -> 
  [(VS (repr (Variable repr)), VS (repr (Value repr)))] -> 
  VS (repr (Value repr))
extNewObjMixedArgs l tp vs ns = tp >>= (\t -> S.call (Just l) (getTypeString t) 
  (return t) Nothing vs ns)

libNewObjMixedArgs :: (RenderSym repr) => Library -> VS (repr (Type repr)) -> 
  [VS (repr (Value repr))] -> 
  [(VS (repr (Variable repr)), VS (repr (Value repr)))] -> 
  VS (repr (Value repr))
libNewObjMixedArgs l tp vs ns = modify (addLibImportVS l) >> 
  S.newObjMixedArgs tp vs ns

notNull :: (RenderSym repr) => VS (repr (Value repr)) -> VS (repr (Value repr))
notNull v = v ?!= S.valueOf (S.var "null" $ onStateValue valueType v)

lambda :: (RenderSym repr) => ([repr (Variable repr)] -> repr (Value repr) -> 
  Doc) -> [VS (repr (Variable repr))] -> VS (repr (Value repr)) ->
  VS (repr (Value repr))
lambda f ps' ex' = sequence ps' >>= (\ps -> ex' >>= (\ex -> funcType (map 
  (toState . variableType) ps) (toState $ valueType ex) >>= (\ft -> 
  toState $ valFromData (Just 0) ft (f ps ex))))

objAccess :: (RenderSym repr) => VS (repr (Value repr)) ->
  VS (repr (Function repr)) -> VS (repr (Value repr))
objAccess = on2StateValues (\v f -> mkVal (functionType f) (objAccessDocD 
  (valueDoc v) (functionDoc f)))

objMethodCall :: (RenderSym repr) => Label -> VS (repr (Type repr)) -> 
  VS (repr (Value repr)) -> [VS (repr (Value repr))] -> 
  [(VS (repr (Variable repr)), VS (repr (Value repr)))] -> 
  VS (repr (Value repr))
objMethodCall f t ob vs ns = ob >>= (\o -> S.call Nothing f t 
  (Just $ valueDoc o <> dot) vs ns)

objMethodCallNoParams :: (RenderSym repr) => Label -> VS (repr (Type repr)) -> 
  VS (repr (Value repr)) -> VS (repr (Value repr))
objMethodCallNoParams f t o = S.objMethodCall t o f []

indexOf :: (RenderSym repr) => Label -> VS (repr (Value repr)) -> 
  VS (repr (Value repr)) -> VS (repr (Value repr))
indexOf f l v = S.objAccess l (S.func f S.int [v])

-- Functions --

func :: (RenderSym repr) => Label -> VS (repr (Type repr)) -> 
  [VS (repr (Value repr))] -> VS (repr (Function repr))
func l t vs = funcApp l t vs >>= ((`funcFromData` t) . funcDocD . valueDoc)

get :: (RenderSym repr) => VS (repr (Value repr)) -> VS (repr (Variable repr)) 
  -> VS (repr (Value repr))
get v vToGet = v $. S.getFunc vToGet

set :: (RenderSym repr) => VS (repr (Value repr)) -> VS (repr (Variable repr)) 
  -> VS (repr (Value repr)) -> VS (repr (Value repr))
set v vToSet toVal = v $. S.setFunc (onStateValue valueType v) vToSet toVal

listSize :: (RenderSym repr) => VS (repr (Value repr)) -> VS (repr (Value repr))
listSize v = v $. S.listSizeFunc

listAdd :: (RenderSym repr) => VS (repr (Value repr)) -> VS (repr (Value repr)) 
  -> VS (repr (Value repr)) -> VS (repr (Value repr))
listAdd v i vToAdd = v $. S.listAddFunc v i vToAdd

listAppend :: (RenderSym repr) => VS (repr (Value repr)) -> 
  VS (repr (Value repr)) -> VS (repr (Value repr))
listAppend v vToApp = v $. S.listAppendFunc vToApp

iterBegin :: (RenderSym repr) => VS (repr (Value repr)) -> 
  VS (repr (Value repr))
iterBegin v = v $. iterBeginFunc (S.listInnerType $ onStateValue valueType v)

iterEnd :: (RenderSym repr) => VS (repr (Value repr)) -> VS (repr (Value repr))
iterEnd v = v $. iterEndFunc (S.listInnerType $ onStateValue valueType v)

listAccess :: (RenderSym repr) => VS (repr (Value repr)) -> 
  VS (repr (Value repr)) -> VS (repr (Value repr))
listAccess v i = do
  v' <- v
  let checkType (List _) = S.listAccessFunc (S.listInnerType $ return $ 
        valueType v') i
      checkType (Array _) = i >>= (\ix -> funcFromData (brackets (valueDoc ix)) 
        (S.listInnerType $ return $ valueType v'))
      checkType _ = error "listAccess called on non-list-type value"
  v $. checkType (getType (valueType v'))

listSet :: (RenderSym repr) => VS (repr (Value repr)) -> VS (repr (Value repr)) 
  -> VS (repr (Value repr)) -> VS (repr (Value repr))
listSet v i toVal = v $. S.listSetFunc v i toVal

getFunc :: (RenderSym repr) => VS (repr (Variable repr)) -> 
  VS (repr (Function repr))
getFunc v = v >>= (\vr -> S.func (getterName $ variableName vr) 
  (toState $ variableType vr) [])

setFunc :: (RenderSym repr) => VS (repr (Type repr)) ->
  VS (repr (Variable repr)) -> VS (repr (Value repr)) -> 
  VS (repr (Function repr))
setFunc t v toVal = v >>= (\vr -> S.func (setterName $ variableName vr) t 
  [toVal])

listSizeFunc :: (RenderSym repr) => VS (repr (Function repr))
listSizeFunc = S.func "size" S.int []

listAddFunc :: (RenderSym repr) => Label -> VS (repr (Value repr)) -> 
  VS (repr (Value repr)) -> VS (repr (Function repr))
listAddFunc f i v = S.func f (S.listType $ onStateValue valueType v) 
  [i, v]

listAppendFunc :: (RenderSym repr) => Label -> VS (repr (Value repr)) -> 
  VS (repr (Function repr))
listAppendFunc f v = S.func f (S.listType $ onStateValue valueType v) [v]

iterBeginError :: String -> String
iterBeginError l = "Attempt to use iterBeginFunc in " ++ l ++ ", but " ++ l ++ 
  " has no iterators"

iterEndError :: String -> String
iterEndError l = "Attempt to use iterEndFunc in " ++ l ++ ", but " ++ l ++ 
  " has no iterators"

listAccessFunc :: (RenderSym repr) => VS (repr (Type repr)) ->
  VS (repr (Value repr)) -> VS (repr (Function repr))
listAccessFunc t v = intValue v >>= ((`funcFromData` t) . listAccessFuncDocD)

listAccessFunc' :: (RenderSym repr) => Label -> VS (repr (Type repr)) -> 
  VS (repr (Value repr)) -> VS (repr (Function repr))
listAccessFunc' f t i = S.func f t [intValue i]

listSetFunc :: (RenderSym repr) => (Doc -> Doc -> Doc) -> 
  VS (repr (Value repr)) -> VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
  VS (repr (Function repr))
listSetFunc f v idx setVal = join $ on2StateValues (\i toVal -> funcFromData 
  (f (valueDoc i) (valueDoc toVal)) (onStateValue valueType v)) (intValue idx) 
  setVal

-- Statements --

printSt :: (RenderSym repr) => VS (repr (Value repr)) -> VS (repr (Value repr))
  -> MS (repr (Statement repr))
printSt p v = zoom lensMStoVS $ on2StateValues (\p' -> mkSt . printDoc p') p v

state :: (RenderSym repr) => MS (repr (Statement repr)) -> 
  MS (repr (Statement repr))
state = onStateValue (\s -> mkStNoEnd (statementDoc s <> getTermDoc 
  (statementTerm s)))
  
loopState :: (RenderSym repr) => MS (repr (Statement repr)) -> 
  MS (repr (Statement repr))
loopState = S.state . setEmpty

emptyState :: (RenderSym repr) => MS (repr (Statement repr))
emptyState = toState $ mkStNoEnd empty

assign :: (RenderSym repr) => Terminator -> VS (repr (Variable repr)) -> 
  VS (repr (Value repr)) -> MS (repr (Statement repr))
assign t vr vl = zoom lensMStoVS $ on2StateValues (\vr' vl' -> stateFromData 
  (assignDocD vr' vl') t) vr vl

multiAssignError :: String -> String
multiAssignError l = "No multiple assignment statements in " ++ l

decrement :: (RenderSym repr) => VS (repr (Variable repr)) -> 
  VS (repr (Value repr)) -> MS (repr (Statement repr))
decrement vr vl = vr &= (S.valueOf vr #- vl)

increment :: (RenderSym repr) => VS (repr (Variable repr)) -> 
  VS (repr (Value repr)) -> MS (repr (Statement repr))
increment vr vl = zoom lensMStoVS $ on2StateValues (\vr' -> mkSt . 
  plusEqualsDocD vr') vr vl

increment1 :: (RenderSym repr) => VS (repr (Variable repr)) -> 
  MS (repr (Statement repr))
increment1 vr = zoom lensMStoVS $ onStateValue (mkSt . plusPlusDocD) vr

increment' :: (RenderSym repr) => VS (repr (Variable repr)) -> 
  VS (repr (Value repr)) -> MS (repr (Statement repr))
increment' vr vl = vr &= S.valueOf vr #+ vl

increment1' :: (RenderSym repr) => VS (repr (Variable repr)) -> 
  MS (repr (Statement repr))
increment1' vr = vr &= S.valueOf vr #+ S.litInt 1

decrement1 :: (RenderSym repr) => VS (repr (Variable repr)) -> 
  MS (repr (Statement repr))
decrement1 v = v &= (S.valueOf v #- S.litInt 1)

varDec :: (RenderSym repr) => repr (Permanence repr) -> repr (Permanence repr) 
  -> VS (repr (Variable repr)) -> MS (repr (Statement repr))
varDec s d v' = onStateValue (\v -> mkSt (permDoc (bind $ variableBind v) 
  <+> getTypeDoc (variableType v) <+> variableDoc v)) (zoom lensMStoVS v')
  where bind Static = s
        bind Dynamic = d

varDecDef :: (RenderSym repr) => VS (repr (Variable repr)) -> 
  VS (repr (Value repr)) -> MS (repr (Statement repr))
varDecDef vr vl' = on2StateValues (\vd vl -> mkSt (statementDoc vd <+> equals 
  <+> valueDoc vl)) (S.varDec vr) (zoom lensMStoVS vl')

listDec :: (RenderSym repr) => (repr (Value repr) -> Doc) -> 
  VS (repr (Value repr)) -> VS (repr (Variable repr)) -> 
  MS (repr (Statement repr))
listDec f vl v = on2StateValues (\sz vd -> mkSt (statementDoc vd <> f 
  sz)) (zoom lensMStoVS vl) (S.varDec v)

listDecDef :: (RenderSym repr) => ([repr (Value repr)] -> Doc) -> 
  VS (repr (Variable repr)) -> [VS (repr (Value repr))] -> 
  MS (repr (Statement repr))
listDecDef f v vls = on1StateValue1List (\vd vs -> mkSt (statementDoc vd <> 
  f vs)) (S.varDec v) (map (zoom lensMStoVS) vls)

listDecDef' :: (RenderSym repr) => VS (repr (Variable repr)) -> 
  [VS (repr (Value repr))] -> MS (repr (Statement repr))
listDecDef' v vals = zoom lensMStoVS v >>= (\vr -> S.varDecDef (return vr) 
  (S.litList (listInnerType $ return $ variableType vr) vals))

arrayDec :: (RenderSym repr) => VS (repr (Value repr)) -> 
  VS (repr (Variable repr)) -> MS (repr (Statement repr))
arrayDec n vr = zoom lensMStoVS $ on3StateValues (\sz v it -> mkSt (getTypeDoc 
  (variableType v) <+> variableDoc v <+> equals <+> new <+> getTypeDoc it <> 
  brackets (valueDoc sz))) n vr (listInnerType $ onStateValue variableType vr)

arrayDecDef :: (RenderSym repr) => VS (repr (Variable repr)) -> 
  [VS (repr (Value repr))] -> MS (repr (Statement repr))
arrayDecDef v vals = on2StateValues (\vd vs -> mkSt (statementDoc vd <+> 
  equals <+> braces (valueList vs))) (S.varDec v) (mapM (zoom lensMStoVS) vals)

objDecNew :: (RenderSym repr) => VS (repr (Variable repr)) -> 
  [VS (repr (Value repr))] -> MS (repr (Statement repr))
objDecNew v vs = S.varDecDef v (newObj (onStateValue variableType v) vs)

objDecNewNoParams :: (RenderSym repr) => VS (repr (Variable repr)) -> 
  MS (repr (Statement repr))
objDecNewNoParams v = S.objDecNew v []

extObjDecNew :: (RenderSym repr) => Library -> VS (repr (Variable repr)) -> 
  [VS (repr (Value repr))] -> MS (repr (Statement repr))
extObjDecNew l v vs = S.varDecDef v (extNewObj l (onStateValue variableType v)
  vs)

extObjDecNewNoParams :: (RenderSym repr) => Library -> VS (repr (Variable repr))
  -> MS (repr (Statement repr))
extObjDecNewNoParams l v = S.extObjDecNew l v []

constDecDef :: (RenderSym repr) => VS (repr (Variable repr)) -> 
  VS (repr (Value repr)) -> MS (repr (Statement repr))
constDecDef vr vl = zoom lensMStoVS $ on2StateValues (\v -> mkSt . 
  constDecDefDocD v) vr vl

funcDecDef :: (RenderSym repr) => VS (repr (Variable repr)) -> 
  [VS (repr (Variable repr))] -> VS (repr (Value repr)) -> 
  MS (repr (Statement repr))
funcDecDef v ps r = S.varDecDef v (S.lambda ps r)

discardInput :: (RenderSym repr) => (repr (Value repr) -> Doc) ->
  MS (repr (Statement repr))
discardInput f = zoom lensMStoVS $ onStateValue (mkSt . f) inputFunc

discardFileInput :: (RenderSym repr) => (repr (Value repr) -> Doc) -> 
  VS (repr (Value repr)) -> MS (repr (Statement repr))
discardFileInput f v = zoom lensMStoVS $ onStateValue (mkSt . f) v

openFileR :: (RenderSym repr) => (VS (repr (Value repr)) -> 
  VS (repr (Type repr)) -> VS (repr (Value repr))) -> VS (repr (Variable repr)) 
  -> VS (repr (Value repr)) -> MS (repr (Statement repr))
openFileR f vr vl = vr &= f vl infile

openFileW :: (RenderSym repr) => (VS (repr (Value repr)) -> 
  VS (repr (Type repr)) -> VS (repr (Value repr)) -> VS (repr (Value repr))) -> 
  VS (repr (Variable repr)) -> VS (repr (Value repr)) ->
  MS (repr (Statement repr))
openFileW f vr vl = vr &= f vl outfile S.litFalse

openFileA :: (RenderSym repr) => (VS (repr (Value repr)) -> 
  VS (repr (Type repr)) -> VS (repr (Value repr)) -> VS (repr (Value repr))) -> 
  VS (repr (Variable repr)) -> VS (repr (Value repr)) -> 
  MS (repr (Statement repr))
openFileA f vr vl = vr &= f vl outfile S.litTrue

closeFile :: (RenderSym repr) => Label -> VS (repr (Value repr)) -> 
  MS (repr (Statement repr))
closeFile n f = S.valState $ S.objMethodCallNoParams S.void f n

discardFileLine :: (RenderSym repr) => Label -> VS (repr (Value repr)) -> 
  MS (repr (Statement repr))
discardFileLine n f = S.valState $ S.objMethodCallNoParams S.string f n 

stringListVals :: (RenderSym repr) => [VS (repr (Variable repr))] -> 
  VS (repr (Value repr)) -> MS (repr (Statement repr))
stringListVals vars sl = zoom lensMStoVS sl >>= (\slst -> multi $ checkList 
  (getType $ valueType slst))
  where checkList (List String) = assignVals vars 0
        checkList _ = error 
          "Value passed to stringListVals must be a list of strings"
        assignVals [] _ = []
        assignVals (v:vs) n = S.assign v (cast (onStateValue variableType v) 
          (S.listAccess sl (S.litInt n))) : assignVals vs (n+1)

stringListLists :: (RenderSym repr) => [VS (repr (Variable repr))] -> 
  VS (repr (Value repr)) -> MS (repr (Statement repr))
stringListLists lsts sl = zoom lensMStoVS sl >>= (\slst -> checkList (getType $ 
  valueType slst))
  where checkList (List String) = mapM (zoom lensMStoVS) lsts >>= listVals . 
          map (getType . variableType)
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

returnState :: (RenderSym repr) => Terminator -> VS (repr (Value repr)) -> 
  MS (repr (Statement repr))
returnState t v' = zoom lensMStoVS $ onStateValue (\v -> stateFromData 
  (returnDocD [v]) t) v'

multiReturnError :: String -> String
multiReturnError l = "Cannot return multiple values in " ++ l

valState :: (RenderSym repr) => Terminator -> VS (repr (Value repr)) ->
  MS (repr (Statement repr))
valState t v' = zoom lensMStoVS $ onStateValue (\v -> stateFromData (valueDoc v)
  t) v'

comment :: (RenderSym repr) => repr (Keyword repr) -> Label -> 
  MS (repr (Statement repr))
comment cs c = toState $ mkStNoEnd (commentDocD c (keyDoc cs))

freeError :: String -> String
freeError l = "Cannot free variables in " ++ l

throw :: (RenderSym repr) => (repr (Value repr) -> Doc) -> Terminator -> 
  Label -> MS (repr (Statement repr))
throw f t = onStateValue (\msg -> stateFromData (f msg) t) . zoom lensMStoVS . 
  S.litString

-- ControlStatements --

ifCond :: (RenderSym repr) => repr (Keyword repr) -> repr (Keyword repr) -> 
  repr (Keyword repr) -> [(VS (repr (Value repr)), MS (repr (Body repr)))] -> 
  MS (repr (Body repr)) -> MS (repr (Statement repr))
ifCond _ _ _ [] _ = error "if condition created with no cases"
ifCond ifst elseif blEnd (c:cs) eBody = 
    let ifStart = keyDoc ifst
        elif = keyDoc elseif
        bEnd = keyDoc blEnd
        ifSect (v, b) = on2StateValues (\val bd -> vcat [
          text "if" <+> parens (valueDoc val) <+> ifStart,
          indent $ bodyDoc bd,
          bEnd]) (zoom lensMStoVS v) b
        elseIfSect (v, b) = on2StateValues (\val bd -> vcat [
          elif <+> parens (valueDoc val) <+> ifStart,
          indent $ bodyDoc bd,
          bEnd]) (zoom lensMStoVS v) b
        elseSect = onStateValue (\bd -> emptyIfEmpty (bodyDoc bd) $ vcat [
          text "else" <+> ifStart,
          indent $ bodyDoc bd,
          bEnd]) eBody
    in onStateList (mkStNoEnd . vcat)
      (ifSect c : map elseIfSect cs ++ [elseSect])

switch :: (RenderSym repr) => VS (repr (Value repr)) -> 
  [(VS (repr (Value repr)), MS (repr (Body repr)))] -> MS (repr (Body repr)) -> 
  MS (repr (Statement repr))
switch v cs bod = (\b val css de -> mkSt (switchDocD b val de css)) <$>
  S.state break <*> zoom lensMStoVS v <*> on2StateLists zip (map (zoom 
  lensMStoVS . fst) cs) (map snd cs) <*> bod

ifExists :: (RenderSym repr) => VS (repr (Value repr)) -> MS (repr (Body repr)) 
  -> MS (repr (Body repr)) -> MS (repr (Statement repr))
ifExists v ifBody = S.ifCond [(S.notNull v, ifBody)]

for :: (RenderSym repr) => repr (Keyword repr) -> repr (Keyword repr) -> 
  MS (repr (Statement repr)) -> VS (repr (Value repr)) -> 
  MS (repr (Statement repr)) -> MS (repr (Body repr)) -> 
  MS (repr (Statement repr))
for bStart bEnd sInit vGuard sUpdate b = (\initl guard upd bod -> mkStNoEnd 
  (vcat [forLabel <+> parens (statementDoc initl <> semi <+> valueDoc guard <> 
    semi <+> statementDoc upd) <+> keyDoc bStart,
  indent $ bodyDoc bod,
  keyDoc bEnd])) <$> S.loopState sInit <*> zoom lensMStoVS vGuard <*> 
  S.loopState sUpdate <*> b

forRange :: (RenderSym repr) => VS (repr (Variable repr)) -> 
  VS (repr (Value repr)) -> VS (repr (Value repr)) -> VS (repr (Value repr)) -> 
  MS (repr (Body repr)) -> MS (repr (Statement repr))
forRange i initv finalv stepv = S.for (S.varDecDef i initv) (S.valueOf i ?< 
  finalv) (i &+= stepv)

forEach :: (RenderSym repr) => repr (Keyword repr) -> repr (Keyword repr) -> 
  repr (Keyword repr) -> repr (Keyword repr) -> VS (repr (Variable repr)) -> 
  VS (repr (Value repr)) -> MS (repr (Body repr)) -> MS (repr (Statement repr))
forEach bStart bEnd forEachLabel inLbl e' v' = on3StateValues (\e v b -> 
  mkStNoEnd (vcat [keyDoc forEachLabel <+> parens (getTypeDoc (variableType e) 
    <+> variableDoc e <+> keyDoc inLbl <+> valueDoc v) <+> keyDoc bStart,
  indent $ bodyDoc b,
  keyDoc bEnd])) (zoom lensMStoVS e') (zoom lensMStoVS v') 

while :: (RenderSym repr) => repr (Keyword repr) -> repr (Keyword repr) -> 
  VS (repr (Value repr)) -> MS (repr (Body repr)) -> MS (repr (Statement repr))
while bStart bEnd v' = on2StateValues (\v b -> mkStNoEnd (vcat [
  text "while" <+> parens (valueDoc v) <+> keyDoc bStart,
  indent $ bodyDoc b,
  keyDoc bEnd])) (zoom lensMStoVS v')

tryCatch :: (RenderSym repr) => (repr (Body repr) -> repr (Body repr) -> Doc) ->
  MS (repr (Body repr)) -> MS (repr (Body repr)) -> MS (repr (Statement repr))
tryCatch f = on2StateValues (\tb -> mkStNoEnd . f tb)

checkState :: (RenderSym repr) => Label -> [(VS (repr (Value repr)), 
  MS (repr (Body repr)))] -> MS (repr (Body repr)) -> MS (repr (Statement repr))
checkState l = S.switch (S.valueOf $ S.var l S.string)

notifyObservers :: (RenderSym repr) => VS (repr (Function repr)) -> 
  VS (repr (Type repr)) -> MS (repr (Statement repr))
notifyObservers f t = S.for initv (v_index ?< S.listSize obsList) 
  (var_index &++) notify
  where obsList = S.valueOf $ observerListName `S.listOf` t 
        var_index = S.var "observerIndex" S.int
        v_index = S.valueOf var_index
        initv = S.varDecDef var_index $ S.litInt 0
        notify = S.oneLiner $ S.valState $ at obsList v_index $. f

-- Methods --

construct :: (RenderSym repr) => Label -> MS (repr (Type repr))
construct n = toState $ typeFromData (Object n) n empty

param :: (RenderSym repr) => (repr (Variable repr) -> Doc) -> 
  VS (repr (Variable repr)) -> MS (repr (Parameter repr))
param f v' = modifyReturnFunc (\v s -> addParameter (variableName v) s) 
  (\v -> paramFromData v (f v)) (zoom lensMStoVS v')

method :: (RenderSym repr) => Label -> repr (Scope repr) -> 
  repr (Permanence repr) -> VS (repr (Type repr)) -> 
  [MS (repr (Parameter repr))] -> MS (repr (Body repr)) -> 
  MS (repr (Method repr))
method n s p t = intMethod False n s p (mType t)

getMethod :: (RenderSym repr) => VS (repr (Variable repr)) -> 
  MS (repr (Method repr))
getMethod v = zoom lensMStoVS v >>= (\vr -> S.method (getterName $ variableName 
  vr) public dynamic (toState $ variableType vr) [] getBody)
  where getBody = S.oneLiner $ S.returnState (S.valueOf $ S.objVarSelf v)

setMethod :: (RenderSym repr) => VS (repr (Variable repr)) -> 
  MS (repr (Method repr))
setMethod v = zoom lensMStoVS v >>= (\vr -> S.method (setterName $ variableName 
  vr) public dynamic S.void [S.param v] setBody)
  where setBody = S.oneLiner $ S.objVarSelf v &= S.valueOf v

constructor :: (RenderSym repr) => Label -> [MS (repr (Parameter repr))] -> 
  [(VS (repr (Variable repr)), VS (repr (Value repr)))] -> 
  MS (repr (Body repr)) -> MS (repr (Method repr))
constructor fName ps is b = getClassName >>= (\c -> intMethod False fName 
  public dynamic (S.construct c) ps (multiBody [ib, b]))
  where ib = bodyStatements (zipWith (\vr vl -> objVarSelf vr &= vl) 
          (map fst is) (map snd is))

docMain :: (RenderSym repr) => MS (repr (Body repr)) -> MS (repr (Method repr))
docMain b = commentedFunc (docComment $ toState $ functionDox 
  "Controls the flow of the program" 
  [("args", "List of command-line arguments")] []) (S.mainFunction b)

function :: (RenderSym repr) => Label -> repr (Scope repr) -> 
  repr (Permanence repr) -> VS (repr (Type repr)) -> 
  [MS (repr (Parameter repr))] -> MS (repr (Body repr)) -> 
  MS (repr (Method repr))
function n s p t = S.intFunc False n s p (mType t)

mainFunction :: (RenderSym repr) => VS (repr (Type repr)) -> Label -> 
  MS (repr (Body repr)) -> MS (repr (Method repr))
mainFunction s n = S.intFunc True n public static (mType S.void)
  [S.param (S.var "args" (onStateValue (\argT -> typeFromData (List String) 
  (render (getTypeDoc argT) ++ "[]") (getTypeDoc argT <> text "[]")) s))]

docFunc :: (RenderSym repr) => String -> [String] -> Maybe String -> 
  MS (repr (Method repr)) -> MS (repr (Method repr))
docFunc desc pComms rComm = docFuncRepr desc pComms (maybeToList rComm)

docInOutFunc :: (RenderSym repr) => (repr (Scope repr) -> repr (Permanence repr)
    -> [VS (repr (Variable repr))] -> [VS (repr (Variable repr))] -> 
    [VS (repr (Variable repr))] -> MS (repr (Body repr)) -> 
    MS (repr (Method repr)))
  -> repr (Scope repr) -> repr (Permanence repr) -> String -> 
  [(String, VS (repr (Variable repr)))] -> [(String, VS (repr (Variable repr)))]
  -> [(String, VS (repr (Variable repr)))] -> MS (repr (Body repr)) -> 
  MS (repr (Method repr))
docInOutFunc f s p desc is [o] [] b = docFuncRepr desc (map fst is) [fst o] 
  (f s p (map snd is) [snd o] [] b)
docInOutFunc f s p desc is [] [both] b = docFuncRepr desc (map fst $ both : is) 
  [fst both] (f s p (map snd is) [] [snd both] b)
docInOutFunc f s p desc is os bs b = docFuncRepr desc (map fst $ bs ++ is ++ os)
  [] (f s p (map snd is) (map snd os) (map snd bs) b)

intFunc :: (RenderSym repr) => Bool -> Label -> repr (Scope repr) -> 
  repr (Permanence repr) -> MS (repr (MethodType repr)) -> 
  [MS (repr (Parameter repr))] -> MS (repr (Body repr)) -> 
  MS (repr (Method repr))
intFunc = intMethod

-- State Variables --

stateVar :: (RenderSym repr) => repr (Scope repr) -> repr (Permanence repr) ->
  VS (repr (Variable repr)) -> CS (repr (StateVar repr))
stateVar s p v = stateVarFromData (zoom lensCStoMS $ onStateValue (stateVarDocD 
  (scopeDoc s) (permDoc p) . statementDoc) (S.state $ S.varDec v))

stateVarDef :: (RenderSym repr) => repr (Scope repr) -> repr (Permanence repr) 
  -> VS (repr (Variable repr)) -> VS (repr (Value repr)) -> 
  CS (repr (StateVar repr))
stateVarDef s p vr vl = stateVarFromData (zoom lensCStoMS $ onStateValue 
  (stateVarDocD (scopeDoc s) (permDoc p) . statementDoc) (S.state $ S.varDecDef 
  vr vl))

constVar :: (RenderSym repr) => Doc -> repr (Scope repr) ->
  VS (repr (Variable repr)) -> VS (repr (Value repr)) -> 
  CS (repr (StateVar repr))
constVar p s vr vl = stateVarFromData (zoom lensCStoMS $ onStateValue 
  (stateVarDocD (scopeDoc s) p . statementDoc) (S.state $ S.constDecDef vr vl))

-- Classes --

buildClass :: (RenderSym repr) => Label -> Maybe Label -> 
  [CS (repr (StateVar repr))] -> [MS (repr (Method repr))] -> 
  CS (repr (Class repr))
buildClass n = S.intClass n public . maybe (keyFromDoc empty) inherit

-- enum :: (RenderSym repr) => Label -> [Label] -> repr (Scope repr) -> 
--   CS (repr (Class repr))
-- enum n es s = modify (setClassName n) >> classFromData (toState $ enumDocD n 
--   (enumElementsDocD es False) (scopeDoc s))

extraClass :: (RenderSym repr) => Label -> Maybe Label -> 
  [CS (repr (StateVar repr))] -> [MS (repr (Method repr))] -> 
  CS (repr (Class repr))
extraClass n = S.intClass n (scopeFromData Priv empty) . 
  maybe (keyFromDoc empty) inherit

implementingClass :: (RenderSym repr) => Label -> [Label] -> 
  [CS (repr (StateVar repr))] -> [MS (repr (Method repr))] 
  -> CS (repr (Class repr))
implementingClass n is = S.intClass n public (implements is)

docClass :: (RenderSym repr) => String -> CS (repr (Class repr))
  -> CS (repr (Class repr))
docClass d = S.commentedClass (docComment $ toState $ classDox d)

commentedClass :: (RenderSym repr) => CS (repr (BlockComment repr))
  -> CS (repr (Class repr)) -> CS (repr (Class repr))
commentedClass cmt cs = classFromData (on2StateValues (\cmt' cs' -> 
  commentedItem (blockCommentDoc cmt') (classDoc cs')) cmt cs)

intClass :: (RenderSym repr) => (Label -> Doc -> Doc -> Doc -> Doc -> Doc) -> 
  Label -> repr (Scope repr) -> repr (Keyword repr) -> 
  [CS (repr (StateVar repr))] -> [MS (repr (Method repr))] ->
  CS (repr (Class repr))
intClass f n s i svs ms = modify (setClassName n) >> classFromData 
  (on2StateValues (f n (keyDoc i) (scopeDoc s)) (onStateList 
  (stateVarListDocD . map stateVarDoc) svs) (onStateList (vibcat . map 
  methodDoc) (map (zoom lensCStoMS) ms)))

-- Modules --

buildModule :: (RenderSym repr) => Label -> FS Doc -> FS Doc -> 
  [MS (repr (Method repr))] -> [CS (repr (Class repr))] -> 
  FS (repr (Module repr))
buildModule n imps bot ms cs = S.modFromData n ((\cls fs is bt -> 
  moduleDocD is (vibcat (map classDoc cls)) (vibcat (map methodDoc fs ++ [bt])))
  <$> mapM (zoom lensFStoCS) cs <*> mapM (zoom lensFStoMS) ms <*> imps <*> bot)

buildModule' :: (RenderSym repr) => Label -> (String -> repr (Import repr)) -> 
  [Label] -> [MS (repr (Method repr))] -> [CS (repr (Class repr))] -> 
  FS (repr (Module repr))
buildModule' n inc is ms cs = S.modFromData n ((\cls lis libis mis -> vibcat [
    vcat (map (importDoc . inc) (lis ++ sort (is ++ libis) ++ mis)),
    vibcat (map classDoc cls)]) <$>
  mapM (zoom lensFStoCS) (if null ms then cs else S.buildClass n Nothing [] ms 
    : cs) <*> getLangImports <*> getLibImports <*> getModuleImports)

modFromData :: Label -> (Doc -> repr (Module repr)) -> FS Doc -> 
  FS (repr (Module repr))
modFromData n f d = modify (setModuleName n) >> onStateValue f d

-- Files --

fileDoc :: (RenderSym repr) => String -> (repr (Module repr) -> 
  repr (Block repr)) -> repr (Block repr) -> FS (repr (Module repr)) -> 
  FS (repr (RenderFile repr))
fileDoc ext topb botb = S.fileFromData (onStateValue (addExt ext) 
  getModuleName) . onStateValue (\m -> updateModuleDoc (\d -> emptyIfEmpty d 
  (fileDoc' (blockDoc $ topb m) d (blockDoc botb))) m)

docMod :: (RenderSym repr) => String -> String -> [String] -> String -> 
  FS (repr (RenderFile repr)) -> FS (repr (RenderFile repr))
docMod e d a dt = commentedMod (docComment $ moduleDox d a dt . addExt e <$> 
  getModuleName)

fileFromData :: (RenderSym repr) => (repr (Module repr) -> FilePath -> 
  repr (RenderFile repr)) -> FS FilePath -> FS (repr (Module repr)) -> 
  FS (repr (RenderFile repr))
fileFromData f fp m = modifyReturnFunc2 (\mdl fpath s -> (if isEmpty 
  (moduleDoc mdl) then id else (if snd s ^. currMain && isSource (snd s ^. currFileType) then over lensFStoGS 
  (setMainMod fpath) else id) . over lensFStoGS (addFile (snd s ^. currFileType) fpath)) s) f m fp 

-- Helper functions

setEmpty :: (RenderSym repr) => MS (repr (Statement repr)) -> 
  MS (repr (Statement repr))
setEmpty = onStateValue (mkStNoEnd . statementDoc)

numType :: (RenderSym repr) => repr (Type repr) -> repr (Type repr) -> 
  repr (Type repr)
numType t1 t2 = numericType (getType t1) (getType t2)
  where numericType Integer Integer = t1
        numericType Float _ = t1
        numericType _ Float = t2
        numericType Double _ = t1
        numericType _ Double = t2
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