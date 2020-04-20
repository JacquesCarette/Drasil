{-# LANGUAGE PostfixOperators #-}

-- | The structure for a class of renderers is defined here.
module GOOL.Drasil.LanguageRenderer.LanguagePolymorphic (fileFromData,
  multiBody, block, multiBlock, bool, int, float, double, char, string, 
  fileType, listType, arrayType, listInnerType, obj, funcType, void, 
  runStrategy, listSlice, unOpPrec, notOp, notOp', negateOp, sqrtOp, sqrtOp', 
  absOp, absOp', expOp, expOp', sinOp, sinOp', cosOp, cosOp', tanOp, tanOp', 
  asinOp, asinOp', acosOp, acosOp', atanOp, atanOp', csc, sec, cot, unExpr, 
  unExpr', unExprNumDbl, typeUnExpr, powerPrec, multPrec, andPrec, orPrec, 
  equalOp, notEqualOp, greaterOp, greaterEqualOp, lessOp, lessEqualOp, plusOp, 
  minusOp, multOp, divideOp, moduloOp, powerOp, andOp, orOp, binExpr, binExpr', 
  binExprNumDbl', typeBinExpr, addmathImport, bindingError, var, staticVar, 
  extVar, self, classVarCheckStatic, classVar, objVar, objVarSelf, listVar, 
  arrayElem, iterVar, litTrue, litFalse, litChar, litDouble, litFloat, litInt, 
  litString, litArray, litList, pi, valueOf, arg, argsList, inlineIf, call', 
  call, funcAppMixedArgs, namedArgError, selfFuncAppMixedArgs, 
  extFuncAppMixedArgs, libFuncAppMixedArgs, newObjMixedArgs, 
  extNewObjMixedArgs, libNewObjMixedArgs, lambda, notNull, objAccess, 
  objMethodCall, objMethodCallNoParams, indexOf, func, get, set, listSize, 
  listAdd, listAppend, iterBegin, iterEnd, listAccess, listSet, getFunc, 
  setFunc, listSizeFunc, listAddFunc, listAppendFunc, iterBeginError, 
  iterEndError, listAccessFunc, listAccessFunc', listSetFunc, printSt, stmt, 
  loopStmt, emptyStmt, assign, multiAssignError, decrement, increment, 
  increment', increment1, increment1', decrement1, varDec, varDecDef, listDec, 
  listDecDef, listDecDef', arrayDec, arrayDecDef, objDecNew, objDecNewNoParams, 
  extObjDecNew, extObjDecNewNoParams, constDecDef, funcDecDef, print, 
  discardInput, discardFileInput, openFileR, openFileW, openFileA, closeFile, 
  discardFileLine, stringListVals, stringListLists, returnStmt, 
  multiReturnError, valStmt, comment, throw, ifCond, switch, ifExists, for, 
  forRange, forEach, while, tryCatch, checkState, notifyObservers, construct, 
  param, method, getMethod, setMethod, constructor, destructorError, docMain, 
  function, mainFunction, docFuncRepr, docFunc, docInOutFunc, intFunc, stateVar,
  stateVarDef, constVar, buildClass, extraClass, implementingClass, docClass, 
  commentedClass, intClass, buildModule, buildModule', modFromData, fileDoc, 
  docMod
) where

import Utils.Drasil (indent)

import GOOL.Drasil.CodeType (CodeType(..), ClassName)
import GOOL.Drasil.ClassInterface (Label, Library, SFile, MSBody, MSBlock, 
  VSType, SVariable, SValue, VSFunction, MSStatement, MSParameter, SMethod, 
  CSStateVar, SClass, FSModule, NamedArgs, Initializers, MixedCall, 
  MixedCtorCall, FileSym(File), BodySym(Body), bodyStatements, oneLiner, 
  BlockSym(Block), PermanenceSym(..), TypeSym(Type, infile, outfile, iterator), 
  TypeElim(getType, getTypeString), VariableSym(Variable), 
  VariableElim(variableName, variableType), listOf, ValueSym(Value, valueType), 
  NumericExpression((#+), (#-), (#*), (#/), sin, cos, tan), Comparison(..), 
  funcApp, newObj, extNewObj, ($.), at, StatementSym(multi), 
  AssignStatement((&+=), (&++)), (&=), 
  IOStatement(printStr, printStrLn, printFile, printFileStr, printFileStrLn),
  ControlStatement(break), ifNoElse, observerListName, ScopeSym(..), 
  ModuleSym(Module), convType)
import qualified GOOL.Drasil.ClassInterface as S (BlockSym(block), 
  TypeSym(bool, int, float, double, char, string, listType, arrayType, 
    listInnerType, void), 
  VariableSym(var, self, objVar, objVarSelf),
  Literal(litTrue, litFalse, litInt, litString, litList), 
  VariableValue(valueOf),
  ValueExpression(funcAppMixedArgs, newObjMixedArgs, notNull, lambda), 
  objMethodCall, objMethodCallNoParams, FunctionSym(func, objAccess), 
  List(listSize, listAppend, listAccess), StatementSym(valStmt), 
  AssignStatement(assign),
  DeclStatement(varDec, varDecDef, listDec, objDecNew, extObjDecNew, 
    constDecDef), 
  IOStatement(print),
  ControlStatement(returnStmt, ifCond, for, forRange, switch), 
  ParameterSym(param), MethodSym(method, mainFunction), ClassSym(buildClass))
import GOOL.Drasil.RendererClasses (VSUnOp, VSBinOp, MSMthdType, RenderSym, 
  RenderFile(commentedMod), RenderBody(docBody), BodyElim(..), 
  RenderBlock(docBlock), BlockElim(..), ImportSym(..), ImportElim(..), 
  PermElim(..), RenderType(..), InternalTypeElim(..), UnaryOpSym(UnaryOp), 
  BinaryOpSym(BinaryOp), OpElim(..), RenderOp(..), RenderVariable(varFromData),
  InternalVarElim(variableBind, variableDoc), 
  RenderValue(inputFunc, cast, valFromData), ValueElim(valuePrec, valueDoc),
  InternalIterator(iterBeginFunc, iterEndFunc), RenderFunction(funcFromData), 
  FunctionElim(functionDoc, functionType), RenderStatement(stmtFromData), 
  StatementElim(statementDoc, statementTerm), RenderScope(..), ScopeElim(..), 
  MethodTypeSym(mType), RenderParam(paramFromData), 
  RenderMethod(intMethod, commentedFunc), MethodElim(methodDoc), 
  RenderStateVar(..), StateVarElim(..), ParentSpec, 
  RenderClass(inherit, implements, classFromData), ClassElim(classDoc),
  RenderMod(updateModuleDoc), ModuleElim(moduleDoc), BlockCommentSym(..), BlockCommentElim(..))
import qualified GOOL.Drasil.RendererClasses as S (RenderFile(fileFromData), 
  RenderBody(multiBody), RenderValue(call), 
  InternalGetSet(getFunc, setFunc),
  InternalListFunc(listSizeFunc, listAddFunc, listAppendFunc, listAccessFunc, 
    listSetFunc),
  RenderStatement(stmt, loopStmt, emptyStmt), InternalIOStmt(..), 
  MethodTypeSym(construct), RenderMethod(intFunc), 
  RenderClass(intClass, commentedClass), RenderMod(modFromData))
import GOOL.Drasil.AST (Binding(..), ScopeTag(..), Terminator(..), isSource)
import GOOL.Drasil.Helpers (angles, doubleQuotedText, vibcat, emptyIfEmpty, 
  toCode, toState, onCodeValue, onStateValue, on2StateValues, on3StateValues, 
  onStateList, on2StateLists, on1StateValue1List, getInnerType, getNestDegree)
import GOOL.Drasil.LanguageRenderer (dot, forLabel, new, addExt, mkSt, 
  mkStNoEnd, mkStateVal, mkVal, mkStateVar, mkVar, mkStaticVar, functionDox, 
  classDox, moduleDox, getterName, setterName, valueList, intValue)
import qualified GOOL.Drasil.LanguageRenderer as R (file, module', block, 
  print, stateVar, stateVarList, switch, assign, addAssign, increment, 
  constDecDef, return', comment, getTerm, var, extVar, self, arg, objVar, func, 
  listAccessFunc, objAccess, commentedItem)
import GOOL.Drasil.State (FS, CS, MS, VS, lensFStoGS, lensFStoCS, lensFStoMS, 
  lensCStoMS, lensMStoVS, lensVStoMS, currMain, currFileType, modifyReturnFunc, 
  modifyReturnFunc2, addFile, setMainMod, addLangImportVS, getLangImports, 
  addLibImportVS, getLibImports, getModuleImports, setModuleName, 
  getModuleName, setClassName, getClassName, addParameter, getParameters)

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

multiBody :: (RenderSym r) => [MSBody r] -> MSBody r
multiBody bs = docBody $ onStateList vibcat $ map (onStateValue bodyDoc) bs

-- Blocks --

block :: (RenderSym r) => [MSStatement r] -> MSBlock r
block sts = docBlock $ onStateList (R.block . map statementDoc) 
  (map S.stmt sts)

multiBlock :: (RenderSym r) => [MSBlock r] -> MSBlock r
multiBlock bs = docBlock $ onStateList vibcat $ map (onStateValue blockDoc) bs

-- Types --

bool :: (RenderSym r) => VSType r
bool = toState $ typeFromData Boolean "Boolean" (text "Boolean")

int :: (RenderSym r) => VSType r
int = toState $ typeFromData Integer "int" (text "int")

float :: (RenderSym r) => VSType r
float = toState $ typeFromData Float "float" (text "float")

double :: (RenderSym r) => VSType r
double = toState $ typeFromData Double "double" (text "double")

char :: (RenderSym r) => VSType r
char = toState $ typeFromData Char "char" (text "char")

string :: (RenderSym r) => VSType r
string = toState $ typeFromData String "string" (text "string")

fileType :: (RenderSym r) => VSType r
fileType = toState $ typeFromData File "File" (text "File")

listType :: (RenderSym r) => String -> VSType r -> VSType r
listType lst = onStateValue (\t -> typeFromData (List (getType t)) (lst ++ "<" 
  ++ getTypeString t ++ ">") (text lst <> angles (getTypeDoc t)))

arrayType :: (RenderSym r) => VSType r -> VSType r
arrayType = onStateValue (\t -> typeFromData (Array (getType t)) 
  (getTypeString t ++ "[]") (getTypeDoc t <> brackets empty)) 

listInnerType :: (RenderSym r) => VSType r -> VSType r
listInnerType t = t >>= (convType . getInnerType . getType)

obj :: (RenderSym r) => ClassName -> VSType r
obj n = toState $ typeFromData (Object n) n (text n)

funcType :: (RenderSym r) => [VSType r] -> VSType r -> VSType r
funcType ps' = on2StateValues (\ps r -> typeFromData (Func (map getType ps) 
  (getType r)) "" empty) (sequence ps')

void :: (RenderSym r) => VSType r
void = toState $ typeFromData Void "void" (text "void")

-- ControlBlock --

strat :: (RenderSym r) => MSStatement r -> MSBody r -> MSBlock r
strat r bd = docBlock $ on2StateValues (\result b -> vcat [bodyDoc b, 
  statementDoc result]) r bd

runStrategy :: (RenderSym r) => String -> [(Label, MSBody r)] -> 
  Maybe (SValue r) -> Maybe (SVariable r) -> MSBlock r
runStrategy l strats rv av = maybe
  (strError l "RunStrategy called on non-existent strategy") 
  (strat (S.stmt resultState)) (Map.lookup l (Map.fromList strats))
  where resultState = maybe S.emptyStmt asgState av
        asgState v = maybe (strError l 
          "Attempt to assign null return to a Value") (v &=) rv
        strError n s = error $ "Strategy '" ++ n ++ "': " ++ s ++ "."

listSlice :: (RenderSym r) => Maybe (SValue r) -> Maybe (SValue r) -> 
  Maybe (SValue r) -> SVariable r -> SValue r -> MSBlock r
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
        (oneLiner $ S.valStmt $ S.listAppend v_temp (S.listAccess vold v_i)),
      vnew &= v_temp]

-- Unary Operators --

unOpPrec :: (RenderSym r) => String -> VSUnOp r
unOpPrec = uOpFromData 9 . text

notOp :: (RenderSym r) => VSUnOp r
notOp = unOpPrec "!"

notOp' :: (RenderSym r) => VSUnOp r
notOp' = unOpPrec "not"

negateOp :: (RenderSym r) => VSUnOp r
negateOp = unOpPrec "-"

sqrtOp :: (RenderSym r) => VSUnOp r
sqrtOp = unOpPrec "sqrt"

sqrtOp' :: (RenderSym r) => VSUnOp r
sqrtOp' = addmathImport $ unOpPrec "math.sqrt"

absOp :: (RenderSym r) => VSUnOp r
absOp = unOpPrec "fabs"

absOp' :: (RenderSym r) => VSUnOp r
absOp' = addmathImport $ unOpPrec "math.fabs"

expOp :: (RenderSym r) => VSUnOp r
expOp = unOpPrec "exp"

expOp' :: (RenderSym r) => VSUnOp r
expOp' = addmathImport $ unOpPrec "math.exp"

sinOp :: (RenderSym r) => VSUnOp r
sinOp = unOpPrec "sin"

sinOp' :: (RenderSym r) => VSUnOp r
sinOp' = addmathImport $ unOpPrec "math.sin"

cosOp :: (RenderSym r) => VSUnOp r
cosOp = unOpPrec "cos"

cosOp' :: (RenderSym r) => VSUnOp r
cosOp' = addmathImport $ unOpPrec "math.cos"

tanOp :: (RenderSym r) => VSUnOp r
tanOp = unOpPrec "tan"

tanOp' :: (RenderSym r) => VSUnOp r
tanOp' = addmathImport $ unOpPrec "math.tan"

asinOp :: (RenderSym r) => VSUnOp r
asinOp = unOpPrec "asin"

asinOp' :: (RenderSym r) => VSUnOp r
asinOp' = addmathImport $ unOpPrec "math.asin"

acosOp :: (RenderSym r) => VSUnOp r
acosOp = unOpPrec "acos"

acosOp' :: (RenderSym r) => VSUnOp r
acosOp' = addmathImport $ unOpPrec "math.acos"

atanOp :: (RenderSym r) => VSUnOp r
atanOp = unOpPrec "atan"

atanOp' :: (RenderSym r) => VSUnOp r
atanOp' = addmathImport $ unOpPrec "math.atan"

csc :: (RenderSym r) => SValue r -> SValue r
csc v = valOfOne (fmap valueType v) #/ sin v

sec :: (RenderSym r) => SValue r -> SValue r
sec v = valOfOne (fmap valueType v) #/ cos v

cot :: (RenderSym r) => SValue r -> SValue r
cot v = valOfOne (fmap valueType v) #/ tan v

valOfOne :: (RenderSym r) => VSType r -> SValue r
valOfOne t = t >>= (getVal . getType)
  where getVal Float = litFloat 1.0
        getVal _ = litDouble 1.0

unOpDocD :: Doc -> Doc -> Doc
unOpDocD op v = op <> parens v

unOpDocD' :: Doc -> Doc -> Doc
unOpDocD' op v = op <> v

unExpr :: (RenderSym r) => VSUnOp r -> SValue r -> SValue r
unExpr = on2StateValues (mkUnExpr unOpDocD)

unExpr' :: (RenderSym r) => VSUnOp r -> SValue r -> SValue r
unExpr' = on2StateValues (mkUnExpr unOpDocD')

mkUnExpr :: (RenderSym r) => (Doc -> Doc -> Doc) -> r (UnaryOp r) -> 
  r (Value r) -> r (Value r)
mkUnExpr d u v = mkExpr (uOpPrec u) (valueType v) (d (uOpDoc u) (valueDoc v))

unExprNumDbl :: (RenderSym r) => VSUnOp r -> SValue r -> SValue r
unExprNumDbl u' v' = u' >>= (\u -> v' >>= (\v -> 
  unExprCastFloat (valueType v) $ return $ mkUnExpr unOpDocD u v))

unExprCastFloat :: (RenderSym r) => r (Type r) -> (SValue r -> SValue r)
unExprCastFloat t = castType $ getType t
  where castType Float = cast float
        castType _ = id
  
typeUnExpr :: (RenderSym r) => VSUnOp r -> VSType r -> SValue r -> SValue r
typeUnExpr = on3StateValues (\u t -> mkExpr (uOpPrec u) t . unOpDocD (uOpDoc u) 
  . valueDoc)

-- Binary Operators --

compEqualPrec :: (RenderSym r) => String -> VSBinOp r
compEqualPrec = bOpFromData 4 . text

compPrec :: (RenderSym r) => String -> VSBinOp r
compPrec = bOpFromData 5 . text

addPrec :: (RenderSym r) => String -> VSBinOp r
addPrec = bOpFromData 6 . text

multPrec :: (RenderSym r) => String -> VSBinOp r
multPrec = bOpFromData 7 . text

powerPrec :: (RenderSym r) => String -> VSBinOp r
powerPrec = bOpFromData 8 . text

andPrec :: (RenderSym r) => String -> VSBinOp r 
andPrec = bOpFromData 3 . text

orPrec :: (RenderSym r) => String -> VSBinOp r
orPrec = bOpFromData 2 . text

equalOp :: (RenderSym r) => VSBinOp r
equalOp = compEqualPrec "=="

notEqualOp :: (RenderSym r) => VSBinOp r
notEqualOp = compEqualPrec "!="

greaterOp :: (RenderSym r) => VSBinOp r
greaterOp = compPrec ">"

greaterEqualOp :: (RenderSym r) => VSBinOp r
greaterEqualOp = compPrec ">="

lessOp :: (RenderSym r) => VSBinOp r
lessOp = compPrec "<"

lessEqualOp :: (RenderSym r) => VSBinOp r
lessEqualOp = compPrec "<="

plusOp :: (RenderSym r) => VSBinOp r
plusOp = addPrec "+"

minusOp :: (RenderSym r) => VSBinOp r
minusOp = addPrec "-"

multOp :: (RenderSym r) => VSBinOp r
multOp = multPrec "*"

divideOp :: (RenderSym r) => VSBinOp r
divideOp = multPrec "/"

moduloOp :: (RenderSym r) => VSBinOp r
moduloOp = multPrec "%"

powerOp :: (RenderSym r) => VSBinOp r
powerOp = powerPrec "pow"

andOp :: (RenderSym r) => VSBinOp r
andOp = andPrec "&&"

orOp :: (RenderSym r) => VSBinOp r
orOp = orPrec "||"

binOpDocD :: Doc -> Doc -> Doc -> Doc
binOpDocD op v1 v2 = v1 <+> op <+> v2

binOpDocD' :: Doc -> Doc -> Doc -> Doc
binOpDocD' op v1 v2 = op <> parens (v1 <> comma <+> v2)

binExpr :: (RenderSym r) => VSBinOp r -> SValue r -> SValue r -> SValue r
binExpr = on3StateValues (\b v1 v2 -> mkExpr (bOpPrec b) (numType (valueType v1)
  (valueType v2)) (binOpDocD (bOpDoc b) (exprParensL b v1 $ valueDoc v1) 
  (exprParensR b v2 $ valueDoc v2)))

binExpr' :: (RenderSym r) => VSBinOp r -> SValue r -> SValue r -> SValue r
binExpr' = on3StateValues (\b v1 v2 -> mkExpr 9 (numType (valueType v1) 
  (valueType v2)) (binOpDocD' (bOpDoc b) (valueDoc v1) (valueDoc v2)))

binExprNumDbl' :: (RenderSym r) => VSBinOp r -> SValue r -> SValue r -> SValue r
binExprNumDbl' b' v1' v2' = b' >>= (\b -> v1' >>= (\v1 -> v2' >>= (\v2 -> 
  let t1 = valueType v1
      t2 = valueType v2
  in binExprCastFloat t1 t2 $ return $ mkExpr 9 (numType t1 t2) 
  (binOpDocD' (bOpDoc b) (valueDoc v1) (valueDoc v2)))))

binExprCastFloat :: (RenderSym r) => r (Type r) -> r (Type r) ->
  (SValue r -> SValue r)
binExprCastFloat t1 t2 = castType (getType t1) (getType t2)
  where castType Float _ = cast float
        castType _ Float = cast float
        castType _ _ = id

typeBinExpr :: (RenderSym r) => VSBinOp r -> VSType r -> SValue r -> SValue r 
  -> SValue r
typeBinExpr bod tp vl1 vl2 = (\b t v1 v2 -> mkExpr (bOpPrec b) t (binOpDocD 
  (bOpDoc b) (exprParensL b v1 $ valueDoc v1) (exprParensR b v2 $ valueDoc v2)))
  <$> bod <*> tp <*> vl1 <*> vl2 

addmathImport :: VS a -> VS a
addmathImport = (>>) $ modify (addLangImportVS "math")

-- Binding --

bindingError :: String -> String
bindingError l = "Binding unimplemented in " ++ l

-- Variables --

var :: (RenderSym r) => Label -> VSType r -> SVariable r
var n t = mkStateVar n t (R.var n)

staticVar :: (RenderSym r) => Label -> VSType r -> SVariable r
staticVar n t = mkStaticVar n t (R.var n)

extVar :: (RenderSym r) => Label -> Label -> VSType r -> SVariable r
extVar l n t = mkStateVar (l ++ "." ++ n) t (R.extVar l n)

self :: (RenderSym r) => SVariable r
self = zoom lensVStoMS getClassName >>= (\l -> mkStateVar "this" (obj l) R.self)

classVarCheckStatic :: (RenderSym r) => r (Variable r) -> r (Variable r)
classVarCheckStatic v = classVarCS (variableBind v)
  where classVarCS Dynamic = error
          "classVar can only be used to access static variables"
        classVarCS Static = v

classVar :: (RenderSym r) => (Doc -> Doc -> Doc) -> VSType r -> SVariable r -> 
  SVariable r
classVar f = on2StateValues (\c v -> classVarCheckStatic $ varFromData 
  (variableBind v) (getTypeString c ++ "." ++ variableName v) 
  (variableType v) (f (getTypeDoc c) (variableDoc v)))

objVar :: (RenderSym r) => SVariable r -> SVariable r -> SVariable r
objVar = on2StateValues (\o v -> mkVar (variableName o ++ "." ++ variableName 
  v) (variableType v) (R.objVar (variableDoc o) (variableDoc v)))

objVarSelf :: (RenderSym r) => SVariable r -> SVariable r
objVarSelf = S.objVar S.self

listVar :: (RenderSym r) => Label -> VSType r -> SVariable r
listVar n t = S.var n (S.listType t)

arrayElem :: (RenderSym r) => SValue r -> SVariable r -> SVariable r
arrayElem i' v' = join $ on2StateValues (\i v -> mkStateVar (variableName v ++ 
  "[" ++ render (valueDoc i) ++ "]") (listInnerType $ toState $ variableType v) 
  (variableDoc v <> brackets (valueDoc i))) i' v'

iterVar :: (RenderSym r) => Label -> VSType r -> SVariable r
iterVar n t = S.var n (iterator t)

-- Values --

litTrue :: (RenderSym r) => SValue r
litTrue = mkStateVal S.bool (text "true")

litFalse :: (RenderSym r) => SValue r
litFalse = mkStateVal S.bool (text "false")

litChar :: (RenderSym r) => Char -> SValue r
litChar c = mkStateVal S.char (quotes $ D.char c)

litDouble :: (RenderSym r) => Double -> SValue r
litDouble d = mkStateVal S.double (D.double d)

litFloat :: (RenderSym r) => Float -> SValue r
litFloat f = mkStateVal S.float (D.float f <> text "f")

litInt :: (RenderSym r) => Integer -> SValue r
litInt i = mkStateVal S.int (integer i)

litString :: (RenderSym r) => String -> SValue r
litString s = mkStateVal S.string (doubleQuotedText s)

litArray :: (RenderSym r) => VSType r -> [SValue r] -> SValue r
litArray t es = sequence es >>= (\elems -> mkStateVal (S.arrayType t) 
  (braces $ valueList elems))

litList :: (RenderSym r) => (VSType r -> VSType r) -> VSType r -> [SValue r] -> 
  SValue r
litList f t = on1StateValue1List (\lt es -> mkVal lt (new <+> getTypeDoc lt <+> 
  braces (valueList es))) (f t)

pi :: (RenderSym r) => SValue r
pi = mkStateVal S.double (text "Math.PI")

valueOf :: (RenderSym r) => SVariable r -> SValue r
valueOf = onStateValue (\v -> mkVal (variableType v) (variableDoc v))

arg :: (RenderSym r) => SValue r -> SValue r -> SValue r
arg = on3StateValues (\s n args -> mkVal s (R.arg n args)) S.string

argsList :: (RenderSym r) => String -> SValue r
argsList l = mkStateVal (S.arrayType S.string) (text l)

inlineIf :: (RenderSym r) => SValue r -> SValue r -> SValue r -> SValue r
inlineIf = on3StateValues (\c v1 v2 -> valFromData (prec c) (valueType v1) 
  (valueDoc c <+> text "?" <+> valueDoc v1 <+> text ":" <+> valueDoc v2)) 
  where prec cd = valuePrec cd <|> Just 0

call' :: (RenderSym r) => String -> Maybe Library -> Label -> VSType r -> 
  Maybe Doc -> [SValue r] -> NamedArgs r -> SValue r
call' l _ _ _ _ _ (_:_) = error $ namedArgError l
call' _ l n t o ps ns = call empty l n t o ps ns

call :: (RenderSym r) => Doc -> Maybe Library -> Label -> VSType r -> 
  Maybe Doc -> [SValue r] -> NamedArgs r -> SValue r
call sep lib n t o pas nas = (\tp pargs nms nargs -> mkVal tp $ obDoc <> libDoc 
  <> text n <> parens (valueList pargs <+> (if null pas || null nas then empty 
  else comma) <+> hcat (intersperse (text ", ") (zipWith (\nm a -> 
  variableDoc nm <> sep <> valueDoc a) nms nargs)))) <$> t <*> sequence pas <*> 
  mapM fst nas <*> mapM snd nas
  where libDoc = maybe empty (text . (++ ".")) lib
        obDoc = fromMaybe empty o

funcAppMixedArgs :: (RenderSym r) => MixedCall r
funcAppMixedArgs n t = S.call Nothing n t Nothing

namedArgError :: String -> String
namedArgError l = "Named arguments not supported in " ++ l 

selfFuncAppMixedArgs :: (RenderSym r) => Doc -> SVariable r -> MixedCall r
selfFuncAppMixedArgs d slf n t vs ns = slf >>= (\s -> S.call Nothing n t 
  (Just $ variableDoc s <> d) vs ns)

extFuncAppMixedArgs :: (RenderSym r) => Library -> MixedCall r
extFuncAppMixedArgs l n t = S.call (Just l) n t Nothing

libFuncAppMixedArgs :: (RenderSym r) => Library -> MixedCall r
libFuncAppMixedArgs l n t vs ns = modify (addLibImportVS l) >> 
  S.funcAppMixedArgs n t vs ns

newObjMixedArgs :: (RenderSym r) => String -> MixedCtorCall r
newObjMixedArgs s tp vs ns = tp >>= 
  (\t -> S.call Nothing (s ++ getTypeString t) (return t) Nothing vs ns)

extNewObjMixedArgs :: (RenderSym r) => Library -> MixedCtorCall r
extNewObjMixedArgs l tp vs ns = tp >>= (\t -> S.call (Just l) (getTypeString t) 
  (return t) Nothing vs ns)

libNewObjMixedArgs :: (RenderSym r) => Library -> MixedCtorCall r
libNewObjMixedArgs l tp vs ns = modify (addLibImportVS l) >> 
  S.newObjMixedArgs tp vs ns

notNull :: (RenderSym r) => SValue r -> SValue r
notNull v = v ?!= S.valueOf (S.var "null" $ onStateValue valueType v)

lambda :: (RenderSym r) => ([r (Variable r)] -> r (Value r) -> Doc) -> 
  [SVariable r] -> SValue r -> SValue r
lambda f ps' ex' = sequence ps' >>= (\ps -> ex' >>= (\ex -> funcType (map 
  (toState . variableType) ps) (toState $ valueType ex) >>= (\ft -> 
  toState $ valFromData (Just 0) ft (f ps ex))))

objAccess :: (RenderSym r) => SValue r -> VSFunction r -> SValue r
objAccess = on2StateValues (\v f -> mkVal (functionType f) (R.objAccess 
  (valueDoc v) (functionDoc f)))

objMethodCall :: (RenderSym r) => Label -> VSType r -> SValue r -> [SValue r] 
  -> NamedArgs r -> SValue r
objMethodCall f t ob vs ns = ob >>= (\o -> S.call Nothing f t 
  (Just $ valueDoc o <> dot) vs ns)

objMethodCallNoParams :: (RenderSym r) => Label -> VSType r -> SValue r -> 
  SValue r
objMethodCallNoParams f t o = S.objMethodCall t o f []

indexOf :: (RenderSym r) => Label -> SValue r -> SValue r -> SValue r
indexOf f l v = S.objAccess l (S.func f S.int [v])

-- Functions --

func :: (RenderSym r) => Label -> VSType r -> [SValue r] -> VSFunction r
func l t vs = funcApp l t vs >>= ((`funcFromData` t) . R.func . valueDoc)

get :: (RenderSym r) => SValue r -> SVariable r -> SValue r
get v vToGet = v $. S.getFunc vToGet

set :: (RenderSym r) => SValue r -> SVariable r -> SValue r -> SValue r
set v vToSet toVal = v $. S.setFunc (onStateValue valueType v) vToSet toVal

listSize :: (RenderSym r) => SValue r -> SValue r
listSize v = v $. S.listSizeFunc

listAdd :: (RenderSym r) => SValue r -> SValue r -> SValue r -> SValue r
listAdd v i vToAdd = v $. S.listAddFunc v i vToAdd

listAppend :: (RenderSym r) => SValue r -> SValue r -> SValue r
listAppend v vToApp = v $. S.listAppendFunc vToApp

iterBegin :: (RenderSym r) => SValue r -> SValue r
iterBegin v = v $. iterBeginFunc (S.listInnerType $ onStateValue valueType v)

iterEnd :: (RenderSym r) => SValue r -> SValue r
iterEnd v = v $. iterEndFunc (S.listInnerType $ onStateValue valueType v)

listAccess :: (RenderSym r) => SValue r -> SValue r -> SValue r
listAccess v i = do
  v' <- v
  let checkType (List _) = S.listAccessFunc (S.listInnerType $ return $ 
        valueType v') i
      checkType (Array _) = i >>= (\ix -> funcFromData (brackets (valueDoc ix)) 
        (S.listInnerType $ return $ valueType v'))
      checkType _ = error "listAccess called on non-list-type value"
  v $. checkType (getType (valueType v'))

listSet :: (RenderSym r) => SValue r -> SValue r -> SValue r -> SValue r
listSet v i toVal = v $. S.listSetFunc v i toVal

getFunc :: (RenderSym r) => SVariable r -> VSFunction r
getFunc v = v >>= (\vr -> S.func (getterName $ variableName vr) 
  (toState $ variableType vr) [])

setFunc :: (RenderSym r) => VSType r -> SVariable r -> SValue r -> VSFunction r
setFunc t v toVal = v >>= (\vr -> S.func (setterName $ variableName vr) t 
  [toVal])

listSizeFunc :: (RenderSym r) => VSFunction r
listSizeFunc = S.func "size" S.int []

listAddFunc :: (RenderSym r) => Label -> SValue r -> SValue r -> VSFunction r
listAddFunc f i v = S.func f (S.listType $ onStateValue valueType v) 
  [i, v]

listAppendFunc :: (RenderSym r) => Label -> SValue r -> VSFunction r
listAppendFunc f v = S.func f (S.listType $ onStateValue valueType v) [v]

iterBeginError :: String -> String
iterBeginError l = "Attempt to use iterBeginFunc in " ++ l ++ ", but " ++ l ++ 
  " has no iterators"

iterEndError :: String -> String
iterEndError l = "Attempt to use iterEndFunc in " ++ l ++ ", but " ++ l ++ 
  " has no iterators"

listAccessFunc :: (RenderSym r) => VSType r -> SValue r -> VSFunction r
listAccessFunc t v = intValue v >>= ((`funcFromData` t) . R.listAccessFunc)

listAccessFunc' :: (RenderSym r) => Label -> VSType r -> SValue r -> 
  VSFunction r
listAccessFunc' f t i = S.func f t [intValue i]

listSetFunc :: (RenderSym r) => (Doc -> Doc -> Doc) -> SValue r -> SValue r -> 
  SValue r -> VSFunction r
listSetFunc f v idx setVal = join $ on2StateValues (\i toVal -> funcFromData 
  (f (valueDoc i) (valueDoc toVal)) (onStateValue valueType v)) (intValue idx) 
  setVal

-- Statements --

printSt :: (RenderSym r) => SValue r -> SValue r -> MSStatement r
printSt p v = zoom lensMStoVS $ on2StateValues (\p' -> mkSt . R.print p') p v

stmt :: (RenderSym r) => MSStatement r -> MSStatement r
stmt = onStateValue (\s -> mkStNoEnd (statementDoc s <> R.getTerm 
  (statementTerm s)))
  
loopStmt :: (RenderSym r) => MSStatement r -> MSStatement r
loopStmt = S.stmt . setEmpty

emptyStmt :: (RenderSym r) => MSStatement r
emptyStmt = toState $ mkStNoEnd empty

assign :: (RenderSym r) => Terminator -> SVariable r -> SValue r -> 
  MSStatement r
assign t vr vl = zoom lensMStoVS $ on2StateValues (\vr' vl' -> stmtFromData 
  (R.assign vr' vl') t) vr vl

multiAssignError :: String -> String
multiAssignError l = "No multiple assignment statements in " ++ l

decrement :: (RenderSym r) => SVariable r -> SValue r -> MSStatement r
decrement vr vl = vr &= (S.valueOf vr #- vl)

increment :: (RenderSym r) => SVariable r -> SValue r -> MSStatement r
increment vr vl = zoom lensMStoVS $ on2StateValues (\vr' -> mkSt . 
  R.addAssign vr') vr vl

increment1 :: (RenderSym r) => SVariable r -> MSStatement r
increment1 vr = zoom lensMStoVS $ onStateValue (mkSt . R.increment) vr

increment' :: (RenderSym r) => SVariable r -> SValue r -> MSStatement r
increment' vr vl = vr &= S.valueOf vr #+ vl

increment1' :: (RenderSym r) => SVariable r -> MSStatement r
increment1' vr = vr &= S.valueOf vr #+ S.litInt 1

decrement1 :: (RenderSym r) => SVariable r -> MSStatement r
decrement1 v = v &= (S.valueOf v #- S.litInt 1)

varDec :: (RenderSym r) => r (Permanence r) -> r (Permanence r) -> SVariable r 
  -> MSStatement r
varDec s d v' = onStateValue (\v -> mkSt (permDoc (bind $ variableBind v) 
  <+> getTypeDoc (variableType v) <+> variableDoc v)) (zoom lensMStoVS v')
  where bind Static = s
        bind Dynamic = d

varDecDef :: (RenderSym r) => SVariable r -> SValue r -> MSStatement r
varDecDef vr vl' = on2StateValues (\vd vl -> mkSt (statementDoc vd <+> equals 
  <+> valueDoc vl)) (S.varDec vr) (zoom lensMStoVS vl')

listDec :: (RenderSym r) => (r (Value r) -> Doc) -> SValue r -> SVariable r -> 
  MSStatement r
listDec f vl v = on2StateValues (\sz vd -> mkSt (statementDoc vd <> f 
  sz)) (zoom lensMStoVS vl) (S.varDec v)

listDecDef :: (RenderSym r) => ([r (Value r)] -> Doc) -> SVariable r -> 
  [SValue r] -> MSStatement r
listDecDef f v vls = on1StateValue1List (\vd vs -> mkSt (statementDoc vd <> 
  f vs)) (S.varDec v) (map (zoom lensMStoVS) vls)

listDecDef' :: (RenderSym r) => SVariable r -> [SValue r] -> MSStatement r
listDecDef' v vals = zoom lensMStoVS v >>= (\vr -> S.varDecDef (return vr) 
  (S.litList (listInnerType $ return $ variableType vr) vals))

arrayDec :: (RenderSym r) => SValue r -> SVariable r -> MSStatement r
arrayDec n vr = zoom lensMStoVS $ on3StateValues (\sz v it -> mkSt (getTypeDoc 
  (variableType v) <+> variableDoc v <+> equals <+> new <+> getTypeDoc it <> 
  brackets (valueDoc sz))) n vr (listInnerType $ onStateValue variableType vr)

arrayDecDef :: (RenderSym r) => SVariable r -> [SValue r] -> MSStatement r
arrayDecDef v vals = on2StateValues (\vd vs -> mkSt (statementDoc vd <+> 
  equals <+> braces (valueList vs))) (S.varDec v) (mapM (zoom lensMStoVS) vals)

objDecNew :: (RenderSym r) => SVariable r -> [SValue r] -> MSStatement r
objDecNew v vs = S.varDecDef v (newObj (onStateValue variableType v) vs)

objDecNewNoParams :: (RenderSym r) => SVariable r -> MSStatement r
objDecNewNoParams v = S.objDecNew v []

extObjDecNew :: (RenderSym r) => Library -> SVariable r -> [SValue r] -> 
  MSStatement r
extObjDecNew l v vs = S.varDecDef v (extNewObj l (onStateValue variableType v)
  vs)

extObjDecNewNoParams :: (RenderSym r) => Library -> SVariable r -> MSStatement r
extObjDecNewNoParams l v = S.extObjDecNew l v []

constDecDef :: (RenderSym r) => SVariable r -> SValue r -> MSStatement r
constDecDef vr vl = zoom lensMStoVS $ on2StateValues (\v -> mkSt . 
  R.constDecDef v) vr vl

funcDecDef :: (RenderSym r) => SVariable r -> [SVariable r] -> SValue r -> 
  MSStatement r
funcDecDef v ps r = S.varDecDef v (S.lambda ps r)

printList :: (RenderSym r) => Integer -> SValue r -> (SValue r -> MSStatement r)
  -> (String -> MSStatement r) -> (String -> MSStatement r) -> MSStatement r
printList n v prFn prStrFn prLnFn = multi [prStrFn "[", 
  S.for (S.varDecDef i (S.litInt 0)) 
    (S.valueOf i ?< (S.listSize v #- S.litInt 1)) (i &++) 
    (bodyStatements [prFn (S.listAccess v (S.valueOf i)), prStrFn ", "]), 
  ifNoElse [(S.listSize v ?> S.litInt 0, oneLiner $
    prFn (S.listAccess v (S.listSize v #- S.litInt 1)))], 
  prLnFn "]"]
  where l_i = "list_i" ++ show n
        i = S.var l_i S.int

printObj :: ClassName -> (String -> MSStatement r) -> MSStatement r
printObj n prLnFn = prLnFn $ "Instance of " ++ n ++ " object"

print :: (RenderSym r) => Bool -> Maybe (SValue r) -> SValue r -> SValue r -> 
  MSStatement r
print newLn f printFn v = zoom lensMStoVS v >>= print' . getType . valueType
  where print' (List t) = printList (getNestDegree 1 t) v prFn prStrFn 
          prLnFn
        print' (Object n) = printObj n prLnFn
        print' _ = S.printSt newLn f printFn v
        prFn = maybe S.print printFile f
        prStrFn = maybe printStr printFileStr f
        prLnFn = if newLn then maybe printStrLn printFileStrLn f else maybe 
          printStr printFileStr f 

discardInput :: (RenderSym r) => (r (Value r) -> Doc) -> MSStatement r
discardInput f = zoom lensMStoVS $ onStateValue (mkSt . f) inputFunc

discardFileInput :: (RenderSym r) => (r (Value r) -> Doc) -> SValue r -> 
  MSStatement r
discardFileInput f v = zoom lensMStoVS $ onStateValue (mkSt . f) v

openFileR :: (RenderSym r) => (SValue r -> VSType r -> SValue r) -> SVariable r 
  -> SValue r -> MSStatement r
openFileR f vr vl = vr &= f vl infile

openFileW :: (RenderSym r) => (SValue r -> VSType r -> SValue r -> SValue r) -> 
  SVariable r -> SValue r -> MSStatement r
openFileW f vr vl = vr &= f vl outfile S.litFalse

openFileA :: (RenderSym r) => (SValue r -> VSType r -> SValue r -> SValue r) -> 
  SVariable r -> SValue r -> MSStatement r
openFileA f vr vl = vr &= f vl outfile S.litTrue

closeFile :: (RenderSym r) => Label -> SValue r -> MSStatement r
closeFile n f = S.valStmt $ S.objMethodCallNoParams S.void f n

discardFileLine :: (RenderSym r) => Label -> SValue r -> MSStatement r
discardFileLine n f = S.valStmt $ S.objMethodCallNoParams S.string f n 

stringListVals :: (RenderSym r) => [SVariable r] -> SValue r -> MSStatement r
stringListVals vars sl = zoom lensMStoVS sl >>= (\slst -> multi $ checkList 
  (getType $ valueType slst))
  where checkList (List String) = assignVals vars 0
        checkList _ = error 
          "Value passed to stringListVals must be a list of strings"
        assignVals [] _ = []
        assignVals (v:vs) n = S.assign v (cast (onStateValue variableType v) 
          (S.listAccess sl (S.litInt n))) : assignVals vs (n+1)

stringListLists :: (RenderSym r) => [SVariable r] -> SValue r -> MSStatement r
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
        appendLists (v:vs) n = S.valStmt (S.listAppend v (cast 
          (S.listInnerType $ onStateValue valueType v)
          (S.listAccess sl ((v_i #* numLists) #+ S.litInt n)))) 
          : appendLists vs (n+1)
        numLists = S.litInt (toInteger $ length lsts)
        var_i = S.var "stringlist_i" S.int
        v_i = S.valueOf var_i

returnStmt :: (RenderSym r) => Terminator -> SValue r -> MSStatement r
returnStmt t v' = zoom lensMStoVS $ onStateValue (\v -> stmtFromData 
  (R.return' [v]) t) v'

multiReturnError :: String -> String
multiReturnError l = "Cannot return multiple values in " ++ l

valStmt :: (RenderSym r) => Terminator -> SValue r -> MSStatement r
valStmt t v' = zoom lensMStoVS $ onStateValue (\v -> stmtFromData (valueDoc v)
  t) v'

comment :: (RenderSym r) => Doc -> Label -> MSStatement r
comment cs c = toState $ mkStNoEnd (R.comment c cs)

throw :: (RenderSym r) => (r (Value r) -> Doc) -> Terminator -> Label -> 
  MSStatement r
throw f t = onStateValue (\msg -> stmtFromData (f msg) t) . zoom lensMStoVS . 
  S.litString

-- ControlStatements --

ifCond :: (RenderSym r) => Doc -> Doc -> Doc -> [(SValue r, MSBody r)] -> 
  MSBody r -> MSStatement r
ifCond _ _ _ [] _ = error "if condition created with no cases"
ifCond ifStart elif bEnd (c:cs) eBody =
    let ifSect (v, b) = on2StateValues (\val bd -> vcat [
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

switch :: (RenderSym r) => SValue r -> [(SValue r, MSBody r)] -> MSBody r -> 
  MSStatement r
switch v cs bod = (\b val css de -> mkSt (R.switch b val de css)) <$>
  S.stmt break <*> zoom lensMStoVS v <*> on2StateLists zip (map (zoom 
  lensMStoVS . fst) cs) (map snd cs) <*> bod

ifExists :: (RenderSym r) => SValue r -> MSBody r -> MSBody r -> MSStatement r
ifExists v ifBody = S.ifCond [(S.notNull v, ifBody)]

for :: (RenderSym r) => Doc -> Doc -> MSStatement r -> SValue r -> 
  MSStatement r -> MSBody r -> MSStatement r
for bStart bEnd sInit vGuard sUpdate b = (\initl guard upd bod -> mkStNoEnd 
  (vcat [forLabel <+> parens (statementDoc initl <> semi <+> valueDoc guard <> 
    semi <+> statementDoc upd) <+> bStart,
  indent $ bodyDoc bod,
  bEnd])) <$> S.loopStmt sInit <*> zoom lensMStoVS vGuard <*> 
  S.loopStmt sUpdate <*> b

forRange :: (RenderSym r) => SVariable r -> SValue r -> SValue r -> SValue r -> 
  MSBody r -> MSStatement r
forRange i initv finalv stepv = S.for (S.varDecDef i initv) (S.valueOf i ?< 
  finalv) (i &+= stepv)

forEach :: (RenderSym r) => Doc -> Doc -> Doc -> Doc -> SVariable r -> SValue r 
  -> MSBody r -> MSStatement r
forEach bStart bEnd forEachLabel inLbl e' v' = on3StateValues (\e v b -> 
  mkStNoEnd (vcat [forEachLabel <+> parens (getTypeDoc (variableType e) 
    <+> variableDoc e <+> inLbl <+> valueDoc v) <+> bStart,
  indent $ bodyDoc b,
  bEnd])) (zoom lensMStoVS e') (zoom lensMStoVS v') 

while :: (RenderSym r) => Doc -> Doc -> SValue r -> MSBody r -> MSStatement r
while bStart bEnd v' = on2StateValues (\v b -> mkStNoEnd (vcat [
  text "while" <+> parens (valueDoc v) <+> bStart,
  indent $ bodyDoc b,
  bEnd])) (zoom lensMStoVS v')

tryCatch :: (RenderSym r) => (r (Body r) -> r (Body r) -> Doc) -> MSBody r -> 
  MSBody r -> MSStatement r
tryCatch f = on2StateValues (\tb -> mkStNoEnd . f tb)

checkState :: (RenderSym r) => Label -> [(SValue r, MSBody r)] -> MSBody r -> 
  MSStatement r
checkState l = S.switch (S.valueOf $ S.var l S.string)

notifyObservers :: (RenderSym r) => VSFunction r -> VSType r -> MSStatement r
notifyObservers f t = S.for initv (v_index ?< S.listSize obsList) 
  (var_index &++) notify
  where obsList = S.valueOf $ observerListName `listOf` t 
        var_index = S.var "observerIndex" S.int
        v_index = S.valueOf var_index
        initv = S.varDecDef var_index $ S.litInt 0
        notify = oneLiner $ S.valStmt $ at obsList v_index $. f

-- Methods --

construct :: (RenderSym r) => Label -> MS (r (Type r))
construct n = toState $ typeFromData (Object n) n empty

param :: (RenderSym r) => (r (Variable r) -> Doc) -> SVariable r -> 
  MSParameter r
param f v' = modifyReturnFunc (\v s -> addParameter (variableName v) s) 
  (\v -> paramFromData v (f v)) (zoom lensMStoVS v')

method :: (RenderSym r) => Label -> r (Scope r) -> r (Permanence r) -> VSType r 
  -> [MSParameter r] -> MSBody r -> SMethod r
method n s p t = intMethod False n s p (mType t)

getMethod :: (RenderSym r) => SVariable r -> SMethod r
getMethod v = zoom lensMStoVS v >>= (\vr -> S.method (getterName $ variableName 
  vr) public dynamic (toState $ variableType vr) [] getBody)
  where getBody = oneLiner $ S.returnStmt (S.valueOf $ S.objVarSelf v)

setMethod :: (RenderSym r) => SVariable r -> SMethod r
setMethod v = zoom lensMStoVS v >>= (\vr -> S.method (setterName $ variableName 
  vr) public dynamic S.void [S.param v] setBody)
  where setBody = oneLiner $ S.objVarSelf v &= S.valueOf v

constructor :: (RenderSym r) => Label -> [MSParameter r] -> Initializers r -> 
  MSBody r -> SMethod r
constructor fName ps is b = getClassName >>= (\c -> intMethod False fName 
  public dynamic (S.construct c) ps (S.multiBody [ib, b]))
  where ib = bodyStatements (zipWith (\vr vl -> objVarSelf vr &= vl) 
          (map fst is) (map snd is))
 
destructorError :: String -> String
destructorError l = "Destructors not allowed in " ++ l

docMain :: (RenderSym r) => MSBody r -> SMethod r
docMain b = commentedFunc (docComment $ toState $ functionDox 
  "Controls the flow of the program" 
  [("args", "List of command-line arguments")] []) (S.mainFunction b)

function :: (RenderSym r) => Label -> r (Scope r) -> r (Permanence r) -> 
  VSType r -> [MSParameter r] -> MSBody r -> SMethod r
function n s p t = S.intFunc False n s p (mType t)

mainFunction :: (RenderSym r) => VSType r -> Label -> MSBody r -> SMethod r
mainFunction s n = S.intFunc True n public static (mType S.void)
  [S.param (S.var "args" (onStateValue (\argT -> typeFromData (List String) 
  (render (getTypeDoc argT) ++ "[]") (getTypeDoc argT <> text "[]")) s))]
  
docFuncRepr :: (RenderSym r) => String -> [String] -> [String] -> SMethod r -> 
  SMethod r
docFuncRepr desc pComms rComms = commentedFunc (docComment $ onStateValue 
  (\ps -> functionDox desc (zip ps pComms) rComms) getParameters)

docFunc :: (RenderSym r) => String -> [String] -> Maybe String -> SMethod r -> 
  SMethod r
docFunc desc pComms rComm = docFuncRepr desc pComms (maybeToList rComm)

docInOutFunc :: (RenderSym r) => (r (Scope r) -> r (Permanence r) -> 
    [SVariable r] -> [SVariable r] -> [SVariable r] -> MSBody r -> SMethod r)
  -> r (Scope r) -> r (Permanence r) -> String -> [(String, SVariable r)] -> 
  [(String, SVariable r)] -> [(String, SVariable r)] -> MSBody r -> SMethod r
docInOutFunc f s p desc is [o] [] b = docFuncRepr desc (map fst is) [fst o] 
  (f s p (map snd is) [snd o] [] b)
docInOutFunc f s p desc is [] [both] b = docFuncRepr desc (map fst $ both : is) 
  [fst both] (f s p (map snd is) [] [snd both] b)
docInOutFunc f s p desc is os bs b = docFuncRepr desc (map fst $ bs ++ is ++ os)
  [] (f s p (map snd is) (map snd os) (map snd bs) b)

intFunc :: (RenderSym r) => Bool -> Label -> r (Scope r) -> r (Permanence r) -> 
  MSMthdType r -> [MSParameter r] -> MSBody r -> SMethod r
intFunc = intMethod

-- State Variables --

stateVar :: (RenderSym r) => r (Scope r) -> r (Permanence r) -> SVariable r -> 
  CSStateVar r
stateVar s p v = stateVarFromData (zoom lensCStoMS $ onStateValue (R.stateVar 
  (scopeDoc s) (permDoc p) . statementDoc) (S.stmt $ S.varDec v))

stateVarDef :: (RenderSym r) => r (Scope r) -> r (Permanence r) -> SVariable r 
  -> SValue r -> CSStateVar r
stateVarDef s p vr vl = stateVarFromData (zoom lensCStoMS $ onStateValue 
  (R.stateVar (scopeDoc s) (permDoc p) . statementDoc) (S.stmt $ S.varDecDef 
  vr vl))

constVar :: (RenderSym r) => Doc -> r (Scope r) -> SVariable r -> SValue r -> 
  CSStateVar r
constVar p s vr vl = stateVarFromData (zoom lensCStoMS $ onStateValue 
  (R.stateVar (scopeDoc s) p . statementDoc) (S.stmt $ S.constDecDef vr vl))

-- Classes --

buildClass :: (RenderSym r) => Label -> Maybe Label -> [CSStateVar r] -> 
  [SMethod r] -> SClass r
buildClass n = S.intClass n public . inherit

extraClass :: (RenderSym r) => Label -> Maybe Label -> [CSStateVar r] -> 
  [SMethod r] -> SClass r
extraClass n = S.intClass n (scopeFromData Priv empty) . inherit

implementingClass :: (RenderSym r) => Label -> [Label] -> [CSStateVar r] -> 
  [SMethod r] -> SClass r
implementingClass n is = S.intClass n public (implements is)

docClass :: (RenderSym r) => String -> SClass r -> SClass r
docClass d = S.commentedClass (docComment $ toState $ classDox d)

commentedClass :: (RenderSym r, Monad r) => CS (r (BlockComment r)) -> SClass r 
  -> SClass r
commentedClass cmt cs = classFromData (on2StateValues (\cmt' cs' -> toCode $
  R.commentedItem (blockCommentDoc cmt') (classDoc cs')) cmt cs)

intClass :: (RenderSym r, Monad r) => (Label -> Doc -> Doc -> Doc -> Doc -> 
  Doc) -> Label -> r (Scope r) -> r ParentSpec -> [CSStateVar r] -> [SMethod r] 
  -> SClass r
intClass f n s i svrs mths = modify (setClassName n) >> classFromData 
  (on2StateValues (\svs ms -> onCodeValue (\p -> f n p (scopeDoc s) svs ms) i) 
  (onStateList (R.stateVarList . map stateVarDoc) svrs) 
  (onStateList (vibcat . map methodDoc) (map (zoom lensCStoMS) mths)))

-- Modules --

buildModule :: (RenderSym r) => Label -> FS Doc -> FS Doc -> [SMethod r] -> 
  [SClass r] -> FSModule r
buildModule n imps bot ms cs = S.modFromData n ((\cls fs is bt -> 
  R.module' is (vibcat (map classDoc cls)) (vibcat (map methodDoc fs ++ [bt])))
  <$> mapM (zoom lensFStoCS) cs <*> mapM (zoom lensFStoMS) ms <*> imps <*> bot)

buildModule' :: (RenderSym r) => Label -> (String -> r (Import r)) -> [Label] 
  -> [SMethod r] -> [SClass r] -> FSModule r
buildModule' n inc is ms cs = S.modFromData n ((\cls lis libis mis -> vibcat [
    vcat (map (importDoc . inc) (lis ++ sort (is ++ libis) ++ mis)),
    vibcat (map classDoc cls)]) <$>
  mapM (zoom lensFStoCS) (if null ms then cs else S.buildClass n Nothing [] ms 
    : cs) <*> getLangImports <*> getLibImports <*> getModuleImports)

modFromData :: Label -> (Doc -> r (Module r)) -> FS Doc -> FSModule r
modFromData n f d = modify (setModuleName n) >> onStateValue f d

-- Files --

fileDoc :: (RenderSym r) => String -> (r (Module r) -> r (Block r)) -> 
  r (Block r) -> FSModule r -> SFile r
fileDoc ext topb botb = S.fileFromData (onStateValue (addExt ext) 
  getModuleName) . onStateValue (\m -> updateModuleDoc (\d -> emptyIfEmpty d 
  (R.file (blockDoc $ topb m) d (blockDoc botb))) m)

docMod :: (RenderSym r) => String -> String -> [String] -> String -> SFile r -> 
  SFile r
docMod e d a dt = commentedMod (docComment $ moduleDox d a dt . addExt e <$> 
  getModuleName)

fileFromData :: (RenderSym r) => (r (Module r) -> FilePath -> r (File r)) 
  -> FS FilePath -> FSModule r -> SFile r
fileFromData f fp m = modifyReturnFunc2 (\mdl fpath s -> (if isEmpty 
  (moduleDoc mdl) then id else (if s ^. currMain && isSource (s ^. currFileType) then over lensFStoGS 
  (setMainMod fpath) else id) . over lensFStoGS (addFile (s ^. currFileType) fpath)) s) f m fp 

-- Helper functions

setEmpty :: (RenderSym r) => MSStatement r -> MSStatement r
setEmpty = onStateValue (mkStNoEnd . statementDoc)

numType :: (RenderSym r) => r (Type r) -> r (Type r) -> r (Type r)
numType t1 t2 = numericType (getType t1) (getType t2)
  where numericType Integer Integer = t1
        numericType Float _ = t1
        numericType _ Float = t2
        numericType Double _ = t1
        numericType _ Double = t2
        numericType _ _ = error "Numeric types required for numeric expression"

mkExpr :: (RenderSym r) => Int -> r (Type r) -> Doc -> r (Value r)
mkExpr p = valFromData (Just p)

exprParensL :: (RenderSym r) => r (BinaryOp r) -> r (Value r) -> (Doc -> Doc)
exprParensL o v = if maybe False (< bOpPrec o) (valuePrec v) then parens else id

exprParensR :: (RenderSym r) => r (BinaryOp r) -> r (Value r) -> (Doc -> Doc)
exprParensR o v = if maybe False (<= bOpPrec o) (valuePrec v) then parens else 
  id