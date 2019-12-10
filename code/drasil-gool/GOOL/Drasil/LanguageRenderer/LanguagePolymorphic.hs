{-# LANGUAGE PostfixOperators #-}

-- | The structure for a class of renderers is defined here.
module GOOL.Drasil.LanguageRenderer.LanguagePolymorphic (fileFromData, oneLiner,
  block, bool, int, float, double, char, string, fileType, listType, 
  listInnerType, obj, enumType, void, runStrategy, listSlice, pi, inlineIf, 
  printSt, state, loopState, emptyState, assign, assignToListIndex, 
  multiAssignError, decrement, increment, increment', increment1, increment1', 
  decrement1, varDec, varDecDef, listDec, listDecDef, objDecNew, 
  objDecNewNoParams, constDecDef, discardInput, discardFileInput, openFileR, 
  openFileW, openFileA, closeFile, discardFileLine, returnState, 
  multiReturnError, valState, comment, freeError, throw, initState, changeState,
  initObserverList, addObserver, ifCond, ifNoElse, switch, switchAsIf, ifExists,
  for, forRange, forEach, while, tryCatch, checkState, notifyObservers, 
  construct, param, method, getMethod, setMethod,privMethod, pubMethod, 
  constructor, docMain, function, mainFunction, docFunc, docInOutFunc, intFunc, 
  stateVar,stateVarDef, constVar, privMVar, pubMVar, pubGVar, buildClass, enum, 
  privClass, pubClass, docClass, commentedClass, buildModule, buildModule', 
  modFromData, fileDoc, docMod
) where

import Utils.Drasil (indent)

import GOOL.Drasil.CodeType (CodeType(..), isObject)
import GOOL.Drasil.Symantics (Label, KeywordSym(..),
  RenderSym(RenderFile, commentedMod), BlockSym(Block), 
  InternalBlock(..), BodySym(Body, body, bodyStatements, bodyDoc), 
  PermanenceSym(..), InternalPerm(..), 
  TypeSym(Type, infile, outfile, getType, getTypeDoc, getTypeString), 
  InternalType(..), VariableSym(..), 
  ValueSym(Value, litTrue, litFalse, litInt, litString, valueOf, valueDoc, 
    valueType), 
  NumericExpression(..), BooleanExpression(..), 
  ValueExpression(newObj, notNull), InternalValue(..), 
  Selector(($.), objMethodCallNoParams), 
  FunctionSym(Function, listSize, listAdd, listAppend), 
  SelectorFunction(listAccess, listSet, at),
  InternalStatement(statementDoc, statementTerm, stateFromData), 
  StatementSym(Statement, (&=), (&+=), (&++), break), ScopeSym(..),
  InternalScope(..), MethodTypeSym(MethodType, mType), ParameterSym(Parameter), 
  InternalParam(paramFromData), MethodSym(Method), 
  InternalMethod(intMethod, commentedFunc, methodDoc), 
  StateVarSym(StateVar), InternalStateVar(..), ClassSym(Class), 
  InternalClass(..), ModuleSym(Module), InternalMod(moduleDoc, updateModuleDoc),
  BlockComment(..))
import qualified GOOL.Drasil.Symantics as S (InternalFile(fileFromData), 
  BodySym(oneLiner), BlockSym(block), 
  TypeSym(int, float, string, listType, void), 
  InternalStatement(state, loopState, emptyState), 
  StatementSym(varDec, varDecDef, listDec, listDecDef, objDecNew, constDecDef, 
    valState, returnState),
  ControlStatementSym(ifCond, for, switch), MethodTypeSym(construct), 
  ParameterSym(param), 
  MethodSym(method, mainFunction), InternalMethod(intFunc), 
  StateVarSym(stateVar), ClassSym(buildClass, commentedClass), 
  InternalMod(modFromData))
import GOOL.Drasil.Data (Binding(..), Terminator(..), TypeData(..), td, 
  FileType)
import GOOL.Drasil.Helpers (angles, vibcat, emptyIfEmpty, toState, onStateValue,
  on2StateValues, on3StateValues, onStateList, getInnerType, convType)
import GOOL.Drasil.LanguageRenderer (forLabel, observerListName, addExt, 
  blockDocD, assignDocD, plusEqualsDocD, plusPlusDocD, constDecDefDocD, 
  printDoc, returnDocD, getTermDoc, switchDocD, stateVarDocD, stateVarListDocD, 
  methodListDocD, enumDocD, enumElementsDocD, moduleDocD, fileDoc', docFuncRepr,
  commentDocD, commentedItem, functionDox, classDox, moduleDox, getterName, 
  setterName)
import GOOL.Drasil.State (GS, FS, MS, lensFStoGS, lensFStoMS, currMain, 
  putAfter, getPutReturn, getPutReturnFunc2, addFile, setMainMod, setFilePath, 
  getFilePath, setModuleName, getModuleName, getCurrMain, addParameter)

import Prelude hiding (break,print,last,mod,pi,(<>))
import Data.Bifunctor (first)
import Data.Map as Map (lookup, fromList)
import Data.Maybe (fromMaybe, maybeToList)
import Control.Applicative ((<|>))
import Control.Lens ((^.), over)
import Control.Lens.Zoom (zoom)
import Text.PrettyPrint.HughesPJ (Doc, text, empty, render, (<>), (<+>), parens,
  vcat, semi, equals, isEmpty)

oneLiner :: (RenderSym repr) => GS (repr (Statement repr)) -> 
  GS (repr (Body repr))
oneLiner s = bodyStatements [s]

block :: (RenderSym repr) => repr (Keyword repr) -> [GS (repr (Statement repr))]
  -> GS (repr (Block repr))
block end sts = docBlock $ onStateList (blockDocD (keyDoc end) . map 
  statementDoc) (map S.state sts)

bool :: (RenderSym repr) => repr (Type repr)
bool = typeFromData Boolean "Boolean" (text "Boolean")

int :: (RenderSym repr) => repr (Type repr)
int = typeFromData Integer "int" (text "int")

float :: (RenderSym repr) => repr (Type repr)
float = typeFromData Float "float" (text "float")

double :: (RenderSym repr) => repr (Type repr)
double = typeFromData Float "double" (text "double")

char :: (RenderSym repr) => repr (Type repr)
char = typeFromData Char "char" (text "char")

string :: (RenderSym repr) => repr (Type repr)
string = typeFromData String "string" (text "string")

fileType :: (RenderSym repr) => repr (Type repr)
fileType = typeFromData File "File" (text "File")

listType :: (RenderSym repr) => repr (Permanence repr) -> repr (Type repr) -> 
  repr (Type repr)
listType p t = typeFromData (List (getType t)) (render (keyDoc $ list p) ++ 
  "<" ++ getTypeString t ++ ">") (keyDoc (list p) <> angles (getTypeDoc t))

listInnerType :: (RenderSym repr) => repr (Type repr) -> repr (Type repr)
listInnerType = convType . getInnerType . getType

obj :: (RenderSym repr) => Label -> repr (Type repr)
obj n = typeFromData (Object n) n (text n)

enumType :: (RenderSym repr) => Label -> repr (Type repr)
enumType e = typeFromData (Enum e) e (text e)

void :: (RenderSym repr) => repr (Type repr)
void = typeFromData Void "void" (text "void")

strat :: (RenderSym repr) => GS (repr (Statement repr)) -> GS (repr (Body repr))
  -> GS (repr (Block repr))
strat r bd = docBlock $ on2StateValues (\result b -> vcat [bodyDoc b, 
  statementDoc result]) r bd

runStrategy :: (RenderSym repr) => String -> [(Label, GS (repr (Body repr)))] 
  -> Maybe (repr (Value repr)) -> Maybe (repr (Variable repr)) -> 
  GS (repr (Block repr))
runStrategy l strats rv av = maybe
  (strError l "RunStrategy called on non-existent strategy") 
  (strat (S.state resultState)) (Map.lookup l (Map.fromList strats))
  where resultState = maybe S.emptyState asgState av
        asgState v = maybe (strError l 
          "Attempt to assign null return to a Value") (v &=) rv
        strError n s = error $ "Strategy '" ++ n ++ "': " ++ s ++ "."

listSlice :: (RenderSym repr) => repr (Variable repr) -> repr (Value repr) -> 
  Maybe (repr (Value repr)) -> Maybe (repr (Value repr)) ->
  Maybe (repr (Value repr)) -> GS (repr (Block repr))
listSlice vnew vold b e s = 
  let l_temp = "temp"
      var_temp = var l_temp (variableType vnew)
      v_temp = valueOf var_temp
      l_i = "i_temp"
      var_i = var l_i S.int
      v_i = valueOf var_i
  in
    S.block [
      S.listDec 0 var_temp,
      S.for (S.varDecDef var_i (fromMaybe (litInt 0) b)) 
        (v_i ?< fromMaybe (listSize vold) e) (maybe (var_i &++) (var_i &+=) s)
        (S.oneLiner $ S.valState $ listAppend v_temp (listAccess vold v_i)),
      vnew &= v_temp]

pi :: (RenderSym repr) => repr (Value repr)
pi = valFromData Nothing S.float (text "Math.PI")

inlineIf :: (RenderSym repr) => repr (Value repr) -> repr (Value repr) -> 
  repr (Value repr) -> repr (Value repr)
inlineIf c v1 v2 = valFromData prec (valueType v1) (valueDoc c <+> text "?" <+> 
  valueDoc v1 <+> text ":" <+> valueDoc v2)
  where prec = valuePrec c <|> Just 0

printSt :: (RenderSym repr) => repr (Value repr) -> repr (Value repr) -> 
  GS (repr (Statement repr))
printSt p v = toState $ stateFromData (printDoc p v) Semi

state :: (RenderSym repr) => GS (repr (Statement repr)) -> 
  GS (repr (Statement repr))
state = onStateValue (\s -> stateFromData (statementDoc s <> getTermDoc 
  (statementTerm s)) Empty)
  
loopState :: (RenderSym repr) => GS (repr (Statement repr)) -> 
  GS (repr (Statement repr))
loopState = S.state . setEmpty

emptyState :: (RenderSym repr) => GS (repr (Statement repr))
emptyState = toState $ stateFromData empty Empty

assign :: (RenderSym repr) => Terminator -> repr (Variable repr) -> 
  repr (Value repr) -> GS (repr (Statement repr))
assign t vr vl = toState $ stateFromData (assignDocD vr vl) t

assignToListIndex :: (RenderSym repr) => repr (Variable repr) -> 
  repr (Value repr) -> repr (Value repr) -> GS (repr (Statement repr))
assignToListIndex lst index v = S.valState $ listSet (valueOf lst) index v

multiAssignError :: String -> String
multiAssignError l = "No multiple assignment statements in " ++ l

decrement :: (RenderSym repr) => repr (Variable repr) -> repr (Value repr) -> 
  GS (repr (Statement repr))
decrement vr vl = vr &= (valueOf vr #- vl)

increment :: (RenderSym repr) => repr (Variable repr) -> repr (Value repr) -> 
  GS (repr (Statement repr))
increment vr vl = toState $ stateFromData (plusEqualsDocD vr vl) Semi

increment1 :: (RenderSym repr) => repr (Variable repr) -> 
  GS (repr (Statement repr))
increment1 v = toState $ stateFromData (plusPlusDocD v) Semi

increment' :: (RenderSym repr) => repr (Variable repr) -> repr (Value repr) -> 
  GS (repr (Statement repr))
increment' vr vl = vr &= valueOf vr #+ vl

increment1' :: (RenderSym repr) => repr (Variable repr) -> 
  GS (repr (Statement repr))
increment1' vr = vr &= valueOf vr #+ litInt 1

decrement1 :: (RenderSym repr) => repr (Variable repr) -> 
  GS (repr (Statement repr))
decrement1 v = v &= (valueOf v #- litInt 1)

varDec :: (RenderSym repr) => repr (Permanence repr) -> repr (Permanence repr) 
  -> repr (Variable repr) -> GS (repr (Statement repr))
varDec s d v = toState $ stateFromData (permDoc (bind $ variableBind v) <+> 
  getTypeDoc (variableType v) <+> variableDoc v) Semi
  where bind Static = s
        bind Dynamic = d

varDecDef :: (RenderSym repr) => repr (Variable repr) -> repr (Value repr) -> 
  GS (repr (Statement repr))
varDecDef vr vl = onStateValue (\vd -> stateFromData (statementDoc vd <+> 
  equals <+> valueDoc vl) Semi) (S.varDec vr)

listDec :: (RenderSym repr) => (repr (Value repr) -> Doc) -> 
  repr (Value repr) -> repr (Variable repr) -> GS (repr (Statement repr))
listDec f sz v = onStateValue (\vd -> stateFromData (statementDoc vd <> f sz) 
  Semi) (S.varDec v)

listDecDef :: (RenderSym repr) => ([repr (Value repr)] -> Doc) -> 
  repr (Variable repr) -> [repr (Value repr)] -> GS (repr (Statement repr))
listDecDef f v vs = onStateValue (\vd -> stateFromData (statementDoc vd <> f vs)
  Semi) (S.varDec v)

objDecNew :: (RenderSym repr) => repr (Variable repr) -> [repr (Value repr)] -> 
  GS (repr (Statement repr))
objDecNew v vs = S.varDecDef v (newObj (variableType v) vs)

objDecNewNoParams :: (RenderSym repr) => repr (Variable repr) -> 
  GS (repr (Statement repr))
objDecNewNoParams v = S.objDecNew v []

constDecDef :: (RenderSym repr) => repr (Variable repr) -> repr (Value repr) 
  -> GS (repr (Statement repr))
constDecDef v def = toState $ stateFromData (constDecDefDocD v def) Semi

discardInput :: (RenderSym repr) => (repr (Value repr) -> Doc) ->
  GS (repr (Statement repr))
discardInput f = toState $ stateFromData (f inputFunc) Semi

discardFileInput :: (RenderSym repr) => (repr (Value repr) -> Doc) -> 
  repr (Value repr) -> GS (repr (Statement repr))
discardFileInput f v = toState $ stateFromData (f v) Semi

openFileR :: (RenderSym repr) => (repr (Value repr) -> repr (Type repr) -> 
  repr (Value repr)) -> repr (Variable repr) -> repr (Value repr) -> 
  GS (repr (Statement repr))
openFileR f vr vl = vr &= f vl infile

openFileW :: (RenderSym repr) => (repr (Value repr) -> repr (Type repr) -> 
  repr (Value repr) -> repr (Value repr)) -> repr (Variable repr) -> 
  repr (Value repr) -> GS (repr (Statement repr))
openFileW f vr vl = vr &= f vl outfile litFalse

openFileA :: (RenderSym repr) => (repr (Value repr) -> repr (Type repr) -> 
  repr (Value repr) -> repr (Value repr)) -> repr (Variable repr) -> 
  repr (Value repr) -> GS (repr (Statement repr))
openFileA f vr vl = vr &= f vl outfile litTrue

closeFile :: (RenderSym repr) => Label -> repr (Value repr) -> 
  GS (repr (Statement repr))
closeFile n f = S.valState $ objMethodCallNoParams S.void f n

discardFileLine :: (RenderSym repr) => Label -> repr (Value repr) -> 
  GS (repr (Statement repr))
discardFileLine n f = S.valState $ objMethodCallNoParams S.string f n 

returnState :: (RenderSym repr) => Terminator -> repr (Value repr) -> 
  GS (repr (Statement repr))
returnState t v = toState $ stateFromData (returnDocD [v]) t

multiReturnError :: String -> String
multiReturnError l = "Cannot return multiple values in " ++ l

valState :: (RenderSym repr) => Terminator -> repr (Value repr) ->
  GS (repr (Statement repr))
valState t v = toState $ stateFromData (valueDoc v) t

comment :: (RenderSym repr) => repr (Keyword repr) -> Label -> 
  GS (repr (Statement repr))
comment cs c = toState $ stateFromData (commentDocD c (keyDoc cs)) Empty

freeError :: String -> String
freeError l = "Cannot free variables in " ++ l

throw :: (RenderSym repr) => (repr (Value repr) -> Doc) -> Terminator -> Label 
  -> GS (repr (Statement repr))
throw f t l = toState $ stateFromData (f (litString l)) t

initState :: (RenderSym repr) => Label -> Label -> GS (repr (Statement repr))
initState fsmName initialState = S.varDecDef (var fsmName S.string) 
  (litString initialState)

changeState :: (RenderSym repr) => Label -> Label -> GS (repr (Statement repr))
changeState fsmName tState = var fsmName S.string &= litString tState

initObserverList :: (RenderSym repr) => repr (Type repr) -> 
  [repr (Value repr)] -> GS (repr (Statement repr))
initObserverList t = S.listDecDef (var observerListName (S.listType static_ t))

addObserver :: (RenderSym repr) => repr (Value repr) -> 
  GS (repr (Statement repr))
addObserver o = S.valState $ listAdd obsList lastelem o
  where obsList = valueOf $ observerListName `listOf` valueType o
        lastelem = listSize obsList

ifCond :: (RenderSym repr) => repr (Keyword repr) -> repr (Keyword repr) -> 
  repr (Keyword repr) -> [(repr (Value repr), GS (repr (Body repr)))] -> 
  GS (repr (Body repr)) -> GS (repr (Statement repr))
ifCond _ _ _ [] _ = error "if condition created with no cases"
ifCond ifst elseif blEnd (c:cs) eBody = 
    let ifStart = keyDoc ifst
        elif = keyDoc elseif
        bEnd = keyDoc blEnd
        ifSect (v, b) = onStateValue (\bd -> vcat [
          text "if" <+> parens (valueDoc v) <+> ifStart,
          indent $ bodyDoc bd,
          bEnd]) b
        elseIfSect (v, b) = onStateValue (\bd -> vcat [
          elif <+> parens (valueDoc v) <+> ifStart,
          indent $ bodyDoc bd,
          bEnd]) b
        elseSect = onStateValue (\bd -> emptyIfEmpty (bodyDoc bd) $ vcat [
          text "else" <+> ifStart,
          indent $ bodyDoc bd,
          bEnd]) eBody
    in onStateList (\l -> stateFromData (vcat l) Empty)
      (ifSect c : map elseIfSect cs ++ [elseSect])

ifNoElse :: (RenderSym repr) => [(repr (Value repr), GS (repr (Body repr)))] 
  -> GS (repr (Statement repr))
ifNoElse bs = S.ifCond bs $ body []

switch :: (RenderSym repr) => repr (Value repr) -> [(repr (Value repr), 
  GS (repr (Body repr)))] -> GS (repr (Body repr)) -> GS (repr (Statement repr))
switch v cs c = on3StateValues (\b de css -> stateFromData (switchDocD b v 
  de css) Semi) (S.state break) c (onStateList (zip (map fst cs)) (map snd cs))

switchAsIf :: (RenderSym repr) => repr (Value repr) -> [(repr (Value repr), 
  GS (repr (Body repr)))] -> GS (repr (Body repr)) -> GS (repr (Statement repr))
switchAsIf v cs = S.ifCond cases
  where cases = map (first (v ?==)) cs

ifExists :: (RenderSym repr) => repr (Value repr) -> GS (repr (Body repr)) -> 
  GS (repr (Body repr)) -> GS (repr (Statement repr))
ifExists v ifBody = S.ifCond [(notNull v, ifBody)]

for :: (RenderSym repr) => repr (Keyword repr) -> repr (Keyword repr) -> 
  GS (repr (Statement repr)) -> repr (Value repr) -> GS (repr (Statement repr))
  -> GS (repr (Body repr)) -> GS (repr (Statement repr))
for bStart bEnd sInit vGuard sUpdate = on3StateValues (\initl upd bod -> 
  stateFromData (vcat [
  forLabel <+> parens (statementDoc initl <> semi <+> valueDoc vGuard <> semi 
    <+> statementDoc upd) <+> keyDoc bStart,
  indent $ bodyDoc bod,
  keyDoc bEnd]) Empty) (S.loopState sInit) (S.loopState sUpdate)

forRange :: (RenderSym repr) => repr (Variable repr) -> repr (Value repr) -> 
  repr (Value repr) -> repr (Value repr) -> GS (repr (Body repr)) -> 
  GS (repr (Statement repr))
forRange i initv finalv stepv = S.for (S.varDecDef i initv) (valueOf i ?< 
  finalv) (i &+= stepv)

forEach :: (RenderSym repr) => repr (Keyword repr) -> repr (Keyword repr) -> 
  repr (Keyword repr) -> repr (Keyword repr) -> repr (Variable repr) -> 
  repr (Value repr) -> GS (repr (Body repr)) -> GS (repr (Statement repr))
forEach bStart bEnd forEachLabel inLbl e v = onStateValue (\b -> stateFromData
  (vcat [keyDoc forEachLabel <+> parens (getTypeDoc (variableType e) <+> 
    variableDoc e <+> keyDoc inLbl <+> valueDoc v) <+> keyDoc bStart,
  indent $ bodyDoc b,
  keyDoc bEnd]) Empty) 

while :: (RenderSym repr) => repr (Keyword repr) -> repr (Keyword repr) -> 
  repr (Value repr) -> GS (repr (Body repr)) -> GS (repr (Statement repr))
while bStart bEnd v = onStateValue (\b -> stateFromData (vcat [
  text "while" <+> parens (valueDoc v) <+> keyDoc bStart,
  indent $ bodyDoc b,
  keyDoc bEnd]) Empty)

tryCatch :: (RenderSym repr) => (repr (Body repr) -> repr (Body repr) -> Doc) ->
  GS (repr (Body repr)) -> GS (repr (Body repr)) -> GS (repr (Statement repr))
tryCatch f = on2StateValues (\tb cb -> stateFromData (f tb cb) Empty)

checkState :: (RenderSym repr) => Label -> [(repr (Value repr), 
  GS (repr (Body repr)))] -> GS (repr (Body repr)) -> GS (repr (Statement repr))
checkState l = S.switch (valueOf $ var l S.string)

notifyObservers :: (RenderSym repr) => repr (Function repr) -> repr (Type repr)
  -> GS (repr (Statement repr))
notifyObservers f t = S.for initv (v_index ?< listSize obsList) 
  (var_index &++) notify
  where obsList = valueOf $ observerListName `listOf` t 
        var_index = var "observerIndex" S.int
        v_index = valueOf var_index
        initv = S.varDecDef var_index $ litInt 0
        notify = S.oneLiner $ S.valState $ at obsList v_index $. f

construct :: Label -> TypeData
construct n = td (Object n) n empty

param :: (RenderSym repr) => (repr (Variable repr) -> Doc) -> 
  repr (Variable repr) -> MS (repr (Parameter repr))
param f v = getPutReturn (addParameter (variableName v)) (paramFromData v (f v))

method :: (RenderSym repr) => Label -> Label -> repr (Scope repr) -> 
  repr (Permanence repr) -> repr (Type repr) -> [MS (repr (Parameter repr))] -> 
  GS (repr (Body repr)) -> MS (repr (Method repr))
method n c s p t = intMethod False n c s p (mType t)

getMethod :: (RenderSym repr) => Label -> repr (Variable repr) -> 
  MS (repr (Method repr))
getMethod c v = S.method (getterName $ variableName v) c public dynamic_ 
    (variableType v) [] getBody
    where getBody = S.oneLiner $ S.returnState (valueOf $ objVarSelf c v)

setMethod :: (RenderSym repr) => Label -> repr (Variable repr) -> 
  MS (repr (Method repr))
setMethod c v = S.method (setterName $ variableName v) c public dynamic_ S.void 
  [S.param v] setBody
  where setBody = S.oneLiner $ objVarSelf c v &= valueOf v

privMethod :: (RenderSym repr) => Label -> Label -> repr (Type repr) -> 
  [MS (repr (Parameter repr))] -> GS (repr (Body repr)) -> 
  MS (repr (Method repr))
privMethod n c = S.method n c private dynamic_

pubMethod :: (RenderSym repr) => Label -> Label -> repr (Type repr) -> 
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
  repr (Permanence repr) -> repr (Type repr) -> [MS (repr (Parameter repr))] -> 
  GS (repr (Body repr)) -> MS (repr (Method repr))
function n s p t = S.intFunc False n s p (mType t)

mainFunction :: (RenderSym repr) => repr (Type repr) -> Label -> 
  GS (repr (Body repr)) -> MS (repr (Method repr))
mainFunction s n = S.intFunc True n public static_ (mType S.void)
  [S.param (var "args" (typeFromData (List String) (render (getTypeDoc s) ++ 
  "[]") (getTypeDoc s <> text "[]")))]

docFunc :: (RenderSym repr) => String -> [String] -> Maybe String -> 
  MS (repr (Method repr)) -> MS (repr (Method repr))
docFunc desc pComms rComm = docFuncRepr desc pComms (maybeToList rComm)

docInOutFunc :: (RenderSym repr) => (repr (Scope repr) -> repr (Permanence repr)
    -> [repr (Variable repr)] -> [repr (Variable repr)] -> 
    [repr (Variable repr)] -> GS (repr (Body repr)) -> 
    MS (repr (Method repr)))
  -> repr (Scope repr) -> repr (Permanence repr) -> String -> 
  [(String, repr (Variable repr))] -> [(String, repr (Variable repr))] -> 
  [(String, repr (Variable repr))] -> GS (repr (Body repr)) -> 
  MS (repr (Method repr))
docInOutFunc f s p desc is [o] [] b = docFuncRepr desc (map fst is) [fst o] 
  (f s p (map snd is) [snd o] [] b)
docInOutFunc f s p desc is [] [both] b = docFuncRepr desc (map fst $ both : is) 
  [fst both | not ((isObject . getType . variableType . snd) both)] 
  (f s p (map snd is) [] [snd both] b)
docInOutFunc f s p desc is os bs b = docFuncRepr desc (map fst $ bs ++ is ++ os)
  [] (f s p (map snd is) (map snd os) (map snd bs) b)

intFunc :: (RenderSym repr) => Bool -> Label -> repr (Scope repr) -> 
  repr (Permanence repr) -> repr (MethodType repr) -> 
  [MS (repr (Parameter repr))] -> GS (repr (Body repr)) -> 
  MS (repr (Method repr))
intFunc m n = intMethod m n ""

stateVar :: (RenderSym repr) => repr (Scope repr) -> repr (Permanence repr) ->
  repr (Variable repr) -> GS (repr (StateVar repr))
stateVar s p v = stateVarFromData (onStateValue (stateVarDocD 
  (scopeDoc s) (permDoc p) . statementDoc) (S.state $ S.varDec v))

stateVarDef :: (RenderSym repr) => repr (Scope repr) -> repr (Permanence repr) 
  -> repr (Variable repr) -> repr (Value repr) -> 
  GS (repr (StateVar repr))
stateVarDef s p vr vl = stateVarFromData (onStateValue (stateVarDocD 
  (scopeDoc s) (permDoc p) . statementDoc) (S.state $ S.varDecDef vr vl))

constVar :: (RenderSym repr) => Doc -> repr (Scope repr) ->
  repr (Variable repr) -> repr (Value repr) -> 
  GS (repr (StateVar repr))
constVar p s vr vl = stateVarFromData (onStateValue (stateVarDocD (scopeDoc s) 
  p . statementDoc) (S.state $ S.constDecDef vr vl))

privMVar :: (RenderSym repr) => repr (Variable repr) -> 
  GS (repr (StateVar repr))
privMVar = S.stateVar private dynamic_

pubMVar :: (RenderSym repr) => repr (Variable repr) -> 
  GS (repr (StateVar repr))
pubMVar = S.stateVar public dynamic_

pubGVar :: (RenderSym repr) => repr (Variable repr) -> 
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
fileFromData f ft fp m = getPutReturnFunc2 m fp (\s mdl fpath -> (if isEmpty 
  (moduleDoc mdl) then id else (if snd s ^. currMain then over lensFStoGS 
  (setMainMod fpath) else id) . over lensFStoGS (addFile ft fpath) . 
  setFilePath fpath) s) f

-- Helper functions

setEmpty :: (RenderSym repr) => GS (repr (Statement repr)) -> 
  GS (repr (Statement repr))
setEmpty = onStateValue (\s -> stateFromData (statementDoc s) Empty)