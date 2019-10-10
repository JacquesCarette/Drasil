{-# LANGUAGE PostfixOperators, GADTs #-}

-- | The structure for a class of renderers is defined here.
module GOOL.Drasil.LanguageRenderer (
  -- * Common Syntax
  classDec, dot, doubleSlash, elseIfLabel, forLabel, inLabel, new, 
  blockCmtStart, blockCmtEnd, docCmtStart, observerListName, addExt,
  
  -- * Default Functions available for use in renderers
  packageDocD, fileDoc', moduleDocD, classDocD, enumDocD, enumElementsDocD, 
  enumElementsDocD', multiStateDocD, blockDocD, bodyDocD, oneLinerD, outDoc, printDoc,
  printFileDocD, boolTypeDocD, intTypeDocD, floatTypeDocD, charTypeDocD, 
  stringTypeDocD, fileTypeDocD, typeDocD, enumTypeDocD, listTypeDocD, listInnerTypeD, voidDocD, 
  constructDocD, paramDocD, paramListDocD, mkParam, methodDocD, 
  methodListDocD, stateVarDocD, stateVarDefDocD, constVarDocD, stateVarListDocD,
  alwaysDel, ifCondDocD, switchDocD, forDocD, forEachDocD, whileDocD, 
  tryCatchDocD, assignDocD, multiAssignDoc, plusEqualsDocD, plusEqualsDocD', 
  plusPlusDocD, plusPlusDocD', varDecDocD, varDecDefDocD, listDecDocD, 
  listDecDefDocD, statementDocD, returnDocD, commentDocD, freeDocD, throwDocD, 
  mkSt, mkStNoEnd, stringListVals', stringListLists', printStD, stateD, loopStateD, emptyStateD, assignD, assignToListIndexD, multiAssignError, decrementD, incrementD, decrement1D, increment1D, constDecDefD, discardInputD,discardFileInputD, openFileRD, openFileWD, openFileAD, closeFileD, discardFileLineD, breakD, continueD, returnD, multiReturnError, valStateD, freeError, throwD, initStateD, changeStateD, initObserverListD, addObserverD, ifNoElseD, switchD, switchAsIfD, ifExistsD, forRangeD, tryCatchD, stratDocD, runStrategyD, listSliceD, checkStateD, notifyObserversD, unOpPrec, 
  notOpDocD, notOpDocD', negateOpDocD, sqrtOpDocD, sqrtOpDocD', absOpDocD, 
  absOpDocD', logOpDocD, logOpDocD', lnOpDocD, lnOpDocD', expOpDocD, expOpDocD',
  sinOpDocD, sinOpDocD', cosOpDocD, cosOpDocD', tanOpDocD, tanOpDocD', 
  asinOpDocD, asinOpDocD', acosOpDocD, acosOpDocD', atanOpDocD, atanOpDocD', 
  unOpDocD, unExpr, unExpr', powerPrec, multPrec, andPrec, orPrec, 
  equalOpDocD, notEqualOpDocD, greaterOpDocD, greaterEqualOpDocD, lessOpDocD, 
  lessEqualOpDocD, plusOpDocD, minusOpDocD, multOpDocD, divideOpDocD, 
  moduloOpDocD, powerOpDocD, andOpDocD, orOpDocD, binOpDocD, binOpDocD', 
  binExpr, binExpr', typeBinExpr, mkVal, mkVar, mkStaticVar, litTrueD, 
  litFalseD, litCharD, litFloatD, litIntD, litStringD, varDocD, extVarDocD, 
  selfDocD, argDocD, enumElemDocD, classVarCheckStatic, classVarDocD,
  objVarDocD, inlineIfD, funcAppDocD, newObjDocD, newObjDocD',
  objDecDefDocD, constDecDefDocD, listIndexExistsDocD, varD, 
  staticVarD, extVarD, selfD, enumVarD, classVarD, objVarD, objVarSelfD, listVarD, listOfD, iterVarD, valueOfD, argD, enumElementD, argsListD, funcAppD, extFuncAppD, newObjD, notNullD, objAccessD, objMethodCallD, objMethodCallNoParamsD, selfAccessD, listIndexExistsD, indexOfD,
  funcDocD, 
  castDocD, sizeDocD, listAccessFuncDocD, listSetFuncDocD, objAccessDocD, 
  castObjDocD, funcD, getD, setD, listSizeD, listAddD, listAppendD, iterBeginD, iterEndD, listAccessD, listSetD, getFuncD, setFuncD, listSizeFuncD, listAddFuncD, listAppendFuncD, iterBeginError, iterEndError, listAccessFuncD, listAccessFuncD', listSetFuncD, includeD, breakDocD, continueDocD, staticDocD, dynamicDocD, 
  privateDocD, publicDocD, blockCmtDoc, docCmtDoc, commentedItem, 
  addCommentsDocD, functionDox, classDoc, moduleDoc, commentedModD, docFuncRepr,
  valList, prependToBody, appendToBody, surroundBody, getterName, setterName, 
  setMainMethod, setEmpty, intValue, filterOutObjs
) where

import Utils.Drasil (blank, capitalize, indent, indentList, stringList)

import GOOL.Drasil.CodeType (CodeType(..), isObject)
import GOOL.Drasil.Symantics (Label, Library,
  RenderSym(..), BodySym(..), BlockSym(..), PermanenceSym(..),
  TypeSym(Type, getTypedType, getType, getTypeString, getTypeDoc, bool, float, 
  string, infile, outfile, listType, listInnerType, obj, enumType, iterator, 
  void), 
  VariableSym(..), InternalVariable(..), ValueSym(..), NumericExpression(..), 
  BooleanExpression(..), ValueExpression(..), InternalValue(..), Selector(..), 
  FunctionSym(..), SelectorFunction(..), InternalFunction(..), 
  InternalStatement(..), StatementSym(..), ControlStatementSym(..), 
  ParameterSym(..), MethodSym(..), InternalMethod(..), BlockCommentSym(..))
import qualified GOOL.Drasil.Symantics as S (TypeSym(char, int))
import GOOL.Drasil.Data (Boolean, Other, Terminator(..), FileData(..), 
  fileD, updateFileMod, ModData(..), updateModDoc, MethodData(..), OpData(..), 
  od, ParamData(..), pd, TypeData(..), td, btd, ltd, TypedType(..), cType, 
  typeDoc, TypedValue(..), valPrec, valDoc, Binding(..), TypedVar(..), 
  getVarData, varBind, varType, varDoc, typeToVal, typeToVar, valToType)
import GOOL.Drasil.Helpers (angles, 
  doubleQuotedText, hicat,vibcat,vmap, emptyIfEmpty, emptyIfNull, getNestDegree)

import Control.Applicative ((<|>))
import Data.List (intersperse, last)
import Data.Bifunctor (first)
import Data.Map as Map (lookup, fromList)
import Data.Maybe (fromMaybe)
import Prelude hiding (LT,break,print,last,mod,(<>))
import Text.PrettyPrint.HughesPJ (Doc, text, empty, render, (<>), (<+>), ($+$),
  brackets, parens, isEmpty, rbrace, lbrace, vcat, char, double, quotes, 
  integer, semi, equals, braces, int, comma, colon, hcat)

----------------------------------------
-- Syntax common to several renderers --
----------------------------------------

classDec, dot, doubleSlash, elseIfLabel, forLabel, inLabel, new, blockCmtStart, 
  blockCmtEnd, docCmtStart :: Doc
classDec = text "class"
dot = text "."
doubleSlash = text "//"
elseIfLabel = text "else if"
forLabel = text "for"
inLabel = text "in"
new = text "new"
blockCmtStart = text "/*"
blockCmtEnd = text "*/"
docCmtStart = text "/**"

observerListName :: Label
observerListName = "observerList"

addExt :: String -> String -> String
addExt ext nm = nm ++ "." ++ ext

----------------------------------
-- Functions for rendering code --
----------------------------------

packageDocD :: Label -> Doc -> FileData -> FileData
packageDocD n end f = fileD (fileType f) (n ++ "/" ++ filePath f) (updateModDoc 
  (emptyIfEmpty (modDoc $ fileMod f) (vibcat [text "package" <+> text n <> end, 
  modDoc (fileMod f)])) (fileMod f))

fileDoc' :: Doc -> Doc -> Doc -> Doc
fileDoc' t m b = vibcat [
  t,
  m,
  b]

-----------------------------------------------
-- 'Default' functions used in the renderers --
-----------------------------------------------

-- Module --

moduleDocD :: [(Doc, Bool)] -> Doc
moduleDocD cs = vibcat (map fst cs)

-- Class --

classDocD :: Label -> Maybe Label -> Doc -> Doc -> Doc -> Doc -> Doc
classDocD n p inh s vs fs = vcat [
  s <+> classDec <+> text n <+> baseClass <+> lbrace, 
  indentList [
    vs,
    blank,
    fs],
  rbrace]
  where baseClass = case p of Nothing -> empty
                              Just pn -> inh <+> text pn

enumDocD :: Label -> Doc -> Doc -> Doc
enumDocD n es s = vcat [
  s <+> text "enum" <+> text n <+> lbrace,
  indent es,
  rbrace]

enumElementsDocD :: [Label] -> Bool -> Doc
enumElementsDocD es enumsEqualInts = vcat $
  zipWith (\e i -> text e <+> equalsInt i <> interComma i) es nums
  where nums = [0..length es - 1]
        equalsInt i = if enumsEqualInts then equals <+> int i else empty 
        interComma i = if i < length es - 1 then text "," else empty

enumElementsDocD' :: [Label] -> Doc
enumElementsDocD' es = vcat $
  zipWith (\e i -> text e <+> equals <+> int i) es nums
    where nums = [0..length es - 1]

-- Groupings --

multiStateDocD :: Doc -> [(Doc, Terminator)] -> (Doc, Terminator)
multiStateDocD end sts = (vcat (applyEnd statements), needsEnd statements)
  where applyEnd [] = []
        applyEnd [(s, _)] = [s]
        applyEnd ((s, t):ss) = (s <> getTermDoc t) : applyEnd ss
        needsEnd [] = Empty
        needsEnd ss = snd (last ss)
        statements = filter notNullStatement sts
        notNullStatement s = not (isEmpty (fst s)) && 
          (render (fst s) /= render end)

blockDocD :: Doc -> [Doc] -> Doc
blockDocD end sts = vcat statements
  where statements = filter notNullStatement sts
        notNullStatement s = not (isEmpty s) && (render s /= render end)

bodyDocD :: [Doc] -> Doc
bodyDocD bs = vibcat blocks
  where blocks = filter notNullBlock bs
        notNullBlock b = not $ isEmpty b

oneLinerD :: (RenderSym repr) => repr (Statement repr) -> repr (Body repr)
oneLinerD s = bodyStatements [s]

-- IO --

printDoc :: (RenderSym repr) => repr (Value repr Other) -> repr (Value repr a) 
  -> Doc
printDoc printFn v = valueDoc printFn <> parens (valueDoc v)

printListDoc :: (RenderSym repr) => Integer -> repr (Value repr [a]) -> 
  (repr (Value repr a) -> repr (Statement repr)) -> 
  (String -> repr (Statement repr)) -> 
  (String -> repr (Statement repr)) -> 
  repr (Statement repr)
printListDoc n v prFn prStrFn prLnFn = multi [prStrFn "[", 
  for (varDecDef i (litInt 0)) (valueOf i ?< (listSize v #- litInt 1))
    (i &++) (bodyStatements [{-FIXME prFn (listAccess v (valueOf i)),-} prStrFn ", "]), 
  {-ifNoElse [(listSize v ?> litInt 0, oneLiner $
    prFn (listAccess v (listSize v #- litInt 1)))],-} 
  prLnFn "]"]
  where l_i = "list_i" ++ show n
        i = var l_i S.int

printObjDoc :: String -> (String -> repr (Statement repr)) 
  -> repr (Statement repr)
printObjDoc n prLnFn = prLnFn $ "Instance of " ++ n ++ " object"

outDoc :: (RenderSym repr) => Bool -> repr (Value repr Other) -> 
  repr (Value repr a) -> Maybe (repr (Value repr Other)) -> 
  repr (Statement repr)
outDoc newLn printFn v f = outDoc' (getTypedType $ valueType v)
  where outDoc' (LT t) = printListDoc (getNestDegree 1 (cType t)) 
          v prFn prStrFn prLnFn
        outDoc' (OT (TD (Object n) _ _)) = printObjDoc n prLnFn
        outDoc' _ = printSt newLn printFn v f
        prFn = maybe print printFile f
        prStrFn = maybe printStr printFileStr f
        prLnFn = if newLn then maybe printStrLn printFileStrLn f else maybe printStr printFileStr f 

printFileDocD :: Label -> TypedValue Other -> Doc
printFileDocD fn f = valDoc f <> dot <> text fn

-- Type Printers --

boolTypeDocD :: TypedType Boolean
boolTypeDocD = btd Boolean "Boolean" (text "Boolean") -- capital B?

intTypeDocD :: TypedType Other
intTypeDocD = td Integer "int" (text "int")

floatTypeDocD :: TypedType Other
floatTypeDocD = td Float "float" (text "float")

charTypeDocD :: TypedType Other
charTypeDocD = td Char "char" (text "char")

stringTypeDocD :: TypedType Other
stringTypeDocD = td String "string" (text "string")

fileTypeDocD :: TypedType Other
fileTypeDocD = td File "File" (text "File")

typeDocD :: Label -> TypedType Other
typeDocD t = td (Object t) t (text t)

enumTypeDocD :: Label -> TypedType Other
enumTypeDocD t = td (Enum t) t (text t)

listTypeDocD :: TypedType a -> Doc -> TypedType [a]
listTypeDocD t lst = ltd t (\s -> render lst ++ "<" ++ s ++ ">") 
  (\d -> lst <> angles d)

listInnerTypeD :: TypedType [a] -> TypedType a
listInnerTypeD (LT t) = t

-- Method Types --

voidDocD :: TypedType Other
voidDocD = td Void "void" (text "void")

constructDocD :: Label -> Doc
constructDocD _ = empty

-- Parameters --

paramDocD :: TypedVar a -> Doc
paramDocD v = tpDoc (varType v) <+> varDoc v

paramListDocD :: [ParamData] -> Doc
paramListDocD = hicat (text ", ") . map paramDoc

mkParam :: (TypedVar a -> Doc) -> TypedVar a -> ParamData
mkParam f v = pd (getVarData v) (f v)

-- Method --

methodDocD :: Label -> Doc -> Doc -> TypedType a -> Doc -> Doc -> Doc
methodDocD n s p t ps b = vcat [
  s <+> p <+> typeDoc t <+> text n <> parens ps <+> lbrace,
  indent b,
  rbrace]

methodListDocD :: [Doc] -> Doc
methodListDocD ms = vibcat methods
  where methods = filter (not . isEmpty) ms

-- StateVar --

stateVarDocD :: Doc -> Doc -> TypedVar a -> Doc -> Doc
stateVarDocD s p v end = s <+> p <+> tpDoc (varType v) <+> varDoc v <> end

stateVarDefDocD :: Doc -> Doc -> Doc -> Doc
stateVarDefDocD s p dec = s <+> p <+> dec

constVarDocD :: Doc -> Doc -> TypedVar a -> Doc -> Doc
constVarDocD s p v end = s <+> p <+> text "const" <+> tpDoc (varType v) <+>
  varDoc v <> end

stateVarListDocD :: [Doc] -> Doc
stateVarListDocD = vcat

alwaysDel :: Int
alwaysDel = 4

-- Controls --

ifCondDocD :: Doc -> Doc -> Doc -> Doc -> [(TypedValue Boolean, Doc)] -> Doc
ifCondDocD _ _ _ _ [] = error "if condition created with no cases"
ifCondDocD ifStart elif bEnd elseBody (c:cs) = 
  let ifSect (v, b) = vcat [
        text "if" <+> parens (valDoc v) <+> ifStart,
        indent b,
        bEnd]
      elseIfSect (v, b) = vcat [
        elif <+> parens (valDoc v) <+> ifStart,
        indent b,
        bEnd]
      elseSect = emptyIfEmpty elseBody $ vcat [
        text "else" <+> ifStart,
        indent elseBody,
        bEnd]
  in vcat [
    ifSect c,
    vmap elseIfSect cs,
    elseSect]

switchDocD :: (RenderSym repr) => repr (Statement repr) -> repr (Value repr a) 
  -> repr (Body repr) -> [(repr (Value repr a), repr (Body repr))] -> Doc
switchDocD breakState v defBody cs = 
  let caseDoc (l, result) = vcat [
        text "case" <+> valueDoc l <> colon,
        indentList [
          bodyDoc result,
          statementDoc breakState]]
      defaultSection = vcat [
        text "default" <> colon,
        indentList [
          bodyDoc defBody,
          statementDoc breakState]]
  in vcat [
      text "switch" <> parens (valueDoc v) <+> lbrace,
      indentList [
        vmap caseDoc cs,
        defaultSection],
      rbrace]

-- These signatures wont be quite so horrendous if/when we pass language options
-- (blockStart, etc.) in as shared environment
forDocD :: Doc -> Doc -> (Doc, Terminator) -> TypedValue Boolean -> 
  (Doc, Terminator) -> Doc -> Doc
forDocD bStart bEnd sInit vGuard sUpdate b = vcat [
  forLabel <+> parens (fst sInit <> semi <+> valDoc vGuard <> semi <+> 
    fst sUpdate) <+> bStart,
  indent b,
  bEnd]

forEachDocD :: TypedVar a -> Doc -> Doc -> Doc -> Doc -> TypedValue [a] -> Doc
  -> Doc
forEachDocD e bStart bEnd forEachLabel inLbl v b =
  vcat [forEachLabel <+> parens (tpDoc (varType e) <+> varDoc e <+> inLbl 
    <+> valDoc v) <+> bStart,
  indent b,
  bEnd]

whileDocD :: Doc -> Doc -> TypedValue Boolean -> Doc -> Doc
whileDocD bStart bEnd v b = vcat [
  text "while" <+> parens (valDoc v) <+> bStart,
  indent b,
  bEnd]

tryCatchDocD :: Doc -> Doc -> Doc 
tryCatchDocD tb cb = vcat [
  text "try" <+> lbrace,
  indent tb,
  rbrace <+> text "catch" <+> parens (text "System.Exception" <+> text "exc") 
    <+> lbrace,
  indent cb,
  rbrace]

stratDocD :: Doc -> Doc -> Doc
stratDocD b resultState = vcat [
  b,
  resultState]

runStrategyD :: (RenderSym repr) => String -> [(Label, repr (Body repr))] -> 
  Maybe (repr (Value repr a)) -> Maybe (repr (Variable repr a)) -> 
  repr (Block repr)
runStrategyD l strats rv av = docBlock $ maybe
  (strError l "RunStrategy called on non-existent strategy") 
  (flip stratDocD (statementDoc $ state resultState) . bodyDoc) 
  (Map.lookup l (Map.fromList strats))
  where resultState = maybe emptyState asgState av
        asgState v = maybe (strError l 
          "Attempt to assign null return to a Value") (assign v) rv
        strError n s = error $ "Strategy '" ++ n ++ "': " ++ s ++ "."

listSliceD :: (RenderSym repr) => repr (Variable repr [a]) -> 
  repr (Value repr [a]) -> Maybe (repr (Value repr Other)) ->
  Maybe (repr (Value repr Other)) -> Maybe (repr (Value repr Other)) -> 
  repr (Block repr)
listSliceD vnew vold b e s = 
  let l_temp = "temp"
      var_temp = var l_temp (variableType vnew)
      v_temp = valueOf var_temp
      l_i = "i_temp"
      var_i = var l_i S.int
      v_i = valueOf var_i
  in
    block [
      listDec 0 var_temp,
      for (varDecDef var_i (fromMaybe (litInt 0) b)) 
        (v_i ?< fromMaybe (listSize vold) e) (maybe (var_i &++) (var_i &+=) s)
        (oneLiner $ valState $ listAppend v_temp (listAccess vold v_i)),
      vnew &= v_temp]

checkStateD :: (RenderSym repr) => Label -> [(repr (Value repr Other), 
  repr (Body repr))] -> repr (Body repr) -> repr (Statement repr)
checkStateD l = switch (valueOf $ var l string)

notifyObserversD :: (RenderSym repr) => repr (Function repr a) -> 
  repr (Type repr b) -> repr (Statement repr)
notifyObserversD f t = for initv (v_index ?< listSize obsList) 
  (var_index &++) notify
  where obsList = valueOf $ observerListName `listOf` t 
        var_index = var "observerIndex" S.int
        v_index = valueOf var_index
        initv = varDecDef var_index $ litInt 0
        notify = oneLiner $ valState $ at obsList v_index $. f

-- Statements --

assignDocD :: (RenderSym repr) => repr (Variable repr a) -> repr (Value repr a) 
  -> Doc
assignDocD vr vl = variableDoc vr <+> equals <+> valueDoc vl

multiAssignDoc :: [TypedVar Other] -> [TypedValue Other] -> Doc
multiAssignDoc vrs vls = varList vrs <+> equals <+> valList vls

plusEqualsDocD :: (RenderSym repr) => repr (Variable repr Other) -> 
  repr (Value repr Other) -> Doc
plusEqualsDocD vr vl = variableDoc vr <+> text "+=" <+> valueDoc vl

plusEqualsDocD' :: TypedVar Other -> OpData -> TypedValue Other -> Doc
plusEqualsDocD' vr plusOp vl = varDoc vr <+> equals <+> varDoc vr <+> 
  opDoc plusOp <+> valDoc vl

plusPlusDocD :: (RenderSym repr) => repr (Variable repr Other) -> Doc
plusPlusDocD v = variableDoc v <> text "++"

plusPlusDocD' :: TypedVar Other -> OpData -> Doc
plusPlusDocD' v plusOp = varDoc v <+> equals <+> varDoc v <+> opDoc plusOp <+>
  int 1

varDecDocD :: TypedVar a -> Doc -> Doc -> Doc
varDecDocD v s d = bind (varBind v) <+> tpDoc (varType v) <+> varDoc v
  where bind Static = s
        bind Dynamic = d

varDecDefDocD :: TypedVar a -> TypedValue a -> Doc -> Doc -> Doc
varDecDefDocD v def s d = varDecDocD v s d <+> equals <+> valDoc def

listDecDocD :: TypedVar [a] -> TypedValue Other -> Doc -> Doc -> Doc
listDecDocD v n s d = varDecDocD v s d <+> equals <+> new <+> 
  tpDoc (varType v) <> parens (valDoc n)

listDecDefDocD :: TypedVar [a] -> [TypedValue a] -> Doc -> Doc -> Doc
listDecDefDocD v vs s d = varDecDocD v s d <+> equals <+> new <+> 
  tpDoc (varType v) <+> braces (valList' vs)

objDecDefDocD :: TypedVar Other -> TypedValue Other -> Doc -> Doc -> Doc
objDecDefDocD = varDecDefDocD

constDecDefDocD :: (RenderSym repr) => repr (Variable repr a) -> 
  repr (Value repr a) -> Doc
constDecDefDocD v def = text "const" <+> getTypeDoc (variableType v) <+> 
  variableDoc v <+> equals <+> valueDoc def

returnDocD :: (RenderSym repr) => [repr (Value repr Other)] -> Doc
returnDocD vs = text "return" <+> valueList vs

commentDocD :: Label -> Doc -> Doc
commentDocD cmt cStart = cStart <+> text cmt

freeDocD :: TypedVar a -> Doc
freeDocD v = text "delete" <+> varDoc v

throwDocD :: Doc -> Doc
throwDocD errMsg = text "throw new" <+> text "System.ApplicationException" <>
  parens errMsg

statementDocD :: (Doc, Terminator) -> (Doc, Terminator)
statementDocD (s, t) = (s <> getTermDoc t, Empty)

getTermDoc :: Terminator -> Doc
getTermDoc Semi = semi
getTermDoc Empty = empty

mkSt :: Doc -> (Doc, Terminator)
mkSt s = (s, Semi)

mkStNoEnd :: Doc -> (Doc, Terminator)
mkStNoEnd s = (s, Empty)

stringListVals' :: (RenderSym repr) => [repr (Variable repr a)] -> 
  repr (Value repr [Other]) -> repr (Statement repr)
stringListVals' vars sl = multi $ checkList (getType $ valueType sl)
    where checkList (List String) = assignVals vars 0
          checkList _ = error 
            "Value passed to stringListVals must be a list of strings"
          assignVals [] _ = []
          assignVals (v:vs) n = assign v (cast (variableType v) 
            (listAccess sl (litInt n))) : assignVals vs (n+1)

stringListLists' :: (RenderSym repr) => [repr (Variable repr [a])] -> 
  repr (Value repr [Other]) -> repr (Statement repr)
stringListLists' lsts sl = checkList (getType $ valueType sl)
  where checkList (List String) = listVals (map (getType . variableType) lsts)
        checkList _ = error 
          "Value passed to stringListLists must be a list of strings"
        listVals [] = loop
        listVals (List _:vs) = listVals vs
        listVals _ = error 
          "All values passed to stringListLists must have list types"
        loop = forRange var_i (litInt 0) (listSize sl #/ numLists) (litInt 1)
          (bodyStatements $ appendLists (map valueOf lsts) 0)
        appendLists [] _ = []
        appendLists (v:vs) n = valState (listAppend v (cast (listInnerType $ 
          valueType v) (listAccess sl ((v_i #* numLists) #+ litInt n)))) 
          : appendLists vs (n+1)
        numLists = litInt (toInteger $ length lsts)
        var_i = var "stringlist_i" S.int
        v_i = valueOf var_i
        
printStD :: (RenderSym repr) => repr (Value repr Other) -> repr (Value repr a) 
  -> repr (Statement repr)
printStD p v = stateFromData (printDoc p v) Semi

stateD :: (RenderSym repr) => repr (Statement repr) -> repr (Statement repr)
stateD s = stateFromData (statementDoc s <> getTermDoc (statementTerm s))
  Empty

loopStateD :: (RenderSym repr) => repr (Statement repr) -> repr (Statement repr)
loopStateD = stateD . setEmpty

emptyStateD :: (RenderSym repr) => repr (Statement repr)
emptyStateD = stateFromData empty Empty

assignD :: (RenderSym repr) => Terminator -> repr (Variable repr a) -> 
  repr (Value repr a) -> repr (Statement repr)
assignD t vr vl = stateFromData (assignDocD vr vl) t

assignToListIndexD :: (RenderSym repr) => repr (Variable repr [a]) -> 
  repr (Value repr Other) -> repr (Value repr a) -> repr (Statement repr)
assignToListIndexD lst index v = valState $ listSet (valueOf lst) index v

multiAssignError :: String -> String
multiAssignError l = "No multiple assignment statements in " ++ l

decrementD :: (RenderSym repr) => repr (Variable repr Other) -> 
  repr (Value repr Other) -> repr (Statement repr)
decrementD vr vl = vr &= (valueOf vr #- vl)

incrementD :: (RenderSym repr) => repr (Variable repr Other) -> 
  repr (Value repr Other) -> repr (Statement repr)
incrementD vr vl = stateFromData (plusEqualsDocD vr vl) Semi

decrement1D :: (RenderSym repr) => repr (Variable repr Other) -> 
  repr (Statement repr)
decrement1D v = v &= (valueOf v #- litInt 1)

increment1D :: (RenderSym repr) => repr (Variable repr Other) -> 
  repr (Statement repr)
increment1D v = stateFromData (plusPlusDocD v) Semi

constDecDefD :: (RenderSym repr) => repr (Variable repr a) -> 
  repr (Value repr a) -> repr (Statement repr)
constDecDefD v def = stateFromData (constDecDefDocD v def) Semi

discardInputD :: (RenderSym repr) => (repr (Value repr Other) -> Doc) ->
  repr (Statement repr)
discardInputD f = stateFromData (f inputFunc) Semi

discardFileInputD :: (RenderSym repr) => (repr (Value repr Other) -> Doc) -> 
  repr (Value repr Other) -> repr (Statement repr)
discardFileInputD f v = stateFromData (f v) Semi

openFileRD :: (RenderSym repr) => (repr (Value repr Other) -> 
  repr (Type repr Other) -> repr (Value repr Other)) -> 
  repr (Variable repr Other) -> repr (Value repr Other) -> repr (Statement repr)
openFileRD f vr vl = vr &= f vl infile

openFileWD :: (RenderSym repr) => (repr (Value repr Other) -> 
  repr (Type repr Other) -> repr (Value repr Boolean) -> 
  repr (Value repr Other)) -> repr (Variable repr Other) -> 
  repr (Value repr Other) -> repr (Statement repr)
openFileWD f vr vl = vr &= f vl outfile litFalse

openFileAD :: (RenderSym repr) => (repr (Value repr Other) -> 
  repr (Type repr Other) -> repr (Value repr Boolean) -> 
  repr (Value repr  Other)) -> repr (Variable repr Other) -> 
  repr (Value repr Other) -> repr (Statement repr)
openFileAD f vr vl = vr &= f vl outfile litTrue

closeFileD :: (RenderSym repr) => Label -> repr (Value repr Other) -> 
  repr (Statement repr)
closeFileD n f = valState $ objMethodCallNoParams void f n

discardFileLineD :: (RenderSym repr) => Label -> repr (Value repr Other) -> 
  repr (Statement repr)
discardFileLineD n f = valState $ objMethodCallNoParams string f n 

breakD :: (RenderSym repr) => Terminator -> repr (Statement repr)
breakD = stateFromData breakDocD

continueD :: (RenderSym repr) => Terminator -> repr (Statement repr)
continueD = stateFromData continueDocD

returnD :: (RenderSym repr) => Terminator -> repr (Value repr a) -> 
  repr (Statement repr)
returnD t v = stateFromData (returnDocD [toOtherValue v]) t

multiReturnError :: String -> String
multiReturnError l = "Cannot return multiple values in " ++ l

valStateD :: (RenderSym repr) => Terminator -> repr (Value repr a) ->
  repr (Statement repr)
valStateD t v = stateFromData (valueDoc v) t

freeError :: String -> String
freeError l = "Cannot free variables in " ++ l

throwD :: (RenderSym repr) => (repr (Value repr Other) -> Doc) -> Terminator -> 
  Label -> repr (Statement repr)
throwD f t l = stateFromData (f (litString l)) t

initStateD :: (RenderSym repr) => Label -> Label -> repr (Statement repr)
initStateD fsmName initialState = varDecDef (var fsmName string) 
  (litString initialState)

changeStateD :: (RenderSym repr) => Label -> Label -> repr (Statement repr)
changeStateD fsmName toState = var fsmName string &= litString toState

initObserverListD :: (RenderSym repr) => repr (Type repr [Other]) -> 
  [repr (Value repr Other)] -> repr (Statement repr)
initObserverListD t = listDecDef (var observerListName t)

addObserverD :: (RenderSym repr) => repr (Value repr Other) -> repr (Statement repr)
addObserverD o = valState $ listAdd obsList lastelem o
  where obsList = valueOf $ observerListName `listOf` valueType o
        lastelem = listSize obsList

ifNoElseD :: (RenderSym repr) => [(repr (Value repr Boolean), repr (Body repr))]
  -> repr (Statement repr)
ifNoElseD bs = ifCond bs $ body []

switchD :: (RenderSym repr) => repr (Value repr a) -> [(repr (Value repr a), 
  repr (Body repr))] -> repr (Body repr) -> repr (Statement repr)
switchD v cs c = stateFromData (switchDocD (state break) v c cs) Semi

switchAsIfD :: (RenderSym repr) => repr (Value repr a) -> [(repr (Value repr a),
  repr (Body repr))] -> repr (Body repr) -> repr (Statement repr)
switchAsIfD v cs = ifCond cases
  where cases = map (first (v ?==)) cs

ifExistsD :: (RenderSym repr) => repr (Value repr Other) -> repr (Body repr) -> 
  repr (Body repr) -> repr (Statement repr)
ifExistsD v ifBody = ifCond [(notNull v, ifBody)]

forRangeD :: (RenderSym repr) => repr (Variable repr Other) -> 
  repr (Value repr Other) -> repr (Value repr Other) -> 
  repr (Value repr Other) -> repr (Body repr) -> repr (Statement repr)
forRangeD i initv finalv stepv = for (varDecDef i initv) (valueOf i ?< finalv) 
  (i &+= stepv)

tryCatchD :: (RenderSym repr) => (repr (Body repr) -> repr (Body repr) -> 
  Doc) -> repr (Body repr) -> repr (Body repr) -> repr (Statement repr)
tryCatchD f tb cb = stateFromData (f tb cb) Empty


-- Unary Operators --

unOpPrec :: String -> OpData
unOpPrec = od 9 . text

notOpDocD :: OpData
notOpDocD = unOpPrec "!"

notOpDocD' :: OpData
notOpDocD' = unOpPrec "not"

negateOpDocD :: OpData
negateOpDocD = unOpPrec "-"

sqrtOpDocD :: OpData
sqrtOpDocD = unOpPrec "sqrt"

sqrtOpDocD' :: OpData
sqrtOpDocD' = unOpPrec "math.sqrt"

absOpDocD :: OpData
absOpDocD = unOpPrec "fabs"

absOpDocD' :: OpData
absOpDocD' = unOpPrec "math.fabs"

logOpDocD :: OpData
logOpDocD = unOpPrec "log"

logOpDocD' :: OpData
logOpDocD' = unOpPrec "math.log"

lnOpDocD :: OpData
lnOpDocD = unOpPrec "ln"

lnOpDocD' :: OpData
lnOpDocD' = unOpPrec "math.ln"

expOpDocD :: OpData
expOpDocD = unOpPrec "exp"

expOpDocD' :: OpData
expOpDocD' = unOpPrec "math.exp"

sinOpDocD :: OpData
sinOpDocD = unOpPrec "sin"

sinOpDocD' :: OpData
sinOpDocD' = unOpPrec "math.sin"

cosOpDocD :: OpData
cosOpDocD = unOpPrec "cos"

cosOpDocD' :: OpData
cosOpDocD' = unOpPrec "math.cos"

tanOpDocD :: OpData
tanOpDocD = unOpPrec "tan"

tanOpDocD' :: OpData
tanOpDocD' = unOpPrec "math.tan"

asinOpDocD :: OpData
asinOpDocD = unOpPrec "asin"

asinOpDocD' :: OpData
asinOpDocD' = unOpPrec "math.asin"

acosOpDocD :: OpData
acosOpDocD = unOpPrec "acos"

acosOpDocD' :: OpData
acosOpDocD' = unOpPrec "math.acos"

atanOpDocD :: OpData
atanOpDocD = unOpPrec "atan"

atanOpDocD' :: OpData
atanOpDocD' = unOpPrec "math.atan"

unOpDocD :: Doc -> Doc -> Doc
unOpDocD op v = op <> parens v

unOpDocD' :: Doc -> Doc -> Doc
unOpDocD' op v = op <> v

unExpr :: OpData -> TypedValue a -> TypedValue a
unExpr u v = mkExpr (opPrec u) (valToType v) (unOpDocD (opDoc u) (valDoc v))

unExpr' :: OpData -> TypedValue a -> TypedValue a
unExpr' u v = mkExpr (opPrec u) (valToType v) (unOpDocD' (opDoc u) (valDoc v))

-- Binary Operators --

compEqualPrec :: String -> OpData
compEqualPrec = od 4 . text

compPrec :: String -> OpData
compPrec = od 5 . text

addPrec :: String -> OpData
addPrec = od 6 . text

multPrec :: String -> OpData
multPrec = od 7 . text

powerPrec :: String -> OpData
powerPrec = od 8 . text

andPrec :: String -> OpData 
andPrec = od 3 . text

orPrec :: String -> OpData
orPrec = od 2 . text

equalOpDocD :: OpData
equalOpDocD = compEqualPrec "=="

notEqualOpDocD :: OpData
notEqualOpDocD = compEqualPrec "!="

greaterOpDocD :: OpData
greaterOpDocD = compPrec ">"

greaterEqualOpDocD :: OpData
greaterEqualOpDocD = compPrec ">="

lessOpDocD :: OpData
lessOpDocD = compPrec "<"

lessEqualOpDocD :: OpData
lessEqualOpDocD = compPrec "<="

plusOpDocD :: OpData
plusOpDocD = addPrec "+"

minusOpDocD :: OpData
minusOpDocD = addPrec "-"

multOpDocD :: OpData
multOpDocD = multPrec "*"

divideOpDocD :: OpData
divideOpDocD = multPrec "/"

moduloOpDocD :: OpData
moduloOpDocD = multPrec "%"

powerOpDocD :: OpData
powerOpDocD = powerPrec "pow"

andOpDocD :: OpData
andOpDocD = andPrec "&&"

orOpDocD :: OpData
orOpDocD = orPrec "||"

binOpDocD :: Doc -> Doc -> Doc -> Doc
binOpDocD op v1 v2 = v1 <+> op <+> v2

binOpDocD' :: Doc -> Doc -> Doc -> Doc
binOpDocD' op v1 v2 = op <> parens (v1 <> comma <+> v2)
  
binExpr :: OpData -> TypedValue a -> TypedValue a -> TypedValue a
binExpr b v1 v2 = mkExpr (opPrec b) (numType (valToType v1) (valToType v2)) 
  (binOpDocD (opDoc b) (exprParensL b v1 $ valDoc v1) (exprParensR b v2 $ 
  valDoc v2))

binExpr' :: OpData -> TypedValue a -> TypedValue a -> TypedValue a
binExpr' b v1 v2 = mkExpr 9 (numType (valToType v1) (valToType v2)) 
  (binOpDocD' (opDoc b) (valDoc v1) (valDoc v2))

numType :: TypedType a -> TypedType a -> TypedType a
numType t1 t2 = numericType (cType t1) (cType t2)
  where numericType Integer Integer = t1
        numericType Float _ = t1
        numericType _ Float = t2
        numericType _ _ = error "Numeric types required for numeric expression"

typeBinExpr :: OpData -> TypedType b -> TypedValue a -> TypedValue a -> 
  TypedValue b
typeBinExpr b t v1 v2 = mkExpr (opPrec b) t (binOpDocD (opDoc b) 
  (exprParensL b v1 $ valDoc v1) (exprParensR b v2 $ valDoc v2))

mkVal :: TypedType a -> Doc -> TypedValue a
mkVal = typeToVal Nothing

mkVar :: String -> TypedType a -> Doc -> TypedVar a
mkVar = typeToVar Dynamic

mkStaticVar :: String -> TypedType a -> Doc -> TypedVar a
mkStaticVar = typeToVar Static 

mkExpr :: Int -> TypedType a -> Doc -> TypedValue a
mkExpr p = typeToVal (Just p)

-- Literals --

litTrueD :: (RenderSym repr) => repr (Value repr Boolean)
litTrueD = valFromData Nothing bool (text "true")

litFalseD :: (RenderSym repr) => repr (Value repr Boolean)
litFalseD = valFromData Nothing bool (text "false")

litCharD :: (RenderSym repr) => Char -> repr (Value repr Other)
litCharD c = valFromData Nothing S.char (quotes $ char c)

litFloatD :: (RenderSym repr) => Double -> repr (Value repr Other)
litFloatD f = valFromData Nothing float (double f)

litIntD :: (RenderSym repr) => Integer -> repr (Value repr Other)
litIntD i = valFromData Nothing S.int (integer i)

litStringD :: (RenderSym repr) => String -> repr (Value repr Other)
litStringD s = valFromData Nothing string (doubleQuotedText s)

-- Value Printers --

varDocD :: Label -> Doc
varDocD = text

extVarDocD :: Library -> Label -> Doc
extVarDocD l n = text l <> dot <> text n

selfDocD :: Doc
selfDocD = text "this"

argDocD :: (RenderSym repr) => repr (Value repr Other) -> 
  repr (Value repr Other) -> Doc
argDocD n args = valueDoc args <> brackets (valueDoc n)

enumElemDocD :: Label -> Label -> Doc
enumElemDocD en e = text en <> dot <> text e

classVarCheckStatic :: (VariableSym repr) => repr (Variable repr a) -> 
  repr (Variable repr a)
classVarCheckStatic v = classVarCS (variableBind v)
  where classVarCS Dynamic = error
          "classVar can only be used to access static variables"
        classVarCS Static = v

classVarDocD :: Doc -> Doc -> Doc
classVarDocD c v = c <> dot <> v

objVarDocD :: Doc -> Doc ->  Doc
objVarDocD n1 n2 = n1 <> dot <> n2

inlineIfD :: TypedValue Boolean -> TypedValue a -> TypedValue a -> TypedValue a
inlineIfD c v1 v2 = typeToVal prec (valToType v1) (valDoc c <+> text "?" <+> 
  valDoc v1 <+> text ":" <+> valDoc v2)
  where prec = valPrec c <|> Just 0

funcAppDocD :: (RenderSym repr) => Label -> [repr (Value repr Other)] -> Doc
funcAppDocD n vs = text n <> parens (valueList vs)

newObjDocD :: (RenderSym repr) => repr (Type repr Other) -> Doc -> Doc
newObjDocD st vs = new <+> newObjDocD' st vs

newObjDocD' :: (RenderSym repr) => repr (Type repr Other) -> Doc -> Doc
newObjDocD' st vs = getTypeDoc st <> parens vs

listIndexExistsDocD :: OpData -> TypedValue Other -> TypedValue Other -> Doc
listIndexExistsDocD greater lst index = parens (valDoc lst <> 
  text ".Length" <+> opDoc greater <+> valDoc index) 

varD :: (RenderSym repr) => Label -> repr (Type repr a) -> 
  repr (Variable repr a)
varD n t = varFromData Dynamic n t (varDocD n)

staticVarD :: (RenderSym repr) => Label -> repr (Type repr a) -> 
  repr (Variable repr a)
staticVarD n t = varFromData Static n t (varDocD n)

extVarD :: (RenderSym repr) => Label -> Label -> repr (Type repr a) -> 
  repr (Variable repr a)
extVarD l n t = varFromData Dynamic (l ++ "." ++ n) t (extVarDocD l n)

selfD :: (RenderSym repr) => Label -> repr (Variable repr Other)
selfD l = varFromData Dynamic "this" (obj l) selfDocD

enumVarD :: (RenderSym repr) => Label -> Label -> repr (Variable repr Other)
enumVarD e en = var e (enumType en)

classVarD :: (RenderSym repr) => (Doc -> Doc -> Doc) -> repr (Type repr Other) 
  -> repr (Variable repr a) -> repr (Variable repr a)
classVarD f c v = classVarCheckStatic $ varFromData (variableBind v) 
  (getTypeString c ++ "." ++ variableName v) 
  (variableType v) (f (getTypeDoc c) (variableDoc v))

objVarD :: (RenderSym repr) => repr (Variable repr Other) -> 
  repr (Variable repr a) -> repr (Variable repr a)
objVarD o v = varFromData Dynamic (variableName o ++ "." ++ variableName v) 
  (variableType v) (objVarDocD (variableDoc o) (variableDoc v))

objVarSelfD :: (RenderSym repr) => Label -> Label -> repr (Type repr a) -> 
  repr (Variable repr a)
objVarSelfD l n t = objVar (self l) (var n t)

listVarD :: (RenderSym repr) => Label -> repr (Permanence repr) -> 
  repr (Type repr a) -> repr (Variable repr [a])
listVarD n p t = var n (listType p t)

listOfD :: (RenderSym repr) => Label -> repr (Type repr a) ->
  repr (Variable repr [a])
listOfD n = listVar n static_

iterVarD :: (RenderSym repr) => Label -> repr (Type repr Other) -> 
  repr (Variable repr Other)
iterVarD n t = var n (iterator t)

valueOfD :: (RenderSym repr) => repr (Variable repr a) -> repr (Value repr a)
valueOfD v = valFromData Nothing (variableType v) (variableDoc v)

argD :: (RenderSym repr) => repr (Value repr Other) -> repr (Value repr Other) 
  -> repr (Value repr Other)
argD n args = valFromData Nothing string (argDocD n args)

enumElementD :: (RenderSym repr) => Label -> Label -> repr (Value repr Other)
enumElementD en e = valFromData Nothing (enumType en) (enumElemDocD en e)

argsListD :: (RenderSym repr) => String -> repr (Value repr [Other])
argsListD l = valFromData Nothing (listType static_ string) (text l)
 
funcAppD :: (RenderSym repr) => Label -> repr (Type repr a) -> 
  [repr (Value repr Other)] -> repr (Value repr a)
funcAppD n t vs = valFromData Nothing t (funcAppDocD n vs)

extFuncAppD :: (RenderSym repr) => Library -> Label -> repr (Type repr a) -> 
  [repr (Value repr Other)] -> repr (Value repr a)
extFuncAppD l n = funcAppD (l ++ "." ++ n)

newObjD :: (RenderSym repr) => (repr (Type repr Other) -> Doc -> Doc) -> 
  repr (Type repr Other) -> [repr (Value repr Other)] -> repr (Value repr Other)
newObjD f t vs = valFromData Nothing t (f t (valueList vs))

notNullD :: (RenderSym repr) => repr (Value repr Other) -> 
  repr (Value repr Boolean)
notNullD v = v ?!= valueOf (var "null" (valueType v))

objAccessD :: (RenderSym repr) => repr (Value repr b) -> repr (Function repr a) 
  -> repr (Value repr a)
objAccessD v f = valFromData Nothing (functionType f) (objAccessDocD 
  (valueDoc v) (functionDoc f))

objMethodCallD :: (RenderSym repr) => repr (Type repr a) -> 
  repr (Value repr Other) -> Label -> [repr (Value repr Other)] -> 
  repr (Value repr a)
objMethodCallD t o f ps = objAccess o (func f t ps)

objMethodCallNoParamsD :: (RenderSym repr) => repr (Type repr a) -> 
  repr (Value repr Other) -> Label -> repr (Value repr a)
objMethodCallNoParamsD t o f = objMethodCall t o f []

selfAccessD :: (RenderSym repr) => Label -> repr (Function repr a) -> 
  repr (Value repr a)
selfAccessD l = objAccess (valueOf $ self l)

listIndexExistsD :: (RenderSym repr) => repr (Value repr [a]) -> 
  repr (Value repr Other) -> repr (Value repr Boolean)
listIndexExistsD lst index = listSize lst ?> index

indexOfD :: (RenderSym repr) => Label -> repr (Value repr [a]) -> 
  repr (Value repr a) -> repr (Value repr Other)
indexOfD f l v = objAccess l (func f S.int [toOtherValue v])

-- Functions --

funcDocD :: Doc -> Doc
funcDocD fnApp = dot <> fnApp

castDocD :: TypedType a -> Doc
castDocD t = parens $ typeDoc t

sizeDocD :: Doc
sizeDocD = dot <> text "Count"

listAccessFuncDocD :: (RenderSym repr) => repr (Value repr Other) -> Doc
listAccessFuncDocD v = brackets $ valueDoc v

listSetFuncDocD :: Doc -> Doc -> Doc
listSetFuncDocD i v = brackets i <+> equals <+> v

objAccessDocD :: Doc -> Doc -> Doc
objAccessDocD v f = v <> f

castObjDocD :: Doc -> TypedValue a -> Doc
castObjDocD t v = t <> parens (valDoc v)

funcD :: (RenderSym repr) => Label -> repr (Type repr a) -> 
  [repr (Value repr Other)] -> repr (Function repr a)
funcD l t vs = funcFromData t (funcDocD (valueDoc $ funcApp l t vs))

getD :: (RenderSym repr) => repr (Value repr Other) -> repr (Variable repr a) 
  -> repr (Value repr a)
getD v vToGet = v $. getFunc vToGet

setD :: (RenderSym repr) => repr (Value repr Other) -> repr (Variable repr a) 
  -> repr (Value repr a) -> repr (Value repr Other)
setD v vToSet toVal = v $. setFunc (valueType v) vToSet toVal

listSizeD :: (RenderSym repr) => repr (Value repr [a]) -> 
  repr (Value repr Other)
listSizeD v = v $. listSizeFunc

listAddD :: (RenderSym repr) => repr (Value repr [a]) -> 
  repr (Value repr Other) -> repr (Value repr a) -> repr (Value repr [a])
listAddD v i vToAdd = v $. listAddFunc v i vToAdd

listAppendD :: (RenderSym repr) => repr (Value repr [a]) -> 
  repr (Value repr a) -> repr (Value repr [a])
listAppendD v vToApp = v $. listAppendFunc vToApp

iterBeginD :: (RenderSym repr) => repr (Value repr [a]) -> 
  repr (Value repr Other)
iterBeginD v = v $. iterBeginFunc (listInnerType $ valueType v)

iterEndD :: (RenderSym repr) => repr (Value repr [a]) -> 
  repr (Value repr Other)
iterEndD v = v $. iterEndFunc (listInnerType $ valueType v)

listAccessD :: (RenderSym repr) => repr (Value repr [a]) -> 
  repr (Value repr Other) -> repr (Value repr a)
listAccessD v i = v $. listAccessFunc (listInnerType $ valueType v) i

listSetD :: (RenderSym repr) => repr (Value repr [a]) -> 
  repr (Value repr Other) -> repr (Value repr a) -> repr (Value repr [a])
listSetD v i toVal = v $. listSetFunc v i toVal

getFuncD :: (RenderSym repr) => repr (Variable repr a) -> repr (Function repr a)
getFuncD v = func (getterName $ variableName v) (variableType v) []

setFuncD :: (RenderSym repr) => repr (Type repr Other) -> 
  repr (Variable repr a) -> repr (Value repr a) -> repr (Function repr Other)
setFuncD t v toVal = func (setterName $ variableName v) t [toOtherValue toVal]

listSizeFuncD :: (RenderSym repr) => repr (Function repr Other)
listSizeFuncD = func "size" S.int []

listAddFuncD :: (RenderSym repr) => Label -> repr (Value repr Other) -> 
  repr (Value repr a) -> repr (Function repr [a])
listAddFuncD f i v = func f (listType static_ $ valueType v) [i, toOtherValue v]

listAppendFuncD :: (RenderSym repr) => Label -> repr (Value repr a) -> 
  repr (Function repr [a])
listAppendFuncD f v = func f (listType static_ $ valueType v) [toOtherValue v]

iterBeginError :: String -> String
iterBeginError l = "Attempt to use iterBeginFunc in " ++ l ++ ", but " ++ l ++ 
  " has no iterators"

iterEndError :: String -> String
iterEndError l = "Attempt to use iterEndFunc in " ++ l ++ ", but " ++ l ++ 
  " has no iterators"

listAccessFuncD :: (RenderSym repr) => repr (Type repr a) -> 
  repr (Value repr Other) -> repr (Function repr a)
listAccessFuncD t i = funcFromData t (listAccessFuncDocD $ intValue i)

listAccessFuncD' :: (RenderSym repr) => Label -> repr (Type repr a) -> 
  repr (Value repr Other) -> repr (Function repr a)
listAccessFuncD' f t i = func f t [intValue i]

listSetFuncD :: (RenderSym repr) => (Doc -> Doc -> Doc) -> 
  repr (Value repr [a]) -> repr (Value repr Other) -> repr (Value repr a) -> 
  repr (Function repr [a])
listSetFuncD f v i toVal = funcFromData (valueType v) (f (valueDoc $ intValue i)
  (valueDoc toVal))

-- Keywords --

includeD :: Label -> Label -> Doc
includeD incl n = text incl <+> text n

-- Permanence --

staticDocD :: Doc
staticDocD = text "static"

dynamicDocD :: Doc
dynamicDocD = empty

-- Jumps --

breakDocD :: Doc
breakDocD = text "break"

continueDocD :: Doc
continueDocD = text "continue"

-- Scope --

privateDocD :: Doc
privateDocD = text "private"

publicDocD :: Doc
publicDocD = text "public"

-- Comment Functions -- 

blockCmtDoc :: [String] -> Doc -> Doc -> Doc
blockCmtDoc lns start end = start <+> vcat (map text lns) <+> end

docCmtDoc :: [String] -> Doc -> Doc -> Doc
docCmtDoc lns start end = emptyIfNull lns $
  vcat $ start : map (indent . text) lns ++ [end]

commentedItem :: Doc -> Doc -> Doc
commentedItem cmt itm = emptyIfEmpty itm cmt $+$ itm

commentLength :: Int
commentLength = 75

endCommentLabel :: Label
endCommentLabel = "End"

addCommentsDocD :: Label -> Doc -> Doc -> Doc
addCommentsDocD c cStart b = vcat [
  commentDelimit c cStart,
  b,
  endCommentDelimit c cStart]

commentDelimit :: Label -> Doc -> Doc
commentDelimit c cStart = 
  let com = cStart <> text (" " ++ c ++ " ")
  in com <> text (dashes (render com) commentLength)

endCommentDelimit :: Label -> Doc -> Doc
endCommentDelimit c = commentDelimit (endCommentLabel ++ " " ++ c)

dashes :: String -> Int -> String
dashes s l = replicate (l - length s) '-'

functionDox :: String -> [(String, String)] -> [String] -> [String]
functionDox desc params returns = [doxBrief ++ desc | not (null desc)]
  ++ map (\(v, vDesc) -> doxParam ++ v ++ " " ++ vDesc) params
  ++ map (doxReturn ++) returns

classDoc :: String -> [String]
classDoc desc = [doxBrief ++ desc | not (null desc)]

moduleDoc :: String -> [String] -> String -> String -> [String]
moduleDoc desc as date m = (doxFile ++ m) : 
  [doxAuthor ++ stringList as | not (null as)] ++
  [doxDate ++ date | not (null date)] ++ 
  [doxBrief ++ desc | not (null desc)]

commentedModD :: Doc -> FileData -> FileData
commentedModD cmt m = updateFileMod (updateModDoc (commentedItem cmt 
  ((modDoc . fileMod) m)) (fileMod m)) m

docFuncRepr :: (MethodSym repr) => String -> [String] -> [String] -> 
  repr (Method repr) -> repr (Method repr)
docFuncRepr desc pComms rComms f = commentedFunc (docComment $ functionDox desc
  (zip (map parameterName (parameters f)) pComms) rComms) f

-- Helper Functions --

valList :: [TypedValue Other] -> Doc
valList vs = hcat (intersperse (text ", ") (map valDoc vs))

valList' :: [TypedValue a] -> Doc
valList' vs = hcat (intersperse (text ", ") (map valDoc vs))

valueList :: (RenderSym repr) => [repr (Value repr a)] -> Doc
valueList vs = hcat (intersperse (text ", ") (map valueDoc vs))

varList :: [TypedVar Other] -> Doc
varList vs = hcat (intersperse (text ", ") (map varDoc vs))

prependToBody :: (Doc, Terminator) -> Doc -> Doc
prependToBody s b = vcat [fst $ statementDocD s, maybeBlank, b]
  where maybeBlank = emptyIfEmpty (fst s) (emptyIfEmpty b blank)

appendToBody :: Doc -> (Doc, Terminator) -> Doc
appendToBody b s = vcat [b, maybeBlank, fst $ statementDocD s]
  where maybeBlank = emptyIfEmpty b (emptyIfEmpty (fst s) blank)

surroundBody :: (Doc, Terminator) -> Doc -> (Doc, Terminator) -> Doc
surroundBody p b a = prependToBody p (appendToBody b a)

getterName :: String -> String
getterName s = "get" ++ capitalize s

setterName :: String -> String
setterName s = "set" ++ capitalize s

setMainMethod :: MethodData -> MethodData
setMainMethod (MthD _ ps d) = MthD True ps d

setEmpty :: (RenderSym repr) => repr (Statement repr) -> repr (Statement repr)
setEmpty s = stateFromData (statementDoc s) Empty

exprParensL :: OpData -> TypedValue a -> (Doc -> Doc)
exprParensL o v = if maybe False (< opPrec o) (valPrec v) then parens else id

exprParensR :: OpData -> TypedValue a -> (Doc -> Doc)
exprParensR o v = if maybe False (<= opPrec o) (valPrec v) then parens else id

intValue :: (RenderSym repr) => repr (Value repr Other) -> repr (Value repr Other)
intValue i = intValue' (getType $ valueType i)
  where intValue' Integer = i
        intValue' (Enum _) = cast S.int i
        intValue' _ = error "Value passed must be Integer or Enum"

filterOutObjs :: (VariableSym repr) => [repr (Variable repr Other)] -> 
  [repr (Variable repr Other)]
filterOutObjs = filter (not . isObject . getType . variableType)

doxCommand, doxBrief, doxParam, doxReturn, doxFile, doxAuthor, doxDate :: String
doxCommand = "\\"
doxBrief = doxCommand ++ "brief "
doxParam = doxCommand ++ "param "
doxReturn = doxCommand ++ "return "
doxFile = doxCommand  ++ "file "
doxAuthor = doxCommand ++ "author "
doxDate = doxCommand ++ "date "
