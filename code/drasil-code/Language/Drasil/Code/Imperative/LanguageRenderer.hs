{-# LANGUAGE PostfixOperators #-}

-- | The structure for a class of renderers is defined here.
module Language.Drasil.Code.Imperative.LanguageRenderer (
  -- * Common Syntax
  classDec, dot, doubleSlash, forLabel, new, blockCmtStart, blockCmtEnd,
  docCmtStart, observerListName, doxConfigName, makefileName, addExt,
  
  -- * Default Functions available for use in renderers
  packageDocD, fileDoc', moduleDocD, classDocD, enumDocD, enumElementsDocD, 
  enumElementsDocD', multiStateDocD, blockDocD, bodyDocD, outDoc, printDoc,
  printFileDocD, boolTypeDocD, intTypeDocD, floatTypeDocD, 
  charTypeDocD, stringTypeDocD, fileTypeDocD, typeDocD, enumTypeDocD, 
  listTypeDocD, voidDocD, constructDocD, stateParamDocD, paramListDocD, mkParam,
  methodDocD, methodListDocD, stateVarDocD, stateVarListDocD, alwaysDel, 
  ifCondDocD, switchDocD, forDocD, forEachDocD, whileDocD, tryCatchDocD, 
  assignDocD, multiAssignDoc, plusEqualsDocD, plusEqualsDocD', plusPlusDocD, 
  plusPlusDocD', varDecDocD, varDecDefDocD, listDecDocD, listDecDefDocD, 
  statementDocD, returnDocD, commentDocD, freeDocD, throwDocD, mkSt, mkStNoEnd,
  stringListVals', stringListLists', stratDocD, unOpPrec, notOpDocD, notOpDocD',
  negateOpDocD, sqrtOpDocD, sqrtOpDocD', absOpDocD, absOpDocD', logOpDocD, 
  logOpDocD', lnOpDocD, lnOpDocD', expOpDocD, expOpDocD', sinOpDocD, sinOpDocD',
  cosOpDocD, cosOpDocD', tanOpDocD, tanOpDocD', asinOpDocD, asinOpDocD', 
  acosOpDocD, acosOpDocD', atanOpDocD, atanOpDocD', unOpDocD, unExpr, unExpr',
  typeUnExpr, powerPrec, multPrec, andPrec, orPrec, equalOpDocD, notEqualOpDocD,
  greaterOpDocD, greaterEqualOpDocD, lessOpDocD, lessEqualOpDocD, plusOpDocD, 
  minusOpDocD, multOpDocD, divideOpDocD, moduloOpDocD, powerOpDocD, andOpDocD, 
  orOpDocD, binOpDocD, binOpDocD', binExpr, binExpr', typeBinExpr, mkVal, 
  litTrueD, litFalseD, litCharD, litFloatD, litIntD, litStringD, varDocD, 
  extVarDocD, selfDocD, argDocD, enumElemDocD, objVarDocD, inlineIfD, 
  funcAppDocD, extFuncAppDocD, stateObjDocD, listStateObjDocD, objDecDefDocD, 
  constDecDefDocD, notNullDocD, listIndexExistsDocD, funcDocD, castDocD, 
  sizeDocD, listAccessFuncDocD, listSetFuncDocD, objAccessDocD, castObjDocD, 
  includeD, breakDocD, continueDocD, staticDocD, dynamicDocD, privateDocD, 
  publicDocD, blockCmtDoc, docCmtDoc, commentedItem, addCommentsDocD, 
  functionDoc, classDoc, moduleDoc, docFuncRepr, valList, prependToBody, 
  appendToBody, surroundBody, getterName, setterName, setMain, setMainMethod, 
  setEmpty, intValue
) where

import Utils.Drasil (capitalize, indent, indentList)

import Language.Drasil.Code.Code (CodeType(..))
import Language.Drasil.Code.Imperative.Symantics (Label, Library,
  RenderSym(..), BodySym(..), StateTypeSym(getType, listInnerType), 
  VariableSym(..), ValueSym(..), NumericExpression(..), BooleanExpression(..), 
  InternalValue(..), FunctionSym(..), SelectorFunction(..), 
  InternalStatement(..), StatementSym(..), ControlStatementSym(..), 
  ParameterSym(..), MethodSym(..), BlockCommentSym(..))
import qualified Language.Drasil.Code.Imperative.Symantics as S (StateTypeSym(int))
import Language.Drasil.Code.Imperative.Data (Terminator(..), FileData(..), 
  fileD, FuncData(..), ModData(..), updateModDoc, MethodData(..), OpData(..), 
  od, ParamData(..), pd, TypeData(..), td, ValData(..), vd, VarData(..))
import Language.Drasil.Code.Imperative.Helpers (angles,blank, doubleQuotedText,
  hicat,vibcat,vmap, emptyIfEmpty, emptyIfNull, getNestDegree)

import Data.List (intersperse, last)
import Prelude hiding (break,print,return,last,mod,(<>))
import Text.PrettyPrint.HughesPJ (Doc, text, empty, render, (<>), (<+>), ($+$),
  brackets, parens, isEmpty, rbrace, lbrace, vcat, char, double, quotes, 
  integer, semi, equals, braces, int, comma, colon, hcat)

----------------------------------------
-- Syntax common to several renderers --
----------------------------------------

classDec, dot, doubleSlash, forLabel, new, blockCmtStart, blockCmtEnd,
  docCmtStart :: Doc
classDec = text "class"
dot = text "."
doubleSlash = text "//"
forLabel = text "for"
new = text "new"
blockCmtStart = text "/*"
blockCmtEnd = text "*/"
docCmtStart = text "/**"

observerListName :: Label
observerListName = "observerList"

doxConfigName, makefileName :: String
doxConfigName = "doxConfig"
makefileName = "Makefile"

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

-- IO --

printDoc :: ValData -> ValData -> Doc
printDoc printFn v = valDoc printFn <> parens (valDoc v)

printListDoc :: (RenderSym repr) => Integer -> repr (Value repr) -> 
  (repr (Value repr) -> repr (Statement repr)) -> 
  (String -> repr (Statement repr)) -> 
  (String -> repr (Statement repr)) -> 
  repr (Statement repr)
printListDoc n v prFn prStrFn prLnFn = multi [prStrFn "[", 
  for (varDecDef i (litInt 0)) (valueOf i ?< (listSize v #- litInt 1))
    (i &++) (bodyStatements [prFn (listAccess v (valueOf i)), prStrFn ", /f "]), 
  ifNoElse [(listSize v ?> litInt 0, oneLiner $
    prFn (listAccess v (listSize v #- litInt 1)))], 
  prLnFn "]"]
  where l_i = "list_i" ++ show n
        i = var l_i S.int

printObjDoc :: String -> (String -> repr (Statement repr)) 
  -> repr (Statement repr)
printObjDoc n prLnFn = prLnFn $ "Instance of " ++ n ++ " object"

outDoc :: (RenderSym repr) => Bool -> repr (Value repr) -> repr (Value repr) 
  -> Maybe (repr (Value repr)) -> repr (Statement repr)
outDoc newLn printFn v f = outDoc' (getType $ valueType v)
  where outDoc' (List t) = printListDoc (getNestDegree 1 t) v prFn prStrFn 
          prLnFn
        outDoc' (Object n) = printObjDoc n prLnFn
        outDoc' _ = printSt newLn printFn v f
        prFn = maybe print printFile f
        prStrFn = maybe printStr printFileStr f
        prLnFn = if newLn then maybe printStrLn printFileStrLn f else maybe printStr printFileStr f 

printFileDocD :: Label -> ValData -> Doc
printFileDocD fn f = valDoc f <> dot <> text fn

-- Type Printers --

boolTypeDocD :: TypeData
boolTypeDocD = td Boolean (text "Boolean") -- capital B?

intTypeDocD :: TypeData
intTypeDocD = td Integer (text "int")

floatTypeDocD :: TypeData
floatTypeDocD = td Float (text "float")

charTypeDocD :: TypeData
charTypeDocD = td Char (text "char")

stringTypeDocD :: TypeData
stringTypeDocD = td String (text "string")

fileTypeDocD :: TypeData
fileTypeDocD = td File (text "File")

typeDocD :: Label -> TypeData
typeDocD t = td (Object t) (text t)

enumTypeDocD :: Label -> TypeData
enumTypeDocD t = td (Enum t) (text t)

listTypeDocD :: TypeData -> Doc -> TypeData
listTypeDocD t lst = td (List (cType t)) (lst <> angles (typeDoc t))

-- Method Types --

voidDocD :: TypeData
voidDocD = td Void (text "void")

constructDocD :: Label -> Doc
constructDocD _ = empty

-- Parameters --

stateParamDocD :: VarData -> Doc
stateParamDocD v = typeDoc (varType v) <+> varDoc v

paramListDocD :: [ParamData] -> Doc
paramListDocD = hicat (text ", ") . map paramDoc

mkParam :: (VarData -> Doc) -> VarData -> ParamData
mkParam f v = pd (varName v) (varType v) (f v)

-- Method --

methodDocD :: Label -> Doc -> Doc -> TypeData -> Doc -> Doc -> Doc
methodDocD n s p t ps b = vcat [
  s <+> p <+> typeDoc t <+> text n <> parens ps <+> lbrace,
  indent b,
  rbrace]

methodListDocD :: [Doc] -> Doc
methodListDocD ms = vibcat methods
  where methods = filter (not . isEmpty) ms

-- StateVar --

stateVarDocD :: Doc -> Doc -> VarData -> Doc -> Doc
stateVarDocD s p v end = s <+> p <+> typeDoc (varType v) <+> varDoc v <> end

stateVarListDocD :: [Doc] -> Doc
stateVarListDocD = vcat

alwaysDel :: Int
alwaysDel = 4

-- Controls --

ifCondDocD :: Doc -> Doc -> Doc -> Doc -> [(ValData, Doc)] -> Doc
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

switchDocD :: (Doc, Terminator) -> ValData -> Doc -> 
  [(ValData, Doc)] -> Doc
switchDocD breakState v defBody cs = 
  let caseDoc (l, result) = vcat [
        text "case" <+> valDoc l <> colon,
        indentList [
          result,
          fst breakState]]
      defaultSection = vcat [
        text "default" <> colon,
        indentList [
          defBody,
          fst breakState]]
  in vcat [
      text "switch" <> parens (valDoc v) <+> lbrace,
      indentList [
        vmap caseDoc cs,
        defaultSection],
      rbrace]

-- These signatures wont be quite so horrendous if/when we pass language options
-- (blockStart, etc.) in as shared environment
forDocD :: Doc -> Doc -> (Doc, Terminator) -> ValData -> 
  (Doc, Terminator) -> Doc -> Doc
forDocD bStart bEnd sInit vGuard sUpdate b = vcat [
  forLabel <+> parens (fst sInit <> semi <+> valDoc vGuard <> semi <+> 
    fst sUpdate) <+> bStart,
  indent b,
  bEnd]

forEachDocD :: Label -> Doc -> Doc -> Doc -> Doc -> TypeData -> ValData -> Doc 
  -> Doc
forEachDocD l bStart bEnd forEachLabel inLabel t v b =
  vcat [forEachLabel <+> parens (typeDoc t <+> text l <+> inLabel <+> valDoc v) 
    <+> bStart,
  indent b,
  bEnd]

whileDocD :: Doc -> Doc -> ValData -> Doc -> Doc
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

stratDocD :: Doc -> (Doc, Terminator) -> Doc
stratDocD b resultState = vcat [
  b,
  fst resultState]

-- Statements --

assignDocD :: VarData -> ValData -> Doc
assignDocD vr vl = varDoc vr <+> equals <+> valDoc vl

multiAssignDoc :: [VarData] -> [ValData] -> Doc
multiAssignDoc vrs vls = varList vrs <+> equals <+> valList vls

plusEqualsDocD :: VarData -> ValData -> Doc
plusEqualsDocD vr vl = varDoc vr <+> text "+=" <+> valDoc vl

plusEqualsDocD' :: VarData -> OpData -> ValData -> Doc
plusEqualsDocD' vr plusOp vl = varDoc vr <+> equals <+> varDoc vr <+> 
  opDoc plusOp <+> valDoc vl

plusPlusDocD :: VarData -> Doc
plusPlusDocD v = varDoc v <> text "++"

plusPlusDocD' :: VarData -> OpData -> Doc
plusPlusDocD' v plusOp = varDoc v <+> equals <+> varDoc v <+> opDoc plusOp <+>
  int 1

varDecDocD :: VarData -> Doc
varDecDocD v = typeDoc (varType v) <+> varDoc v

varDecDefDocD :: VarData -> ValData -> Doc
varDecDefDocD v def = typeDoc (varType v) <+> varDoc v <+> equals <+> valDoc def

listDecDocD :: VarData -> ValData -> Doc
listDecDocD v n = typeDoc (varType v) <+> varDoc v <+> equals <+> new <+> 
  typeDoc (varType v) <> parens (valDoc n)

listDecDefDocD :: VarData -> [ValData] -> Doc
listDecDefDocD v vs = typeDoc (varType v) <+> varDoc v <+> equals <+> new <+> 
  typeDoc (varType v) <+> braces (valList vs)

objDecDefDocD :: VarData -> ValData -> Doc
objDecDefDocD = varDecDefDocD

constDecDefDocD :: VarData -> ValData -> Doc
constDecDefDocD v def = text "const" <+> typeDoc (varType v) <+> varDoc v <+> 
  equals <+> valDoc def

returnDocD :: [ValData] -> Doc
returnDocD vs = text "return" <+> valList vs

commentDocD :: Label -> Doc -> Doc
commentDocD cmt cStart = cStart <+> text cmt

freeDocD :: VarData -> Doc
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

stringListVals' :: (RenderSym repr) => [repr (Variable repr)] -> 
  repr (Value repr) -> repr (Statement repr)
stringListVals' vars sl = multi $ stringList (getType $ valueType sl)
    where stringList (List String) = assignVals vars 0
          stringList _ = error 
            "Value passed to stringListVals must be a list of strings"
          assignVals [] _ = []
          assignVals (v:vs) n = assign v (cast (variableType v) 
            (listAccess sl (litInt n))) : assignVals vs (n+1)

stringListLists' :: (RenderSym repr) => [repr (Variable repr)] -> repr (Value repr)
  -> repr (Statement repr)
stringListLists' lsts sl = stringList (getType $ valueType sl)
  where stringList (List String) = listVals (map (getType . variableType) lsts)
        stringList _ = error 
          "Value passed to stringListLists must be a list of strings"
        listVals [] = loop
        listVals (List _:vs) = listVals vs
        listVals _ = error 
          "All values passed to stringListLists must have list types"
        loop = forRange l_i (litInt 0) (listSize sl #/ numLists) (litInt 1)
          (bodyStatements $ appendLists (map valueOf lsts) 0)
        appendLists [] _ = []
        appendLists (v:vs) n = valState (listAppend v (cast (listInnerType $ 
          valueType v) (listAccess sl ((v_i #* numLists) #+ litInt n)))) 
          : appendLists vs (n+1)
        numLists = litInt (toInteger $ length lsts)
        l_i = "stringlist_i"
        v_i = valueOf $ var l_i S.int
        

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

unExpr :: OpData -> ValData -> ValData
unExpr u v = mkExpr (opPrec u) (valType v) (unOpDocD (opDoc u) (valDoc v))

unExpr' :: OpData -> ValData -> ValData
unExpr' u v = mkExpr (opPrec u) (valType v) (unOpDocD' (opDoc u) (valDoc v))

typeUnExpr :: OpData -> TypeData -> ValData -> ValData
typeUnExpr u t v = mkExpr (opPrec u) t (unOpDocD (opDoc u) (valDoc v))

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
  
binExpr :: OpData -> ValData -> ValData -> ValData
binExpr b v1 v2 = mkExpr (opPrec b) (numType (valType v1) (valType v2)) 
  (binOpDocD (opDoc b) (exprParensL b v1 $ valDoc v1) (exprParensR b v2 $ 
  valDoc v2))

binExpr' :: OpData -> ValData -> ValData -> ValData
binExpr' b v1 v2 = mkExpr 9 (numType (valType v1) (valType v2)) 
  (binOpDocD' (opDoc b) (valDoc v1) (valDoc v2))

numType :: TypeData -> TypeData -> TypeData
numType t1 t2 = numericType (cType t1) (cType t2)
  where numericType Integer Integer = t1
        numericType Float _ = t1
        numericType _ Float = t2
        numericType _ _ = error "Numeric types required for numeric expression"

typeBinExpr :: OpData -> TypeData -> ValData -> ValData -> ValData
typeBinExpr b t v1 v2 = mkExpr (opPrec b) t (binOpDocD (opDoc b) (exprParensL b 
  v1 $ valDoc v1) (exprParensR b v2 $ valDoc v2))

mkVal :: TypeData -> Doc -> ValData
mkVal = vd Nothing

mkExpr :: Int -> TypeData -> Doc -> ValData
mkExpr p = vd (Just p)

-- Literals --

litTrueD :: Doc
litTrueD = text "true"

litFalseD :: Doc
litFalseD = text "false"

litCharD :: Char -> Doc
litCharD c = quotes $ char c

litFloatD :: Double -> Doc
litFloatD = double

litIntD :: Integer -> Doc
litIntD = integer

litStringD :: String -> Doc
litStringD = doubleQuotedText

-- Value Printers --

varDocD :: Label -> Doc
varDocD = text

extVarDocD :: Library -> Label -> Doc
extVarDocD l n = text l <> dot <> text n

selfDocD :: Doc
selfDocD = text "this"

argDocD :: ValData -> ValData -> Doc
argDocD n args = valDoc args <> brackets (valDoc n)

enumElemDocD :: Label -> Label -> Doc
enumElemDocD en e = text en <> dot <> text e

objVarDocD :: VarData -> VarData ->  Doc
objVarDocD n1 n2 = varDoc n1 <> dot <> varDoc n2

inlineIfD :: ValData -> ValData -> ValData -> ValData
inlineIfD c v1 v2 = vd (valPrec c) (valType v1) (valDoc c <+> text "?" <+> 
  valDoc v1 <+> text ":" <+> valDoc v2)

funcAppDocD :: Label -> [ValData] -> Doc
funcAppDocD n vs = text n <> parens (valList vs)

extFuncAppDocD :: Library -> Label -> [ValData] -> Doc
extFuncAppDocD l n = funcAppDocD (l ++ "." ++ n)

stateObjDocD :: TypeData -> Doc -> Doc
stateObjDocD st vs = new <+> typeDoc st <> parens vs

listStateObjDocD :: Doc -> TypeData -> Doc -> Doc
listStateObjDocD lstObj st vs = lstObj <+> typeDoc st <> parens vs

notNullDocD :: OpData -> ValData -> ValData -> Doc
notNullDocD op v1 v2 = binOpDocD (opDoc op) (valDoc v1) (valDoc v2)

listIndexExistsDocD :: OpData -> ValData -> ValData -> Doc
listIndexExistsDocD greater lst index = parens (valDoc lst <> 
  text ".Length" <+> opDoc greater <+> valDoc index) 

-- Functions --

funcDocD :: ValData -> Doc
funcDocD fnApp = dot <> valDoc fnApp

castDocD :: TypeData -> Doc
castDocD t = parens $ typeDoc t

sizeDocD :: Doc
sizeDocD = dot <> text "Count"

listAccessFuncDocD :: ValData -> Doc
listAccessFuncDocD v = brackets $ valDoc v

listSetFuncDocD :: ValData -> ValData -> Doc
listSetFuncDocD i v = brackets (valDoc i) <+> equals <+> valDoc v

objAccessDocD :: ValData -> FuncData -> Doc
objAccessDocD v f = valDoc v <> funcDoc f

castObjDocD :: Doc -> ValData -> Doc
castObjDocD t v = t <> parens (valDoc v)

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

functionDoc :: String -> [(String, String)] -> [String]
functionDoc desc params = [doxBrief ++ desc | not (null desc)]
  ++ map (\(v, vDesc) -> doxParam ++ v ++ " " ++ vDesc) params

classDoc :: String -> [String]
classDoc desc = [doxBrief ++ desc | not (null desc)]

moduleDoc :: String -> String -> String -> [String]
moduleDoc desc m ext = (doxFile ++ addExt ext m) : 
  [doxBrief ++ desc | not (null desc)]

docFuncRepr :: (MethodSym repr) => String -> [String] -> repr (Method repr) -> 
  repr (Method repr)
docFuncRepr desc pComms f = commentedFunc (docComment $ functionDoc desc
  (zip (map parameterName (parameters f)) pComms)) f

-- Helper Functions --

valList :: [ValData] -> Doc
valList vs = hcat (intersperse (text ", ") (map valDoc vs))

varList :: [VarData] -> Doc
varList vs = hcat (intersperse (text ", ") (map varDoc vs))

prependToBody :: (Doc, Terminator) -> Doc -> Doc
prependToBody s b = vcat [fst $ statementDocD s, maybeBlank, b]
  where maybeBlank = emptyIfEmpty b blank

appendToBody :: Doc -> (Doc, Terminator) -> Doc
appendToBody b s = vcat [b, maybeBlank, fst $ statementDocD s]
  where maybeBlank = emptyIfEmpty b blank

surroundBody :: (Doc, Terminator) -> Doc -> (Doc, Terminator) -> Doc
surroundBody p b a = prependToBody p (appendToBody b a)

getterName :: String -> String
getterName s = "Get" ++ capitalize s

setterName :: String -> String
setterName s = "Set" ++ capitalize s

setMain :: (Doc, Bool) -> (Doc, Bool)
setMain (d, _) = (d, True)

setMainMethod :: MethodData -> MethodData
setMainMethod (MthD _ ps d) = MthD True ps d

setEmpty :: (Doc, Terminator) -> (Doc, Terminator)
setEmpty (d, _) = (d, Empty)

exprParensL :: OpData -> ValData -> (Doc -> Doc)
exprParensL o v = if maybe False (< opPrec o) (valPrec v) then parens else id

exprParensR :: OpData -> ValData -> (Doc -> Doc)
exprParensR o v = if maybe False (<= opPrec o) (valPrec v) then parens else id

intValue :: (RenderSym repr) => repr (Value repr) -> repr (Value repr)
intValue i = intValue' (getType $ valueType i)
  where intValue' Integer = i
        intValue' (Enum _) = cast S.int i
        intValue' _ = error "Value passed must be Integer or Enum"

doxCommand, doxBrief, doxParam, doxFile :: String
doxCommand = "\\"
doxBrief = doxCommand ++ "brief "
doxParam = doxCommand ++ "param "
doxFile = doxCommand  ++ "file "
