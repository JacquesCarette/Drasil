{-# LANGUAGE PostfixOperators #-}

-- | The structure for a class of renderers is defined here.
module GOOL.Drasil.LanguageRenderer (
  -- * Common Syntax
  classDec, dot, doubleSlash, elseIfLabel, forLabel, inLabel, new, 
  blockCmtStart, blockCmtEnd, docCmtStart, observerListName, addExt,
  
  -- * Default Functions available for use in renderers
  packageDocD, fileDoc', moduleDocD, classDocD, enumDocD, enumElementsDocD, 
  enumElementsDocD', multiStateDocD, blockDocD, bodyDocD, outDoc, 
  printDoc, printFileDocD, destructorError, paramDocD, methodDocD, 
  methodListDocD, stateVarDocD, constVarDocD, stateVarListDocD, switchDocD, 
  assignDocD, multiAssignDoc, plusEqualsDocD, plusPlusDocD, listDecDocD, 
  listDecDefDocD, statementDocD, getTermDoc, returnDocD, commentDocD, freeDocD, 
  mkSt, mkStNoEnd, stringListVals', stringListLists', unOpPrec, notOpDocD, 
  notOpDocD', negateOpDocD, sqrtOpDocD, sqrtOpDocD', absOpDocD, absOpDocD',
  expOpDocD, expOpDocD', sinOpDocD, sinOpDocD', cosOpDocD, cosOpDocD', 
  tanOpDocD, tanOpDocD', asinOpDocD, asinOpDocD', acosOpDocD, acosOpDocD', 
  atanOpDocD, atanOpDocD', unOpDocD, unExpr, unExpr', typeUnExpr, powerPrec, 
  multPrec, andPrec, orPrec, equalOpDocD, notEqualOpDocD, greaterOpDocD, 
  greaterEqualOpDocD, lessOpDocD, lessEqualOpDocD, plusOpDocD, minusOpDocD, 
  multOpDocD, divideOpDocD, moduloOpDocD, powerOpDocD, andOpDocD, orOpDocD, 
  binOpDocD, binOpDocD', binExpr, binExpr', typeBinExpr, mkVal, mkVar,
  mkStaticVar, litTrueD, litFalseD, litCharD, litFloatD, litIntD, litStringD, 
  varDocD, extVarDocD, selfDocD, argDocD, enumElemDocD, classVarCheckStatic, 
  classVarDocD, objVarDocD, funcAppDocD, newObjDocD, newObjDocD', 
  constDecDefDocD, varD, staticVarD, extVarD, selfD, enumVarD, classVarD, 
  objVarD, objVarSelfD, listVarD, listOfD, iterVarD, valueOfD, argD, 
  enumElementD, argsListD, funcAppD, selfFuncAppD, extFuncAppD, newObjD, 
  notNullD, objAccessD, objMethodCallD, objMethodCallNoParamsD, selfAccessD, 
  listIndexExistsD, indexOfD, funcDocD, castDocD, listAccessFuncDocD, 
  listSetFuncDocD, objAccessDocD, castObjDocD, funcD, getD, setD, listSizeD, 
  listAddD, listAppendD, iterBeginD, iterEndD, listAccessD, listSetD, getFuncD, 
  setFuncD, listSizeFuncD, listAddFuncD, listAppendFuncD, iterBeginError, 
  iterEndError, listAccessFuncD, listAccessFuncD', listSetFuncD, includeD, 
  breakDocD, continueDocD, staticDocD, dynamicDocD, bindingError, privateDocD, 
  publicDocD, blockCmtDoc, docCmtDoc, commentedItem, addCommentsDocD, 
  functionDox, classDox, moduleDox, commentedModD, docFuncRepr, valueList, 
  variableList, parameterList, prependToBody, appendToBody, surroundBody, 
  getterName, setterName, intValue, filterOutObjs
) where

import Utils.Drasil (blank, capitalize, indent, indentList, stringList)

import GOOL.Drasil.CodeType (CodeType(..), isObject)
import GOOL.Drasil.Symantics (Label, Library, RenderSym(..), BodySym(..), 
  PermanenceSym(..), InternalPerm(..),
  TypeSym(Type, getType, getTypeString, getTypeDoc, bool, float, string,
    listType, listInnerType, obj, enumType, iterator), 
  UnaryOpSym(..), BinaryOpSym(..), InternalOp(..), VariableSym(..), 
  InternalVariable(..), ValueSym(..), NumericExpression(..), 
  BooleanExpression(..), ValueExpression(..), InternalValue(..), Selector(..), 
  FunctionSym(..), SelectorFunction(..), InternalFunction(..), 
  InternalStatement(..), StatementSym(..), ControlStatementSym(..), 
  ScopeSym(..), InternalScope(..), ParameterSym(..), InternalParam(..), 
  MethodSym(..), InternalMethod(..), BlockCommentSym(..))
import qualified GOOL.Drasil.Symantics as S (TypeSym(char, int))
import GOOL.Drasil.Data (Terminator(..), FileData(..), fileD, updateFileMod, 
  updateModDoc, OpData(..), od, TypeData(..), Binding(..), VarData(..))
import GOOL.Drasil.Helpers (doubleQuotedText, hicat, vibcat, vmap, 
  emptyIfEmpty, emptyIfNull, onStateValue, getNestDegree)
import GOOL.Drasil.State (GS, MS, getParameters)

import Data.List (last)
import Prelude hiding (break,print,last,mod,(<>))
import Text.PrettyPrint.HughesPJ (Doc, text, empty, render, (<>), (<+>), ($+$),
  space, brackets, parens, isEmpty, rbrace, lbrace, vcat, char, double, quotes, 
  integer, semi, equals, braces, int, comma, colon)

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
packageDocD n end f = fileD (n ++ "/" ++ filePath f) (updateModDoc 
  (\d -> emptyIfEmpty d (vibcat [text "package" <+> text n <> end, d])) 
  (fileMod f))

fileDoc' :: Doc -> Doc -> Doc -> Doc
fileDoc' t m b = vibcat (filter (not . isEmpty) [
  t,
  m,
  b])

-----------------------------------------------
-- 'Default' functions used in the renderers --
-----------------------------------------------

-- Module --

moduleDocD :: Doc -> Doc -> Doc -> Doc
moduleDocD ls fs cs = emptyIfEmpty (fs <> cs) (vibcat (filter (not . isEmpty) 
  [ls, fs, cs]))

-- Class --

classDocD :: Label -> Doc -> Doc -> Doc -> Doc -> Doc
classDocD n p s vs fs = vcat [
  s <+> classDec <+> text n <+> p <+> lbrace, 
  indentList [
    vs,
    blank,
    fs],
  rbrace]

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

printDoc :: (RenderSym repr) => repr (Value repr) -> repr (Value repr) -> Doc
printDoc printFn v = valueDoc printFn <> parens (valueDoc v)

printListDoc :: (RenderSym repr) => Integer -> repr (Value repr) -> 
  (repr (Value repr) -> GS (repr (Statement repr))) -> 
  (String -> GS (repr (Statement repr))) -> 
  (String -> GS (repr (Statement repr))) -> GS (repr (Statement repr))
printListDoc n v prFn prStrFn prLnFn = multi [prStrFn "[", 
  for (varDecDef i (litInt 0)) (valueOf i ?< (listSize v #- litInt 1))
    (i &++) (bodyStatements [prFn (listAccess v (valueOf i)), prStrFn ", "]), 
  ifNoElse [(listSize v ?> litInt 0, oneLiner $
    prFn (listAccess v (listSize v #- litInt 1)))], 
  prLnFn "]"]
  where l_i = "list_i" ++ show n
        i = var l_i S.int

printObjDoc :: String -> (String -> GS (repr (Statement repr)))
  -> GS (repr (Statement repr))
printObjDoc n prLnFn = prLnFn $ "Instance of " ++ n ++ " object"

outDoc :: (RenderSym repr) => Bool -> repr (Value repr) -> repr (Value repr) 
  -> Maybe (repr (Value repr)) -> GS (repr (Statement repr))
outDoc newLn printFn v f = outDoc' (getType $ valueType v)
  where outDoc' (List t) = printListDoc (getNestDegree 1 t) v prFn prStrFn 
          prLnFn
        outDoc' (Object n) = printObjDoc n prLnFn
        outDoc' _ = printSt newLn printFn v f
        prFn = maybe print printFile f
        prStrFn = maybe printStr printFileStr f
        prLnFn = if newLn then maybe printStrLn printFileStrLn f else maybe 
          printStr printFileStr f 

printFileDocD :: Label -> Doc -> Doc
printFileDocD fn f = f <> dot <> text fn

-- Parameters --

paramDocD :: (RenderSym repr) => repr (Variable repr) -> Doc
paramDocD v = getTypeDoc (variableType v) <+> variableDoc v

-- Method --

methodDocD :: (RenderSym repr) => Label -> repr (Scope repr) -> 
  repr (Permanence repr) -> repr (Type repr) -> [repr (Parameter repr)] -> 
  repr (Body repr) -> Doc
methodDocD n s p t ps b = vcat [
  scopeDoc s <+> permDoc p <+> getTypeDoc t <+> text n <> 
    parens (parameterList ps) <+> lbrace,
  indent (bodyDoc b),
  rbrace]

methodListDocD :: [Doc] -> Doc
methodListDocD ms = vibcat methods
  where methods = filter (not . isEmpty) ms
  
destructorError :: String -> String
destructorError l = "Destructors not allowed in " ++ l

-- StateVar --

stateVarDocD :: Doc -> Doc -> Doc -> Doc
stateVarDocD s p dec = s <+> p <+> dec

constVarDocD :: Doc -> Doc -> VarData -> Doc -> Doc
constVarDocD s p v end = s <+> p <+> text "const" <+> typeDoc (varType v) <+>
  varDoc v <> end

stateVarListDocD :: [Doc] -> Doc
stateVarListDocD = vcat

-- Controls --

switchDocD :: (RenderSym repr) => repr (Statement repr) -> repr (Value repr) -> 
  repr (Body repr) -> [(repr (Value repr), repr (Body repr))] -> Doc
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

-- Statements --

assignDocD :: (RenderSym repr) => repr (Variable repr) -> repr (Value repr) -> 
  Doc
assignDocD vr vl = variableDoc vr <+> equals <+> valueDoc vl

multiAssignDoc :: (RenderSym repr) => [repr (Variable repr)] -> 
  [repr (Value repr)] -> Doc
multiAssignDoc vrs vls = variableList vrs <+> equals <+> valueList vls

plusEqualsDocD :: (RenderSym repr) => repr (Variable repr) -> repr (Value repr) 
  -> Doc
plusEqualsDocD vr vl = variableDoc vr <+> text "+=" <+> valueDoc vl

plusPlusDocD :: (RenderSym repr) => repr (Variable repr) -> Doc
plusPlusDocD v = variableDoc v <> text "++"

listDecDocD :: (RenderSym repr) => repr (Variable repr) -> repr (Value repr) -> 
  Doc
listDecDocD v n = space <> equals <+> new <+> getTypeDoc (variableType v) 
  <> parens (valueDoc n)

listDecDefDocD :: (RenderSym repr) => repr (Variable repr) -> 
  [repr (Value repr)] -> Doc
listDecDefDocD v vs = space <> equals <+> new <+> getTypeDoc (variableType v) 
  <+> braces (valueList vs)

constDecDefDocD :: (RenderSym repr) => repr (Variable repr) -> 
  repr (Value repr) -> Doc
constDecDefDocD v def = text "const" <+> getTypeDoc (variableType v) <+> 
  variableDoc v <+> equals <+> valueDoc def

returnDocD :: (RenderSym repr) => [repr (Value repr)] -> Doc
returnDocD vs = text "return" <+> valueList vs

commentDocD :: Label -> Doc -> Doc
commentDocD cmt cStart = cStart <+> text cmt

freeDocD :: (RenderSym repr) => repr (Variable repr) -> Doc
freeDocD v = text "delete" <+> text "&" <> variableDoc v

statementDocD :: (Doc, Terminator) -> (Doc, Terminator)
statementDocD (s, t) = (s <> getTermDoc t, Empty)

getTermDoc :: Terminator -> Doc
getTermDoc Semi = semi
getTermDoc Empty = empty

mkSt :: (RenderSym repr) => Doc -> repr (Statement repr)
mkSt = flip stateFromData Semi

mkStNoEnd :: (RenderSym repr) => Doc -> repr (Statement repr)
mkStNoEnd = flip stateFromData Empty

stringListVals' :: (RenderSym repr) => [repr (Variable repr)] -> 
  repr (Value repr) -> GS (repr (Statement repr))
stringListVals' vars sl = multi $ checkList (getType $ valueType sl)
    where checkList (List String) = assignVals vars 0
          checkList _ = error 
            "Value passed to stringListVals must be a list of strings"
          assignVals [] _ = []
          assignVals (v:vs) n = assign v (cast (variableType v) 
            (listAccess sl (litInt n))) : assignVals vs (n+1)

stringListLists' :: (RenderSym repr) => [repr (Variable repr)] -> 
  repr (Value repr) -> GS (repr (Statement repr))
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

unExpr :: (RenderSym repr) => repr (UnaryOp repr) -> repr (Value repr) -> 
  repr (Value repr)
unExpr u v = mkExpr (uOpPrec u) (valueType v) (unOpDocD (uOpDoc u) (valueDoc v))

unExpr' :: (RenderSym repr) => repr (UnaryOp repr) -> repr (Value repr) -> 
  repr (Value repr)
unExpr' u v = mkExpr (uOpPrec u) (valueType v) (unOpDocD' (uOpDoc u) 
  (valueDoc v))

typeUnExpr :: (RenderSym repr) => repr (UnaryOp repr) -> repr (Type repr) -> 
  repr (Value repr) -> repr (Value repr)
typeUnExpr u t v = mkExpr (uOpPrec u) t (unOpDocD (uOpDoc u) (valueDoc v))

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
  
binExpr :: (RenderSym repr) => repr (BinaryOp repr) -> repr (Value repr) -> 
  repr (Value repr) -> repr (Value repr)
binExpr b v1 v2 = mkExpr (bOpPrec b) (numType (valueType v1) (valueType v2)) 
  (binOpDocD (bOpDoc b) (exprParensL b v1 $ valueDoc v1) (exprParensR b v2 $ 
  valueDoc v2))

binExpr' :: (RenderSym repr) => repr (BinaryOp repr) -> repr (Value repr) -> 
  repr (Value repr) -> repr (Value repr)
binExpr' b v1 v2 = mkExpr 9 (numType (valueType v1) (valueType v2)) 
  (binOpDocD' (bOpDoc b) (valueDoc v1) (valueDoc v2))

numType :: (RenderSym repr) => repr (Type repr) -> repr (Type repr) -> 
  repr (Type repr)
numType t1 t2 = numericType (getType t1) (getType t2)
  where numericType Integer Integer = t1
        numericType Float _ = t1
        numericType _ Float = t2
        numericType _ _ = error "Numeric types required for numeric expression"

typeBinExpr :: (RenderSym repr) => repr (BinaryOp repr) -> repr (Type repr) -> 
  repr (Value repr) -> repr (Value repr) -> repr (Value repr)
typeBinExpr b t v1 v2 = mkExpr (bOpPrec b) t (binOpDocD (bOpDoc b) 
  (exprParensL b v1 $ valueDoc v1) (exprParensR b v2 $ valueDoc v2))

mkVal :: (RenderSym repr) => repr (Type repr) -> Doc -> repr (Value repr)
mkVal = valFromData Nothing

mkVar :: (RenderSym repr) => String -> repr (Type repr) -> Doc -> 
  repr (Variable repr)
mkVar = varFromData Dynamic

mkStaticVar :: (RenderSym repr) => String -> repr (Type repr) -> Doc -> 
  repr (Variable repr)
mkStaticVar = varFromData Static

mkExpr :: (RenderSym repr) => Int -> repr (Type repr) -> Doc -> 
  repr (Value repr)
mkExpr p = valFromData (Just p)

-- Literals --

litTrueD :: (RenderSym repr) => repr (Value repr)
litTrueD = valFromData Nothing bool (text "true")

litFalseD :: (RenderSym repr) => repr (Value repr)
litFalseD = valFromData Nothing bool (text "false")

litCharD :: (RenderSym repr) => Char -> repr (Value repr)
litCharD c = valFromData Nothing S.char (quotes $ char c)

litFloatD :: (RenderSym repr) => Double -> repr (Value repr)
litFloatD f = valFromData Nothing float (double f)

litIntD :: (RenderSym repr) => Integer -> repr (Value repr)
litIntD i = valFromData Nothing S.int (integer i)

litStringD :: (RenderSym repr) => String -> repr (Value repr)
litStringD s = valFromData Nothing string (doubleQuotedText s)

-- Value Printers --

varDocD :: Label -> Doc
varDocD = text

extVarDocD :: Library -> Label -> Doc
extVarDocD l n = text l <> dot <> text n

selfDocD :: Doc
selfDocD = text "this"

argDocD :: (RenderSym repr) => repr (Value repr) -> repr (Value repr) -> Doc
argDocD n args = valueDoc args <> brackets (valueDoc n)

enumElemDocD :: Label -> Label -> Doc
enumElemDocD en e = text en <> dot <> text e

classVarCheckStatic :: (VariableSym repr) => repr (Variable repr) -> 
  repr (Variable repr)
classVarCheckStatic v = classVarCS (variableBind v)
  where classVarCS Dynamic = error
          "classVar can only be used to access static variables"
        classVarCS Static = v

classVarDocD :: Doc -> Doc -> Doc
classVarDocD c v = c <> dot <> v

objVarDocD :: Doc -> Doc ->  Doc
objVarDocD n1 n2 = n1 <> dot <> n2

funcAppDocD :: (RenderSym repr) => Label -> [repr (Value repr)] -> Doc
funcAppDocD n vs = text n <> parens (valueList vs)

newObjDocD :: (RenderSym repr) => repr (Type repr) -> Doc -> Doc
newObjDocD st vs = new <+> newObjDocD' st vs

newObjDocD' :: (RenderSym repr) => repr (Type repr) -> Doc -> Doc
newObjDocD' st vs = getTypeDoc st <> parens vs

varD :: (RenderSym repr) => Label -> repr (Type repr) -> repr (Variable repr)
varD n t = varFromData Dynamic n t (varDocD n)

staticVarD :: (RenderSym repr) => Label -> repr (Type repr) -> 
  repr (Variable repr)
staticVarD n t = mkStaticVar n t (varDocD n)

extVarD :: (RenderSym repr) => Label -> Label -> repr (Type repr) -> 
  repr (Variable repr)
extVarD l n t = varFromData Dynamic (l ++ "." ++ n) t (extVarDocD l n)

selfD :: (RenderSym repr) => Label -> repr (Variable repr)
selfD l = varFromData Dynamic "this" (obj l) selfDocD

enumVarD :: (RenderSym repr) => Label -> Label -> repr (Variable repr)
enumVarD e en = var e (enumType en)

classVarD :: (RenderSym repr) => (Doc -> Doc -> Doc) -> repr (Type repr) -> 
  repr (Variable repr) -> repr (Variable repr)
classVarD f c v = classVarCheckStatic $ varFromData (variableBind v) 
  (getTypeString c ++ "." ++ variableName v) 
  (variableType v) (f (getTypeDoc c) (variableDoc v))

objVarD :: (RenderSym repr) => repr (Variable repr) -> repr (Variable repr) -> 
  repr (Variable repr)
objVarD o v = varFromData Dynamic (variableName o ++ "." ++ variableName v) 
  (variableType v) (objVarDocD (variableDoc o) (variableDoc v))

objVarSelfD :: (RenderSym repr) => Label -> repr (Variable repr) -> 
  repr (Variable repr)
objVarSelfD l = objVar (self l)

listVarD :: (RenderSym repr) => Label -> repr (Permanence repr) -> 
  repr (Type repr) -> repr (Variable repr)
listVarD n p t = var n (listType p t)

listOfD :: (RenderSym repr) => Label -> repr (Type repr) -> repr (Variable repr)
listOfD n = listVar n static_

iterVarD :: (RenderSym repr) => Label -> repr (Type repr) -> 
  repr (Variable repr)
iterVarD n t = var n (iterator t)

valueOfD :: (RenderSym repr) => repr (Variable repr) -> repr (Value repr)
valueOfD v = valFromData Nothing (variableType v) (variableDoc v)

argD :: (RenderSym repr) => repr (Value repr) -> repr (Value repr) ->
  repr (Value repr)
argD n args = valFromData Nothing string (argDocD n args)

enumElementD :: (RenderSym repr) => Label -> Label -> repr (Value repr)
enumElementD en e = valFromData Nothing (enumType en) (enumElemDocD en e)

argsListD :: (RenderSym repr) => String -> repr (Value repr)
argsListD l = valFromData Nothing (listType static_ string) (text l)
 
funcAppD :: (RenderSym repr) => Label -> repr (Type repr) -> [repr (Value repr)]
  -> repr (Value repr)
funcAppD n t vs = valFromData Nothing t (funcAppDocD n vs)

selfFuncAppD :: (RenderSym repr) => repr (Variable repr) -> Label -> 
  repr (Type repr) -> [repr (Value repr)] -> repr (Value repr)
selfFuncAppD s n = funcAppD (variableName s ++ "." ++ n)

extFuncAppD :: (RenderSym repr) => Library -> Label -> repr (Type repr) -> 
  [repr (Value repr)] -> repr (Value repr)
extFuncAppD l n = funcAppD (l ++ "." ++ n)

newObjD :: (RenderSym repr) => (repr (Type repr) -> Doc -> Doc) -> 
  repr (Type repr) -> [repr (Value repr)] -> repr (Value repr)
newObjD f t vs = valFromData Nothing t (f t (valueList vs))

notNullD :: (RenderSym repr) => repr (Value repr) -> repr (Value repr)
notNullD v = v ?!= valueOf (var "null" (valueType v))

objAccessD :: (RenderSym repr) => repr (Value repr) -> repr (Function repr) -> 
  repr (Value repr)
objAccessD v f = valFromData Nothing (functionType f) (objAccessDocD 
  (valueDoc v) (functionDoc f))

objMethodCallD :: (RenderSym repr) => repr (Type repr) -> repr (Value repr) -> 
  Label -> [repr (Value repr)] -> repr (Value repr)
objMethodCallD t o f ps = objAccess o (func f t ps)

objMethodCallNoParamsD :: (RenderSym repr) => repr (Type repr) -> 
  repr (Value repr) -> Label -> repr (Value repr)
objMethodCallNoParamsD t o f = objMethodCall t o f []

selfAccessD :: (RenderSym repr) => Label -> repr (Function repr) -> 
  repr (Value repr)
selfAccessD l = objAccess (valueOf $ self l)

listIndexExistsD :: (RenderSym repr) => repr (Value repr) -> repr (Value repr) 
  -> repr (Value repr)
listIndexExistsD lst index = listSize lst ?> index

indexOfD :: (RenderSym repr) => Label -> repr (Value repr) -> repr (Value repr) 
  -> repr (Value repr)
indexOfD f l v = objAccess l (func f S.int [v])

-- Functions --

funcDocD :: Doc -> Doc
funcDocD fnApp = dot <> fnApp

castDocD :: Doc -> Doc
castDocD = parens

listAccessFuncDocD :: (RenderSym repr) => repr (Value repr) -> Doc
listAccessFuncDocD v = brackets $ valueDoc v

listSetFuncDocD :: Doc -> Doc -> Doc
listSetFuncDocD i v = brackets i <+> equals <+> v

objAccessDocD :: Doc -> Doc -> Doc
objAccessDocD v f = v <> f

castObjDocD :: Doc -> Doc -> Doc
castObjDocD t v = t <> parens v

funcD :: (RenderSym repr) => Label -> repr (Type repr) -> [repr (Value repr)] 
  -> repr (Function repr)
funcD l t vs = funcFromData t (funcDocD (valueDoc $ funcApp l t vs))

getD :: (RenderSym repr) => repr (Value repr) -> repr (Variable repr) -> 
  repr (Value repr)
getD v vToGet = v $. getFunc vToGet

setD :: (RenderSym repr) => repr (Value repr) -> repr (Variable repr) -> 
  repr (Value repr) -> repr (Value repr)
setD v vToSet toVal = v $. setFunc (valueType v) vToSet toVal

listSizeD :: (RenderSym repr) => repr (Value repr) -> repr (Value repr)
listSizeD v = v $. listSizeFunc

listAddD :: (RenderSym repr) => repr (Value repr) -> repr (Value repr) -> 
  repr (Value repr) -> repr (Value repr)
listAddD v i vToAdd = v $. listAddFunc v i vToAdd

listAppendD :: (RenderSym repr) => repr (Value repr) -> repr (Value repr) -> 
  repr (Value repr)
listAppendD v vToApp = v $. listAppendFunc vToApp

iterBeginD :: (RenderSym repr) => repr (Value repr) -> repr (Value repr)
iterBeginD v = v $. iterBeginFunc (listInnerType $ valueType v)

iterEndD :: (RenderSym repr) => repr (Value repr) -> repr (Value repr)
iterEndD v = v $. iterEndFunc (listInnerType $ valueType v)

listAccessD :: (RenderSym repr) => repr (Value repr) -> repr (Value repr) -> 
  repr (Value repr)
listAccessD v i = v $. listAccessFunc (listInnerType $ valueType v) i

listSetD :: (RenderSym repr) => repr (Value repr) -> repr (Value repr) -> 
  repr (Value repr) -> repr (Value repr)
listSetD v i toVal = v $. listSetFunc v i toVal

getFuncD :: (RenderSym repr) => repr (Variable repr) -> repr (Function repr)
getFuncD v = func (getterName $ variableName v) (variableType v) []

setFuncD :: (RenderSym repr) => repr (Type repr) -> repr (Variable repr) -> 
  repr (Value repr) -> repr (Function repr)
setFuncD t v toVal = func (setterName $ variableName v) t [toVal]

listSizeFuncD :: (RenderSym repr) => repr (Function repr)
listSizeFuncD = func "size" S.int []

listAddFuncD :: (RenderSym repr) => Label -> repr (Value repr) -> 
  repr (Value repr) -> repr (Function repr)
listAddFuncD f i v = func f (listType static_ $ valueType v) [i, v]

listAppendFuncD :: (RenderSym repr) => Label -> repr (Value repr) -> 
  repr (Function repr)
listAppendFuncD f v = func f (listType static_ $ valueType v) [v]

iterBeginError :: String -> String
iterBeginError l = "Attempt to use iterBeginFunc in " ++ l ++ ", but " ++ l ++ 
  " has no iterators"

iterEndError :: String -> String
iterEndError l = "Attempt to use iterEndFunc in " ++ l ++ ", but " ++ l ++ 
  " has no iterators"

listAccessFuncD :: (RenderSym repr) => repr (Type repr) -> repr (Value repr) ->
  repr (Function repr)
listAccessFuncD t i = funcFromData t (listAccessFuncDocD $ intValue i)

listAccessFuncD' :: (RenderSym repr) => Label -> repr (Type repr) -> 
  repr (Value repr) -> repr (Function repr)
listAccessFuncD' f t i = func f t [intValue i]

listSetFuncD :: (RenderSym repr) => (Doc -> Doc -> Doc) -> repr (Value repr) -> 
  repr (Value repr) -> repr (Value repr) -> repr (Function repr)
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

bindingError :: String -> String
bindingError l = "Binding unimplemented in " ++ l

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

classDox :: String -> [String]
classDox desc = [doxBrief ++ desc | not (null desc)]

moduleDox :: String -> [String] -> String -> String -> [String]
moduleDox desc as date m = (doxFile ++ m) : 
  [doxAuthor ++ stringList as | not (null as)] ++
  [doxDate ++ date | not (null date)] ++ 
  [doxBrief ++ desc | not (null desc)]

commentedModD :: FileData -> Doc -> FileData
commentedModD m cmt = updateFileMod (updateModDoc (commentedItem cmt) (fileMod m)) m

docFuncRepr :: (MethodSym repr) => String -> [String] -> [String] -> 
  MS (repr (Method repr)) -> MS (repr (Method repr))
docFuncRepr desc pComms rComms = commentedFunc (docComment $ onStateValue 
  (\ps -> functionDox desc (zip ps pComms) rComms) getParameters)

-- Helper Functions --

valueList :: (RenderSym repr) => [repr (Value repr)] -> Doc
valueList = hicat (text ", ") . map valueDoc

variableList :: (RenderSym repr) => [repr (Variable repr)] -> Doc
variableList = hicat (text ", ") . map variableDoc

parameterList :: (RenderSym repr) => [repr (Parameter repr)] -> Doc
parameterList = hicat (text ", ") . map parameterDoc

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

exprParensL :: (RenderSym repr) => repr (BinaryOp repr) -> repr (Value repr) -> 
  (Doc -> Doc)
exprParensL o v = if maybe False (< bOpPrec o) (valuePrec v) then parens else id

exprParensR :: (RenderSym repr) => repr (BinaryOp repr) -> repr (Value repr) -> 
  (Doc -> Doc)
exprParensR o v = if maybe False (<= bOpPrec o) (valuePrec v) then parens else 
  id

intValue :: (RenderSym repr) => repr (Value repr) -> repr (Value repr)
intValue i = intValue' (getType $ valueType i)
  where intValue' Integer = i
        intValue' (Enum _) = cast S.int i
        intValue' _ = error "Value passed must be Integer or Enum"

filterOutObjs :: (VariableSym repr) => [repr (Variable repr)] -> 
  [repr (Variable repr)]
filterOutObjs = filter (not . isObject . getType . variableType)

doxCommand, doxBrief, doxParam, doxReturn, doxFile, doxAuthor, doxDate :: String
doxCommand = "\\"
doxBrief = doxCommand ++ "brief "
doxParam = doxCommand ++ "param "
doxReturn = doxCommand ++ "return "
doxFile = doxCommand  ++ "file "
doxAuthor = doxCommand ++ "author "
doxDate = doxCommand ++ "date "
