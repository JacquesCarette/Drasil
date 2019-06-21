-- | The structure for a class of renderers is defined here.
module Language.Drasil.Code.Imperative.LanguageRenderer (
  -- * Common Syntax
  classDec, dot, doubleSlash, forLabel, new, observerListName,
  
  -- * Default Functions available for use in renderers
  packageDocD, fileDoc', moduleDocD, classDocD, enumDocD, enumElementsDocD, 
  enumElementsDocD', multiStateDocD, blockDocD, bodyDocD, outDocD, 
  printListDocD, printFileDocD, boolTypeDocD, intTypeDocD, floatTypeDocD, 
  charTypeDocD, stringTypeDocD, fileTypeDocD, typeDocD, listTypeDocD, 
  voidDocD, constructDocD, stateParamDocD, paramListDocD, methodDocD, 
  methodListDocD, stateVarDocD, stateVarListDocD, alwaysDel, ifCondDocD, 
  switchDocD, forDocD, forEachDocD, whileDocD, tryCatchDocD, assignDocD, 
  plusEqualsDocD, plusEqualsDocD', plusPlusDocD, plusPlusDocD', varDecDocD, 
  varDecDefDocD, listDecDocD, listDecDefDocD, statementDocD, returnDocD, 
  commentDocD, freeDocD, throwDocD, mkSt, mkStNoEnd, stratDocD, notOpDocD, 
  notOpDocD', negateOpDocD, sqrtOpDocD, sqrtOpDocD', absOpDocD, absOpDocD', 
  logOpDocD, logOpDocD', lnOpDocD, lnOpDocD', expOpDocD, expOpDocD', sinOpDocD,
  sinOpDocD', cosOpDocD, cosOpDocD', tanOpDocD, tanOpDocD', asinOpDocD, 
  asinOpDocD', acosOpDocD, acosOpDocD', atanOpDocD, atanOpDocD', unOpDocD, 
  unExpr, equalOpDocD, notEqualOpDocD, greaterOpDocD, greaterEqualOpDocD, 
  lessOpDocD, lessEqualOpDocD, plusOpDocD, minusOpDocD, multOpDocD, 
  divideOpDocD, moduloOpDocD, powerOpDocD, andOpDocD, orOpDocD, binOpDocD, 
  binOpDocD', binExpr, binExpr', mkVal, litTrueD, litFalseD, litCharD, 
  litFloatD, litIntD, litStringD, varDocD, extVarDocD, selfDocD, argDocD, 
  enumElemDocD, objVarDocD, inlineIfDocD, funcAppDocD, extFuncAppDocD, 
  stateObjDocD, listStateObjDocD, objDecDefDocD, constDecDefDocD, notNullDocD, 
  listIndexExistsDocD, funcDocD, castDocD, sizeDocD, listAccessDocD, 
  listSetDocD, objAccessDocD, castObjDocD, includeD, breakDocD, continueDocD, 
  staticDocD, dynamicDocD, privateDocD, publicDocD, addCommentsDocD, 
  callFuncParamList, getterName, setterName, setMain, setEmpty, 
  statementsToStateVars
) where

import Utils.Drasil (capitalize, indent, indentList)

import Language.Drasil.Code.Code (CodeType(..))
import Language.Drasil.Code.Imperative.Symantics (Label, Library)
import Language.Drasil.Code.Imperative.Helpers (Terminator(..), ModData(..), md,
  TypeData(..), td, angles,blank, doubleQuotedText,hicat,vibcat,vmap)

import Data.List (intersperse, last)
import Prelude hiding (break,print,return,last,mod,(<>))
import Text.PrettyPrint.HughesPJ (Doc, text, empty, render, (<>), (<+>), 
  brackets, parens, isEmpty, rbrace, lbrace, vcat, char, double, quotes, 
  integer, semi, equals, braces, int, comma, colon, hcat)

----------------------------------------
-- Syntax common to several renderers --
----------------------------------------

classDec,dot,doubleSlash,forLabel,new :: Doc
classDec = text "class"
dot = text "."
doubleSlash = text "//"
forLabel = text "for"
new = text "new"

observerListName :: Label
observerListName = "observerList"

----------------------------------
-- Functions for rendering code --
----------------------------------

packageDocD :: Label -> Doc -> ModData -> ModData
packageDocD n end (MD l b m) = md l b (vibcat [text "package" <+> text n <> end,
  m])

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
classDocD n p inherit s vs fs = vcat [
  s <+> classDec <+> text n <+> baseClass <+> lbrace, 
  indentList [
    vs,
    blank,
    fs],
  rbrace]
  where baseClass = case p of Nothing -> empty
                              Just pn -> inherit <+> text pn

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

outDocD :: Doc -> (Doc, Maybe String) -> Doc
outDocD printFn (v, _) = printFn <> parens v

printListDocD :: Doc -> Doc -> Doc -> Doc -> Doc
printListDocD open b lastElem close = vcat [open, b, lastElem, close]

printFileDocD :: Label -> (Doc, Maybe String) -> Doc
printFileDocD fn (f, _) = f <> dot <> text fn

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

listTypeDocD :: TypeData -> Doc -> TypeData
listTypeDocD t list = td (List (cType t)) (list <> angles (typeDoc t))

-- Method Types --

voidDocD :: Doc
voidDocD = text "void"

constructDocD :: Label -> Doc
constructDocD _ = empty

-- Parameters --

stateParamDocD :: Label -> TypeData -> Doc
stateParamDocD n t = typeDoc t <+> text n

paramListDocD :: [Doc] -> Doc
paramListDocD = hicat (text ", ")

-- Method --

methodDocD :: Label -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc
methodDocD n s p t ps b = vcat [
  s <+> p <+> t <+> text n <> parens ps <+> lbrace,
  indent b,
  rbrace]

methodListDocD :: [(Doc, Bool)] -> Doc
methodListDocD ms = vibcat methods
  where methods = filter (not . isEmpty) (map fst ms)

-- StateVar --

stateVarDocD :: Label -> Doc -> Doc -> TypeData -> Doc -> Doc
stateVarDocD l s p t end = s <+> p <+> typeDoc t <+> text l <> end

stateVarListDocD :: [Doc] -> Doc
stateVarListDocD = vcat

alwaysDel :: Int
alwaysDel = 4

-- Controls --

ifCondDocD :: Doc -> Doc -> Doc -> Doc -> [((Doc, Maybe String), Doc)] -> Doc
ifCondDocD _ _ _ _ [] = error "if condition created with no cases"
ifCondDocD ifStart elseIf blockEnd elseBody (c:cs) = 
  let ifSect ((v, _), b) = vcat [
        text "if" <+> parens v <+> ifStart,
        indent b,
        blockEnd]
      elseIfSect ((v, _), b) = vcat [
        elseIf <+> parens v <+> ifStart,
        indent b,
        blockEnd]
      elseSect = if isEmpty elseBody then empty else vcat [
        text "else" <+> ifStart,
        indent elseBody,
        blockEnd]
  in vcat [
    ifSect c,
    vmap elseIfSect cs,
    elseSect]

switchDocD :: (Doc, Terminator) -> (Doc, Maybe String) -> Doc -> 
  [((Doc, Maybe String), Doc)] -> Doc
switchDocD breakState (v, _) defBody cs = 
  let caseDoc ((l, _), result) = vcat [
        text "case" <+> l <> colon,
        indentList [
          result,
          fst breakState]]
      defaultSection = vcat [
        text "default" <> colon,
        indentList [
          defBody,
          fst breakState]]
  in vcat [
      text "switch" <> parens v <+> lbrace,
      indentList [
        vmap caseDoc cs,
        defaultSection],
      rbrace]

-- These signatures wont be quite so horrendous if/when we pass language options
-- (blockStart, etc.) in as shared environment
forDocD :: Doc -> Doc -> (Doc, Terminator) -> (Doc, Maybe String) -> 
  (Doc, Terminator) -> Doc -> Doc
forDocD blockStart blockEnd sInit (vGuard, _) sUpdate b = vcat [
  forLabel <+> parens (fst sInit <> semi <+> vGuard <> semi <+> fst sUpdate) 
    <+> blockStart,
  indent b,
  blockEnd]

forEachDocD :: Label -> Doc -> Doc -> Doc -> Doc -> TypeData -> 
  (Doc, Maybe String) -> Doc -> Doc
forEachDocD l blockStart blockEnd iterForEachLabel iterInLabel t (v, _) b =
  vcat [iterForEachLabel <+> parens (typeDoc t <+> text l <+> iterInLabel <+> v) <+>
    blockStart,
  indent b,
  blockEnd]

whileDocD :: Doc -> Doc -> (Doc, Maybe String) -> Doc -> Doc
whileDocD blockStart blockEnd (v, _) b = vcat [
  text "while" <+> parens v <+> blockStart,
  indent b,
  blockEnd]

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

assignDocD :: (Doc, Maybe String) -> (Doc, Maybe String) -> Doc
assignDocD (v1, _) (v2, _) = v1 <+> equals <+> v2

plusEqualsDocD :: (Doc, Maybe String) -> (Doc, Maybe String) -> Doc
plusEqualsDocD (v1, _) (v2, _) = v1 <+> text "+=" <+> v2

plusEqualsDocD' :: (Doc, Maybe String) -> Doc -> (Doc, Maybe String) -> Doc
plusEqualsDocD' (v1, _) plusOp (v2, _) = v1 <+> equals <+> v1 <+> plusOp <+> v2

plusPlusDocD :: (Doc, Maybe String) -> Doc
plusPlusDocD (v, _) = v <> text "++"

plusPlusDocD' :: (Doc, Maybe String) -> Doc -> Doc
plusPlusDocD' (v, _) plusOp = v <+> equals <+> v <+> plusOp <+> int 1

varDecDocD :: Label -> TypeData -> Doc
varDecDocD l st = typeDoc st <+> text l

varDecDefDocD :: Label -> TypeData -> (Doc, Maybe String) -> Doc
varDecDefDocD l st (v, _) = typeDoc st <+> text l <+> equals <+> v

listDecDocD :: Label -> (Doc, Maybe String) -> TypeData -> Doc
listDecDocD l (n, _) st = typeDoc st <+> text l <+> equals <+> new <+> 
  typeDoc st <> parens n

listDecDefDocD :: Label -> TypeData -> [(Doc, Maybe String)] -> Doc
listDecDefDocD l st vs = typeDoc st <+> text l <+> equals <+> new <+> 
  typeDoc st <+> braces (callFuncParamList vs)

objDecDefDocD :: Label -> TypeData -> (Doc, Maybe String) -> Doc
objDecDefDocD = varDecDefDocD

constDecDefDocD :: Label -> TypeData -> (Doc, Maybe String) -> Doc -- can this be done without StateType (infer from value)?
constDecDefDocD l st (v, _) = text "const" <+> typeDoc st <+> text l <+> 
  equals <+> v

returnDocD :: (Doc, Maybe String) -> Doc
returnDocD (v, _) = text "return" <+> v

commentDocD :: Label -> Doc -> Doc
commentDocD cmt cStart = cStart <+> text cmt

freeDocD :: (Doc, Maybe String) -> Doc
freeDocD (v, _) = text "delete" <+> v

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

-- Unary Operators --

notOpDocD :: Doc
notOpDocD = text "!"

notOpDocD' :: Doc
notOpDocD' = text "not"

negateOpDocD :: Doc
negateOpDocD = text "-"

sqrtOpDocD :: Doc
sqrtOpDocD = text "sqrt"

sqrtOpDocD' :: Doc
sqrtOpDocD' = text "math.sqrt"

absOpDocD :: Doc
absOpDocD = text "fabs"

absOpDocD' :: Doc
absOpDocD' = text "math.fabs"

logOpDocD :: Doc
logOpDocD = text "log"

logOpDocD' :: Doc
logOpDocD' = text "math.log"

lnOpDocD :: Doc
lnOpDocD = text "ln"

lnOpDocD' :: Doc
lnOpDocD' = text "math.ln"

expOpDocD :: Doc
expOpDocD = text "exp"

expOpDocD' :: Doc
expOpDocD' = text "math.exp"

sinOpDocD :: Doc
sinOpDocD = text "sin"

sinOpDocD' :: Doc
sinOpDocD' = text "math.sin"

cosOpDocD :: Doc
cosOpDocD = text "cos"

cosOpDocD' :: Doc
cosOpDocD' = text "math.cos"

tanOpDocD :: Doc
tanOpDocD = text "tan"

tanOpDocD' :: Doc
tanOpDocD' = text "math.tan"

asinOpDocD :: Doc
asinOpDocD = text "asin"

asinOpDocD' :: Doc
asinOpDocD' = text "math.asin"

acosOpDocD :: Doc
acosOpDocD = text "acos"

acosOpDocD' :: Doc
acosOpDocD' = text "math.acos"

atanOpDocD :: Doc
atanOpDocD = text "atan"

atanOpDocD' :: Doc
atanOpDocD' = text "math.atan"

unOpDocD :: Doc -> (Doc, Maybe String) -> Doc
unOpDocD op (v, _) = op <> parens v

unExpr :: Doc -> (Doc, Maybe String) -> (Doc, Maybe String)
unExpr u v = mkVal $ unOpDocD u v

-- Binary Operators --

equalOpDocD :: Doc
equalOpDocD = text "=="

notEqualOpDocD :: Doc
notEqualOpDocD = text "!="

greaterOpDocD :: Doc
greaterOpDocD = text ">"

greaterEqualOpDocD :: Doc
greaterEqualOpDocD = text ">="

lessOpDocD :: Doc
lessOpDocD = text "<"

lessEqualOpDocD :: Doc
lessEqualOpDocD = text "<="

plusOpDocD :: Doc
plusOpDocD = text "+"

minusOpDocD :: Doc
minusOpDocD = text "-"

multOpDocD :: Doc
multOpDocD = text "*"

divideOpDocD :: Doc
divideOpDocD = text "/"

moduloOpDocD :: Doc
moduloOpDocD = text "%"

powerOpDocD :: Doc
powerOpDocD = text "pow"

andOpDocD :: Doc
andOpDocD = text "&&"

orOpDocD :: Doc
orOpDocD = text "||"

binOpDocD :: Doc -> (Doc, Maybe String) -> (Doc, Maybe String) -> Doc
binOpDocD op (v1, _) (v2, _) = parens (v1 <+> op <+> v2)

binOpDocD' :: Doc -> (Doc, Maybe String) -> (Doc, Maybe String) -> Doc
binOpDocD' op (v1, _) (v2, _) = op <> parens (v1 <> comma <+> v2)
  
binExpr :: Doc -> (Doc, Maybe String) -> (Doc, Maybe String) -> (Doc, Maybe String)
binExpr b v1 v2 = mkVal $ binOpDocD b v1 v2

binExpr' :: Doc -> (Doc, Maybe String) -> (Doc, Maybe String) -> (Doc, Maybe String)
binExpr' b v1 v2 = mkVal $ binOpDocD' b v1 v2

mkVal :: Doc -> (Doc, Maybe String)
mkVal v = (v, Nothing)

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

argDocD :: (Doc, Maybe String) -> (Doc, Maybe String) -> Doc
argDocD (n, _) (args, _) = args <> brackets n

enumElemDocD :: Label -> Label -> Doc
enumElemDocD en e = text en <> dot <> text e

objVarDocD :: (Doc, Maybe String) -> (Doc, Maybe String) ->  Doc
objVarDocD (n1, _) (n2, _) = n1 <> dot <> n2

inlineIfDocD :: (Doc, Maybe String) -> (Doc, Maybe String) -> 
  (Doc, Maybe String) -> Doc
inlineIfDocD (c, _) (v1, _) (v2, _) = parens 
  (parens c <+> text "?" <+> v1 <+> text ":" <+> v2)

funcAppDocD :: Label -> [(Doc, Maybe String)] -> Doc
funcAppDocD n vs = text n <> parens (callFuncParamList vs)

extFuncAppDocD :: Library -> Label -> [(Doc, Maybe String)] -> Doc
extFuncAppDocD l n = funcAppDocD (l ++ "." ++ n)

stateObjDocD :: TypeData -> Doc -> Doc
stateObjDocD st vs = new <+> typeDoc st <> parens vs

listStateObjDocD :: Doc -> TypeData -> Doc -> Doc
listStateObjDocD lstObj st vs = lstObj <+> typeDoc st <> parens vs

notNullDocD :: Doc -> (Doc, Maybe String) -> (Doc, Maybe String) -> Doc
notNullDocD = binOpDocD

listIndexExistsDocD :: Doc -> (Doc, Maybe String) -> (Doc, Maybe String) -> Doc
listIndexExistsDocD greater (lst, _) (index, _) = parens (lst <> 
  text ".Length" <+> greater <+> index) 

-- Functions --

funcDocD :: (Doc, Maybe String) -> Doc
funcDocD (fnApp, _) = dot <> fnApp

castDocD :: TypeData -> Doc
castDocD t = parens $ typeDoc t

sizeDocD :: Doc
sizeDocD = dot <> text "Count"

listAccessDocD :: (Doc, Maybe String) -> Doc
listAccessDocD (v, _) = brackets v

listSetDocD :: (Doc, Maybe String) -> (Doc, Maybe String) -> Doc
listSetDocD (i, _) (v, _) = brackets i <+> equals <+> v

objAccessDocD :: (Doc, Maybe String) -> Doc -> Doc
objAccessDocD (v, _) f = v <> f

castObjDocD :: Doc -> (Doc, Maybe String) -> Doc
castObjDocD f (v, _) = f <> parens v

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

-- Helper Functions --

callFuncParamList :: [(Doc, Maybe String)] -> Doc
callFuncParamList vs = hcat (intersperse (text ", ") (map fst vs))

getterName :: String -> String
getterName s = "Get" ++ capitalize s

setterName :: String -> String
setterName s = "Set" ++ capitalize s

setMain :: (Doc, Bool) -> (Doc, Bool)
setMain (d, _) = (d, True)

setEmpty :: (Doc, Terminator) -> (Doc, Terminator)
setEmpty (d, _) = (d, Empty)

-- Hack because modules accept Statement representations of their state variables. Modules should be redesigned/rethought
statementsToStateVars :: Doc -> Doc -> Doc -> (Doc, Terminator) -> Doc
statementsToStateVars s p end (v, _) = s <+> p <+> v <> end