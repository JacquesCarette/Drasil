-- | The structure for a class of renderers is defined here.
module Language.Drasil.Code.Imperative.NewLanguageRenderer (
    -- * Code Generation Funcitons
    makeCode, createCodeFiles,
    
    -- * Common Syntax
    classDec, dot, doubleSlash, forLabel, new, observerListName,
    
    -- * Default Functions available for use in renderers
    fileDoc', moduleDocD, classDocD, enumDocD, enumElementsDocD, enumElementsDocD', multiStateDocD, blockDocD, bodyDocD, outDocD, 
    printListDocD, printFileDocD, boolTypeDocD, intTypeDocD, floatTypeDocD, 
    charTypeDocD, stringTypeDocD, fileTypeDocD, typeDocD, listTypeDocD, 
    voidDocD, constructDocD, stateParamDocD, paramListDocD, methodDocD, 
    methodListDocD, stateVarDocD, stateVarListDocD, ifCondDocD, switchDocD, 
    forDocD, forEachDocD, whileDocD, tryCatchDocD, assignDocD, plusEqualsDocD,
    plusEqualsDocD', plusPlusDocD, plusPlusDocD', varDecDocD, 
    varDecDefDocD, listDecDocD, listDecDefDocD, statementDocD, returnDocD, 
    commentDocD, freeDocD, throwDocD, stratDocD, notOpDocD, notOpDocD', negateOpDocD, 
    sqrtOpDocD, sqrtOpDocD', absOpDocD, absOpDocD', logOpDocD, logOpDocD', 
    lnOpDocD, lnOpDocD', expOpDocD, expOpDocD', sinOpDocD, sinOpDocD', 
    cosOpDocD, cosOpDocD', tanOpDocD, tanOpDocD', unOpDocD, equalOpDocD, 
    notEqualOpDocD, greaterOpDocD, greaterEqualOpDocD, lessOpDocD, 
    lessEqualOpDocD, plusOpDocD, minusOpDocD, multOpDocD, divideOpDocD, 
    moduloOpDocD, powerOpDocD, andOpDocD, orOpDocD, binOpDocD, binOpDocD', 
    litTrueD, litFalseD, litCharD, litFloatD, litIntD,
    litStringD, defaultCharD, defaultFloatD, defaultIntD, defaultStringD, 
    varDocD, extVarDocD, selfDocD, argDocD, enumElemDocD, objVarDocD, 
    inlineIfDocD, funcAppDocD, extFuncAppDocD, stateObjDocD, listStateObjDocD, 
    objDecDefDocD, constDecDefDocD, notNullDocD, listIndexExistsDocD, funcDocD,
    castDocD, sizeDocD, listAccessDocD, listSetDocD, 
    objAccessDocD, castObjDocD, includeD, breakDocD, continueDocD, staticDocD, 
    dynamicDocD, privateDocD, publicDocD, addCommentsDocD, callFuncParamList, 
    getterName, setterName
) where

import Language.Drasil.Code.Code (Code(..))
import Language.Drasil.Code.Imperative.New (Label, Library)
import Language.Drasil.Code.Imperative.Helpers (angles,blank,doubleQuotedText,
    oneTab,capitalize,oneTabbed,hicat,vibcat,vmap)

import Data.List (intersperse)
import Prelude hiding (break,print,return,last,mod,(<>))
import System.IO (hPutStrLn, hClose, openFile, IOMode(WriteMode))
import Text.PrettyPrint.HughesPJ (Doc, text, empty, render, (<>), (<+>), 
    brackets, parens, isEmpty, rbrace, lbrace, vcat, char, double, quotes, 
    integer, semi, equals, braces, int, comma, colon, hcat)

-- | Takes code, filenames, and extensions
makeCode :: [Doc] -> [Label] -> [Label] -> [(FilePath, Doc)]
makeCode files names exts =
    [(name ++ ext, file) | (name, (file, ext)) <- zip (repeatListElems (length exts) names) (zip files (cycle exts))]

repeatListElems :: Int -> [a] -> [a]
repeatListElems _ [] = []
repeatListElems 1 xs = xs
repeatListElems n (x:xs) = (take n (repeat x)) ++ repeatListElems n xs

------------------
-- IO Functions --
------------------

-- | Creates the requested 'Code' by producing files
createCodeFiles :: [(FilePath, Doc)] -> IO () -- [(FilePath, Doc)] -> IO ()
createCodeFiles cs = mapM_ createCodeFile cs

createCodeFile :: (FilePath, Doc) -> IO ()
createCodeFile (path, code) = do
    h <- openFile path WriteMode
    hPutStrLn h (render code)
    hClose h

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

fileDoc' :: Doc -> Doc -> Doc -> Doc
fileDoc' t m b = vibcat [
    t,
    m,
    b]
    
-- fileNameD :: Module -> String
-- fileNameD _ = moduleName

-----------------------------------------------
-- 'Default' functions used in the renderers --
-----------------------------------------------

-- Module --

moduleDocD :: [Doc] -> Doc
moduleDocD cs = vibcat cs

-- Class --

classDocD :: Label -> Maybe Label -> Doc -> Doc -> Doc -> Doc -> Doc
classDocD n p inherit s vs fs = vcat [
    s <+> classDec <+> text n <+> baseClass <+> lbrace, 
    oneTabbed [
        vs,
        blank,
        fs],
    rbrace]
    where baseClass = case p of Nothing -> empty
                                Just pn -> inherit <+> text pn

enumDocD :: Label -> Doc -> Doc -> Doc
enumDocD n es s = vcat [
    s <+> text "enum" <+> text n <+> lbrace,
    oneTab $ es,
    rbrace]

enumElementsDocD :: [Label] -> Bool -> Doc
enumElementsDocD es enumsEqualInts = vcat $
    zipWith (\e i -> text e <+> equalsInt i <> interComma i) es nums
    where nums = [0..length es - 1]
          equalsInt i = if enumsEqualInts then (equals <+> int i) else empty 
          interComma i = if i < length es - 1 then text "," else empty

enumElementsDocD' :: [Label] -> Doc
enumElementsDocD' es = vcat $
    zipWith (\e i -> text e <+> equals <+> int i) es nums
        where nums = [0..length es - 1]

-- Groupings --

multiStateDocD :: Doc -> [Doc] -> Doc
multiStateDocD end sts = vcat (applyEnd statements)
  where applyEnd [] = []
        applyEnd [s] = [s]
        applyEnd (s:ss) = (s <> end):(applyEnd ss)
        statements = filter notNullStatement sts
        notNullStatement s = (not $ isEmpty s) && (render s /= render end)

blockDocD :: Doc -> [Doc] -> Doc
blockDocD end sts = vcat statements
  where statements = filter notNullStatement sts
        notNullStatement s = (not $ isEmpty s) && (render s /= render end)

bodyDocD :: [Doc] -> Doc
bodyDocD bs = vibcat blocks
  where blocks = filter notNullBlock bs
        notNullBlock b = (not $ isEmpty b)

-- IO --

outDocD :: Doc -> Doc -> Doc
outDocD printFn v = printFn <> parens (v)

printListDocD :: Doc -> Doc -> Doc -> Doc -> Doc
printListDocD open b lastElem close = vcat [open, b, lastElem, close]

printFileDocD :: Label -> Doc -> Doc
printFileDocD fn f = f <> dot <> text fn

-- Type Printers --

boolTypeDocD :: Doc
boolTypeDocD = text "Boolean" -- capital B?

intTypeDocD :: Doc
intTypeDocD = text "int"

floatTypeDocD :: Doc
floatTypeDocD = text "float"

charTypeDocD :: Doc
charTypeDocD = text "char"

stringTypeDocD :: Doc
stringTypeDocD = text "string"

fileTypeDocD :: Doc
fileTypeDocD = text "File"

typeDocD :: Label -> Doc
typeDocD t = text t

listTypeDocD :: Doc -> Doc -> Doc
listTypeDocD st list = list <> angles st

-- Method Types --

voidDocD :: Doc
voidDocD = text "void"

constructDocD :: Label -> Doc
constructDocD _ = empty

-- Parameters --

stateParamDocD :: Label -> Doc -> Doc
stateParamDocD n t = t <+> text n

paramListDocD :: [Doc] -> Doc
paramListDocD ps = hicat (text ", ") ps

-- Method --

methodDocD :: Label -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc
methodDocD n s p t ps b = vcat [
    s <+> p <+> t <+> text n <> parens (ps) <+> lbrace,
    oneTab $ b,
    rbrace]

methodListDocD :: [Doc] -> Doc
methodListDocD ms = vibcat methods
    where methods = filter (\m -> not $ isEmpty m) ms

-- StateVar --

stateVarDocD :: Label -> Doc -> Doc -> Doc -> Doc -> Doc
stateVarDocD l s p t end = s <+> p <+> t <+> text l <> end

stateVarListDocD :: [Doc] -> Doc
stateVarListDocD svs = vcat svs

-- Controls --

ifCondDocD :: Doc -> Doc -> Doc -> Doc -> [(Doc, Doc)] -> Doc
ifCondDocD _ _ _ _ [] = error "if condition created with no cases"
ifCondDocD ifStart elseIf blockEnd elseBody (c:cs) = 
    let ifSect (v, b) = vcat [
            text "if" <+> parens v <+> ifStart,
            oneTab b,
            blockEnd]
        elseIfSect (v, b) = vcat [
            elseIf <+> parens v <+> ifStart,
            oneTab b,
            blockEnd]
        elseSect = if isEmpty elseBody then empty else vcat [
            text "else" <+> ifStart,
            oneTab elseBody,
            blockEnd]
    in vcat [
        ifSect c,
        vmap elseIfSect cs,
        elseSect]

switchDocD :: Doc -> Doc -> Doc -> [(Doc, Doc)] -> Doc
switchDocD breakState v defBody cs = 
    let caseDoc (l, result) = vcat [
            text "case" <+> l <> colon,
            oneTabbed [
                result,
                breakState]]
        defaultSection = vcat [
            text "default" <> colon,
            oneTabbed [
                defBody,
                breakState]]
    in vcat [
        text "switch" <> parens v <+> lbrace,
        oneTabbed [
            vmap caseDoc cs,
            defaultSection],
        rbrace]

-- These signatures wont be quite so horrendous if/when we pass language options
-- (blockStart, etc.) in as shared environment
forDocD :: Doc -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc
forDocD blockStart blockEnd sInit vGuard sUpdate b = vcat [
    forLabel <+> parens (sInit <> semi <+> vGuard <> semi <+> sUpdate) <+> blockStart,
    oneTab b,
    blockEnd]

forEachDocD :: Label -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc
forEachDocD l blockStart blockEnd iterForEachLabel iterInLabel t v b = vcat [
    iterForEachLabel <+> parens (t <+> text l <+> iterInLabel <+> v) <+> blockStart,
    oneTab $ b,
    blockEnd]

whileDocD :: Doc -> Doc -> Doc -> Doc -> Doc
whileDocD blockStart blockEnd v b= vcat [
    text "while" <+> parens v <+> blockStart,
    oneTab b,
    blockEnd]

tryCatchDocD :: Doc -> Doc -> Doc 
tryCatchDocD tb cb = vcat [
    text "try" <+> lbrace,
    oneTab $ tb,
    rbrace <+> text "catch" <+> parens (text "System.Exception" <+> text "exc") <+> lbrace,
    oneTab $ cb,
    rbrace]

stratDocD :: Doc -> Doc -> Doc
stratDocD b resultState = vcat [
    b,
    resultState]

-- Statements --

assignDocD :: Doc-> Doc -> Doc
assignDocD v1 v2 = v1 <+> equals <+> v2

plusEqualsDocD :: Doc -> Doc -> Doc
plusEqualsDocD v1 v2 = v1 <+> text "+=" <+> v2

plusEqualsDocD' :: Doc -> Doc -> Doc -> Doc
plusEqualsDocD' v1 plusOp v2 = v1 <+> equals <+> v1 <+> plusOp <+> v2

plusPlusDocD :: Doc -> Doc
plusPlusDocD v = v <> text "++"

plusPlusDocD' :: Doc -> Doc -> Doc
plusPlusDocD' v plusOp = v <+> equals <+> v <+> plusOp <+> int 1

varDecDocD :: Label -> Doc -> Doc
varDecDocD l st = st <+> text l

varDecDefDocD :: Label -> Doc -> Doc -> Doc
varDecDefDocD l st v = st <+> text l <+> equals <+> v

listDecDocD :: Label -> Doc -> Doc -> Doc
listDecDocD l n st = st <+> text l <+> equals <+> new <+> st <> parens n

listDecDefDocD :: Label -> Doc -> [Doc] -> Doc
listDecDefDocD l st vs = st <+> text l <+> equals <+> new <+> st <+> braces (callFuncParamList vs)

objDecDefDocD :: Label -> Doc -> Doc -> Doc
objDecDefDocD l st v = varDecDefDocD l st v

constDecDefDocD :: Label -> Doc -> Doc -> Doc -- can this be done without StateType (infer from value)?
constDecDefDocD l st v = text "const" <+> st <+> text l <+> v

returnDocD :: Doc -> Doc
returnDocD v = text "return" <+> v

commentDocD :: Label -> Doc -> Doc
commentDocD cmt cStart = cStart <+> text cmt

freeDocD :: Doc -> Doc
freeDocD v = text "delete" <+> v

throwDocD :: Doc -> Doc
throwDocD errMsg = text "throw new" <+> text "System.ApplicationException" <>
    parens errMsg

statementDocD :: Doc -> Doc -> Doc
statementDocD s end = s <> end

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

unOpDocD :: Doc -> Doc -> Doc
unOpDocD op v = op <> parens v

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

binOpDocD :: Doc -> Doc -> Doc -> Doc
binOpDocD v1 op v2 = parens (v1 <+> op <+> v2)

binOpDocD' :: Doc -> Doc -> Doc -> Doc
binOpDocD' v1 op v2 = op <> parens (v1 <> comma <+> v2)

-- Literals --

litTrueD :: Doc
litTrueD = text "true"

litFalseD :: Doc
litFalseD = text "false"

litCharD :: Char -> Doc
litCharD c = quotes $ char c

litFloatD :: Double -> Doc
litFloatD v = double v

litIntD :: Integer -> Doc
litIntD v = integer v

litStringD :: String -> Doc
litStringD s = doubleQuotedText s

defaultCharD :: Doc
defaultCharD = char ' '

defaultFloatD :: Doc
defaultFloatD = double 0.0

defaultIntD :: Doc
defaultIntD = integer 0

defaultStringD :: Doc
defaultStringD = doubleQuotedText ""

-- Value Printers --

varDocD :: Label -> Doc
varDocD l = text l

extVarDocD :: Library -> Label -> Doc
extVarDocD l n = text l <> dot <> text n

selfDocD :: Doc
selfDocD = text "this"

argDocD :: Doc -> Doc -> Doc
argDocD n args = args <> brackets n

enumElemDocD :: Label -> Label -> Doc
enumElemDocD en e = text en <> dot <> text e

objVarDocD :: Doc -> Doc ->  Doc
objVarDocD n1 n2 = n1 <> dot <> n2

inlineIfDocD :: Doc -> Doc -> Doc -> Doc
inlineIfDocD c v1 v2 = parens (parens c <+> text "?" <+> v1 <+> text ":" <+> v2)

funcAppDocD :: Label -> [Doc] -> Doc
funcAppDocD n vs = text n <> parens (callFuncParamList vs)

extFuncAppDocD :: Library -> Label -> [Doc] -> Doc
extFuncAppDocD l n vs = funcAppDocD (l ++ "." ++ n) vs

stateObjDocD :: Doc -> Doc -> Doc
stateObjDocD st vs = new <+> st <> parens vs

listStateObjDocD :: Doc -> Doc -> Doc -> Doc
listStateObjDocD lstObj st vs = lstObj <+> st <> parens vs

notNullDocD :: Doc -> Doc -> Doc -> Doc
notNullDocD v op nullvar = binOpDocD v op nullvar

listIndexExistsDocD :: Doc -> Doc -> Doc -> Doc
listIndexExistsDocD lst greater index = parens (lst <> text ".Length" <+>      
    greater <+> index) 

-- Functions --

funcDocD :: Doc -> Doc
funcDocD fnApp = dot <> fnApp

castDocD :: Doc -> Doc
castDocD targT = parens targT

sizeDocD :: Doc
sizeDocD = dot <> text "Count"

listAccessDocD :: Doc -> Doc
listAccessDocD i = brackets i

listSetDocD :: Doc -> Doc -> Doc
listSetDocD i v = brackets i <+> equals <+> v

objAccessDocD :: Doc -> Doc -> Doc
objAccessDocD v f = v <> f

castObjDocD :: Doc -> Doc -> Doc
castObjDocD f v = f <> parens v

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
endCommentDelimit c cStart = commentDelimit (endCommentLabel ++ " " ++ c) cStart

dashes :: String -> Int -> String
dashes s l = take (l - length s) (repeat '-')

-- Helper Functions --

callFuncParamList :: [Doc] -> Doc
callFuncParamList vs = hcat (intersperse (text ", ") vs)

getterName :: String -> String
getterName s = "Get" ++ capitalize s

setterName :: String -> String
setterName s = "Set" ++ capitalize s