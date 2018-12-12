-- | The structure for a class of renderers is defined here.
module NewLanguageRenderer (
    -- * Code Generation Funcitons
    makeCode, createCodeFiles,
    
    -- * Common Syntax
    classDec, dot, doubleSlash, forLabel, new,
    
    -- * Default Functions available for use in renderers
    fileDoc', blockDocD, ioDocOutD, boolTypeDocD, intTypeDocD, floatTypeDocD, 
    charTypeDocD, stringTypeDocD, fileTypeDocD, typeDocD, listTypeDocD,
    assignDocD, plusEqualsDocD, plusPlusDocD, varDecDocD, varDecDefDocD, 
    listDecDocD, listDecDefDocD, statementDocD,
    notOpDocD, negateOpDocD, sqrtOpDocD, absOpDocD, logOpDocD, lnOpDocD, 
    expOpDocD, sinOpDocD, cosOpDocD, tanOpDocD, unOpDocD, equalOpDocD, 
    notEqualOpDocD, greaterOpDocD, greaterEqualOpDocD, lessOpDocD, 
    lessEqualOpDocD, plusOpDocD, minusOpDocD, multOpDocD, divideOpDocD, 
    moduloOpDocD, powerOpDocD, andOpDocD, orOpDocD, binOpDocD, binOpDocD', 
    litTrueD, litFalseD, litCharD, litFloatD, litIntD,
    litStringD, defaultCharD, defaultFloatD, defaultIntD, defaultStringD, 
    varDocD, extVarDocD, selfDocD, argDocD, enumElemDocD, objVarDocD, inlineIfDocD,
    funcAppDocD, extFuncAppDocD, stateObjDocD, listStateObjDocD, objDecDefDocD,
    constDecDefDocD, notNullDocD, 
    staticDocD, dynamicDocD, includeD, callFuncParamList
) where

import New (Label, Library)
import Helpers (angles,blank,doubleQuotedText,oneTab,
                            oneTabbed,himap,vibcat,vmap,vibmap)

import Data.List (find, intersperse)
import Prelude hiding (break,print,return,last,mod,(<>))
import System.IO (hPutStrLn, hClose, openFile, IOMode(WriteMode))
import Text.PrettyPrint.HughesPJ (Doc, text, empty, render, (<>), (<+>), brackets, parens,
  isEmpty, rbrace, lbrace, vcat, space, char, double, quotes, integer, semi, equals, braces,
  int, comma, colon, hcat)


-- | Takes code, filenames, and extensions
makeCode :: [Doc] -> [Label] -> [Label] -> [(FilePath, Doc)]
makeCode files names exts =
    [(name ++ ext, file) | (name, (file, ext)) <- zip (duplicateListElems names) (zip files (cycle exts))]

duplicateListElems :: [a] -> [a]
duplicateListElems [] = []
duplicateListElems (x:xs) = x:x:duplicateListElems xs

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

-- Uncomment classDocD when inherit works

-- classDocD :: Label -> Maybe Label -> Doc -> Doc -> Doc -> Doc
-- classDocD n p s vs fs = vcat [
--     s <+> clsDec c <+> text n <+> baseClass <+> lbrace, 
--     oneTabbed [
--         vs,
--         blank,
--         fs],
--     rbrace]
--     where baseClass = case p of Nothing -> empty
--                                 Just pn -> inherit c <+> text pn

blockDocD :: Doc -> [Doc] -> Doc
blockDocD end sts = vcat statements
  where statements = filter notNullStatement sts
        notNullStatement s = (not $ isEmpty s) && (render s /= render end)

bodyDocD :: [Doc] -> Doc
bodyDocD bs = vcat blocks
  where blocks = filter notNullStatement bs
        notNullStatement s = (not $ isEmpty s) && (render s /= render end)

-- ioState just returns itself. don't need a function for this.
-- statementDocD :: Config -> StatementLocation -> Statement -> Doc
-- statementDocD c _ (IOState io) = ioDoc c io

ioDocOutD :: Doc -> Doc -> Doc
ioDocOutD printFn v = printFn <> parens (v)

-- Replaced by ioDocOutD
-- printDocConsoleD :: Bool -> Doc -> Doc
-- printDocConsoleD newLn v = printFn <> parens (valueDoc c v)
--     where printFn = if newLn then printLnFunc c else printFunc c

-- valueDocD :: Config -> Value -> Doc
-- valueDocD c (Lit v) = litDoc c v

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

-- Statements --

assignDocD :: Doc-> Doc -> Doc
assignDocD v1 v2 = v1 <+> equals <+> v2

plusEqualsDocD :: Doc -> Doc -> Doc
plusEqualsDocD v1 v2 = v1 <+> text "+=" <+> v2

plusPlusDocD :: Doc -> Doc
plusPlusDocD v = v <> text "++"

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

statementDocD :: Doc -> Doc -> Doc
statementDocD s end = s <> end

-- Unary Operators --

notOpDocD :: Doc
notOpDocD = text "!"

negateOpDocD :: Doc
negateOpDocD = text "-"

sqrtOpDocD :: Doc
sqrtOpDocD = text "sqrt"

absOpDocD :: Doc
absOpDocD = text "fabs"

logOpDocD :: Doc
logOpDocD = text "log"

lnOpDocD :: Doc
lnOpDocD = text "ln"

expOpDocD :: Doc
expOpDocD = text "exp"

sinOpDocD :: Doc
sinOpDocD = text "sin"

cosOpDocD :: Doc
cosOpDocD = text "cos"

tanOpDocD :: Doc
tanOpDocD = text "tan"

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
binOpDocD v1 op v2 = parens v1 <+> op <+> parens v2

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

stateObjDocD :: Doc -> [Doc] -> Doc
stateObjDocD st vs = new <+> st <> parens (callFuncParamList vs)

listStateObjDocD :: Doc -> Doc -> [Doc] -> Doc
listStateObjDocD lstObj st vs = lstObj <+> st <> parens (callFuncParamList vs)

notNullDocD :: Doc -> Doc -> Doc -> Doc
notNullDocD v op nullvar = binOpDocD v op nullvar

-- Keywords --

includeD :: Label -> Doc
includeD incl = text incl

-- Permanence --

staticDocD :: Doc
staticDocD = text "static"

dynamicDocD :: Doc
dynamicDocD = empty

-- Helper Functions --

callFuncParamList :: [Doc] -> Doc
callFuncParamList vs = hcat (intersperse (text ", ") vs)
