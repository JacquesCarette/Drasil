module LanguageRenderer.NewJavaRenderer (
    -- * Java Code Configuration -- defines syntax of all Java code
    JavaCode(..)
) where

import New (Class, Method, Body, Block, Statement, Declaration, Value, StateType,
  Function, StateVar, IOType, IOSt, Scope, UnaryOp, Keyword, Label, Library, VarDecl, 
  FunctionDecl,
  RenderSym(..), KeywordSym(..), ClassSym(..), MethodSym(..), 
  BodySym(..), Symantics(..), StateTypeSym(..), StatementSym(..), IOTypeSym(..),
  IOStSym(..), UnaryOpSym(..), ValueSym(..), Selector(..), FunctionSym(..))
import NewLanguageRenderer (fileDoc', blockDocD, ioDocOutD, boolTypeDocD, intTypeDocD,
  charTypeDocD, typeDocD, listTypeDocD, notOpDocD, negateOpDocD, unOpDocD, litTrueD, litFalseD, 
  litCharD, litFloatD, litIntD, litStringD, defaultCharD, defaultFloatD, defaultIntD, 
  defaultStringD, includeD, dot)
import Helpers (blank,angles,oneTab,vibmap)

import Prelude hiding (break,print,(<>))
import Control.Applicative (Applicative, liftA, liftA2, liftA3)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), parens, empty, equals, 
  semi, vcat, lbrace, rbrace, doubleQuotes, render, colon)

newtype JavaCode a = JC {unJC :: a}

instance Functor JavaCode where
    fmap f (JC x) = JC (f x)

instance Applicative JavaCode where
    pure = JC
    (JC f) <*> (JC x) = JC (f x)

instance Monad JavaCode where
    return = JC
    JC x >>= f = f x

-- type JavaCode a = JavaCodeMonad Doc

-- liftJC0 :: Doc -> JavaCode Doc
-- liftJC0 = JC

-- liftJC1 :: (Doc -> Doc) -> JavaCode Doc -> JavaCode Doc
-- liftJC1 f a = JC $ f (unJC a)

-- liftJC2 :: (Doc -> Doc -> Doc) -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc
-- liftJC2 f a b = JC $ f (unJC a) (unJC b)

-- liftJC3 :: (Doc -> Doc -> Doc -> Doc) -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc
-- liftJC3 f a b c = JC $ f (unJC a) (unJC b) (unJC c)

liftList :: ([Doc] -> Doc) -> [JavaCode Doc] -> JavaCode Doc
liftList f as = JC $ f (map unJC as)

instance RenderSym JavaCode where
    fileDoc code = liftA3 fileDoc' top code bottom
    top = liftA3 jtop endStatement include list
    codeBody m = m -- don't need right now
    bottom = return empty

instance KeywordSym JavaCode where
    endStatement = return semi
    include = return $ includeD "import"
    list = return $ text "Vector"
    printFunc = return $ text "System.out.print"
    printLnFunc = return $ text "System.out.println"
    printFileFunc f = liftA jPrintFileFunc f
    printFileLnFunc f = liftA jPrintFileLnFunc f
    
instance ClassSym JavaCode where
    -- buildClass n p s vs fs = liftA3 (classDocD n p) s vs fs -- vs and fs are actually lists... should 

-- instance ClassSym JavaCode where
--     buildClass n p s vs fs = do
--         scope <- s
--         vars <- vs
--         funcs <- fs --ignore for the moment that fs and vs are lists
--         liftJC0 (classDocD n p scope vars funcs)

-- instance ClassSym JavaCode where
--     buildClass n p s vs fs = JC $ do
--         c <- ask -- not needed, only ask for config when you need to actually extract from it
--         liftJC0 (classDoc c n p s vs fs)

instance StateTypeSym JavaCode where
    bool = return $ boolTypeDocD
    int = return $ intTypeDocD
    float = return $ jFloatTypeDocD
    char = return $ charTypeDocD
    string = return $ jStringTypeDoc
    infile = return $ jInfileTypeDoc
    outfile = return $ jOutfileTypeDoc
    listType st = liftA2 listTypeDocD st list
    intListType = liftA jIntListTypeDoc list
    floatListType = liftA jFloatListTypeDoc list
    obj t = return $ typeDocD t
    enumType t = return $ typeDocD t

instance UnaryOpSym JavaCode where
    notOp = return $ notOpDocD
    negateOp = return $ negateOpDocD
    sqrtOp = return $ text "Math.sqrt"
    absOp = return $ text "Math.abs"
    logOp = return $ text "Math.log10"
    lnOp = return $ text "Math.log"
    expOp = return $ text "Math.exp"
    sinOp = return $ text "Math.sin"
    cosOp = return $ text "Math.cos"
    tanOp = return $ text "Math.tan"

instance ValueSym JavaCode where
    litTrue = return $ litTrueD
    litFalse = return $ litFalseD
    litChar c = return $ litCharD c
    litFloat v = return $ litFloatD v
    litInt v = return $ litIntD v
    litString s = return $ litStringD s

    defaultChar = return $ defaultCharD
    defaultFloat = return $ defaultFloatD
    defaultInt = return $ defaultIntD
    defaultString = return $ defaultStringD

    (?!) v = liftA2 unOpDocD notOp v

    (#~) v = liftA2 unOpDocD negateOp v
    (#/^) v = liftA2 unOpDocD sqrtOp v
    (#|) v = liftA2 unOpDocD absOp v

    log v = liftA2 unOpDocD logOp v
    ln v = liftA2 unOpDocD lnOp v
    exp v = liftA2 unOpDocD expOp v
    sin v = liftA2 unOpDocD sinOp v
    cos v = liftA2 unOpDocD cosOp v
    tan v = liftA2 unOpDocD tanOp v
    -- csc v = liftA2 unOpDocD Op v             -- need binaryOp first
    -- sec v = liftA2 unOpDocD logOp v
    -- cot v = liftA2 unOpDocD logOp v

instance IOTypeSym JavaCode

instance IOStSym JavaCode where
    out prf v = liftA3 ioDocOutD prf v endStatement

instance StatementSym JavaCode where
    print _ v = ioState (out printFunc v)
    printLn _ v = ioState (out printLnFunc v)
    printStr s = ioState (out printFunc (litString s))
    printStrLn s = ioState (out printLnFunc (litString s))

    printFile f _ v = ioState (out (printFileFunc f) v)
    printFileLn f _ v = ioState (out (printFileLnFunc f) v)
    printFileStr f s = ioState (out (printFileFunc f) (litString s))
    printFileStrLn f s = ioState (out (printFileLnFunc f) (litString s))

    ioState = id -- Now that types are Doc synonyms, I think this will work

instance Symantics JavaCode where
    block sts = do
        end <- endStatement
        liftList (blockDocD end) sts

instance BodySym JavaCode where
    -- body sts = JC $ do
    --     c <- ask
    --     liftJC0 (statementDoc c sts)

jtop :: Doc -> Doc -> Doc -> Doc
jtop end inc lst = vcat [
    inc <+> text "java.util.Arrays" <> end,
    inc <+> text "java.util.BitSet" <> end,     --TODO: only include these if they are used in the code?
    inc <+> text "java.util.Scanner" <> end,
    inc <+> text "java.io.PrintWriter" <> end,
    inc <+> text "java.io.File" <> end,
    inc <+> text ("java.util." ++ render (lst)) <> end]

jPrintFileFunc :: Doc -> Doc
jPrintFileFunc f = f <> dot <> text "print"

jPrintFileLnFunc :: Doc -> Doc
jPrintFileLnFunc f = f <> dot <> text "println"

jFloatTypeDocD :: Doc
jFloatTypeDocD = text "double"

jStringTypeDoc :: Doc
jStringTypeDoc = text "String"

jInfileTypeDoc :: Doc
jInfileTypeDoc = text "Scanner"

jOutfileTypeDoc :: Doc
jOutfileTypeDoc = text "PrintWriter"

jIntListTypeDoc :: Doc -> Doc
jIntListTypeDoc lst = lst <> angles (text "Integer")

jFloatListTypeDoc :: Doc -> Doc
jFloatListTypeDoc lst = lst <> angles (text "Double")