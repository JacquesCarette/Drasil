module LanguageRenderer.NewJavaRenderer (
    -- * Java Code Configuration -- defines syntax of all Java code
    JavaCode(..)
) where

import New (Class, Method, Body, Block, Statement, Declaration, Value, StateType,
  Function, StateVar, IOType, IOSt, Scope, Keyword, Label, Library, VarDecl, 
  FunctionDecl,
  RenderSym(..), KeywordSym(..), ClassSym(..), MethodSym(..), 
  BodySym(..), Symantics(..), StateTypeSym(..), StatementSym(..), IOTypeSym(..),
  IOStSym(..), ValueSym(..), Selector(..), FunctionSym(..))
import NewLanguageRenderer (fileDoc', ioDocOutD, 
  litStringD, includeD, dot)
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

liftJC0 :: Doc -> JavaCode Doc
liftJC0 = JC

liftJC1 :: (Doc -> Doc) -> JavaCode Doc -> JavaCode Doc
liftJC1 f a = JC $ f (unJC a)

liftJC2 :: (Doc -> Doc -> Doc) -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc
liftJC2 f a b = JC $ f (unJC a) (unJC b)

liftJC3 :: (Doc -> Doc -> Doc -> Doc) -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc
liftJC3 f a b c = JC $ f (unJC a) (unJC b) (unJC c)

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

instance StateTypeSym JavaCode

instance ValueSym JavaCode where
    litString s = return $ litStringD s

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

instance Symantics JavaCode

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