{-# LANGUAGE TypeFamilies #-}

module LanguageRenderer.NewJavaRenderer (
    -- * Java Code Configuration -- defines syntax of all Java code
    JavaCode(..)
) where

import New (Class, Method, Body, Block, Statement, Declaration, Value, StateType,
  Function, StateVar, IOType, IOSt, Scope, UnaryOp, BinaryOp, Permanence, Label, Library, VarDecl, 
  FunctionDecl,
  RenderSym(..), KeywordSym(..), PermanenceSym(..), ClassSym(..), MethodSym(..), 
  BodySym(..), Symantics(..), StateTypeSym(..), StatementSym(..), IOTypeSym(..),
  IOStSym(..), UnaryOpSym(..), BinaryOpSym(..), ValueSym(..), Selector(..), FunctionSym(..))
import NewLanguageRenderer (fileDoc', blockDocD, ioDocOutD, boolTypeDocD, intTypeDocD,
  charTypeDocD, typeDocD, listTypeDocD, assignDocD, plusEqualsDocD, plusPlusDocD,
  varDecDocD, varDecDefDocD, listDecDocD, listDecDefDocD, statementDocD,
  notOpDocD, negateOpDocD, unOpDocD, equalOpDocD, 
  notEqualOpDocD, greaterOpDocD, greaterEqualOpDocD, lessOpDocD, 
  lessEqualOpDocD, plusOpDocD, minusOpDocD, multOpDocD, divideOpDocD, 
  moduloOpDocD, andOpDocD, orOpDocD, binOpDocD, binOpDocD', litTrueD, litFalseD, 
  litCharD, litFloatD, litIntD, litStringD, defaultCharD, defaultFloatD, defaultIntD, 
  defaultStringD, varDocD, extVarDocD, selfDocD, argDocD, enumElemDocD, objVarDocD, 
  inlineIfDocD, funcAppDocD, extFuncAppDocD, stateObjDocD, listStateObjDocD, 
  notNullDocD, staticDocD, dynamicDocD, includeD, dot, new)
import Helpers (blank,angles,oneTab,vibmap)

import Prelude hiding (break,print,(<>),sin,cos,tan)
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

lift1List :: (Doc -> [Doc] -> Doc) -> JavaCode Doc -> [JavaCode Doc] -> JavaCode Doc
lift1List f a as = JC $ f (unJC a) (map unJC as)

lift2List :: (Doc -> Doc -> [Doc] -> Doc) -> JavaCode Doc -> JavaCode Doc -> [JavaCode Doc] -> JavaCode Doc
lift2List f a1 a2 as = JC $ f (unJC a1) (unJC a2) (map unJC as)

instance RenderSym JavaCode where
    fileDoc code = liftA3 fileDoc' top code bottom
    top = liftA3 jtop endStatement include (list static)
    codeBody m = m -- don't need right now
    bottom = return empty

instance KeywordSym JavaCode where
    type Keyword JavaCode = Doc
    endStatement = return semi
    include = return $ includeD "import"
    list _ = return $ text "Vector"
    printFunc = return $ text "System.out.print"
    printLnFunc = return $ text "System.out.println"
    printFileFunc f = liftA jPrintFileFunc f
    printFileLnFunc f = liftA jPrintFileLnFunc f
    argsList = return $ text "args"
    listObj = return new

instance PermanenceSym JavaCode where
    static = return staticDocD
    dynamic = return dynamicDocD
    
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
    listType p st = liftA2 listTypeDocD st (list p)
    intListType p = liftA jIntListTypeDoc (list p)
    floatListType p = liftA jFloatListTypeDoc (list p)
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

instance BinaryOpSym JavaCode where
    equalOp = return $ equalOpDocD
    notEqualOp = return $ notEqualOpDocD
    greaterOp = return $ greaterOpDocD
    greaterEqualOp = return $ greaterEqualOpDocD
    lessOp = return $ lessOpDocD
    lessEqualOp = return $ lessEqualOpDocD
    plusOp = return $ plusOpDocD
    minusOp = return $ minusOpDocD
    multOp = return $ multOpDocD
    divideOp = return $ divideOpDocD
    powerOp = return $ text "Math.pow"
    moduloOp = return $ moduloOpDocD
    andOp = return $ andOpDocD
    orOp = return $ orOpDocD

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
    (?<)  v1 v2 = liftA3 binOpDocD v1 lessOp v2
    (?<=) v1 v2 = liftA3 binOpDocD v1 lessEqualOp v2
    (?>)  v1 v2 = liftA3 binOpDocD v1 greaterOp v2
    (?>=) v1 v2 = liftA3 binOpDocD v1 greaterEqualOp v2
    (?==) v1 v2 = liftA3 binOpDocD v1 equalOp v2
    (?!=) v1 v2 = liftA3 binOpDocD v1 notEqualOp v2
    (?&&) v1 v2 = liftA3 binOpDocD v1 andOp v2
    (?||) v1 v2 = liftA3 binOpDocD v1 orOp v2

    (#~) v = liftA2 unOpDocD negateOp v
    (#/^) v = liftA2 unOpDocD sqrtOp v
    (#|) v = liftA2 unOpDocD absOp v
    (#+)  v1 v2 = liftA3 binOpDocD v1 plusOp v2
    (#-)  v1 v2 = liftA3 binOpDocD v1 minusOp v2
    (#*)  v1 v2 = liftA3 binOpDocD v1 multOp v2
    (#/)  v1 v2 = liftA3 binOpDocD v1 divideOp v2
    (#%)  v1 v2 = liftA3 binOpDocD v1 moduloOp v2
    (#^)  v1 v2 = liftA3 binOpDocD' v1 powerOp v2

    log v = liftA2 unOpDocD logOp v
    ln v = liftA2 unOpDocD lnOp v
    exp v = liftA2 unOpDocD expOp v
    sin v = liftA2 unOpDocD sinOp v
    cos v = liftA2 unOpDocD cosOp v
    tan v = liftA2 unOpDocD tanOp v
    csc v = (litFloat 1.0) #/ (sin v)
    sec v = (litFloat 1.0) #/ (cos v)
    cot v = (litFloat 1.0) #/ (tan v)

    const n = var n
    var n = return $ varDocD n
    extVar l n = return $ extVarDocD l n
    self = return $ selfDocD
    arg n = liftA2 argDocD (litInt n) argsList
    enumElement en e = return $ enumElemDocD en e
    enumVar n = var n
    objVar n1 n2 = liftA2 objVarDocD n1 n2
    objVarSelf n = var n
    listVar n _ = var n
    inlineIf c v1 v2 = liftA3 inlineIfDocD c v1 v2
    funcApp n vs = liftList (funcAppDocD n) vs
    extFuncApp l n vs = liftList (extFuncAppDocD l n) vs
    stateObj st vs = lift1List stateObjDocD st vs
    listStateObj st vs = lift2List listStateObjDocD listObj st vs

    exists v = v
    notNull v = liftA3 notNullDocD v notEqualOp (var "null")

instance IOTypeSym JavaCode

instance IOStSym JavaCode where
    out prf v = liftA2 ioDocOutD prf v

instance StatementSym JavaCode where
    assign v1 v2 = state (liftA2 assignDocD v1 v2)
    (&=) v1 v2 = assign v1 v2
    (&.=) l v = assign (var l) v
    (&=.) v l = assign v (var l)
    (&-=) v1 v2 = v1 &= (v1 #- v2)
    (&.-=) l v = l &.= (var l #- v)
    (&+=) v1 v2 = state (liftA2 plusEqualsDocD v1 v2)
    (&.+=) l v = (var l) &+= v
    (&++) v = state (liftA plusPlusDocD v)
    (&.++) l = (&++) (var l)
    (&~-) v = v &= (v #- (litInt 1))
    (&.~-) l = (&~-) (var l)

    varDec l st = state $ liftA (varDecDocD l) st
    varDecDef l st v = state $ liftA2 (varDecDefDocD l) st v
    listDec l n st = state $ liftA2 (listDecDocD l) (litInt n) st -- this means that the type you declare must already be a list. Not sure how I feel about this. On the bright side, it also means you don't need to pass permanence
    -- listDecDef l st vs = state $ lift1List (listDecDefDocD l) st vs -- this syntax isn't available for vectors, will come back to this later. Could have the function return 2 statements, one for initializing and one for adding elements

    print _ v = ioState (out printFunc v)
    printLn _ v = ioState (out printLnFunc v)
    printStr s = ioState (out printFunc (litString s))
    printStrLn s = ioState (out printLnFunc (litString s))

    printFile f _ v = ioState (out (printFileFunc f) v)
    printFileLn f _ v = ioState (out (printFileLnFunc f) v)
    printFileStr f s = ioState (out (printFileFunc f) (litString s))
    printFileStrLn f s = ioState (out (printFileLnFunc f) (litString s))

    ioState = state -- Now that types are Doc synonyms, I think this will work

    state s = liftA2 statementDocD s endStatement

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