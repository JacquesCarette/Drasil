{-# LANGUAGE TypeFamilies #-}

module LanguageRenderer.NewJavaRenderer (
    -- * Java Code Configuration -- defines syntax of all Java code
    JavaCode(..)
) where

import New (Declaration, StateVar, Scope, Label, Library,
  RenderSym(..), KeywordSym(..), PermanenceSym(..), InputTypeSym(..), ClassSym(..), MethodSym(..), 
  ProgramBodySym(..), BodySym(..), BlockSym(..), ControlSym(..), StateTypeSym(..),
  PreStatementSym(..),  StatementSym(..),
  IOStSym(..), UnaryOpSym(..), BinaryOpSym(..), ValueSym(..), Selector(..), 
  FunctionSym(..), SelectorFunction(..))
import NewLanguageRenderer (fileDoc', blockDocD, bodyDocD, progDocD, outDocD, 
  printListDocD, boolTypeDocD, intTypeDocD,
  charTypeDocD, typeDocD, listTypeDocD, ifCondDocD, switchDocD, forDocD, 
  forEachDocD, whileDocD, stratDocD, assignDocD, plusEqualsDocD, plusPlusDocD,
  varDecDocD, varDecDefDocD, listDecDocD, objDecDefDocD, statementDocD, returnDocD,
  
  commentDocD, notOpDocD, negateOpDocD, unOpDocD, equalOpDocD, 
  notEqualOpDocD, greaterOpDocD, greaterEqualOpDocD, lessOpDocD, 
  lessEqualOpDocD, plusOpDocD, minusOpDocD, multOpDocD, divideOpDocD, 
  moduloOpDocD, andOpDocD, orOpDocD, binOpDocD, binOpDocD', litTrueD, litFalseD, 
  litCharD, litFloatD, litIntD, litStringD, defaultCharD, defaultFloatD, defaultIntD, 
  defaultStringD, varDocD, extVarDocD, selfDocD, argDocD, enumElemDocD, objVarDocD, 
  inlineIfDocD, funcAppDocD, extFuncAppDocD, stateObjDocD, listStateObjDocD, 
  notNullDocD, breakDocD, continueDocD, staticDocD, dynamicDocD, includeD, 
  funcDocD, castDocD, sizeDocD, listAccessDocD, objAccessDocD, castObjDocD,dot, 
  new, forLabel, doubleSlash, callFuncParamList, getterName, setterName)
import Helpers (blank,angles,oneTab,vibmap)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor)
import qualified Data.Map as Map (fromList,lookup)
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

liftA4 :: (Doc -> Doc -> Doc -> Doc -> Doc) -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc
liftA4 f a1 a2 a3 a4 = JC $ f (unJC a1) (unJC a2) (unJC a3) (unJC a4)

liftA6 :: (Doc -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc) -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc
liftA6 f a1 a2 a3 a4 a5 a6 = JC $ f (unJC a1) (unJC a2) (unJC a3) (unJC a4) (unJC a5) (unJC a6)

liftA7 :: (Doc -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc) -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc
liftA7 f a1 a2 a3 a4 a5 a6 a7 = JC $ f (unJC a1) (unJC a2) (unJC a3) (unJC a4) (unJC a5) (unJC a6) (unJC a7)

liftList :: ([Doc] -> Doc) -> [JavaCode Doc] -> JavaCode Doc
liftList f as = JC $ f (map unJC as)

lift1List :: (Doc -> [Doc] -> Doc) -> JavaCode Doc -> [JavaCode Doc] -> JavaCode Doc
lift1List f a as = JC $ f (unJC a) (map unJC as)

lift2List :: (Doc -> Doc -> [Doc] -> Doc) -> JavaCode Doc -> JavaCode Doc -> [JavaCode Doc] -> JavaCode Doc
lift2List f a1 a2 as = JC $ f (unJC a1) (unJC a2) (map unJC as)

unJCPair :: (JavaCode Doc, JavaCode Doc) -> (Doc, Doc)
unJCPair (a1, a2) = (unJC a1, unJC a2) 

lift4Pair :: (Doc -> Doc -> Doc -> Doc -> [(Doc, Doc)] -> Doc) -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> [(JavaCode Doc, JavaCode Doc)] -> JavaCode Doc
lift4Pair f a1 a2 a3 a4 as = JC $ f (unJC a1) (unJC a2) (unJC a3) (unJC a4) (map unJCPair as)

lift3Pair :: (Doc -> Doc -> Doc -> [(Doc, Doc)] -> Doc) -> JavaCode Doc -> JavaCode Doc -> JavaCode Doc -> [(JavaCode Doc, JavaCode Doc)] -> JavaCode Doc
lift3Pair f a1 a2 a3 as = JC $ f (unJC a1) (unJC a2) (unJC a3) (map unJCPair as)


instance RenderSym JavaCode where
    type RenderFile JavaCode = Doc
    fileDoc code = liftA3 fileDoc' top code bottom
    top = liftA3 jtop endStatement include (list static)
    -- codeBody m = m -- don't need right now
    bottom = return empty

instance KeywordSym JavaCode where
    type Keyword JavaCode = Doc
    endStatement = return semi
    endStatementLoop = return empty
    include = return $ includeD "import"
    list _ = return $ text "ArrayList"
    argsList = return $ text "args"
    listObj = return new
    blockStart = return lbrace
    blockEnd = return rbrace
    ifBodyStart = blockStart
    elseIf = return $ text "else if"
    iterForEachLabel = return forLabel
    iterInLabel = return colon
    commentStart = return doubleSlash
    inputFunc = return (parens (text "new Scanner(System.in)"))
    
    printFunc = return $ text "System.out.print"
    printLnFunc = return $ text "System.out.println"
    printFileFunc f = liftA jPrintFileFunc f
    printFileLnFunc f = liftA jPrintFileLnFunc f

instance InputTypeSym JavaCode where
    type InputType JavaCode = Doc
    inputInt = return $ text "nextInt()"
    inputFloat = return $ text "nextDouble()"
    inputBool = return $ text "nextBoolean()"
    inputString = return $ text "nextLine()"

instance PermanenceSym JavaCode where
    type Permanence JavaCode = Doc
    static = return staticDocD
    dynamic = return dynamicDocD

instance ProgramBodySym JavaCode where
    type ProgramBody JavaCode = Doc
    prog cs = liftList progDocD cs

instance BodySym JavaCode where
    type Body JavaCode = Doc
    body bs = liftList bodyDocD bs
    bodyStatements sts = block sts
    oneLiner s = bodyStatements [s]

instance BlockSym JavaCode where
    type Block JavaCode = Doc
    block sts = do
        end <- endStatement
        liftList (blockDocD end) (map state sts)
    
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
    type StateType JavaCode = Doc
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

instance ControlSym JavaCode where
    type Control JavaCode = Doc
    ifCond bs b = lift4Pair ifCondDocD ifBodyStart elseIf blockEnd b bs
    switch v cs c = lift3Pair switchDocD (state break) v c cs

    for sInit vGuard sUpdate b = liftA6 forDocD blockStart blockEnd (loopState sInit) vGuard (loopState sUpdate) b
    forEach l t v b = liftA7 (forEachDocD l) blockStart blockEnd iterForEachLabel iterInLabel t v b
    while v b = liftA4 whileDocD blockStart blockEnd v b

    tryCatch tb cb = liftA2 jTryCatch tb cb

    checkState l cs c = switch (var l) cs c
    runStrategy l strats rv av = 
        case Map.lookup l (Map.fromList strats) of Nothing -> error $ "Strategy '" ++ l ++ "': RunStrategy called on non-existent strategy."
                                                   Just b  -> liftA2 stratDocD b (state resultState)
        where resultState = case av of Nothing    -> return empty
                                       Just vari  -> case rv of Nothing  -> error $ "Strategy '" ++ l ++ "': Attempt to assign null return to a Value."
                                                                Just res -> assign vari res

    statement s = state s
    statements s = block s

instance UnaryOpSym JavaCode where
    type UnaryOp JavaCode = Doc
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
    floorOp = return $ text "Math.floor"
    ceilOp = return $ text "Math.ceil"

instance BinaryOpSym JavaCode where
    type BinaryOp JavaCode = Doc
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
    type Value JavaCode = Doc
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
    defaultBool = litFalse

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
    floor v = liftA2 unOpDocD floorOp v
    ceil v = liftA2 unOpDocD ceilOp v

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

instance IOStSym JavaCode where
    type IOSt JavaCode = Doc
    out prf v = liftA2 outDocD prf v

instance PreStatementSym JavaCode where
    type PreStatement JavaCode = Doc
    assign v1 v2 = liftA2 assignDocD v1 v2
    (&=) v1 v2 = assign v1 v2
    (&.=) l v = assign (var l) v
    (&=.) v l = assign v (var l)
    (&-=) v1 v2 = v1 &= (v1 #- v2)
    (&.-=) l v = l &.= (var l #- v)
    (&+=) v1 v2 = liftA2 plusEqualsDocD v1 v2
    (&.+=) l v = (var l) &+= v
    (&++) v = liftA plusPlusDocD v
    (&.++) l = (&++) (var l)
    (&~-) v = v &= (v #- (litInt 1))
    (&.~-) l = (&~-) (var l)

    varDec l st = liftA (varDecDocD l) st
    varDecDef l st v = liftA2 (varDecDefDocD l) st v
    listDec l n st = liftA2 (listDecDocD l) (litInt n) st -- this means that the type you declare must already be a list. Not sure how I feel about this. On the bright side, it also means you don't need to pass permanence
    listDecDef l st vs = lift1List (jListDecDef l) st vs
    objDecDef l st v = liftA2 (objDecDefDocD l) st v
    constDecDef l st v = liftA2 (jConstDecDef l) st v

    print _ v = out printFunc v
    printLn _ v = out printLnFunc v
    printStr s = out printFunc (litString s)
    printStrLn s = out printLnFunc (litString s)

    printFile f _ v = out (printFileFunc f) v
    printFileLn f _ v = out (printFileLnFunc f) v
    printFileStr f s = out (printFileFunc f) (litString s)
    printFileStrLn f s = out (printFileLnFunc f) (litString s)

    printList t v = liftA3 printListDocD (state (printStr "[")) (forEach "listElem" t v (bodyStatements [print t (var "listElem"), printStr ","])) (printStr "]")
    printLnList t v = liftA3 printListDocD (state (printStr "[")) (forEach "listElem" t v (bodyStatements [print t (var "listElem"), printStr ","])) (printStrLn "]")
    printFileList f t v = liftA3 printListDocD (state (printFileStr f "[")) (forEach "listElem" t v (bodyStatements [printFile f t (var "listElem"), printFileStr f ","])) (printFileStr f "]")
    printFileLnList f t v = liftA3 printListDocD (state (printFileStr f "[")) (forEach "listElem" t v (bodyStatements [printFile f t (var "listElem"), printFileStr f ","])) (printFileStrLn f "]")

    getInput it v = liftA3 jInput it v inputFunc
    discardInput _ = liftA jDiscardInput inputFunc
    getFileInput f it v = liftA3 jInput it v f
    discardFileInput f = liftA jDiscardInput f

    openFileR f n = liftA2 jOpenFileR f n
    openFileW f n = liftA2 jOpenFileW f n
    ---closeFile f = liftA closeFileDocD f  -- needs objAccess

    -- getFileInputLine f v =  -- need Func
    -- discardFileLine f =
    -- getFileInputAll f v =
    -- listSlice v1 v2 b e s
    -- stringSplit v1 v2 d

    break = return breakDocD  -- I could have a JumpSym class with functions for "return $ text "break" and then reference those functions here?
    continue = return continueDocD

    returnState v = liftA returnDocD v
    returnVar l = liftA returnDocD (var l)

    valState v = v

    comment cmt = liftA (commentDocD cmt) commentStart

    free _ = error "Cannot free variables in Java" -- could set variable to null?

    throw errMsg = liftA jThrowDoc (litString errMsg)

    initState fsmName initialState = varDecDef fsmName string (litString initialState)
    changeState fsmName toState = fsmName &.= (litString toState)

instance StatementSym JavaCode where
    type Statement JavaCode = Doc
    ioState = state -- Now that types are Doc synonyms, I think this will work

    state s = liftA2 statementDocD s endStatement
    loopState s = liftA2 statementDocD s endStatementLoop

instance Selector JavaCode where
    objAccess v f = liftA2 objAccessDocD v f
    ($.) v f = objAccess v f

    selfAccess = funcApp
    castObj f v = liftA2 castObjDocD f v
    castStrToFloat v = funcApp "Double.parseDouble" [v]

instance FunctionSym JavaCode where
    type Function JavaCode = Doc
    func l vs = liftA funcDocD (funcApp l vs)
    cast targT _ = liftA castDocD targT
    castListToInt = liftA funcDocD (funcApp "ordinal" [])
    get n = liftA funcDocD (funcApp (getterName n) [])
    set n v = liftA funcDocD (funcApp (setterName n) [v])

    indexOf v = liftA funcDocD (funcApp "indexOf" [v])

    listSize = liftA funcDocD (funcApp "size" [])
    listAdd i v = liftA funcDocD (funcApp "add" [i, v])
    listPopulate _ _ = return empty
    listAppend v = liftA funcDocD (funcApp "add" [v])
    listExtendInt = liftA jListExtend defaultInt 
    listExtendFloat = liftA jListExtend defaultFloat 
    listExtendChar = liftA jListExtend defaultChar 
    listExtendBool = liftA jListExtend defaultBool
    listExtendString = liftA jListExtend defaultString
    listExtendList t = liftA jListExtendList t


    iterBegin = liftA funcDocD (funcApp "begin" [])
    iterEnd = liftA funcDocD (funcApp "end" [])

instance SelectorFunction JavaCode where
    listAccess i = liftA funcDocD (funcApp "get" [i])
    listSet i v = liftA funcDocD (funcApp "set" [i, v])

    listAccessEnum t v = listAccess (v $. (cast int t))
    listSetEnum t i v = listSet (i $. (cast int t)) v

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

jListDecDef :: Label -> Doc -> [Doc] -> Doc
jListDecDef l st vs = st <+> text l <+> equals <+> new <+> st <+> parens (listElements)
    where listElements = if null vs then empty else text "Arrays.asList" <> parens (callFuncParamList vs)

jConstDecDef :: Label -> Doc -> Doc -> Doc
jConstDecDef l st v = text "final" <+> st <+> text l <+> equals <+> v

jThrowDoc :: Doc -> Doc
jThrowDoc errMsg = text "throw new" <+> text "Exception" <> parens errMsg

jTryCatch :: Doc -> Doc -> Doc
jTryCatch tb cb= vcat [
    text "try" <+> lbrace,
    oneTab $ tb,
    rbrace <+> text "catch" <+> parens (text "Exception" <+> text "exc") <+> lbrace,
    oneTab $ cb,
    rbrace]

jDiscardInput :: Doc -> Doc
jDiscardInput inFn = inFn <> dot <> text "next()"

jInput :: Doc -> Doc -> Doc -> Doc
jInput it v inFn = v <+> equals <+> parens (inFn <> dot <> it) -- Changed from original GOOL, original GOOL was wrong.

jOpenFileR :: Doc -> Doc -> Doc
jOpenFileR f n = f <+> equals <+> new <+> text "Scanner" <> parens (new <+> text "File" <> parens n)

jOpenFileW :: Doc -> Doc -> Doc
jOpenFileW f n = f <+> equals <+> new <+> text "PrintWriter" <> parens n

jListExtend :: Doc -> Doc
jListExtend v = dot <> text "add" <> parens v

jListExtendList :: Doc -> Doc
jListExtendList t = new <+> t <> parens (empty)