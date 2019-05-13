{-# LANGUAGE TypeFamilies #-}

module LanguageRenderer.NewJavaRenderer (
    -- * Java Code Configuration -- defines syntax of all Java code
    JavaCode(..)
) where

import New (Label,
    PackageSym(..), RenderSym(..), KeywordSym(..), PermanenceSym(..),
    BodySym(..), BlockSym(..), ControlBlockSym(..), StateTypeSym(..),
    UnaryOpSym(..), BinaryOpSym(..), ValueSym(..), 
    NumericExpression(..), BooleanExpression(..), ValueExpression(..), 
    Selector(..), FunctionSym(..), SelectorFunction(..), StatementSym(..), 
    ControlStatementSym(..), ScopeSym(..), MethodTypeSym(..), ParameterSym(..),
    MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..))
import NewLanguageRenderer (packageDocD, 
    fileDoc', moduleDocD, classDocD, enumDocD, enumElementsDocD, 
    multiStateDocD, blockDocD, bodyDocD, outDocD, printFileDocD, boolTypeDocD,
    intTypeDocD, charTypeDocD, typeDocD, listTypeDocD, voidDocD, constructDocD, 
    stateParamDocD, paramListDocD, methodListDocD, stateVarDocD, 
    stateVarListDocD, ifCondDocD, switchDocD, forDocD, forEachDocD, whileDocD, 
    stratDocD, assignDocD, plusEqualsDocD, plusPlusDocD, varDecDocD,
    varDecDefDocD, listDecDocD, objDecDefDocD, statementDocD, returnDocD,
    commentDocD, notOpDocD, negateOpDocD, unOpDocD, equalOpDocD, 
    notEqualOpDocD, greaterOpDocD, greaterEqualOpDocD, lessOpDocD, 
    lessEqualOpDocD, plusOpDocD, minusOpDocD, multOpDocD, divideOpDocD, 
    moduloOpDocD, andOpDocD, orOpDocD, binOpDocD, binOpDocD', litTrueD, 
    litFalseD, litCharD, litFloatD, litIntD, litStringD, defaultCharD, 
    defaultFloatD, defaultIntD, defaultStringD, varDocD, extVarDocD, selfDocD, 
    argDocD, enumElemDocD, objVarDocD, inlineIfDocD, funcAppDocD, 
    extFuncAppDocD, stateObjDocD, listStateObjDocD, notNullDocD, funcDocD, 
    castDocD, objAccessDocD, castObjDocD, breakDocD, continueDocD, staticDocD, 
    dynamicDocD, privateDocD, publicDocD, dot, new, forLabel, observerListName,
    doubleSlash, addCommentsDocD, callFuncParamList, getterName, setterName,
    setMain, statementsToStateVars)
import Helpers (angles,oneTab,tripFst,tripSnd,tripThird)

import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import qualified Data.Map as Map (fromList,lookup)
import Control.Applicative (Applicative, liftA2, liftA3)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), parens, empty, equals,
    semi, vcat, lbrace, rbrace, render, colon, comma, isEmpty, render)

newtype JavaCode a = JC {unJC :: a}

instance Functor JavaCode where
    fmap f (JC x) = JC (f x)

instance Applicative JavaCode where
    pure = JC
    (JC f) <*> (JC x) = JC (f x)

instance Monad JavaCode where
    return = JC
    JC x >>= f = f x

liftA4 :: (a -> b -> c -> d -> e) -> JavaCode a -> JavaCode b -> JavaCode c -> JavaCode d -> JavaCode e
liftA4 f a1 a2 a3 a4 = JC $ f (unJC a1) (unJC a2) (unJC a3) (unJC a4)

liftA5 :: (a -> b -> c -> d -> e -> f) -> JavaCode a -> JavaCode b -> JavaCode c -> JavaCode d -> JavaCode e -> JavaCode f
liftA5 f a1 a2 a3 a4 a5 = JC $ f (unJC a1) (unJC a2) (unJC a3) (unJC a4) (unJC a5)

liftA6 :: (a -> b -> c -> d -> e -> f -> g) -> JavaCode a -> JavaCode b -> JavaCode c -> JavaCode d -> JavaCode e -> JavaCode f -> JavaCode g
liftA6 f a1 a2 a3 a4 a5 a6 = JC $ f (unJC a1) (unJC a2) (unJC a3) (unJC a4) (unJC a5) (unJC a6)

liftA7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> JavaCode a -> JavaCode b -> JavaCode c -> JavaCode d -> JavaCode e -> JavaCode f -> JavaCode g -> JavaCode h
liftA7 f a1 a2 a3 a4 a5 a6 a7 = JC $ f (unJC a1) (unJC a2) (unJC a3) (unJC a4) (unJC a5) (unJC a6) (unJC a7)

liftList :: ([a] -> b) -> [JavaCode a] -> JavaCode b
liftList f as = JC $ f (map unJC as)

lift1List :: (a -> [b] -> c) -> JavaCode a -> [JavaCode b] -> JavaCode c
lift1List f a as = JC $ f (unJC a) (map unJC as)

unJCPair :: (JavaCode a, JavaCode b) -> (a, b)
unJCPair (a1, a2) = (unJC a1, unJC a2) 

lift4Pair :: (a -> b -> c -> d -> [(e, f)] -> g) -> JavaCode a -> JavaCode b -> JavaCode c -> JavaCode d -> [(JavaCode e, JavaCode f)] -> JavaCode g
lift4Pair f a1 a2 a3 a4 as = JC $ f (unJC a1) (unJC a2) (unJC a3) (unJC a4) (map unJCPair as)

lift3Pair :: (a -> b -> c -> [(d, e)] -> f) -> JavaCode a -> JavaCode b -> JavaCode c -> [(JavaCode d, JavaCode e)] -> JavaCode f
lift3Pair f a1 a2 a3 as = JC $ f (unJC a1) (unJC a2) (unJC a3) (map unJCPair as)

liftPairFst :: (JavaCode a, b) -> JavaCode (a, b)
liftPairFst (c, n) = JC $ (unJC c, n)

liftTripFst :: (JavaCode a, b, c) -> JavaCode (a, b, c)
liftTripFst (c, n, b) = JC $ (unJC c, n, b)

instance PackageSym JavaCode where
    type Package JavaCode = ([(Doc, Label, Bool)], Label)
    packMods n ms = liftPairFst (mapM (liftA2 (packageDocD n) endStatement) ms, n)

instance RenderSym JavaCode where
    type RenderFile JavaCode = (Doc, Label, Bool)
    fileDoc code = liftTripFst (liftA3 fileDoc' (top code) (fmap tripFst code) bottom, tripSnd $ unJC code, tripThird $ unJC code)
    top _ = liftA3 jtop endStatement (include "") (list static)
    bottom = return empty

instance KeywordSym JavaCode where
    type Keyword JavaCode = Doc
    endStatement = return semi
    endStatementLoop = return empty

    include _ = return $ text "import"
    inherit = return $ text "extends"

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
    
    printFunc = return $ text "System.out.print"
    printLnFunc = return $ text "System.out.println"
    printFileFunc = fmap (printFileDocD "print")
    printFileLnFunc = fmap (printFileDocD "println")

instance PermanenceSym JavaCode where
    type Permanence JavaCode = Doc
    static = return staticDocD
    dynamic = return dynamicDocD

instance BodySym JavaCode where
    type Body JavaCode = Doc
    body = liftList bodyDocD
    bodyStatements = block
    oneLiner s = bodyStatements [s]

    addComments s = liftA2 (addCommentsDocD s) commentStart

instance BlockSym JavaCode where
    type Block JavaCode = Doc
    block sts = lift1List blockDocD endStatement (map (fmap fst) (map state sts))

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
    intListType p = fmap jIntListTypeDoc (list p)
    floatListType p = fmap jFloatListTypeDoc (list p)
    boolListType = return jBoolListTypeDocD
    obj t = return $ typeDocD t
    enumType t = return $ typeDocD t
    iterator _ = error "Iterator-type variables do not exist in Java"

instance ControlBlockSym JavaCode where
    runStrategy l strats rv av = 
        case Map.lookup l (Map.fromList strats) of Nothing -> error $ "Strategy '" ++ l ++ "': RunStrategy called on non-existent strategy."
                                                   Just b  -> liftA2 stratDocD b (state resultState)
        where resultState = case av of Nothing    -> return (empty, False)
                                       Just vari  -> case rv of Nothing  -> error $ "Strategy '" ++ l ++ "': Attempt to assign null return to a Value."
                                                                Just res -> assign vari res

    listSlice t vnew vold b e s = 
        let l_temp = "temp"
            v_temp = var l_temp
            l_i = "i_temp"
            v_i = var l_i
        in
        (body [
            block [(listDec l_temp 0 t),
            for (varDecDef l_i int (getB b)) (v_i ?< getE e) (getS s v_i)
                (oneLiner $ valState $ v_temp $. (listAppend (vold $. (listAccess v_i)))),
            (vnew &= v_temp)]])
        where getB Nothing = litInt 0
              getB (Just n) = n
              getE Nothing = vold $. listSize
              getE (Just n) = n
              getS Nothing v = (&++) v
              getS (Just n) v = v &+= n

    stringSplit d vnew s = block [liftPairFst (liftA3 jStringSplit vnew (listType dynamic string) 
        (funcApp "Arrays.asList" [s $. (func "split" [litString [d]])]), True)]

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
    asinOp = return $ text "Math.asin"
    acosOp = return $ text "Math.acos"
    atanOp = return $ text "Math.atan"
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

    ($->) = objVar
    ($:) = enumElement

    const = var
    var n = return $ varDocD n
    extVar l n = return $ extVarDocD l n
    self = return $ selfDocD
    arg n = liftA2 argDocD (litInt n) argsList
    enumElement en e = return $ enumElemDocD en e
    enumVar = var
    objVar = liftA2 objVarDocD
    objVarSelf = var
    listVar n _ = var n
    n `listOf` t = listVar n t
    iterVar = var
    
    inputFunc = return (parens (text "new Scanner(System.in)"))

    valName v = unJC $ fmap render v

instance NumericExpression JavaCode where
    (#~) = liftA2 unOpDocD negateOp
    (#/^) = liftA2 unOpDocD sqrtOp
    (#|) = liftA2 unOpDocD absOp
    (#+) = liftA3 binOpDocD plusOp
    (#-) = liftA3 binOpDocD minusOp
    (#*) = liftA3 binOpDocD multOp
    (#/) = liftA3 binOpDocD divideOp
    (#%) = liftA3 binOpDocD moduloOp
    (#^) = liftA3 binOpDocD' powerOp

    log = liftA2 unOpDocD logOp
    ln = liftA2 unOpDocD lnOp
    exp = liftA2 unOpDocD expOp
    sin = liftA2 unOpDocD sinOp
    cos = liftA2 unOpDocD cosOp
    tan = liftA2 unOpDocD tanOp
    csc v = (litFloat 1.0) #/ (sin v)
    sec v = (litFloat 1.0) #/ (cos v)
    cot v = (litFloat 1.0) #/ (tan v)
    arcsin = liftA2 unOpDocD asinOp
    arccos = liftA2 unOpDocD acosOp
    arctan = liftA2 unOpDocD atanOp
    floor = liftA2 unOpDocD floorOp
    ceil = liftA2 unOpDocD ceilOp

instance BooleanExpression JavaCode where
    (?!) = liftA2 unOpDocD notOp
    (?&&) = liftA3 binOpDocD andOp
    (?||) = liftA3 binOpDocD orOp

    (?<) = liftA3 binOpDocD lessOp
    (?<=) = liftA3 binOpDocD lessEqualOp
    (?>) = liftA3 binOpDocD greaterOp
    (?>=) = liftA3 binOpDocD greaterEqualOp
    (?==) = liftA3 binOpDocD equalOp
    (?!=) = liftA3 binOpDocD notEqualOp
    
instance ValueExpression JavaCode where
    inlineIf = liftA3 inlineIfDocD
    funcApp n = liftList (funcAppDocD n)
    selfFuncApp = funcApp
    extFuncApp l n = liftList (extFuncAppDocD l n)
    stateObj t vs = liftA2 stateObjDocD t (liftList callFuncParamList vs)
    extStateObj _ = stateObj
    listStateObj t vs = liftA3 listStateObjDocD listObj t (liftList callFuncParamList vs)

    exists = notNull
    notNull v = liftA3 notNullDocD notEqualOp v (var "null")

instance Selector JavaCode where
    objAccess = liftA2 objAccessDocD
    ($.) = objAccess

    objMethodCall o f ps = objAccess o (func f ps)
    objMethodCallVoid o f = objMethodCall o f []

    selfAccess = objAccess self

    listPopulateAccess _ _ = return empty
    listSizeAccess v = objAccess v listSize

    listIndexExists = liftA3 jListIndexExists greaterOp
    argExists i = objAccess argsList (listAccess (litInt $ fromIntegral i))

    indexOf l v = objAccess l (fmap funcDocD (funcApp "indexOf" [v]))

    stringEqual v1 str = objAccess v1 (func "equals" [str])

    castObj = liftA2 castObjDocD
    castStrToFloat v = funcApp "Double.parseDouble" [v]

instance FunctionSym JavaCode where
    type Function JavaCode = Doc
    func l vs = fmap funcDocD (funcApp l vs)
    cast targT _ = fmap castDocD targT
    castListToInt = fmap funcDocD (funcApp "ordinal" [])
    get n = fmap funcDocD (funcApp (getterName n) [])
    set n v = fmap funcDocD (funcApp (setterName n) [v])

    listSize = fmap funcDocD (funcApp "size" [])
    listAdd i v = fmap funcDocD (funcApp "add" [i, v])
    listPopulateInt _ = return empty
    listPopulateFloat _ = return empty
    listPopulateChar _ = return empty
    listPopulateBool _ = return empty
    listPopulateString _ = return empty
    listAppend v = fmap funcDocD (funcApp "add" [v])
    listExtendInt = fmap jListExtend defaultInt 
    listExtendFloat = fmap jListExtend defaultFloat 
    listExtendChar = fmap jListExtend defaultChar 
    listExtendBool = fmap jListExtend defaultBool
    listExtendString = fmap jListExtend defaultString
    listExtendList _ = fmap jListExtendList

    iterBegin = fmap funcDocD (funcApp "begin" [])
    iterEnd = fmap funcDocD (funcApp "end" [])

instance SelectorFunction JavaCode where
    listAccess i = fmap funcDocD (funcApp "get" [i])
    listSet i v = fmap funcDocD (funcApp "set" [i, v])

    listAccessEnum t v = listAccess (castObj (cast int t) v)
    listSetEnum t i = listSet (castObj (cast int t) i)

    at l = listAccess (var l)

instance StatementSym JavaCode where
    -- Bool determines whether the statement needs to end in a separator
    type Statement JavaCode = (Doc, Bool)
    assign v1 v2 = liftPairFst (liftA2 assignDocD v1 v2, True)
    assignToListIndex lst index v = valState $ lst $. listSet index v
    (&=) = assign
    (&.=) l = assign (var l)
    (&=.) v l = assign v (var l)
    (&-=) v1 v2 = v1 &= (v1 #- v2)
    (&.-=) l v = l &.= (var l #- v)
    (&+=) v1 v2 = liftPairFst (liftA2 plusEqualsDocD v1 v2, True)
    (&.+=) l v = (var l) &+= v
    (&++) v = liftPairFst (fmap plusPlusDocD v, True)
    (&.++) l = (&++) (var l)
    (&~-) v = v &= (v #- (litInt 1))
    (&.~-) l = (&~-) (var l)

    varDec l t = liftPairFst (fmap (varDecDocD l) t, True)
    varDecDef l t v = liftPairFst (liftA2 (varDecDefDocD l) t v, True)
    listDec l n t = liftPairFst (liftA2 (listDecDocD l) (litInt n) t, True) -- this means that the type you declare must already be a list. Not sure how I feel about this. On the bright side, it also means you don't need to pass permanence
    listDecDef l t vs = liftPairFst (liftA2 (jListDecDef l) t (liftList callFuncParamList vs), True)
    objDecDef l t v = liftPairFst (liftA2 (objDecDefDocD l) t v, True)
    objDecNew l t vs = liftPairFst (liftA2 (objDecDefDocD l) t (stateObj t vs), True)
    extObjDecNew l _ = objDecNew l
    objDecNewVoid l t = liftPairFst (liftA2 (objDecDefDocD l) t (stateObj t []), True)
    extObjDecNewVoid l _ = objDecNewVoid l
    constDecDef l t v = liftPairFst (liftA2 (jConstDecDef l) t v, True)

    print _ v = liftPairFst (liftA2 outDocD printFunc v, True)
    printLn _ v = liftPairFst (liftA2 outDocD printLnFunc v, True)
    printStr s = liftPairFst (liftA2 outDocD printFunc (litString s), True)
    printStrLn s = liftPairFst (liftA2 outDocD printLnFunc (litString s), True)

    printFile f _ v = liftPairFst (liftA2 outDocD (printFileFunc f) v, True)
    printFileLn f _ v = liftPairFst (liftA2 outDocD (printFileLnFunc f) v, True)
    printFileStr f s = liftPairFst (liftA2 outDocD (printFileFunc f) (litString s), True)
    printFileStrLn f s = liftPairFst (liftA2 outDocD (printFileLnFunc f) (litString s), True)

    printList t v = multi [(state (printStr "[")), (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. listSize) #- (litInt 1))) ((&.++) "i") (bodyStatements [print t (v $. (listAccess (var "i"))), printStr ","])), (state (print t (v $. (listAccess ((v $. listSize) #- (litInt 1)))))), (printStr "]")]
    printLnList t v = multi [(state (printStr "[")), (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. listSize) #- (litInt 1))) ((&.++) "i") (bodyStatements [print t (v $. (listAccess (var "i"))), printStr ","])), (state (print t (v $. (listAccess ((v $. listSize) #- (litInt 1)))))), (printStrLn "]")]
    printFileList f t v = multi [(state (printFileStr f "[")), (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. listSize) #- (litInt 1))) ((&.++) "i") (bodyStatements [printFile f t (v $. (listAccess (var "i"))), printFileStr f ","])), (state (printFile f t (v $. (listAccess ((v $. listSize) #- (litInt 1)))))), (printFileStr f "]")]
    printFileLnList f t v = multi [(state (printFileStr f "[")), (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. listSize) #- (litInt 1))) ((&.++) "i") (bodyStatements [printFile f t (v $. (listAccess (var "i"))), printFileStr f ","])), (state (printFile f t (v $. (listAccess ((v $. listSize) #- (litInt 1)))))), (printFileStrLn f "]")]

    getIntInput v = liftPairFst (liftA3 jInput' (return $ text "Integer.parseInt") v inputFunc, True)
    getFloatInput v = liftPairFst (liftA3 jInput' (return $ text "Double.parseDouble") v inputFunc, True)
    getBoolInput v = liftPairFst (liftA3 jInput (return $ text "nextBoolean()") v inputFunc, True)
    getStringInput v = liftPairFst (liftA3 jInput (return $ text "nextLine()") v inputFunc, True)
    getCharInput _ = return (empty, False)
    discardInput = liftPairFst (fmap jDiscardInput inputFunc, True)
    getIntFileInput f v = liftPairFst (liftA3 jInput' (return $ text "Integer.parseInt") v f, True)
    getFloatFileInput f v = liftPairFst (liftA3 jInput' (return $ text "Double.parseDouble") v f, True)
    getBoolFileInput f v = liftPairFst (liftA3 jInput (return $ text "nextBoolean()") v f, True)
    getStringFileInput f v = liftPairFst (liftA3 jInput (return $ text "nextLine()") v f, True)
    getCharFileInput _ _ = return (empty, False)
    discardFileInput f = liftPairFst (fmap jDiscardInput f, True)

    openFileR f n = liftPairFst (liftA2 jOpenFileR f n, True)
    openFileW f n = liftPairFst (liftA3 jOpenFileWorA f n litFalse, True)
    openFileA f n = liftPairFst (liftA3 jOpenFileWorA f n litTrue, True)
    closeFile f = valState $ objMethodCall f "close" []

    getFileInputLine f v = v &= (f $. (func "nextLine" []))
    discardFileLine f = valState $ f $. (func "nextLine" [])

    break = return (breakDocD, True)  -- I could have a JumpSym class with functions for "return $ text "break" and then reference those functions here?
    continue = return (continueDocD, True)

    returnState v = liftPairFst (fmap returnDocD v, True)
    returnVar l = liftPairFst (fmap returnDocD (var l), True)

    valState v = liftPairFst (v, True)

    comment cmt = liftPairFst (fmap (commentDocD cmt) commentStart, False)

    free _ = error "Cannot free variables in Java" -- could set variable to null? Might be misleading.

    throw errMsg = liftPairFst (fmap jThrowDoc (litString errMsg), True)

    initState fsmName initialState = varDecDef fsmName string (litString initialState)
    changeState fsmName toState = fsmName &.= (litString toState)

    initObserverList = listDecDef observerListName
    addObserver t o = valState $ obsList $. listAdd lastelem o
        where obsList = observerListName `listOf` t
              lastelem = obsList $. listSize

    state s = liftA2 statementDocD s endStatement
    loopState s = liftA2 statementDocD s endStatementLoop
    multi = lift1List multiStateDocD endStatement

instance ControlStatementSym JavaCode where
    ifCond bs b = liftPairFst (lift4Pair ifCondDocD ifBodyStart elseIf blockEnd b bs, False)
    ifNoElse bs = ifCond bs $ body []
    switch v cs c = liftPairFst (lift3Pair switchDocD (state break) v c cs, False)
    switchAsIf v cs = ifCond cases
        where cases = map (\(l, b) -> (v ?== l, b)) cs

    ifExists v ifBody = ifCond [(notNull v, ifBody)]

    for sInit vGuard sUpdate b = liftPairFst (liftA6 forDocD blockStart blockEnd (loopState sInit) vGuard (loopState sUpdate) b, False)
    forRange i initv finalv stepv = for (varDecDef i int initv) ((var i) ?< finalv) (i &.+= stepv)
    forEach l t v b = liftPairFst (liftA7 (forEachDocD l) blockStart blockEnd iterForEachLabel iterInLabel t v b, False)
    while v b = liftPairFst (liftA4 whileDocD blockStart blockEnd v b, False)

    tryCatch tb cb = liftPairFst (liftA2 jTryCatch tb cb, False)
    
    checkState l = switch (var l)
    notifyObservers fn t ps = for initv (var index ?< (obsList $. listSize)) ((&.++) index) notify
        where obsList = observerListName `listOf` t
              index = "observerIndex"
              initv = varDecDef index int $ litInt 0
              notify = oneLiner $ valState $ (obsList $. at index) $. func fn ps

    getFileInputAll f v = while (f $. (func "hasNextLine" []))
        (oneLiner $ valState $ v $. (listAppend $ f $. (func "nextLine" [])))

instance ScopeSym JavaCode where
    type Scope JavaCode = Doc
    private = return privateDocD
    public = return publicDocD

    includeScope s = s

instance MethodTypeSym JavaCode where
    type MethodType JavaCode = Doc
    mState t = t
    void = return voidDocD
    construct n = return $ constructDocD n

instance ParameterSym JavaCode where
    type Parameter JavaCode = Doc
    stateParam n = fmap (stateParamDocD n)
    pointerParam = stateParam

instance MethodSym JavaCode where
    -- Bool is True if the method is a main method, False otherwise
    type Method JavaCode = (Doc, Bool)
    method n s p t ps b = liftPairFst (liftA5 (jMethod n) s p t (liftList paramListDocD ps) b, False)
    getMethod n t = method (getterName n) public dynamic t [] getBody
        where getBody = oneLiner $ returnState (self $-> (var n))
    setMethod setLbl paramLbl t = method (setterName setLbl) public dynamic void [(stateParam paramLbl t)] setBody
        where setBody = oneLiner $ (self $-> (var setLbl)) &=. paramLbl
    mainMethod b = fmap setMain $ method "main" public static void [return $ text "String[] args"] b
    privMethod n = method n private dynamic
    pubMethod n = method n public dynamic
    constructor n = method n public dynamic (construct n)
    destructor _ _ = error "Destructors not allowed in Java"


    function = method

instance StateVarSym JavaCode where
    type StateVar JavaCode = Doc
    stateVar _ l s p t = liftA4 (stateVarDocD l) (includeScope s) p t endStatement
    privMVar del l = stateVar del l private dynamic
    pubMVar del l = stateVar del l public dynamic
    pubGVar del l = stateVar del l public static
    listStateVar = stateVar

instance ClassSym JavaCode where
    -- Bool is True if the method is a main method, False otherwise
    type Class JavaCode = (Doc, Bool)
    buildClass n p s vs fs = liftPairFst (liftA4 (classDocD n p) inherit s (liftList stateVarListDocD vs) (liftList methodListDocD fs), any (snd . unJC) fs)
    enum n es s = liftPairFst (liftA2 (enumDocD n) (return $ enumElementsDocD es False) s, False)
    mainClass n vs fs = fmap setMain $ buildClass n Nothing public vs fs
    privClass n p = buildClass n p private
    pubClass n p = buildClass n p public

instance ModuleSym JavaCode where
    -- Label is module name
    -- Bool is True if the method is a main method, False otherwise
    type Module JavaCode = (Doc, Label, Bool)
    buildModule n _ vs ms cs = 
        case null vs && null ms of True -> liftTripFst (liftList moduleDocD cs, n, any (snd . unJC) cs) 
                                   _  -> liftTripFst (liftList moduleDocD ((pubClass n 
                                        Nothing (map (liftA4 statementsToStateVars
                                        public static endStatement) vs) ms):cs), n, or [any (snd . unJC) ms, any (snd . unJC) cs])

jtop :: Doc -> Doc -> Doc -> Doc
jtop end inc lst = vcat [
    inc <+> text "java.util.Arrays" <> end,
    inc <+> text "java.util.BitSet" <> end,     --TODO: only include these if they are used in the code?
    inc <+> text "java.util.Scanner" <> end,
    inc <+> text "java.io.PrintWriter" <> end,
    inc <+> text "java.io.FileWriter" <> end,
    inc <+> text "java.io.File" <> end,
    inc <+> text ("java.util." ++ render lst) <> end]

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

jBoolListTypeDocD :: Doc
jBoolListTypeDocD = text "BitSet"

jListDecDef :: Label -> Doc -> Doc -> Doc
jListDecDef l st vs = st <+> text l <+> equals <+> new <+> st <+> parens listElements
    where listElements = if isEmpty vs then empty else text "Arrays.asList" <> parens vs

jConstDecDef :: Label -> Doc -> Doc -> Doc
jConstDecDef l st v = text "final" <+> st <+> text l <+> equals <+> v

jThrowDoc :: Doc -> Doc
jThrowDoc errMsg = text "throw new" <+> text "Exception" <> parens errMsg

jTryCatch :: Doc -> Doc -> Doc
jTryCatch tb cb = vcat [
    text "try" <+> lbrace,
    oneTab $ tb,
    rbrace <+> text "catch" <+> parens (text "Exception" <+> text "exc") <+> lbrace,
    oneTab $ cb,
    rbrace]

jDiscardInput :: Doc -> Doc
jDiscardInput inFn = inFn <> dot <> text "next()"

jInput :: Doc -> Doc -> Doc -> Doc
jInput it v inFn = v <+> equals <+> parens (inFn <> dot <> it) -- Changed from original GOOL, original GOOL was wrong.

jInput' :: Doc -> Doc -> Doc -> Doc
jInput' it v inFn = v <+> equals <+> it <> parens (inFn <> dot <> text "nextLine()")

jOpenFileR :: Doc -> Doc -> Doc
jOpenFileR f n = f <+> equals <+> new <+> text "Scanner" <> parens (new <+> text "File" <> parens n)

jOpenFileWorA :: Doc -> Doc -> Doc -> Doc
jOpenFileWorA f n wa = f <+> equals <+> new <+> text "PrintWriter" <> parens (new <+> text "FileWriter" <> parens (new <+> text "File" <> parens n <> comma <+> wa))

jListExtend :: Doc -> Doc
jListExtend v = dot <> text "add" <> parens v

jListExtendList :: Doc -> Doc
jListExtendList t = dot <> text "add" <> parens (new <+> t <> parens empty)

jStringSplit :: Doc -> Doc -> Doc -> Doc
jStringSplit vnew t s = vnew <+> equals <+> new <+> t
    <> parens s

jMethod :: Label -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc
jMethod n s p t ps b = vcat [
    s <+> p <+> t <+> text n <> parens ps <+> text "throws Exception" <+> lbrace,
    oneTab $ b,
    rbrace]

jListIndexExists :: Doc -> Doc -> Doc -> Doc
jListIndexExists greater lst index = parens (lst <> text ".length" <+> greater <+> index)
    