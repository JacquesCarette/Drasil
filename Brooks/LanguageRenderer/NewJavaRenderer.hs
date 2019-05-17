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
import NewLanguageRenderer (Terminator(..), packageDocD, 
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
    setMain, setEmpty, statementsToStateVars)
import Helpers (angles, oneTab, tripFst, tripSnd, tripThird, liftA4, liftA5,
    liftA6, liftA7, liftList, lift1List, lift3Pair, lift4Pair, 
    liftPairFst, liftTripFst)

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
    boolListType = listType dynamic bool
    obj t = return $ typeDocD t
    enumType t = return $ typeDocD t
    iterator _ = error "Iterator-type variables do not exist in Java"

instance ControlBlockSym JavaCode where
    runStrategy l strats rv av = 
        case Map.lookup l (Map.fromList strats) of Nothing -> error $ "Strategy '" ++ l ++ "': RunStrategy called on non-existent strategy."
                                                   Just b  -> liftA2 stratDocD b (state resultState)
        where resultState = case av of Nothing    -> return (empty, Empty)
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
    -- Maybe String is the String representation of the value
    type Value JavaCode = (Doc, Maybe String)
    litTrue = return $ (litTrueD, Just "true")
    litFalse = return $ (litFalseD, Just "false")
    litChar c = return $ (litCharD c, Just $ "\'" ++ [c] ++ "\'")
    litFloat v = return $ (litFloatD v, Just $ show v)
    litInt v = return $ (litIntD v, Just $ show v)
    litString s = return $ (litStringD s, Just $ "\"" ++ s ++ "\"")

    defaultChar = return $ (defaultCharD, Just "space character")
    defaultFloat = return $ (defaultFloatD, Just "0.0")
    defaultInt = return $ (defaultIntD, Just "0")
    defaultString = return $ (defaultStringD, Just "empty string")
    defaultBool = litFalse

    ($->) = objVar
    ($:) = enumElement

    const = var
    var n = return $ (varDocD n, Just n)
    extVar l n = return $ (extVarDocD l n, Just $ l ++ "." ++ n)
    self = return $ (selfDocD, Just "this")
    arg n = liftPairFst (liftA2 argDocD (litInt n) argsList, Nothing)
    enumElement en e = return $ (enumElemDocD en e, Just $ en ++ "." ++ e)
    enumVar = var
    objVar o v = liftPairFst (liftA2 objVarDocD o v, Just $ valName o ++ "." ++ valName v)
    objVarSelf = var
    listVar n _ = var n
    n `listOf` t = listVar n t
    iterVar = var
    
    inputFunc = return (parens (text "new Scanner(System.in)"), Nothing)
    argsList = return $ (text "args", Nothing)

    valName (JC (v, s)) = case s of Nothing -> error $ "Attempt to print unprintable Value (" ++ render v ++ ")"
                                    Just valstr -> valstr

instance NumericExpression JavaCode where
    (#~) v = liftPairFst (liftA2 unOpDocD negateOp v, Nothing)
    (#/^) v = liftPairFst (liftA2 unOpDocD sqrtOp v, Nothing)
    (#|) v = liftPairFst (liftA2 unOpDocD absOp v, Nothing)
    (#+) v1 v2 = liftPairFst (liftA3 binOpDocD plusOp v1 v2, Nothing)
    (#-) v1 v2 = liftPairFst (liftA3 binOpDocD minusOp v1 v2, Nothing)
    (#*) v1 v2 = liftPairFst (liftA3 binOpDocD multOp v1 v2, Nothing)
    (#/) v1 v2 = liftPairFst (liftA3 binOpDocD divideOp v1 v2, Nothing)
    (#%) v1 v2 = liftPairFst (liftA3 binOpDocD moduloOp v1 v2, Nothing)
    (#^) v1 v2 = liftPairFst (liftA3 binOpDocD' powerOp v1 v2, Nothing)

    log v = liftPairFst (liftA2 unOpDocD logOp v, Nothing)
    ln v = liftPairFst (liftA2 unOpDocD lnOp v, Nothing)
    exp v = liftPairFst (liftA2 unOpDocD expOp v, Nothing)
    sin v = liftPairFst (liftA2 unOpDocD sinOp v, Nothing)
    cos v = liftPairFst (liftA2 unOpDocD cosOp v, Nothing)
    tan v = liftPairFst (liftA2 unOpDocD tanOp v, Nothing)
    csc v = (litFloat 1.0) #/ (sin v)
    sec v = (litFloat 1.0) #/ (cos v)
    cot v = (litFloat 1.0) #/ (tan v)
    arcsin v = liftPairFst (liftA2 unOpDocD asinOp v, Nothing)
    arccos v = liftPairFst (liftA2 unOpDocD acosOp v, Nothing)
    arctan v = liftPairFst (liftA2 unOpDocD atanOp v, Nothing)
    floor v = liftPairFst (liftA2 unOpDocD floorOp v, Nothing)
    ceil v = liftPairFst (liftA2 unOpDocD ceilOp v, Nothing)

instance BooleanExpression JavaCode where
    (?!) v = liftPairFst (liftA2 unOpDocD notOp v, Nothing)
    (?&&) v1 v2 = liftPairFst (liftA3 binOpDocD andOp v1 v2, Nothing)
    (?||) v1 v2 = liftPairFst (liftA3 binOpDocD orOp v1 v2, Nothing)

    (?<) v1 v2 = liftPairFst (liftA3 binOpDocD lessOp v1 v2, Nothing)
    (?<=) v1 v2 = liftPairFst (liftA3 binOpDocD lessEqualOp v1 v2, Nothing)
    (?>) v1 v2 = liftPairFst (liftA3 binOpDocD greaterOp v1 v2, Nothing)
    (?>=) v1 v2 = liftPairFst (liftA3 binOpDocD greaterEqualOp v1 v2, Nothing)
    (?==) v1 v2 = liftPairFst (liftA3 binOpDocD equalOp v1 v2, Nothing)
    (?!=) v1 v2 = liftPairFst (liftA3 binOpDocD notEqualOp v1 v2, Nothing)
    
instance ValueExpression JavaCode where
    inlineIf b v1 v2 = liftPairFst (liftA3 inlineIfDocD b v1 v2, Nothing)
    funcApp n vs = liftPairFst (liftList (funcAppDocD n) vs, Nothing)
    selfFuncApp = funcApp
    extFuncApp l n vs = liftPairFst (liftList (extFuncAppDocD l n) vs, Nothing)
    stateObj t vs = liftPairFst (liftA2 stateObjDocD t (liftList callFuncParamList vs), Nothing)
    extStateObj _ = stateObj
    listStateObj t vs = liftPairFst (liftA3 listStateObjDocD listObj t (liftList callFuncParamList vs), Nothing)

    exists = notNull
    notNull v = liftPairFst (liftA3 notNullDocD notEqualOp v (var "null"), Nothing)

instance Selector JavaCode where
    objAccess v f = liftPairFst (liftA2 objAccessDocD v f, Nothing)
    ($.) = objAccess

    objMethodCall o f ps = objAccess o (func f ps)
    objMethodCallVoid o f = objMethodCall o f []

    selfAccess = objAccess self

    listPopulateAccess _ _ = return (empty, Nothing)
    listSizeAccess v = objAccess v listSize

    listIndexExists l i = liftPairFst (liftA3 jListIndexExists greaterOp l i, Nothing)
    argExists i = objAccess argsList (listAccess (litInt $ fromIntegral i))

    indexOf l v = objAccess l (fmap funcDocD (funcApp "indexOf" [v]))

    stringEqual v1 str = objAccess v1 (func "equals" [str])

    castObj f v = liftPairFst (liftA2 castObjDocD f v, Nothing)
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
    -- Terminator determines how statements end
    type Statement JavaCode = (Doc, Terminator)
    assign v1 v2 = liftPairFst (liftA2 assignDocD v1 v2, Semi)
    assignToListIndex lst index v = valState $ lst $. listSet index v
    (&=) = assign
    (&.=) l = assign (var l)
    (&=.) v l = assign v (var l)
    (&-=) v1 v2 = v1 &= (v1 #- v2)
    (&.-=) l v = l &.= (var l #- v)
    (&+=) v1 v2 = liftPairFst (liftA2 plusEqualsDocD v1 v2, Semi)
    (&.+=) l v = (var l) &+= v
    (&++) v = liftPairFst (fmap plusPlusDocD v, Semi)
    (&.++) l = (&++) (var l)
    (&~-) v = v &= (v #- (litInt 1))
    (&.~-) l = (&~-) (var l)

    varDec l t = liftPairFst (fmap (varDecDocD l) t, Semi)
    varDecDef l t v = liftPairFst (liftA2 (varDecDefDocD l) t v, Semi)
    listDec l n t = liftPairFst (liftA2 (listDecDocD l) (litInt n) t, Semi) -- this means that the type you declare must already be a list. Not sure how I feel about this. On the bright side, it also means you don't need to pass permanence
    listDecDef l t vs = liftPairFst (liftA2 (jListDecDef l) t (liftList callFuncParamList vs), Semi)
    objDecDef l t v = liftPairFst (liftA2 (objDecDefDocD l) t v, Semi)
    objDecNew l t vs = liftPairFst (liftA2 (objDecDefDocD l) t (stateObj t vs), Semi)
    extObjDecNew l _ = objDecNew l
    objDecNewVoid l t = liftPairFst (liftA2 (objDecDefDocD l) t (stateObj t []), Semi)
    extObjDecNewVoid l _ = objDecNewVoid l
    constDecDef l t v = liftPairFst (liftA2 (jConstDecDef l) t v, Semi)

    print _ v = liftPairFst (liftA2 outDocD printFunc v, Semi)
    printLn _ v = liftPairFst (liftA2 outDocD printLnFunc v, Semi)
    printStr s = liftPairFst (liftA2 outDocD printFunc (litString s), Semi)
    printStrLn s = liftPairFst (liftA2 outDocD printLnFunc (litString s), Semi)

    printFile f _ v = liftPairFst (liftA2 outDocD (printFileFunc f) v, Semi)
    printFileLn f _ v = liftPairFst (liftA2 outDocD (printFileLnFunc f) v, Semi)
    printFileStr f s = liftPairFst (liftA2 outDocD (printFileFunc f) (litString s), Semi)
    printFileStrLn f s = liftPairFst (liftA2 outDocD (printFileLnFunc f) (litString s), Semi)

    printList t v = multi [(state (printStr "[")), (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. listSize) #- (litInt 1))) ((&.++) "i") (bodyStatements [print t (v $. (listAccess (var "i"))), printStr ","])), (state (print t (v $. (listAccess ((v $. listSize) #- (litInt 1)))))), (printStr "]")]
    printLnList t v = multi [(state (printStr "[")), (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. listSize) #- (litInt 1))) ((&.++) "i") (bodyStatements [print t (v $. (listAccess (var "i"))), printStr ","])), (state (print t (v $. (listAccess ((v $. listSize) #- (litInt 1)))))), (printStrLn "]")]
    printFileList f t v = multi [(state (printFileStr f "[")), (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. listSize) #- (litInt 1))) ((&.++) "i") (bodyStatements [printFile f t (v $. (listAccess (var "i"))), printFileStr f ","])), (state (printFile f t (v $. (listAccess ((v $. listSize) #- (litInt 1)))))), (printFileStr f "]")]
    printFileLnList f t v = multi [(state (printFileStr f "[")), (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. listSize) #- (litInt 1))) ((&.++) "i") (bodyStatements [printFile f t (v $. (listAccess (var "i"))), printFileStr f ","])), (state (printFile f t (v $. (listAccess ((v $. listSize) #- (litInt 1)))))), (printFileStrLn f "]")]

    getIntInput v = liftPairFst (liftA3 jInput' (return $ text "Integer.parseInt") v inputFunc, Semi)
    getFloatInput v = liftPairFst (liftA3 jInput' (return $ text "Double.parseDouble") v inputFunc, Semi)
    getBoolInput v = liftPairFst (liftA3 jInput (return $ text "nextBoolean()") v inputFunc, Semi)
    getStringInput v = liftPairFst (liftA3 jInput (return $ text "nextLine()") v inputFunc, Semi)
    getCharInput _ = return (empty, Empty)
    discardInput = liftPairFst (fmap jDiscardInput inputFunc, Semi)
    getIntFileInput f v = liftPairFst (liftA3 jInput' (return $ text "Integer.parseInt") v f, Semi)
    getFloatFileInput f v = liftPairFst (liftA3 jInput' (return $ text "Double.parseDouble") v f, Semi)
    getBoolFileInput f v = liftPairFst (liftA3 jInput (return $ text "nextBoolean()") v f, Semi)
    getStringFileInput f v = liftPairFst (liftA3 jInput (return $ text "nextLine()") v f, Semi)
    getCharFileInput _ _ = return (empty, Empty)
    discardFileInput f = liftPairFst (fmap jDiscardInput f, Semi)

    openFileR f n = liftPairFst (liftA2 jOpenFileR f n, Semi)
    openFileW f n = liftPairFst (liftA3 jOpenFileWorA f n litFalse, Semi)
    openFileA f n = liftPairFst (liftA3 jOpenFileWorA f n litTrue, Semi)
    closeFile f = valState $ objMethodCall f "close" []

    getFileInputLine f v = v &= (f $. (func "nextLine" []))
    discardFileLine f = valState $ f $. (func "nextLine" [])
    stringSplit d vnew s = liftPairFst (liftA3 jStringSplit vnew (listType dynamic string) 
        (funcApp "Arrays.asList" [s $. (func "split" [litString [d]])]), Semi)

    break = return (breakDocD, Semi)  -- I could have a JumpSym class with functions for "return $ text "break" and then reference those functions here?
    continue = return (continueDocD, Semi)

    returnState v = liftPairFst (fmap returnDocD v, Semi)
    returnVar l = liftPairFst (fmap returnDocD (var l), Semi)

    valState v = liftPairFst (fmap fst v, Semi)

    comment cmt = liftPairFst (fmap (commentDocD cmt) commentStart, Empty)

    free _ = error "Cannot free variables in Java" -- could set variable to null? Might be misleading.

    throw errMsg = liftPairFst (fmap jThrowDoc (litString errMsg), Semi)

    initState fsmName initialState = varDecDef fsmName string (litString initialState)
    changeState fsmName toState = fsmName &.= (litString toState)

    initObserverList = listDecDef observerListName
    addObserver t o = valState $ obsList $. listAdd lastelem o
        where obsList = observerListName `listOf` t
              lastelem = obsList $. listSize

    state = fmap statementDocD
    loopState = fmap (statementDocD . setEmpty)
    multi = lift1List multiStateDocD endStatement

instance ControlStatementSym JavaCode where
    ifCond bs b = liftPairFst (lift4Pair ifCondDocD ifBodyStart elseIf blockEnd b bs, Empty)
    ifNoElse bs = ifCond bs $ body []
    switch v cs c = liftPairFst (lift3Pair switchDocD (state break) v c cs, Empty)
    switchAsIf v cs = ifCond cases
        where cases = map (\(l, b) -> (v ?== l, b)) cs

    ifExists v ifBody = ifCond [(notNull v, ifBody)]

    for sInit vGuard sUpdate b = liftPairFst (liftA6 forDocD blockStart blockEnd (loopState sInit) vGuard (loopState sUpdate) b, Empty)
    forRange i initv finalv stepv = for (varDecDef i int initv) ((var i) ?< finalv) (i &.+= stepv)
    forEach l t v b = liftPairFst (liftA7 (forEachDocD l) blockStart blockEnd iterForEachLabel iterInLabel t v b, Empty)
    while v b = liftPairFst (liftA4 whileDocD blockStart blockEnd v b, Empty)

    tryCatch tb cb = liftPairFst (liftA2 jTryCatch tb cb, Empty)
    
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
    method n _ s p t ps b = liftPairFst (liftA5 (jMethod n) s p t (liftList paramListDocD ps) b, False)
    getMethod n c t = method (getterName n) c public dynamic t [] getBody
        where getBody = oneLiner $ returnState (self $-> (var n))
    setMethod setLbl c paramLbl t = method (setterName setLbl) c public dynamic void [(stateParam paramLbl t)] setBody
        where setBody = oneLiner $ (self $-> (var setLbl)) &=. paramLbl
    mainMethod c b = fmap setMain $ method "main" c public static void [return $ text "String[] args"] b
    privMethod n c = method n c private dynamic
    pubMethod n c = method n c public dynamic
    constructor n = method n n public dynamic (construct n)
    destructor _ _ = error "Destructors not allowed in Java"

    function n = method n ""

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
    enum n es s = liftPairFst (liftA2 (enumDocD n) (return $ enumElementsDocD es enumsEqualInts) s, False)
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

enumsEqualInts :: Bool
enumsEqualInts = False

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

jListDecDef :: Label -> Doc -> Doc -> Doc
jListDecDef l st vs = st <+> text l <+> equals <+> new <+> st <+> parens listElements
    where listElements = if isEmpty vs then empty else text "Arrays.asList" <> parens vs

jConstDecDef :: Label -> Doc -> (Doc, Maybe String) -> Doc
jConstDecDef l st (v, _) = text "final" <+> st <+> text l <+> equals <+> v

jThrowDoc :: (Doc, Maybe String) -> Doc
jThrowDoc (errMsg, _) = text "throw new" <+> text "Exception" <> parens errMsg

jTryCatch :: Doc -> Doc -> Doc
jTryCatch tb cb = vcat [
    text "try" <+> lbrace,
    oneTab $ tb,
    rbrace <+> text "catch" <+> parens (text "Exception" <+> text "exc") <+> lbrace,
    oneTab $ cb,
    rbrace]

jDiscardInput :: (Doc, Maybe String) -> Doc
jDiscardInput (inFn, _) = inFn <> dot <> text "next()"

jInput :: Doc -> (Doc, Maybe String) -> (Doc, Maybe String) -> Doc
jInput it (v, _) (inFn, _) = v <+> equals <+> parens (inFn <> dot <> it) -- Changed from original GOOL, original GOOL was wrong.

jInput' :: Doc -> (Doc, Maybe String) -> (Doc, Maybe String) -> Doc
jInput' it (v, _) (inFn, _) = v <+> equals <+> it <> parens (inFn <> dot <> text "nextLine()")

jOpenFileR :: (Doc, Maybe String) -> (Doc, Maybe String) -> Doc
jOpenFileR (f, _) (n, _) = f <+> equals <+> new <+> text "Scanner" <> parens (new <+> text "File" <> parens n)

jOpenFileWorA :: (Doc, Maybe String) -> (Doc, Maybe String) -> (Doc, Maybe String) -> Doc
jOpenFileWorA (f, _) (n, _) (wa, _) = f <+> equals <+> new <+> text "PrintWriter" <> parens (new <+> text "FileWriter" <> parens (new <+> text "File" <> parens n <> comma <+> wa))

jListExtend :: (Doc, Maybe String) -> Doc
jListExtend (v, _) = dot <> text "add" <> parens v

jListExtendList :: Doc -> Doc
jListExtendList t = dot <> text "add" <> parens (new <+> t <> parens empty)

jStringSplit :: (Doc, Maybe String) -> Doc -> (Doc, Maybe String) -> Doc
jStringSplit (vnew, _) t (s, _) = vnew <+> equals <+> new <+> t
    <> parens s

jMethod :: Label -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc
jMethod n s p t ps b = vcat [
    s <+> p <+> t <+> text n <> parens ps <+> text "throws Exception" <+> lbrace,
    oneTab $ b,
    rbrace]

jListIndexExists :: Doc -> (Doc, Maybe String) -> (Doc, Maybe String) -> Doc
jListIndexExists greater (lst, _) (index, _) = parens (lst <> text ".length" <+> greater <+> index)