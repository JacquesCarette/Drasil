{-# LANGUAGE TypeFamilies #-}

module LanguageRenderer.NewCppRenderer (
    -- * C++ Code Configuration -- defines syntax of all C++ code
    CppSrcCode(..)
) where

import New (Label,
    PackageSym(..), RenderSym(..), KeywordSym(..), PermanenceSym(..),
    BodySym(..), BlockSym(..), ControlBlockSym(..), StateTypeSym(..),
    UnaryOpSym(..), BinaryOpSym(..), ValueSym(..), NumericExpression(..), 
    BooleanExpression(..), ValueExpression(..), Selector(..), FunctionSym(..), 
    SelectorFunction(..), StatementSym(..), ControlStatementSym(..), ScopeSym(..),
    MethodTypeSym(..), ParameterSym(..), MethodSym(..), StateVarSym(..), 
    ClassSym(..), ModuleSym(..))
import NewLanguageRenderer (fileDoc', moduleDocD, classDocD, enumDocD,
    enumElementsDocD, multiStateDocD, blockDocD, bodyDocD, outDocD,
    printFileDocD, boolTypeDocD, intTypeDocD, charTypeDocD, stringTypeDocD,
    typeDocD, listTypeDocD, voidDocD, constructDocD, stateParamDocD, 
    paramListDocD, methodDocD, methodListDocD, stateVarDocD, stateVarListDocD,
    ifCondDocD, switchDocD, forDocD, forEachDocD, whileDocD, stratDocD, 
    assignDocD, plusEqualsDocD, plusPlusDocD, varDecDocD, varDecDefDocD, 
    listDecDocD, listDecDefDocD, objDecDefDocD, constDecDefDocD, statementDocD,
    returnDocD, commentDocD, notOpDocD, negateOpDocD, unOpDocD, equalOpDocD, 
    notEqualOpDocD, greaterOpDocD, greaterEqualOpDocD, lessOpDocD, 
    lessEqualOpDocD, plusOpDocD, minusOpDocD, multOpDocD, divideOpDocD, 
    moduloOpDocD, andOpDocD, orOpDocD, binOpDocD, binOpDocD', litTrueD, 
    litFalseD, litCharD, litFloatD, litIntD, litStringD, defaultCharD, 
    defaultFloatD, defaultIntD, defaultStringD, varDocD, extVarDocD, selfDocD,
    argDocD, enumElemDocD, objVarDocD, inlineIfDocD, funcAppDocD, 
    extFuncAppDocD, stateObjDocD, listStateObjDocD, notNullDocD, 
    listIndexExistsDocD, funcDocD, castDocD, listSetDocD, listAccessDocD, 
    objAccessDocD, 
    castObjDocD, breakDocD, continueDocD, staticDocD, dynamicDocD, privateDocD, 
    publicDocD, dot, new, observerListName, doubleSlash, addCommentsDocD, 
    callFuncParamList, getterName, setterName, setMain, statementsToStateVars)
import Helpers (angles, oneTab, tripFst, tripSnd, tripThird)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor)
import qualified Data.Map as Map (fromList,lookup)
import Control.Applicative (Applicative, liftA2, liftA3)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), parens, comma, empty,
  equals, semi, vcat, lbrace, rbrace, render, colon, render)

newtype CppSrcCode a = CPPSC {unCPPSC :: a}

instance Functor CppSrcCode where
    fmap f (CPPSC x) = CPPSC (f x)

instance Applicative CppSrcCode where
    pure = CPPSC
    (CPPSC f) <*> (CPPSC x) = CPPSC (f x)

instance Monad CppSrcCode where
    return = CPPSC
    CPPSC x >>= f = f x

liftA4 :: (a -> b -> c -> d -> e) -> CppSrcCode a -> CppSrcCode b -> CppSrcCode c -> CppSrcCode d -> CppSrcCode e
liftA4 f a1 a2 a3 a4 = CPPSC $ f (unCPPSC a1) (unCPPSC a2) (unCPPSC a3) (unCPPSC a4)

liftA5 :: (a -> b -> c -> d -> e -> f) -> CppSrcCode a -> CppSrcCode b -> CppSrcCode c -> CppSrcCode d -> CppSrcCode e -> CppSrcCode f
liftA5 f a1 a2 a3 a4 a5 = CPPSC $ f (unCPPSC a1) (unCPPSC a2) (unCPPSC a3) (unCPPSC a4) (unCPPSC a5)

liftA6 :: (a -> b -> c -> d -> e -> f -> g) -> CppSrcCode a -> CppSrcCode b -> CppSrcCode c -> CppSrcCode d -> CppSrcCode e -> CppSrcCode f -> CppSrcCode g
liftA6 f a1 a2 a3 a4 a5 a6 = CPPSC $ f (unCPPSC a1) (unCPPSC a2) (unCPPSC a3) (unCPPSC a4) (unCPPSC a5) (unCPPSC a6)

liftA7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> CppSrcCode a -> CppSrcCode b -> CppSrcCode c -> CppSrcCode d -> CppSrcCode e -> CppSrcCode f -> CppSrcCode g -> CppSrcCode h
liftA7 f a1 a2 a3 a4 a5 a6 a7 = CPPSC $ f (unCPPSC a1) (unCPPSC a2) (unCPPSC a3) (unCPPSC a4) (unCPPSC a5) (unCPPSC a6) (unCPPSC a7)

liftList :: ([a] -> b) -> [CppSrcCode a] -> CppSrcCode b
liftList f as = CPPSC $ f (map unCPPSC as)

lift1List :: (a -> [b] -> c) -> CppSrcCode a -> [CppSrcCode b] -> CppSrcCode c
lift1List f a as = CPPSC $ f (unCPPSC a) (map unCPPSC as)

unCPPSCPair :: (CppSrcCode a, CppSrcCode b) -> (a, b)
unCPPSCPair (a1, a2) = (unCPPSC a1, unCPPSC a2) 

lift4Pair :: (a -> b -> c -> d -> [(e, f)] -> g) -> CppSrcCode a -> CppSrcCode b -> CppSrcCode c -> CppSrcCode d -> [(CppSrcCode e, CppSrcCode f)] -> CppSrcCode g
lift4Pair f a1 a2 a3 a4 as = CPPSC $ f (unCPPSC a1) (unCPPSC a2) (unCPPSC a3) (unCPPSC a4) (map unCPPSCPair as)

lift3Pair :: (a -> b -> c -> [(d, e)] -> f) -> CppSrcCode a -> CppSrcCode b -> CppSrcCode c -> [(CppSrcCode d, CppSrcCode e)] -> CppSrcCode f
lift3Pair f a1 a2 a3 as = CPPSC $ f (unCPPSC a1) (unCPPSC a2) (unCPPSC a3) (map unCPPSCPair as)

liftPairFst :: (CppSrcCode a, b) -> CppSrcCode (a, b)
liftPairFst (c, n) = CPPSC $ (unCPPSC c, n)

liftTripFst :: (CppSrcCode a, b, c) -> CppSrcCode (a, b, c)
liftTripFst (c, n, b) = CPPSC $ (unCPPSC c, n, b)

instance PackageSym CppSrcCode where
    type Package CppSrcCode = ([(Doc, Label, Bool)], Label)
    packMods n ms = liftPairFst (sequence ms, n)

instance RenderSym CppSrcCode where
    type RenderFile CppSrcCode = (Doc, Label, Bool)
    fileDoc code = liftTripFst (liftA3 fileDoc' (top code) (fmap tripFst code) bottom, tripSnd $ unCPPSC code, tripThird $ unCPPSC code)
    top m = liftA3 cppstop m (include "") (list dynamic) endStatement
    bottom = return empty

instance KeywordSym CppSrcCode where
    type Keyword CppSrcCode = Doc
    endStatement = return semi
    endStatementLoop = return empty

    include _ = return $ text "#include"
    inherit = return colon

    list _ = return $ text "vector"
    argsList = return $ text "argv"
    listObj = return empty

    blockStart = return lbrace
    blockEnd = return rbrace

    ifBodyStart = blockStart
    elseIf = return $ text "else if"
    
    iterForEachLabel = return empty
    iterInLabel = return empty

    commentStart = return doubleSlash
    
    printFunc = return $ text "std::cout"
    printLnFunc = return $ text "std::cout"
    printFileFunc v = v -- is this right?
    printFileLnFunc v = v

instance PermanenceSym CppSrcCode where
    type Permanence CppSrcCode = Doc
    static = return staticDocD
    dynamic = return dynamicDocD

instance BodySym CppSrcCode where
    type Body CppSrcCode = Doc
    body = liftList bodyDocD
    bodyStatements = block
    oneLiner s = bodyStatements [s]

    addComments s = liftA2 (addCommentsDocD s) commentStart

instance BlockSym CppSrcCode where
    type Block CppSrcCode = Doc
    block sts = (lift1List blockDocD endStatement (map (fmap fst) (map state sts)))

instance StateTypeSym CppSrcCode where
    type StateType CppSrcCode = Doc
    bool = return $ boolTypeDocD
    int = return $ intTypeDocD
    float = return $ csFloatTypeDocD
    char = return $ charTypeDocD
    string = return $ stringTypeDocD
    infile = return $ csInfileTypeDoc
    outfile = return $ csOutfileTypeDoc
    listType p st = liftA2 listTypeDocD st (list p)
    intListType p = listType p int
    floatListType p = listType p float
    boolListType = return csBoolListTypeDocD
    obj t = return $ typeDocD t
    enumType t = return $ typeDocD t

instance ControlBlockSym CppSrcCode where
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
        (bodyStatements [
            (listDec l_temp 0 t),
            for (varDecDef l_i int (getB b)) (v_i ?< getE e) (getS s v_i)
                (oneLiner $ valState $ v_temp $. (listAppend (vold $. (listAccess v_i)))),
            (vnew &= v_temp)])
        where getB Nothing = litInt 0
              getB (Just n) = n
              getE Nothing = vold $. listSize
              getE (Just n) = n
              getS Nothing v = (&++) v
              getS (Just n) v = v &+= n

instance UnaryOpSym CppSrcCode where
    type UnaryOp CppSrcCode = Doc
    notOp = return $ notOpDocD
    negateOp = return $ negateOpDocD
    sqrtOp = return $ text "Math.Sqrt"
    absOp = return $ text "Math.Abs"
    logOp = return $ text "Math.Log10"
    lnOp = return $ text "Math.Log"
    expOp = return $ text "Math.Exp"
    sinOp = return $ text "Math.Sin"
    cosOp = return $ text "Math.Cos"
    tanOp = return $ text "Math.Tan"
    asinOp = return $ text "Math.Asin"
    acosOp = return $ text "Math.Acos"
    atanOp = return $ text "Math.Atan"
    floorOp = return $ text "Math.Floor"
    ceilOp = return $ text "Math.Ceiling"

instance BinaryOpSym CppSrcCode where
    type BinaryOp CppSrcCode = Doc
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
    powerOp = return $ text "Math.Pow"
    moduloOp = return $ moduloOpDocD
    andOp = return $ andOpDocD
    orOp = return $ orOpDocD

instance ValueSym CppSrcCode where
    type Value CppSrcCode = Doc
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
    objVarSelf n = liftA2 objVarDocD self (var n)
    listVar n _ = var n
    n `listOf` t = listVar n t
    
    inputFunc = return $ text "Console.ReadLine()"

    valName v = unCPPSC $ fmap render v

instance NumericExpression CppSrcCode where
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

instance BooleanExpression CppSrcCode where
    (?!) = liftA2 unOpDocD notOp
    (?&&) = liftA3 binOpDocD andOp
    (?||)= liftA3 binOpDocD orOp

    (?<) = liftA3 binOpDocD lessOp
    (?<=) = liftA3 binOpDocD lessEqualOp
    (?>) = liftA3 binOpDocD greaterOp
    (?>=) = liftA3 binOpDocD greaterEqualOp
    (?==) = liftA3 binOpDocD equalOp
    (?!=) = liftA3 binOpDocD notEqualOp
   
instance ValueExpression CppSrcCode where
    inlineIf = liftA3 inlineIfDocD
    funcApp n = liftList (funcAppDocD n)
    selfFuncApp = funcApp
    extFuncApp l n = liftList (extFuncAppDocD l n)
    stateObj t vs = liftA2 stateObjDocD t (liftList callFuncParamList vs)
    extStateObj _ = stateObj
    listStateObj t vs = liftA3 listStateObjDocD listObj t (liftList callFuncParamList vs)

    exists = notNull
    notNull v = liftA3 notNullDocD v notEqualOp (var "null")

instance Selector CppSrcCode where
    objAccess = liftA2 objAccessDocD
    ($.) = objAccess

    objMethodCall o f ps = objAccess o (func f ps)
    objMethodCallVoid o f = objMethodCall o f []

    selfAccess = objAccess self

    listPopulateAccess _ _ = return empty
    listSizeAccess v = objAccess v listSize

    listIndexExists = liftA3 listIndexExistsDocD greaterOp
    argExists i = objAccess argsList (listAccess (litInt $ fromIntegral i))

    stringEqual v1 v2 = v1 ?== v2

    castObj = liftA2 castObjDocD
    castStrToFloat v = funcApp "Double.Parse" [v]

instance FunctionSym CppSrcCode where
    type Function CppSrcCode = Doc
    func l vs = fmap funcDocD (funcApp l vs)
    cast targT _ = fmap castDocD targT
    castListToInt = cast (listType static int) int
    get n = fmap funcDocD (funcApp (getterName n) [])
    set n v = fmap funcDocD (funcApp (setterName n) [v])

    indexOf v = fmap funcDocD (funcApp "IndexOf" [v])

    listSize = fmap funcDocD (var "Count")
    listAdd i v = fmap funcDocD (funcApp "Insert" [i, v])
    listPopulateInt _ = return empty
    listPopulateFloat _ = return empty
    listPopulateChar _ = return empty
    listPopulateBool _ = return empty
    listPopulateString _ = return empty
    listAppend v = fmap funcDocD (funcApp "Add" [v])
    listExtendInt = fmap csListExtend defaultInt 
    listExtendFloat = fmap csListExtend defaultFloat 
    listExtendChar = fmap csListExtend defaultChar 
    listExtendBool = fmap csListExtend defaultBool
    listExtendString = fmap csListExtend defaultString
    listExtendList _ = fmap csListExtendList

    iterBegin = fmap funcDocD (funcApp "begin" [])
    iterEnd = fmap funcDocD (funcApp "end" [])

instance SelectorFunction CppSrcCode where
    listAccess = fmap listAccessDocD
    listSet = liftA2 listSetDocD

    listAccessEnum t v = listAccess (castObj (cast int t) v)
    listSetEnum t i = listSet (castObj (cast int t) i)

    at l = listAccess (var l)

instance StatementSym CppSrcCode where
    type Statement CppSrcCode = (Doc, Bool)
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
    listDecDef l t vs = liftPairFst (lift1List (listDecDefDocD l) t vs, True)
    objDecDef l t v = liftPairFst (liftA2 (objDecDefDocD l) t v, True)
    objDecNew l t vs = liftPairFst (liftA2 (objDecDefDocD l) t (stateObj t vs), True)
    extObjDecNew l _ = objDecNew l
    objDecNewVoid l t = liftPairFst (liftA2 (objDecDefDocD l) t (stateObj t []), True)
    extObjDecNewVoid l _ = objDecNewVoid l
    constDecDef l t v = liftPairFst (liftA2 (constDecDefDocD l) t v, True)

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

    getIntInput v = liftPairFst (liftA2 (csInput "Int32.Parse") v inputFunc, True)
    getFloatInput v = liftPairFst (liftA2 (csInput "Double.Parse") v inputFunc, True)
    getBoolInput _ = error "Boolean input not yet implemented for C#"
    getStringInput v = liftPairFst (liftA2 (csInput "") v inputFunc, True)
    getCharInput _ = error "Char input not yet implemented for C#"
    discardInput = liftPairFst (fmap csDiscardInput inputFunc, True)

    getIntFileInput f v = liftPairFst (liftA2 (csInput "Int32.Parse") v (fmap csFileInput f), True)
    getFloatFileInput f v = liftPairFst (liftA2 (csInput "Double.Parse") v (fmap csFileInput f), True)
    getBoolFileInput _ _ = error "Boolean input not yet implemented for C#"
    getStringFileInput f v = liftPairFst (liftA2 (csInput "") v (fmap csFileInput f), True)
    getCharFileInput _ _ = error "Char input not yet implemented for C#"
    discardFileInput f = liftPairFst (fmap csFileInput f, True)

    openFileR f n = liftPairFst (liftA3 csOpenFileR f n infile, True)
    openFileW f n = liftPairFst (liftA4 csOpenFileWorA f n outfile litFalse, True)
    openFileA f n = liftPairFst (liftA4 csOpenFileWorA f n outfile litTrue, True)
    closeFile f = valState $ objMethodCall f "Close" []

    getFileInputLine = getStringFileInput
    discardFileLine f = liftPairFst (fmap csFileInput f, True)
    stringSplit d vnew s = assign vnew $ listStateObj (listType dynamic string) [s $. (func "Split" [litChar d])]

    break = return (breakDocD, True)
    continue = return (continueDocD, True)

    returnState v = liftPairFst (fmap returnDocD v, True)
    returnVar l = liftPairFst (fmap returnDocD (var l), True)

    valState v = liftPairFst (v, True)

    comment cmt = liftPairFst (fmap (commentDocD cmt) commentStart, False)

    free _ = error "Cannot free variables in C#" -- could set variable to null? Might be misleading.

    throw errMsg = liftPairFst (fmap csThrowDoc (litString errMsg), True)

    initState fsmName initialState = varDecDef fsmName string (litString initialState)
    changeState fsmName toState = fsmName &.= (litString toState)

    initObserverList = listDecDef observerListName
    addObserver t o = valState $ obsList $. listAdd lastelem o
        where obsList = observerListName `listOf` t
              lastelem = obsList $. listSize

    state s = liftA2 statementDocD s endStatement
    loopState s = liftA2 statementDocD s endStatementLoop
    multi = lift1List multiStateDocD endStatement

instance ControlStatementSym CppSrcCode where
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

    tryCatch tb cb = liftPairFst (liftA2 csTryCatch tb cb, False)

    checkState l = switch (var l)

    notifyObservers fn t ps = for initv (var index ?< (obsList $. listSize)) ((&.++) index) notify
        where obsList = observerListName `listOf` t
              index = "observerIndex"
              initv = varDecDef index int $ litInt 0
              notify = oneLiner $ valState $ (obsList $. at index) $. func fn ps

    getFileInputAll f v = while ((?!) (objVar f (var "EndOfStream")))
        (oneLiner $ valState $ v $. (listAppend $ fmap csFileInput f))

instance ScopeSym CppSrcCode where
    type Scope CppSrcCode = Doc
    private = return privateDocD
    public = return publicDocD

    includeScope s = s

instance MethodTypeSym CppSrcCode where
    type MethodType CppSrcCode = Doc
    mState t = t
    void = return voidDocD
    construct n = return $ constructDocD n

instance ParameterSym CppSrcCode where
    type Parameter CppSrcCode = Doc
    stateParam n = fmap (stateParamDocD n)

instance MethodSym CppSrcCode where
    -- Bool is True if the method is a main method, False otherwise
    type Method CppSrcCode = (Doc, Bool)
    method n s p t ps b = liftPairFst (liftA5 (methodDocD n) s p t (liftList paramListDocD ps) b, False)
    getMethod n t = method (getterName n) public dynamic t [] getBody
        where getBody = oneLiner $ returnState (self $-> (var n))
    setMethod setLbl paramLbl t = method (setterName setLbl) public dynamic void [(stateParam paramLbl t)] setBody
        where setBody = oneLiner $ (self $-> (var setLbl)) &=. paramLbl
    mainMethod b = fmap setMain $ method "Main" public static void [return $ text "string[] args"] b
    privMethod n = method n private dynamic
    pubMethod n = method n public dynamic
    constructor n = method n public dynamic (construct n)

    function = method

instance StateVarSym CppSrcCode where
    type StateVar CppSrcCode = Doc
    stateVar _ l s p t = liftA4 (stateVarDocD l) (includeScope s) p t endStatement
    privMVar del l = stateVar del l private dynamic
    pubMVar del l = stateVar del l public dynamic
    pubGVar del l = stateVar del l public static

instance ClassSym CppSrcCode where
    -- Bool is True if the method is a main method, False otherwise
    type Class CppSrcCode = (Doc, Bool)
    buildClass n p s vs fs = liftPairFst (liftA4 (classDocD n p) inherit s (liftList stateVarListDocD vs) (liftList methodListDocD fs), any (snd . unCPPSC) fs)
    enum n es s = liftPairFst (liftA2 (enumDocD n) (return $ enumElementsDocD es False) s, False)
    mainClass n vs fs = fmap setMain $ buildClass n Nothing public vs fs
    privClass n p = buildClass n p private
    pubClass n p = buildClass n p public

instance ModuleSym CppSrcCode where
    -- Label is module name
    -- Bool is True if the method is a main method, False otherwise
    type Module CppSrcCode = (Doc, Label, Bool)
    buildModule n _ vs ms cs = 
        case null vs && null ms of True -> liftTripFst (liftList moduleDocD cs, n, any (snd . unCPPSC) cs) 
                                   _  -> liftTripFst (liftList moduleDocD ((pubClass n 
                                        Nothing (map (liftA4 statementsToStateVars
                                        public static endStatement) vs) ms):cs), n, or [any (snd . unCPPSC) ms, any (snd . unCPPSC) cs])
    -- Note: need to print libraries here instead of in cppstop

-- convenience
cppHeaderExt :: Label
cppHeaderExt = ".hpp"

cppstop :: (Doc, Label, Bool) -> Doc -> Doc -> Doc -> Doc
cppstop (m, n, b) inc lst end = vcat [
    if b then empty else inc <+> doubleQuotedText (n ++ cppHeaderExt),
    blank,
    inc <+> angles (text "algorithm"),
    inc <+> angles (text "iostream"),
    inc <+> angles (text "fstream"),
    inc <+> angles (text "iterator"),
    inc <+> angles (text "string"),
    inc <+> angles (text "math.h"),
    inc <+> angles (text "sstream"),
    inc <+> angles (text "limits"),
    inc <+> angles lst,
    blank,
    usingNameSpace "std" (Just "string") end,
    usingNameSpace "std" (Just $ render lst) end,
    usingNameSpace "std" (Just "ifstream") end,
    usingNameSpace "std" (Just "ofstream") end]

usingNameSpace :: Label -> Maybe Label -> Doc -> Doc
usingNameSpace n (Just m) end = text "using" <+> text n <> colon <> colon <> text m <> end
usingNameSpace n Nothing end = text "using namespace" <+> text n <> end

csFloatTypeDocD :: Doc
csFloatTypeDocD = text "double" -- Same as Java, maybe make a common function

csInfileTypeDoc :: Doc
csInfileTypeDoc = text "StreamReader"

csOutfileTypeDoc :: Doc
csOutfileTypeDoc = text "StreamWriter"

csBoolListTypeDocD :: Doc
csBoolListTypeDocD = text "BitArray"

csThrowDoc :: Doc -> Doc
csThrowDoc errMsg = text "throw new" <+> text "Exception" <> parens errMsg

csTryCatch :: Doc -> Doc -> Doc
csTryCatch tb cb= vcat [
    text "try" <+> lbrace,
    oneTab $ tb,
    rbrace <+> text "catch" <+> parens (text "Exception" <+> text "exc") <+> lbrace,
    oneTab $ cb,
    rbrace]

csDiscardInput :: Doc -> Doc
csDiscardInput inFn = inFn

csInput :: Label -> Doc -> Doc -> Doc
csInput it v inFn = v <+> equals <+> text it <> parens inFn

csFileInput :: Doc -> Doc
csFileInput f = f <> dot <> text "ReadLine()"

csOpenFileR :: Doc -> Doc -> Doc -> Doc
csOpenFileR f n r = f <+> equals <+> new <+> r <> parens n

csOpenFileWorA :: Doc -> Doc -> Doc -> Doc -> Doc
csOpenFileWorA f n w a = f <+> equals <+> new <+> w <> parens (n <> comma <+> a)

csListExtend :: Doc -> Doc
csListExtend v = dot <> text "Add" <> parens v

csListExtendList :: Doc -> Doc
csListExtendList t = dot <> text "Add" <> parens (new <+> t <> parens empty)