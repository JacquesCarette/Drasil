{-# LANGUAGE TypeFamilies #-}

module LanguageRenderer.NewPythonRenderer (
    -- * Python Code Configuration -- defines syntax of all Python code
    PythonCode(..)
) where

import New (Label,
    PackageSym(..), RenderSym(..), KeywordSym(..), PermanenceSym(..),
    BodySym(..), BlockSym(..), ControlBlockSym(..), StateTypeSym(..),
    UnaryOpSym(..), BinaryOpSym(..), ValueSym(..), NumericExpression(..), 
    BooleanExpression(..), ValueExpression(..), Selector(..), FunctionSym(..), 
    SelectorFunction(..), StatementSym(..), ControlStatementSym(..), 
    ScopeSym(..), MethodTypeSym(..), ParameterSym(..), MethodSym(..), 
    StateVarSym(..), ClassSym(..), ModuleSym(..))
import NewLanguageRenderer (fileDoc', 
    enumElementsDocD', multiStateDocD, blockDocD, bodyDocD, intTypeDocD, 
    floatTypeDocD, typeDocD, voidDocD, constructDocD, paramListDocD, 
    methodListDocD, ifCondDocD, stratDocD, assignDocD, plusEqualsDocD', 
    plusPlusDocD', statementDocD, returnDocD, commentDocD, notOpDocD', 
    negateOpDocD, sqrtOpDocD', absOpDocD', expOpDocD', sinOpDocD', cosOpDocD', 
    tanOpDocD', asinOpDocD', acosOpDocD', atanOpDocD', unOpDocD, equalOpDocD, 
    notEqualOpDocD, greaterOpDocD, greaterEqualOpDocD, lessOpDocD, 
    lessEqualOpDocD, plusOpDocD, minusOpDocD, multOpDocD, divideOpDocD, 
    moduloOpDocD, binOpDocD, litCharD, litFloatD, litIntD, litStringD, 
    defaultCharD, defaultFloatD, defaultIntD, defaultStringD, varDocD, 
    extVarDocD, argDocD, enumElemDocD, objVarDocD, funcAppDocD, extFuncAppDocD,
    funcDocD, listSetDocD, objAccessDocD, castObjDocD, breakDocD, continueDocD,
    staticDocD, dynamicDocD, classDec, dot, forLabel, observerListName,
    addCommentsDocD, callFuncParamList, getterName, setterName)
import Helpers (blank,oneTab,vibcat,tripFst,tripSnd,tripThird)

import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import qualified Data.Map as Map (fromList,lookup)
import Control.Applicative (Applicative, liftA2, liftA3)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), ($+$), parens, empty,
    equals, vcat, colon, brackets, isEmpty, render)

newtype PythonCode a = PC {unPC :: a}

instance Functor PythonCode where
    fmap f (PC x) = PC (f x)

instance Applicative PythonCode where
    pure = PC
    (PC f) <*> (PC x) = PC (f x)

instance Monad PythonCode where
    return = PC
    PC x >>= f = f x

liftA4 :: (a -> b -> c -> d -> e) -> PythonCode a -> PythonCode b -> PythonCode c -> PythonCode d -> PythonCode e
liftA4 f a1 a2 a3 a4 = PC $ f (unPC a1) (unPC a2) (unPC a3) (unPC a4)

liftA5 :: (a -> b -> c -> d -> e -> f) -> PythonCode a -> PythonCode b -> PythonCode c -> PythonCode d -> PythonCode e -> PythonCode f
liftA5 f a1 a2 a3 a4 a5 = PC $ f (unPC a1) (unPC a2) (unPC a3) (unPC a4) (unPC a5)

liftList :: ([a] -> b) -> [PythonCode a] -> PythonCode b
liftList f as = PC $ f (map unPC as)

lift1List :: (a -> [b] -> c) -> PythonCode a -> [PythonCode b] -> PythonCode c
lift1List f a as = PC $ f (unPC a) (map unPC as)

unPCPair :: (PythonCode a, PythonCode b) -> (a, b)
unPCPair (a1, a2) = (unPC a1, unPC a2) 

lift4Pair :: (a -> b -> c -> d -> [(e, f)] -> g) -> PythonCode a -> PythonCode b -> PythonCode c -> PythonCode d -> [(PythonCode e, PythonCode f)] -> PythonCode g
lift4Pair f a1 a2 a3 a4 as = PC $ f (unPC a1) (unPC a2) (unPC a3) (unPC a4) (map unPCPair as)

liftPairFst :: (PythonCode a, b) -> PythonCode (a, b)
liftPairFst (c, n) = PC $ (unPC c, n)

liftTripFst :: (PythonCode a, b, c) -> PythonCode (a, b, c)
liftTripFst (c, n, b) = PC $ (unPC c, n, b)

instance PackageSym PythonCode where
    type Package PythonCode = ([(Doc, Label, Bool)], Label)
    packMods n ms = liftPairFst (sequence ms, n)

instance RenderSym PythonCode where
    type RenderFile PythonCode = (Doc, Label, Bool)
    fileDoc code = liftTripFst (liftA3 fileDoc' (top code) (fmap tripFst code) bottom, tripSnd $ unPC code, tripThird $ unPC code)
    top _ = return pytop
    bottom = return empty

instance KeywordSym PythonCode where
    type Keyword PythonCode = Doc
    endStatement = return empty
    endStatementLoop = return empty

    include n = return $ pyInclude n
    inherit = return $ empty

    list _ = return $ empty
    listObj = return empty

    blockStart = return colon
    blockEnd = return empty

    ifBodyStart = blockStart
    elseIf = return $ text "elif"
    
    iterForEachLabel = return forLabel
    iterInLabel = return $ text "in"

    commentStart = return $ text "#"
    
    printFunc = return $ text "print"
    printLnFunc = return empty
    printFileFunc _ = return empty
    printFileLnFunc _ = return empty

instance PermanenceSym PythonCode where
    type Permanence PythonCode = Doc
    static = return staticDocD
    dynamic = return dynamicDocD

instance BodySym PythonCode where
    type Body PythonCode = Doc
    body = liftList bodyDocD
    bodyStatements = block
    oneLiner s = bodyStatements [s]

    addComments s = liftA2 (addCommentsDocD s) commentStart

instance BlockSym PythonCode where
    type Block PythonCode = Doc
    block sts = lift1List blockDocD endStatement (map (fmap fst) (map state sts))

instance StateTypeSym PythonCode where
    type StateType PythonCode = Doc
    bool = return empty
    int = return intTypeDocD
    float = return floatTypeDocD
    char = return empty
    string = return pyStringType
    infile = return empty
    outfile = return empty
    listType _ _ = return $ brackets empty
    intListType _ = return $ brackets empty
    floatListType _ = return $ brackets empty
    boolListType = return $ brackets empty
    obj t = return $ typeDocD t
    enumType t = return $ typeDocD t
    iterator _ = error "Iterator-type variables do not exist in Python"

instance ControlBlockSym PythonCode where
    runStrategy l strats rv av = 
        case Map.lookup l (Map.fromList strats) of Nothing -> error $ "Strategy '" ++ l ++ "': RunStrategy called on non-existent strategy."
                                                    Just b  -> liftA2 stratDocD b (state resultState)
        where resultState = case av of Nothing    -> return (empty, Empty)
                                        Just vari  -> case rv of Nothing  -> error $ "Strategy '" ++ l ++ "': Attempt to assign null return to a Value."
                                                                Just res -> assign vari res

    listSlice _ vnew vold b e s = liftA5 pyListSlice vnew vold (getVal b) (getVal e) (getVal s)
        where getVal Nothing = return (empty, Nothing)
                getVal (Just v) = v                           

instance UnaryOpSym PythonCode where
    type UnaryOp PythonCode = Doc
    notOp = return $ notOpDocD'
    negateOp = return $ negateOpDocD
    sqrtOp = return $ sqrtOpDocD'
    absOp = return $ absOpDocD'
    logOp = return $ pyLogOp
    lnOp = return $ pyLnOp
    expOp = return $ expOpDocD'
    sinOp = return $ sinOpDocD'
    cosOp = return $ cosOpDocD'
    tanOp = return $ tanOpDocD'
    asinOp = return $ asinOpDocD'
    acosOp = return $ acosOpDocD'
    atanOp = return $ atanOpDocD'
    floorOp = return $ text "math.floor"
    ceilOp = return $ text "math.ceil"

instance BinaryOpSym PythonCode where
    type BinaryOp PythonCode = Doc
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
    powerOp = return $ text "**"
    moduloOp = return $ moduloOpDocD
    andOp = return $ text "and"
    orOp = return $ text "or"

instance ValueSym PythonCode where
    type Value PythonCode = (Doc, Maybe String)
    litTrue = return $ (text "True", Just "True")
    litFalse = return $ (text "False", Just "False")
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
    self = return $ (text "self", Just "self")
    arg n = liftPairFst (liftA2 argDocD (litInt (n + 1)) argsList, Nothing)
    enumElement en e = return $ (enumElemDocD en e, Just $ en ++ "." ++ e)
    enumVar = var
    objVar o v = liftPairFst (liftA2 objVarDocD o v, Just $ valName o ++ "." ++ valName v)
    objVarSelf n = liftPairFst (liftA2 objVarDocD self (var n), Just $ "self." ++ n)
    listVar n _ = var n
    n `listOf` t = listVar n t
    iterVar = var

    inputFunc = return $ (text "input()", Nothing)  -- raw_input() for < Python 3.0
    argsList = return $ (text "sys.argv", Nothing)

    valName (PC (v, s)) = case s of Nothing -> error $ "Attempt to print unprintable Value (" ++ render v ++ ")"
                                    Just valstr -> valstr

instance NumericExpression PythonCode where
    (#~) v = liftPairFst (liftA2 unOpDocD negateOp v, Nothing)
    (#/^) v = liftPairFst (liftA2 unOpDocD sqrtOp v, Nothing)
    (#|) v = liftPairFst (liftA2 unOpDocD absOp v, Nothing)
    (#+) v1 v2 = liftPairFst (liftA3 binOpDocD plusOp v1 v2, Nothing)
    (#-) v1 v2 = liftPairFst (liftA3 binOpDocD minusOp v1 v2, Nothing)
    (#*) v1 v2 = liftPairFst (liftA3 binOpDocD multOp v1 v2, Nothing)
    (#/) v1 v2 = liftPairFst (liftA3 binOpDocD divideOp v1 v2, Nothing)
    (#%) v1 v2 = liftPairFst (liftA3 binOpDocD moduloOp v1 v2, Nothing)
    (#^) v1 v2 = liftPairFst (liftA3 binOpDocD powerOp v1 v2, Nothing)

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

instance BooleanExpression PythonCode where
    (?!) v = liftPairFst (liftA2 unOpDocD notOp v, Nothing)
    (?&&) v1 v2 = liftPairFst (liftA3 binOpDocD andOp v1 v2, Nothing)
    (?||) v1 v2 = liftPairFst (liftA3 binOpDocD orOp v1 v2, Nothing)

    (?<) v1 v2 = liftPairFst (liftA3 binOpDocD lessOp v1 v2, Nothing)
    (?<=) v1 v2 = liftPairFst (liftA3 binOpDocD lessEqualOp v1 v2, Nothing)
    (?>) v1 v2 = liftPairFst (liftA3 binOpDocD greaterOp v1 v2, Nothing)
    (?>=) v1 v2 = liftPairFst (liftA3 binOpDocD greaterEqualOp v1 v2, Nothing)
    (?==) v1 v2 = liftPairFst (liftA3 binOpDocD equalOp v1 v2, Nothing)
    (?!=) v1 v2 = liftPairFst (liftA3 binOpDocD notEqualOp v1 v2, Nothing)

instance ValueExpression PythonCode where
    inlineIf b v1 v2 = liftPairFst (liftA3 pyInlineIf b v1 v2, Nothing)
    funcApp n vs = liftPairFst (liftList (funcAppDocD n) vs, Nothing)
    selfFuncApp = funcApp
    extFuncApp l n vs = liftPairFst (liftList (extFuncAppDocD l n) vs, Nothing)
    stateObj t vs = liftPairFst (liftA2 pyStateObj t (liftList callFuncParamList vs), Nothing)
    extStateObj l t vs = liftPairFst (liftA2 (pyExtStateObj l) t (liftList callFuncParamList vs), Nothing)
    listStateObj t _ = liftPairFst (t, Nothing)

    exists v = v ?!= (var "None")
    notNull = exists

instance Selector PythonCode where
    objAccess v f = liftPairFst (liftA2 objAccessDocD v f, Nothing)
    ($.) = objAccess 

    objMethodCall o f ps = objAccess o (func f ps)
    objMethodCallVoid o f = objMethodCall o f []

    selfAccess = objAccess self

    listPopulateAccess v f = liftPairFst (liftA2 pyListPopAccess v f, Nothing)
    listSizeAccess v = liftPairFst (liftA2 pyListSizeAccess v listSize, Nothing)

    listIndexExists lst index = (listSizeAccess lst) ?> index
    argExists i = objAccess argsList (listAccess (litInt $ fromIntegral i))
    
    indexOf l v = objAccess l (fmap funcDocD (funcApp "index" [v]))

    stringEqual v1 v2 = v1 ?== v2

    castObj f v = liftPairFst (liftA2 castObjDocD f v, Nothing)
    castStrToFloat = castObj (cast float string)

instance FunctionSym PythonCode where
    type Function PythonCode = Doc
    func l vs = fmap funcDocD (funcApp l vs)
    cast targT _ = targT
    castListToInt = cast int (listType static int)
    get n = fmap funcDocD (var n)
    set n v = fmap funcDocD (liftPairFst (fmap fst $ assign (var n) v, Nothing))

    listSize = return $ text "len"
    listAdd i v = fmap funcDocD (funcApp "insert" [i, v])
    listPopulateInt = liftA2 pyListPop defaultInt
    listPopulateFloat = liftA2 pyListPop defaultFloat
    listPopulateChar = liftA2 pyListPop defaultChar
    listPopulateBool = liftA2 pyListPop defaultBool
    listPopulateString = liftA2 pyListPop defaultString
    listAppend v = fmap funcDocD (funcApp "append" [v])
    listExtendInt = fmap pyListExtend defaultInt 
    listExtendFloat = fmap pyListExtend defaultFloat 
    listExtendChar = fmap pyListExtend defaultChar 
    listExtendBool = fmap pyListExtend defaultBool
    listExtendString = fmap pyListExtend defaultString
    listExtendList n _ = return $ pyListExtendList n

    iterBegin = fmap funcDocD (funcApp "begin" [])
    iterEnd = fmap funcDocD (funcApp "end" [])

instance SelectorFunction PythonCode where
    listAccess = fmap pyListAccess
    listSet = liftA2 listSetDocD

    listAccessEnum _ = listAccess
    listSetEnum t i = listSet (castObj (cast int t) i)

    at l = listAccess (var l)

instance StatementSym PythonCode where
    -- Terminator determines how statements end to end in a separator
    type Statement PythonCode = (Doc, Terminator)
    assign v1 v2 = liftPairFst (liftA2 assignDocD v1 v2, Empty)
    assignToListIndex lst index v = valState $ lst $. listSet index v
    (&=) = assign
    (&.=) l = assign (var l)
    (&=.) v l = assign v (var l)
    (&-=) v1 v2 = v1 &= (v1 #- v2)
    (&.-=) l v = l &.= (var l #- v)
    (&+=) v1 v2 = liftPairFst (liftA3 plusEqualsDocD' v1 plusOp v2, Empty)
    (&.+=) l v = (var l) &+= v
    (&++) v = liftPairFst (liftA2 plusPlusDocD' v plusOp, Empty)
    (&.++) l = (&++) (var l)
    (&~-) v = v &= (v #- (litInt 1))
    (&.~-) l = (&~-) (var l)

    varDec _ _ = return (empty, Empty)
    varDecDef l _ v = liftPairFst (fmap (pyVarDecDef l) v, Empty)
    listDec l _ t = liftPairFst (fmap (pyListDec l) (listType static t), Empty)
    listDecDef l _ vs = liftPairFst (fmap (pyListDecDef l) (liftList callFuncParamList vs), Empty)
    objDecDef = varDecDef
    objDecNew l t vs = varDecDef l t (stateObj t vs)
    extObjDecNew l lib t vs = varDecDef l t (extStateObj lib t vs)
    objDecNewVoid l t = varDecDef l t (stateObj t [])
    extObjDecNewVoid l lib t = varDecDef l t (extStateObj lib t [])
    constDecDef = varDecDef

    print _ v = liftPairFst (liftA4 pyOut printFunc v (return $ text ", end=''") (return (empty, Nothing)), Empty)
    printLn _ v = liftPairFst (liftA4 pyOut printFunc v (return empty) (return (empty, Nothing)), Empty)
    printStr s = print string (litString s)
    printStrLn s = printLn string (litString s)

    printFile f _ v = liftPairFst (liftA4 pyOut printFunc v (return $ text ", end='', file=") f, Empty)
    printFileLn f _ v = liftPairFst (liftA4 pyOut printFunc v (return $ text ", file=") f, Empty)
    printFileStr f s = printFile f string (litString s)
    printFileStrLn f s = printFileLn f string (litString s)

    printList = print
    printLnList = printLn
    printFileList = printFile
    printFileLnList = printFileLn

    getIntInput v = v &= (funcApp "int" [inputFunc])
    getFloatInput v = v &= (funcApp "float" [inputFunc])
    getBoolInput v = v &= (inputFunc ?!= (litString "0"))
    getStringInput v = v &= (objMethodCall inputFunc "rstrip" [])
    getCharInput v = v &= inputFunc
    discardInput = valState inputFunc
    getIntFileInput f v = v &= (funcApp "int" [(objMethodCall f "readline" [])])
    getFloatFileInput f v = v &= (funcApp "float" [(objMethodCall f "readline" [])])
    getBoolFileInput f v =  v &= ((objMethodCall f "readline" []) ?!= (litString "0"))
    getStringFileInput f v = v &= (objMethodCall (objMethodCall f "readline" []) "rstrip" [])
    getCharFileInput f v = v &= (objMethodCall f "readline" [])
    discardFileInput f = valState (objMethodCall f "readline" [])

    openFileR f n = f &= (funcApp "open" [n, litString "r"])
    openFileW f n = f &= (funcApp "open" [n, litString "w"])
    openFileA f n = f &= (funcApp "open" [n, litString "a"])
    closeFile f = valState $ objMethodCall f "close" []

    getFileInputLine f v = v &= (objMethodCall f "readline" [])
    discardFileLine f = valState $ objMethodCall f "readline" []
    stringSplit d vnew s = assign vnew (objAccess s 
        (func "split" [litString [d]]))    

    break = return (breakDocD, Empty)
    continue = return (continueDocD, Empty)

    returnState v = liftPairFst (fmap returnDocD v, Empty)
    returnVar l = liftPairFst (fmap returnDocD (var l), Empty)

    valState v = liftPairFst (fmap fst v, Empty)

    comment cmt = liftPairFst (fmap (commentDocD cmt) commentStart, Empty)

    free v = v &= (var "None")

    throw errMsg = liftPairFst (fmap pyThrow (litString errMsg), Empty)

    initState fsmName initialState = varDecDef fsmName string (litString initialState)
    changeState fsmName toState = fsmName &.= (litString toState)

    initObserverList = listDecDef observerListName
    addObserver t o = valState $ obsList $. listAdd lastelem o
        where obsList = observerListName `listOf` t
                lastelem = listSizeAccess obsList

    state = fmap statementDocD
    loopState = fmap statementDocD 
    multi = lift1List multiStateDocD endStatement

instance ControlStatementSym PythonCode where
    ifCond bs b = liftPairFst (lift4Pair ifCondDocD ifBodyStart elseIf blockEnd b bs, Empty)
    ifNoElse bs = ifCond bs $ body []
    switch = switchAsIf
    switchAsIf v cs = ifCond cases
        where cases = map (\(l, b) -> (v ?== l, b)) cs

    ifExists v ifBody = ifCond [(notNull v, ifBody)]

    for _ _ _ _ = error "Classic for loops not available in Python, please use forRange, forEach, or while instead"
    forRange i initv finalv stepv b = liftPairFst (liftA5 (pyForRange i) iterInLabel initv finalv stepv b, Empty)
    forEach l _ v b = liftPairFst (liftA4 (pyForEach l) iterForEachLabel iterInLabel v b, Empty)
    while v b = liftPairFst (liftA2 pyWhile v b, Empty)

    tryCatch tb cb = liftPairFst (liftA2 pyTryCatch tb cb, Empty)

    checkState l = switch (var l)
    notifyObservers fn t ps = forRange index initv ((listSizeAccess obsList)) (litInt 1) notify
        where obsList = observerListName `listOf` t
                index = "observerIndex"
                initv = litInt 0
                notify = oneLiner $ valState $ (obsList $. at index) $. func fn ps
    
    getFileInputAll f v = v &= (objMethodCall f "readlines" [])

instance ScopeSym PythonCode where
    type Scope PythonCode = Doc
    private = return empty
    public = return empty

    includeScope s = s

instance MethodTypeSym PythonCode where
    type MethodType PythonCode = Doc
    mState t = t
    void = return voidDocD
    construct n = return $ constructDocD n

instance ParameterSym PythonCode where
    type Parameter PythonCode = Doc
    stateParam n _ = return $ text n
    pointerParam = stateParam

instance MethodSym PythonCode where
    type Method PythonCode = (Doc, Bool)
    method n _ _ _ ps b = liftPairFst (liftA3 (pyMethod n) self (liftList paramListDocD ps) b, False)
    getMethod n t = method (getterName n) public dynamic t [] getBody
        where getBody = oneLiner $ returnState (self $-> (var n))
    setMethod setLbl paramLbl t = method (setterName setLbl) public dynamic void [(stateParam paramLbl t)] setBody
        where setBody = oneLiner $ (self $-> (var setLbl)) &=. paramLbl
    mainMethod b = liftPairFst (b, True)
    privMethod n = method n private dynamic
    pubMethod n = method n public dynamic
    constructor n = method initName public dynamic (construct n)
    destructor _ _ = error "Destructors not allowed in Python"


    function n _ _ _ ps b = liftPairFst (liftA2 (pyFunction n) (liftList paramListDocD ps) b, False)

instance StateVarSym PythonCode where
    type StateVar PythonCode = Doc
    stateVar _ _ _ _ _ = return empty
    privMVar del l = stateVar del l private dynamic
    pubMVar del l = stateVar del l public dynamic
    pubGVar del l = stateVar del l public static
    listStateVar = stateVar

instance ClassSym PythonCode where
    type Class PythonCode = (Doc, Bool)
    buildClass n p _ _ fs = liftPairFst (liftA2 (pyClass n) pname (liftList methodListDocD fs), any (snd . unPC) fs)
        where pname = case p of Nothing -> return empty
                                Just pn -> return $ parens (text pn)
    enum n es _ = liftPairFst (liftA2 (pyClass n) (return empty) (return $ enumElementsDocD' es), False)
    mainClass _ _ fs = liftPairFst (liftList methodListDocD fs, True)
    privClass n p = buildClass n p private
    pubClass n p = buildClass n p public

instance ModuleSym PythonCode where
    type Module PythonCode = (Doc, Label, Bool)
    buildModule n ls vs fs cs = liftTripFst (liftA4 pyModule (liftList pyModuleImportList (map include ls)) (liftList pyModuleVarList (map state vs)) (liftList methodListDocD fs) (liftList pyModuleClassList cs), n, or [any (snd . unPC) fs, any (snd . unPC) cs])

-- convenience
imp, incl, initName :: Label
imp = "import"
incl = "from"
initName = "__init__"

pytop :: Doc 
pytop = vcat [   -- There are also imports from the libraries supplied by module. These will be handled by module.
    text incl <+> text "__future__" <+> text imp <+> text "print_function",
    text imp <+> text "sys",
    text imp <+> text "math"] 

pyInclude :: Label -> Doc
pyInclude n = text imp <+> text n

pyLogOp :: Doc
pyLogOp = text "math.log10"

pyLnOp :: Doc
pyLnOp = text "math.log"

pyStateObj :: Doc -> Doc -> Doc
pyStateObj t vs = t <> parens vs

pyExtStateObj :: Label -> Doc -> Doc -> Doc
pyExtStateObj l t vs = text l <> dot <> t <> parens vs

pyInlineIf :: (Doc, Maybe String) -> (Doc, Maybe String) -> (Doc, Maybe String) -> Doc
pyInlineIf (c, _) (v1, _) (v2, _) = parens $ v1 <+> text "if" <+> c <+> text "else" <+> v2

pyListPopAccess :: (Doc, Maybe String) -> Doc -> Doc
pyListPopAccess (v, _) f = v <+> equals <+> f

pyListSizeAccess :: (Doc, Maybe String) -> Doc -> Doc
pyListSizeAccess (v, _) f = f <> parens v

pyStringType :: Doc
pyStringType = text "str"

pyListPop :: (Doc, Maybe String) -> (Doc, Maybe String) -> Doc
pyListPop (dftVal, _) (size, _) = brackets dftVal <+> text "*" <+> size

pyListExtend :: (Doc, Maybe String) -> Doc
pyListExtend (dftVal, _) = dot <> text "append" <> parens dftVal

pyListExtendList :: Integer -> Doc
pyListExtendList ns = dot <> text "append" <> parens (nestedList ns)
    where nestedList 0 = empty
            nestedList n = brackets $ nestedList (n-1)

pyVarDecDef :: Label ->  (Doc, Maybe String) -> Doc
pyVarDecDef l (v, _) = text l <+> equals <+> v

pyListDec :: Label -> Doc -> Doc
pyListDec l t = text l <+> equals <+> t

pyListDecDef :: Label -> Doc -> Doc
pyListDecDef l vs = text l <+> equals <+> brackets vs

pyOut :: Doc ->  (Doc, Maybe String) -> Doc ->  (Doc, Maybe String) -> Doc
pyOut prf (v, _) txt (f, _) = prf <> parens (v <> txt <> f)

pyThrow ::  (Doc, Maybe String) -> Doc
pyThrow (errMsg, _) = text "raise" <+> text "Exception" <> parens errMsg

pyListAccess :: (Doc, Maybe String) -> Doc
pyListAccess (v, _) = brackets v

pyForRange :: Label -> Doc ->  (Doc, Maybe String) ->  (Doc, Maybe String) ->  (Doc, Maybe String) -> Doc -> Doc
pyForRange i inLabel (initv, _) (finalv, _) (stepv, _) b = vcat [
    forLabel <+> text i <+> inLabel <+> text "range" <> parens (initv <> text ", " <> finalv <> text ", " <> stepv) <> colon,
    oneTab $ b]

pyForEach :: Label -> Doc -> Doc ->  (Doc, Maybe String) -> Doc -> Doc
pyForEach i forEachLabel inLabel (lstVar, _) b = vcat [
    forEachLabel <+> text i <+> inLabel <+> lstVar <> colon,
    oneTab $ b]

pyWhile ::  (Doc, Maybe String) -> Doc -> Doc
pyWhile (v, _) b = vcat [
    text "while" <+> v <> colon,
    oneTab $ b]

pyTryCatch :: Doc -> Doc -> Doc
pyTryCatch tryB catchB = vcat [
    text "try" <+> colon,
    oneTab $ tryB,
    text "except" <+> text "Exception" <+> text "as" <+> text "exc" <+> colon,
    oneTab $ catchB]

pyListSlice :: (Doc, Maybe String) -> (Doc, Maybe String) -> (Doc, Maybe String) -> (Doc, Maybe String) -> (Doc, Maybe String) -> Doc
pyListSlice (vnew, _) (vold, _) (b, _) (e, _) (s, _) = vnew <+> equals <+> 
    vold <> (brackets $ b <> colon <> e <> colon <> s)

pyMethod :: Label ->  (Doc, Maybe String) -> Doc -> Doc -> Doc
pyMethod n (slf, _) ps b = vcat [
    text "def" <+> text n <> parens (slf <> oneParam <> ps) <> colon,
    oneTab $ bodyD]
        where oneParam | isEmpty ps = empty
                        | otherwise  = text ", "
                bodyD | isEmpty b = text "None"
                    | otherwise = b

pyFunction :: Label -> Doc -> Doc -> Doc
pyFunction n ps b = vcat [
    text "def" <+> text n <> parens ps <> colon,
    oneTab $ bodyD]
        where bodyD | isEmpty b = text "None"
                    | otherwise = b

pyClass :: Label -> Doc -> Doc -> Doc
pyClass n pn fs = vcat [
    classDec <+> text n <> pn <> colon,
    oneTab $ fs]

pyModuleImportList :: [Doc] -> Doc
pyModuleImportList = vcat

pyModuleVarList :: [(Doc, Terminator)] -> Doc
pyModuleVarList vs = vcat (map fst vs)

pyModuleClassList :: [(Doc, Bool)] -> Doc
pyModuleClassList cs = vibcat $ map fst cs 

pyModule :: Doc -> Doc -> Doc -> Doc -> Doc
pyModule ls vs fs cs =
    libs $+$
    vars $+$
    funcs $+$
    cs
    where libs | isEmpty ls = empty
                | otherwise  = ls $+$ blank
            vars | isEmpty vs = empty
                | otherwise  = vs $+$ blank
            funcs | isEmpty fs = empty
                | otherwise  = fs $+$ blank