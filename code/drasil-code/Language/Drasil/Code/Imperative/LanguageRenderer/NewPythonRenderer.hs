{-# LANGUAGE TypeFamilies #-}

-- | The logic to render Java code from an 'AbstractCode' is contained in this module
module Language.Drasil.Code.Imperative.LanguageRenderer.NewPythonRenderer (
    -- * Python Code Configuration -- defines syntax of all Python code
    PythonCode(..)
) where

import Language.Drasil.Code.Imperative.New (Label,
    RenderSym(..), KeywordSym(..), PermanenceSym(..),
    BodySym(..), BlockSym(..), ControlBlockSym(..), StateTypeSym(..),
    StatementSym(..), UnaryOpSym(..), BinaryOpSym(..), ValueSym(..), 
    NumericExpression(..), BooleanExpression(..), ValueExpression(..), 
    Selector(..), FunctionSym(..), SelectorFunction(..), ScopeSym(..), 
    MethodTypeSym(..), ParameterSym(..), MethodSym(..), StateVarSym(..), 
    ClassSym(..), ModuleSym(..))
import Language.Drasil.Code.Imperative.NewLanguageRenderer (fileDoc', 
    enumElementsDocD', multiStateDocD, blockDocD, bodyDocD, intTypeDocD, floatTypeDocD, 
    typeDocD, voidDocD, constructDocD, paramListDocD, methodListDocD, 
    ifCondDocD, stratDocD, assignDocD, plusEqualsDocD', plusPlusDocD',
    statementDocD, returnDocD, commentDocD, notOpDocD', negateOpDocD,
    sqrtOpDocD', absOpDocD', expOpDocD', sinOpDocD', cosOpDocD', tanOpDocD',
    unOpDocD, equalOpDocD, notEqualOpDocD, greaterOpDocD, greaterEqualOpDocD, 
    lessOpDocD, lessEqualOpDocD, plusOpDocD, minusOpDocD, multOpDocD, 
    divideOpDocD, moduloOpDocD, binOpDocD, litCharD, litFloatD, litIntD, 
    litStringD, defaultCharD, defaultFloatD, defaultIntD, 
    defaultStringD, varDocD, extVarDocD, argDocD, enumElemDocD, objVarDocD, 
    funcAppDocD, extFuncAppDocD,
    funcDocD, listSetDocD, objAccessDocD, 
    castObjDocD, breakDocD, continueDocD, staticDocD, dynamicDocD,
    classDec, dot, forLabel, observerListName,
    addCommentsDocD, callFuncParamList, getterName, setterName)
import Language.Drasil.Code.Imperative.Helpers (blank,oneTab,vibcat)

import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import qualified Data.Map as Map (fromList,lookup)
import Control.Applicative (Applicative, liftA, liftA2, liftA3)
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

liftA4 :: (Doc -> Doc -> Doc -> Doc -> Doc) -> PythonCode Doc -> PythonCode Doc -> PythonCode Doc -> PythonCode Doc -> PythonCode Doc
liftA4 f a1 a2 a3 a4 = PC $ f (unPC a1) (unPC a2) (unPC a3) (unPC a4)

liftA5 :: (Doc -> Doc -> Doc -> Doc -> Doc -> Doc) -> PythonCode Doc -> PythonCode Doc -> PythonCode Doc -> PythonCode Doc -> PythonCode Doc -> PythonCode Doc
liftA5 f a1 a2 a3 a4 a5 = PC $ f (unPC a1) (unPC a2) (unPC a3) (unPC a4) (unPC a5)

liftList :: ([Doc] -> Doc) -> [PythonCode Doc] -> PythonCode Doc
liftList f as = PC $ f (map unPC as)

lift1List :: (Doc -> [Doc] -> Doc) -> PythonCode Doc -> [PythonCode Doc] -> PythonCode Doc
lift1List f a as = PC $ f (unPC a) (map unPC as)

unPCPair :: (PythonCode Doc, PythonCode Doc) -> (Doc, Doc)
unPCPair (a1, a2) = (unPC a1, unPC a2) 

lift4Pair :: (Doc -> Doc -> Doc -> Doc -> [(Doc, Doc)] -> Doc) -> PythonCode Doc -> PythonCode Doc -> PythonCode Doc -> PythonCode Doc -> [(PythonCode Doc, PythonCode Doc)] -> PythonCode Doc
lift4Pair f a1 a2 a3 a4 as = PC $ f (unPC a1) (unPC a2) (unPC a3) (unPC a4) (map unPCPair as)

liftPairFst :: (PythonCode Doc, a) -> PythonCode (Doc, a)
liftPairFst (c, n) = PC $ (unPC c, n)

instance RenderSym PythonCode where
    type RenderFile PythonCode = (Doc, Label)
    fileDoc code = liftPairFst (liftA3 fileDoc' top (return $ fst $ unPC code) bottom, snd $ unPC code)
    top = return pytop
    bottom = return empty

instance KeywordSym PythonCode where
    type Keyword PythonCode = Doc
    endStatement = return empty
    endStatementLoop = return empty

    include n = return $ pyInclude n
    inherit = return $ empty

    list _ = return $ empty
    argsList = return $ text "sys.argv"
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
    body bs = liftList bodyDocD bs
    bodyStatements sts = block sts
    oneLiner s = bodyStatements [s]

    addComments s b = liftA2 (addCommentsDocD s) commentStart b

instance BlockSym PythonCode where
    type Block PythonCode = Doc
    block sts = lift1List blockDocD endStatement (map state sts)

instance StateTypeSym PythonCode where
    type StateType PythonCode = Doc
    bool = return empty
    int = return intTypeDocD
    float = return floatTypeDocD
    char = return empty
    string = return pyStringType
    infile = return empty
    outfile = return empty
    listType _ _ = return $ brackets (empty)
    intListType _ = return $ brackets (empty)
    floatListType _ = return $ brackets (empty)
    boolListType = return $ brackets (empty)
    obj t = return $ typeDocD t
    enumType t = return $ typeDocD t

instance ControlBlockSym PythonCode where
    ifCond bs b = lift4Pair ifCondDocD ifBodyStart elseIf blockEnd b bs
    ifNoElse bs = ifCond bs $ body []
    switch v cs c = switchAsIf v cs c
    switchAsIf v cs c = ifCond cases c
        where cases = map (\(l, b) -> (v ?== l, b)) cs

    ifExists v ifBody elseBody = ifCond [(notNull v, ifBody)] elseBody

    for _ _ _ _ = error "Classic for loops not available in Python, please use forRange, forEach, or while instead"
    forRange i initv finalv stepv b = liftA5 (pyForRange i) iterInLabel initv finalv stepv b
    forEach l _ v b = liftA4 (pyForEach l) iterForEachLabel iterInLabel v b
    while v b = liftA2 pyWhile v b

    tryCatch tb cb = liftA2 pyTryCatch tb cb

    checkState l cs c = switch (var l) cs c
    runStrategy l strats rv av = 
        case Map.lookup l (Map.fromList strats) of Nothing -> error $ "Strategy '" ++ l ++ "': RunStrategy called on non-existent strategy."
                                                   Just b  -> liftA2 stratDocD b (state resultState)
        where resultState = case av of Nothing    -> return empty
                                       Just vari  -> case rv of Nothing  -> error $ "Strategy '" ++ l ++ "': Attempt to assign null return to a Value."
                                                                Just res -> assign vari res
    notifyObservers fn t ps = forRange index initv ((listSizeAccess obsList)) (litInt 1) notify
        where obsList = observerListName `listOf` t
              index = "observerIndex"
              initv = litInt 0
              notify = oneLiner $ valState $ (obsList $. at index) $. func fn ps

    getFileInputAll f v = v &= (objMethodCall f "readlines" [])
    listSlice _ vnew vold b e s = liftA5 pyListSlice vnew vold (getVal b) (getVal e) (getVal s)
        where getVal Nothing = return empty
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
    type Value PythonCode = Doc
    litTrue = return $ text "True"
    litFalse = return $ text "False"
    litChar c = return $ litCharD c
    litFloat v = return $ litFloatD v
    litInt v = return $ litIntD v
    litString s = return $ litStringD s

    defaultChar = return $ defaultCharD
    defaultFloat = return $ defaultFloatD
    defaultInt = return $ defaultIntD
    defaultString = return $ defaultStringD
    defaultBool = litFalse

    ($->) v vr = objVar v vr
    ($:) en e = enumElement en e

    const n = var n
    var n = return $ varDocD n
    extVar l n = return $ extVarDocD l n
    self = return $ text "self"
    arg n = liftA2 argDocD (litInt (n + 1)) argsList
    enumElement en e = return $ enumElemDocD en e
    enumVar n = var n
    objVar n1 n2 = liftA2 objVarDocD n1 n2
    objVarSelf n = liftA2 objVarDocD self (var n)
    listVar n _ = var n
    n `listOf` t = listVar n t

    inputFunc = return $ text "input()" -- raw_input() for < Python 3.0
    
    valName v = unPC $ liftA render v

instance NumericExpression PythonCode where
    (#~) v = liftA2 unOpDocD negateOp v
    (#/^) v = liftA2 unOpDocD sqrtOp v
    (#|) v = liftA2 unOpDocD absOp v
    (#+)  v1 v2 = liftA3 binOpDocD v1 plusOp v2
    (#-)  v1 v2 = liftA3 binOpDocD v1 minusOp v2
    (#*)  v1 v2 = liftA3 binOpDocD v1 multOp v2
    (#/)  v1 v2 = liftA3 binOpDocD v1 divideOp v2
    (#%)  v1 v2 = liftA3 binOpDocD v1 moduloOp v2
    (#^)  v1 v2 = liftA3 binOpDocD v1 powerOp v2

    log v = liftA2 unOpDocD logOp v
    ln v = liftA2 unOpDocD lnOp v
    exp v = liftA2 unOpDocD expOp v
    sin v = liftA2 unOpDocD sinOp v
    cos v = liftA2 unOpDocD cosOp v
    tan v = liftA2 unOpDocD tanOp v
    csc v = (litFloat 1.0) #/ (sin v)
    sec v = (litFloat 1.0) #/ (cos v)
    cot v = (litFloat 1.0) #/ (tan v)
    floor v = liftA2 unOpDocD floorOp v
    ceil v = liftA2 unOpDocD ceilOp v

instance BooleanExpression PythonCode where
    (?!) v = liftA2 unOpDocD notOp v
    (?&&) v1 v2 = liftA3 binOpDocD v1 andOp v2
    (?||) v1 v2 = liftA3 binOpDocD v1 orOp v2

    (?<)  v1 v2 = liftA3 binOpDocD v1 lessOp v2
    (?<=) v1 v2 = liftA3 binOpDocD v1 lessEqualOp v2
    (?>)  v1 v2 = liftA3 binOpDocD v1 greaterOp v2
    (?>=) v1 v2 = liftA3 binOpDocD v1 greaterEqualOp v2
    (?==) v1 v2 = liftA3 binOpDocD v1 equalOp v2
    (?!=) v1 v2 = liftA3 binOpDocD v1 notEqualOp v2

instance ValueExpression PythonCode where
    inlineIf c v1 v2 = liftA3 pyInlineIf c v1 v2
    funcApp n vs = liftList (funcAppDocD n) vs
    selfFuncApp = funcApp
    extFuncApp l n vs = liftList (extFuncAppDocD l n) vs
    stateObj t vs = liftA2 pyStateObj t (liftList callFuncParamList vs)
    extStateObj l t vs = liftA2 (pyExtStateObj l) t (liftList callFuncParamList vs)
    listStateObj t _ = t

    exists v = v ?!= (var "None")
    notNull = exists

instance Selector PythonCode where
    objAccess v f = liftA2 objAccessDocD v f
    ($.) v f = objAccess v f

    objMethodCall o f ps = objAccess o (func f ps)
    objMethodCallVoid o f = objMethodCall o f []

    selfAccess f = objAccess self f

    listPopulateAccess v f = liftA2 pyListPopAccess v f
    listSizeAccess v = liftA2 pyListSizeAccess v listSize

    listIndexExists lst index = (listSizeAccess lst) ?> index
    argExists i = objAccess argsList (listAccess (litInt $ fromIntegral i))

    stringEqual v1 v2 = v1 ?== v2

    castObj f v = liftA2 castObjDocD f v
    castStrToFloat v = castObj (cast float string) v

instance FunctionSym PythonCode where
    type Function PythonCode = Doc
    func l vs = liftA funcDocD (funcApp l vs)
    cast targT _ = targT
    castListToInt = cast int (listType static int)
    get n = liftA funcDocD (var n)
    set n v = liftA funcDocD (assign (var n) v)

    indexOf v = liftA funcDocD (funcApp "index" [v])

    listSize = return $ text "len"
    listAdd i v = liftA funcDocD (funcApp "insert" [i, v])
    listPopulateInt size = liftA2 pyListPop defaultInt size
    listPopulateFloat size = liftA2 pyListPop defaultFloat size
    listPopulateChar size = liftA2 pyListPop defaultChar size
    listPopulateBool size = liftA2 pyListPop defaultBool size
    listPopulateString size = liftA2 pyListPop defaultString size
    listAppend v = liftA funcDocD (funcApp "append" [v])
    listExtendInt = liftA pyListExtend defaultInt 
    listExtendFloat = liftA pyListExtend defaultFloat 
    listExtendChar = liftA pyListExtend defaultChar 
    listExtendBool = liftA pyListExtend defaultBool
    listExtendString = liftA pyListExtend defaultString
    listExtendList n _ = return $ pyListExtendList n

    iterBegin = liftA funcDocD (funcApp "begin" [])
    iterEnd = liftA funcDocD (funcApp "end" [])

instance SelectorFunction PythonCode where
    listAccess i = liftA pyListAccess i
    listSet i v = liftA2 listSetDocD i v

    listAccessEnum _ v = listAccess v
    listSetEnum t i v = listSet (castObj (cast int t) i) v

    at l = listAccess (var l)

instance StatementSym PythonCode where
    type Statement PythonCode = Doc
    assign v1 v2 = liftA2 assignDocD v1 v2
    assignToListIndex lst index v = lst $. listSet index v
    (&=) v1 v2 = assign v1 v2
    (&.=) l v = assign (var l) v
    (&=.) v l = assign v (var l)
    (&-=) v1 v2 = v1 &= (v1 #- v2)
    (&.-=) l v = l &.= (var l #- v)
    (&+=) v1 v2 = liftA3 plusEqualsDocD' v1 plusOp v2
    (&.+=) l v = (var l) &+= v
    (&++) v = liftA2 plusPlusDocD' v plusOp
    (&.++) l = (&++) (var l)
    (&~-) v = v &= (v #- (litInt 1))
    (&.~-) l = (&~-) (var l)

    varDec _ _ = return empty
    varDecDef l _ v = liftA (pyVarDecDef l) v
    listDec l _ t = liftA (pyListDec l) (listType static t) 
    listDecDef l _ vs = liftA (pyListDecDef l) (liftList callFuncParamList vs)
    objDecDef l t v = varDecDef l t v
    objDecNew l t vs = varDecDef l t (stateObj t vs)
    extObjDecNew l lib t vs = varDecDef l t (extStateObj lib t vs)
    objDecNewVoid l t = varDecDef l t (stateObj t [])
    extObjDecNewVoid l lib t = varDecDef l t (extStateObj lib t [])
    constDecDef l t v = varDecDef l t v 

    print _ v = liftA4 pyOut printFunc v (return $ text ", end=''") (return empty)
    printLn _ v = liftA4 pyOut printFunc v (return empty) (return empty)
    printStr s = print string (litString s)
    printStrLn s = printLn string (litString s)

    printFile f _ v = liftA4 pyOut printFunc v (return $ text ", end='', file=") f
    printFileLn f _ v = liftA4 pyOut printFunc v (return $ text ", file=") f
    printFileStr f s = printFile f string (litString s)
    printFileStrLn f s = printFileLn f string (litString s)

    printList t v = print t v
    printLnList t v = printLn t v
    printFileList f t v = printFile f t v
    printFileLnList f t v = printFileLn f t v

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
    closeFile f = valState $ objMethodCall f "close" []

    getFileInputLine f v = v &= (objMethodCall f "readline" [])
    discardFileLine f = valState $ objMethodCall f "readline" []
    stringSplit d vnew s = liftA3 pyStringSplit vnew s
        (funcApp "split" [litString [d]])

    break = return breakDocD
    continue = return continueDocD

    returnState v = liftA returnDocD v
    returnVar l = liftA returnDocD (var l)

    valState v = v

    comment cmt = liftA (commentDocD cmt) commentStart

    free v = v &= (var "None")

    throw errMsg = liftA pyThrow (litString errMsg)

    initState fsmName initialState = varDecDef fsmName string (litString initialState)
    changeState fsmName toState = fsmName &.= (litString toState)

    initObserverList t os = listDecDef observerListName t os
    addObserver t o = obsList $. listAdd lastelem o
        where obsList = observerListName `listOf` t
              lastelem = listSizeAccess obsList

    state s = liftA2 statementDocD s endStatement
    loopState s = liftA2 statementDocD s endStatementLoop
    multi s = lift1List multiStateDocD endStatement s

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

instance MethodSym PythonCode where
    type Method PythonCode = Doc
    method n _ _ _ ps b = liftA3 (pyMethod n) self (liftList (paramListDocD) ps) b
    getMethod n t = method (getterName n) public dynamic t [] getBody
        where getBody = oneLiner $ returnState (self $-> (var n))
    setMethod setLbl paramLbl t = method (setterName setLbl) public dynamic void [(stateParam paramLbl t)] setBody
        where setBody = oneLiner $ (self $-> (var setLbl)) &=. paramLbl
    mainMethod b = b
    privMethod n t ps b = method n private dynamic t ps b
    pubMethod n t ps b = method n public dynamic t ps b
    constructor n ps b = method initName public dynamic (construct n) ps b

    function n _ _ _ ps b = liftA2 (pyFunction n) (liftList (paramListDocD) ps) b

instance StateVarSym PythonCode where
    type StateVar PythonCode = Doc
    stateVar _ _ _ _ _ = return empty
    privMVar del l t = stateVar del l private dynamic t
    pubMVar del l t = stateVar del l public dynamic t
    pubGVar del l t = stateVar del l public static t

instance ClassSym PythonCode where
    type Class PythonCode = Doc
    buildClass n p _ _ fs = liftA2 (pyClass n) pname (liftList methodListDocD fs)
        where pname = case p of Nothing -> return empty
                                Just pn -> return $ parens (text pn)
    enum n es _ = liftA2 (pyClass n) (return empty) (return $ enumElementsDocD' es)
    mainClass _ _ fs = liftList methodListDocD fs
    privClass n p vs fs = buildClass n p private vs fs
    pubClass n p vs fs = buildClass n p public vs fs

instance ModuleSym PythonCode where
    type Module PythonCode = (Doc, Label)
    buildModule n ls vs fs cs = liftPairFst (liftA4 pyModule (liftList pyModuleImportList (map include ls)) (liftList pyModuleVarList vs) (liftList methodListDocD fs) (liftList pyModuleClassList cs), n)

-- convenience
imp, incl, initName :: Label
imp = "import"
incl = "from"
initName = "__init__"

pytop :: Doc 
pytop = vcat [   -- There are also imports from the libraries supplied by module. These will be handled by module.
    text "from __future__ import print_function",
    text "import sys",
    text "import math"] 

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

pyInlineIf :: Doc -> Doc -> Doc -> Doc
pyInlineIf c v1 v2 = parens $ v1 <+> text "if" <+> c <+> text "else" <+> v2

pyListPopAccess :: Doc -> Doc -> Doc
pyListPopAccess v f = v <+> equals <+> f

pyListSizeAccess :: Doc -> Doc -> Doc
pyListSizeAccess v f = f <> parens v

pyStringType :: Doc
pyStringType = text "str"

pyListPop :: Doc -> Doc -> Doc
pyListPop dftVal size = brackets dftVal <+> text "*" <+> size

pyListExtend :: Doc -> Doc
pyListExtend dftVal = dot <> text "append" <> parens dftVal

pyListExtendList :: Integer -> Doc
pyListExtendList ns = dot <> text "append" <> parens (nestedList ns)
  where nestedList 0 = empty
        nestedList n = brackets $ nestedList (n-1)

pyVarDecDef :: Label -> Doc -> Doc
pyVarDecDef l v = text l <+> equals <+> v

pyListDec :: Label -> Doc -> Doc
pyListDec l t = text l <+> equals <+> t

pyListDecDef :: Label -> Doc -> Doc
pyListDecDef l vs = text l <+> equals <+> brackets vs

pyOut :: Doc -> Doc -> Doc -> Doc -> Doc
pyOut prf v txt f = prf <> parens (v <> txt <> f)

pyStringSplit :: Doc -> Doc -> Doc -> Doc
pyStringSplit vnew s dsplit = vnew <+> equals <+> s <> dot <> dsplit

pyThrow :: Doc -> Doc
pyThrow errMsg = text "raise" <+> text "Exception" <> parens errMsg

pyListAccess :: Doc -> Doc
pyListAccess i = brackets i

pyForRange :: Label -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc
pyForRange i inLabel initv finalv stepv b = vcat [
    forLabel <+> text i <+> inLabel <+> text "range" <> parens (initv <> text ", " <> finalv <> text ", " <> stepv) <> colon,
    oneTab $ b]

pyForEach :: Label -> Doc -> Doc -> Doc -> Doc -> Doc
pyForEach i forEachLabel inLabel lstVar b = vcat [
    forEachLabel <+> text i <+> inLabel <+> lstVar <> colon,
    oneTab $ b]

pyWhile :: Doc -> Doc -> Doc
pyWhile v b = vcat [
    text "while" <+> v <> colon,
    oneTab $ b]

pyTryCatch :: Doc -> Doc -> Doc
pyTryCatch tryB catchB = vcat [
    text "try" <+> colon,
    oneTab $ tryB,
    text "except" <+> text "Exception" <+> text "as" <+> text "exc" <+> colon,
    oneTab $ catchB]

pyListSlice :: Doc -> Doc -> Doc -> Doc -> Doc -> Doc
pyListSlice vnew vold b e s = vnew <+> equals <+> vold <> (brackets $ 
    b <> colon <> e <> colon <> s)

pyMethod :: Label -> Doc -> Doc -> Doc -> Doc
pyMethod n slf ps b = vcat [
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
pyModuleImportList ls = vcat ls

pyModuleVarList :: [Doc] -> Doc
pyModuleVarList vs = vcat vs

pyModuleClassList :: [Doc] -> Doc
pyModuleClassList cs = vibcat cs

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