{-# LANGUAGE TypeFamilies #-}

module LanguageRenderer.NewCSharpRenderer (
    -- * C# Code Configuration -- defines syntax of all C# code
    CSharpCode(..)
) where

import New (Label,
  RenderSym(..), KeywordSym(..), PermanenceSym(..),
  BodySym(..), BlockSym(..), ControlBlockSym(..), StateTypeSym(..),
  UnaryOpSym(..), BinaryOpSym(..), ValueSym(..), NumericExpression(..), 
  BooleanExpression(..), ValueExpression(..), Selector(..), 
  FunctionSym(..), SelectorFunction(..), StatementSym(..), 
  ControlStatementSym(..), ScopeSym(..), MethodTypeSym(..),
  ParameterSym(..), MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..))
import NewLanguageRenderer (fileDoc', moduleDocD, classDocD, enumDocD,
  enumElementsDocD, multiStateDocD, blockDocD, bodyDocD, outDocD, printListDocD,
  printFileDocD, boolTypeDocD, 
  intTypeDocD, charTypeDocD, stringTypeDocD, typeDocD, listTypeDocD, voidDocD,
  constructDocD, stateParamDocD, paramListDocD, methodDocD, methodListDocD, 
  stateVarDocD, stateVarListDocD, ifCondDocD, switchDocD, forDocD, 
  forEachDocD, whileDocD, stratDocD, assignDocD, plusEqualsDocD, plusPlusDocD,
  varDecDocD, varDecDefDocD, listDecDocD, listDecDefDocD, objDecDefDocD, 
  constDecDefDocD, statementDocD, returnDocD,
  commentDocD, notOpDocD, negateOpDocD, unOpDocD, equalOpDocD, 
  notEqualOpDocD, greaterOpDocD, greaterEqualOpDocD, lessOpDocD, 
  lessEqualOpDocD, plusOpDocD, minusOpDocD, multOpDocD, divideOpDocD, 
  moduloOpDocD, andOpDocD, orOpDocD, binOpDocD, binOpDocD', litTrueD, litFalseD,
  litCharD, litFloatD, litIntD, litStringD, defaultCharD, defaultFloatD, defaultIntD, 
  defaultStringD, varDocD, extVarDocD, selfDocD, argDocD, enumElemDocD, objVarDocD, 
  inlineIfDocD, funcAppDocD, extFuncAppDocD, stateObjDocD, listStateObjDocD, 
  notNullDocD, listIndexExistsDocD, funcDocD, castDocD, listSetDocD, 
  listAccessDocD, objAccessDocD, 
  castObjDocD, breakDocD, continueDocD, staticDocD, dynamicDocD, privateDocD, 
  publicDocD, dot, new, forLabel, observerListName, doubleSlash, 
  addCommentsDocD, callFuncParamList, getterName, setterName)
import Helpers (angles,oneTab)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor)
import qualified Data.Map as Map (fromList,lookup)
import Control.Applicative (Applicative, liftA, liftA2, liftA3)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), parens, empty, equals, 
  semi, vcat, lbrace, rbrace, render, colon, isEmpty, render)

newtype CSharpCode a = CSC {unCSC :: a}

instance Functor CSharpCode where
    fmap f (CSC x) = CSC (f x)

instance Applicative CSharpCode where
    pure = CSC
    (CSC f) <*> (CSC x) = CSC (f x)

instance Monad CSharpCode where
    return = CSC
    CSC x >>= f = f x

liftA4 :: (a -> b -> c -> d -> e) -> CSharpCode a -> CSharpCode b -> CSharpCode c -> CSharpCode d -> CSharpCode e
liftA4 f a1 a2 a3 a4 = CSC $ f (unCSC a1) (unCSC a2) (unCSC a3) (unCSC a4)

liftA5 :: (a -> b -> c -> d -> e -> f) -> CSharpCode a -> CSharpCode b -> CSharpCode c -> CSharpCode d -> CSharpCode e -> CSharpCode f
liftA5 f a1 a2 a3 a4 a5 = CSC $ f (unCSC a1) (unCSC a2) (unCSC a3) (unCSC a4) (unCSC a5)

liftA6 :: (a -> b -> c -> d -> e -> f -> g) -> CSharpCode a -> CSharpCode b -> CSharpCode c -> CSharpCode d -> CSharpCode e -> CSharpCode f -> CSharpCode g
liftA6 f a1 a2 a3 a4 a5 a6 = CSC $ f (unCSC a1) (unCSC a2) (unCSC a3) (unCSC a4) (unCSC a5) (unCSC a6)

liftA7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> CSharpCode a -> CSharpCode b -> CSharpCode c -> CSharpCode d -> CSharpCode e -> CSharpCode f -> CSharpCode g -> CSharpCode h
liftA7 f a1 a2 a3 a4 a5 a6 a7 = CSC $ f (unCSC a1) (unCSC a2) (unCSC a3) (unCSC a4) (unCSC a5) (unCSC a6) (unCSC a7)

liftList :: ([a] -> b) -> [CSharpCode a] -> CSharpCode b
liftList f as = CSC $ f (map unCSC as)

lift1List :: (a -> [b] -> c) -> CSharpCode a -> [CSharpCode b] -> CSharpCode c
lift1List f a as = CSC $ f (unCSC a) (map unCSC as)

unCSCPair :: (CSharpCode a, CSharpCode b) -> (a, b)
unCSCPair (a1, a2) = (unCSC a1, unCSC a2) 

lift4Pair :: (a -> b -> c -> d -> [(e, f)] -> g) -> CSharpCode a -> CSharpCode b -> CSharpCode c -> CSharpCode d -> [(CSharpCode e, CSharpCode f)] -> CSharpCode g
lift4Pair f a1 a2 a3 a4 as = CSC $ f (unCSC a1) (unCSC a2) (unCSC a3) (unCSC a4) (map unCSCPair as)

lift3Pair :: (a -> b -> c -> [(d, e)] -> f) -> CSharpCode a -> CSharpCode b -> CSharpCode c -> [(CSharpCode d, CSharpCode e)] -> CSharpCode f
lift3Pair f a1 a2 a3 as = CSC $ f (unCSC a1) (unCSC a2) (unCSC a3) (map unCSCPair as)

liftPairFst :: (CSharpCode a, b) -> CSharpCode (a, b)
liftPairFst (c, n) = CSC $ (unCSC c, n)

instance RenderSym CSharpCode where
    type RenderFile CSharpCode = (Doc, Label)
    fileDoc code = liftPairFst (liftA3 fileDoc' top (return $ fst $ unCSC code) bottom, snd $ unCSC code)
    top = liftA2 cstop endStatement (include "")
    bottom = return empty

instance KeywordSym CSharpCode where
    type Keyword CSharpCode = Doc
    endStatement = return semi
    endStatementLoop = return empty

    include _ = return $ text "using"
    inherit = return colon

    list _ = return $ text "List"
    argsList = return $ text "args"
    listObj = return new

    blockStart = return lbrace
    blockEnd = return rbrace

    ifBodyStart = blockStart
    elseIf = return $ text "else if"
    
    iterForEachLabel = return $ text "foreach"
    iterInLabel = return $ text "in"

    commentStart = return doubleSlash
    
    printFunc = return $ text "Console.Write"
    printLnFunc = return $ text "Console.WriteLine"
    printFileFunc f = liftA (printFileDocD "Write") f
    printFileLnFunc f = liftA (printFileDocD "WriteLine") f

instance PermanenceSym CSharpCode where
    type Permanence CSharpCode = Doc
    static = return staticDocD
    dynamic = return dynamicDocD

instance BodySym CSharpCode where
    type Body CSharpCode = Doc
    body bs = liftList bodyDocD bs
    bodyStatements sts = block sts
    oneLiner s = bodyStatements [s]

    addComments s b = liftA2 (addCommentsDocD s) commentStart b

instance BlockSym CSharpCode where
    type Block CSharpCode = Doc
    block sts = lift1List blockDocD endStatement (map state sts)

instance StateTypeSym CSharpCode where
    type StateType CSharpCode = Doc
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

-- Translation outstanding for this instance
instance ControlBlockSym CSharpCode where
    runStrategy l strats rv av = 
        case Map.lookup l (Map.fromList strats) of Nothing -> error $ "Strategy '" ++ l ++ "': RunStrategy called on non-existent strategy."
                                                   Just b  -> liftA2 stratDocD b (state resultState)
        where resultState = case av of Nothing    -> return empty
                                       Just vari  -> case rv of Nothing  -> error $ "Strategy '" ++ l ++ "': Attempt to assign null return to a Value."
                                                                Just res -> assign vari res

    listSlice t vnew vold b e s = 
        let l_temp = "temp"
            v_temp = var l_temp
            l_i = "i_temp"
            v_i = var l_i
        in
        (body [
            block [(listDec l_temp 0 t)],
            for (varDecDef l_i (int) (getB b)) (v_i ?< getE e) (getS s v_i)
                (oneLiner $ valState $ v_temp $. (listAppend (vold $. (listAccess v_i)))),
            block [(vnew &= v_temp)]])
        where getB Nothing = litInt 0
              getB (Just n) = n
              getE Nothing = vold $. listSize
              getE (Just n) = n
              getS Nothing v = (&++) v
              getS (Just n) v = v &+= n

instance UnaryOpSym CSharpCode where
    type UnaryOp CSharpCode = Doc
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
    floorOp = return $ text "Math.Floor"
    ceilOp = return $ text "Math.Ceiling"

instance BinaryOpSym CSharpCode where
    type BinaryOp CSharpCode = Doc
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

instance ValueSym CSharpCode where
    type Value CSharpCode = Doc
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

    ($->) v vr = objVar v vr
    ($:) en e = enumElement en e

    const n = var n
    var n = return $ varDocD n
    extVar l n = return $ extVarDocD l n
    self = return $ selfDocD
    arg n = liftA2 argDocD (litInt n) argsList
    enumElement en e = return $ enumElemDocD en e
    enumVar n = var n
    objVar n1 n2 = liftA2 objVarDocD n1 n2
    objVarSelf n = liftA2 objVarDocD self (var n)
    listVar n _ = var n
    n `listOf` t = listVar n t
    
    inputFunc = return $ text "Console.ReadLine()"

    valName v = unCSC $ liftA render v

instance NumericExpression CSharpCode where
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
    floor v = liftA2 unOpDocD floorOp v
    ceil v = liftA2 unOpDocD ceilOp v

instance BooleanExpression CSharpCode where
    (?!) v = liftA2 unOpDocD notOp v
    (?&&) v1 v2 = liftA3 binOpDocD v1 andOp v2
    (?||) v1 v2 = liftA3 binOpDocD v1 orOp v2

    (?<)  v1 v2 = liftA3 binOpDocD v1 lessOp v2
    (?<=) v1 v2 = liftA3 binOpDocD v1 lessEqualOp v2
    (?>)  v1 v2 = liftA3 binOpDocD v1 greaterOp v2
    (?>=) v1 v2 = liftA3 binOpDocD v1 greaterEqualOp v2
    (?==) v1 v2 = liftA3 binOpDocD v1 equalOp v2
    (?!=) v1 v2 = liftA3 binOpDocD v1 notEqualOp v2
   
instance ValueExpression CSharpCode where
    inlineIf c v1 v2 = liftA3 inlineIfDocD c v1 v2
    funcApp n vs = liftList (funcAppDocD n) vs
    selfFuncApp = funcApp
    extFuncApp l n vs = liftList (extFuncAppDocD l n) vs
    stateObj t vs = liftA2 stateObjDocD t (liftList callFuncParamList vs)
    extStateObj _ t vs = stateObj t vs
    listStateObj t vs = liftA3 listStateObjDocD listObj t (liftList callFuncParamList vs)

    exists = notNull
    notNull v = liftA3 notNullDocD v notEqualOp (var "null")

instance Selector CSharpCode where
    objAccess v f = liftA2 objAccessDocD v f
    ($.) v f = objAccess v f

    objMethodCall o f ps = objAccess o (func f ps)
    objMethodCallVoid o f = objMethodCall o f []

    selfAccess f = objAccess self f

    listPopulateAccess _ _ = return empty
    listSizeAccess v = objAccess v listSize

    listIndexExists lst index = liftA3 listIndexExistsDocD lst greaterOp index
    argExists i = objAccess argsList (listAccess (litInt $ fromIntegral i))

    stringEqual v1 v2 = v1 ?== v2

    castObj f v = liftA2 castObjDocD f v
    castStrToFloat v = funcApp "Double.Parse" [v]

instance FunctionSym CSharpCode where
    type Function CSharpCode = Doc
    func l vs = liftA funcDocD (funcApp l vs)
    cast targT _ = liftA castDocD targT
    castListToInt = cast (listType static int) int
    get n = liftA funcDocD (funcApp (getterName n) [])
    set n v = liftA funcDocD (funcApp (setterName n) [v])

    indexOf v = liftA funcDocD (funcApp "IndexOf" [v])

    listSize = liftA funcDocD (funcApp "Count" [])
    listAdd i v = liftA funcDocD (funcApp "Insert" [i, v])
    listPopulateInt _ = return empty
    listPopulateFloat _ = return empty
    listPopulateChar _ = return empty
    listPopulateBool _ = return empty
    listPopulateString _ = return empty
    listAppend v = liftA funcDocD (funcApp "Add" [v])
    listExtendInt = liftA csListExtend defaultInt 
    listExtendFloat = liftA csListExtend defaultFloat 
    listExtendChar = liftA csListExtend defaultChar 
    listExtendBool = liftA csListExtend defaultBool
    listExtendString = liftA csListExtend defaultString
    listExtendList _ t = liftA csListExtendList t

    iterBegin = liftA funcDocD (funcApp "begin" [])
    iterEnd = liftA funcDocD (funcApp "end" [])

instance SelectorFunction CSharpCode where
    listAccess i = liftA listAccessDocD i
    listSet i v = liftA2 listSetDocD i v

    listAccessEnum t v = listAccess (castObj (cast int t) v)
    listSetEnum t i v = listSet (castObj (cast int t) i) v

    at l = listAccess (var l)

instance StatementSym CSharpCode where
    type Statement CSharpCode = Doc
    assign v1 v2 = liftA2 assignDocD v1 v2
    assignToListIndex lst index v = lst $. listSet index v
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

    varDec l t = liftA (varDecDocD l) t
    varDecDef l t v = liftA2 (varDecDefDocD l) t v
    listDec l n t = liftA2 (listDecDocD l) (litInt n) t -- this means that the type you declare must already be a list. Not sure how I feel about this. On the bright side, it also means you don't need to pass permanence
    listDecDef l t vs = lift1List (listDecDefDocD l) t vs
    objDecDef l t v = liftA2 (objDecDefDocD l) t v
    objDecNew l t vs = liftA2 (objDecDefDocD l) t (stateObj t vs)
    extObjDecNew l _ t vs = objDecNew l t vs
    objDecNewVoid l t = liftA2 (objDecDefDocD l) t (stateObj t [])
    extObjDecNewVoid l _ t = objDecNewVoid l t
    constDecDef l t v = liftA2 (constDecDefDocD l) t v

    print _ v = liftA2 outDocD printFunc v
    printLn _ v = liftA2 outDocD printLnFunc v
    printStr s = liftA2 outDocD printFunc (litString s)
    printStrLn s = liftA2 outDocD printLnFunc (litString s)

    printFile f _ v = liftA2 outDocD (printFileFunc f) v
    printFileLn f _ v = liftA2 outDocD (printFileLnFunc f) v
    printFileStr f s = liftA2 outDocD (printFileFunc f) (litString s)
    printFileStrLn f s = liftA2 outDocD (printFileLnFunc f) (litString s)

    printList t v = liftA4 printListDocD (state (printStr "[")) (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. (listSize)) #- (litInt 1))) ((&.++) "i") (bodyStatements [print t (v $. (listAccess (var "i"))), printStr ","])) (state (print t (v $. (listAccess ((v $. (listSize)) #- (litInt 1)))))) (printStr "]")
    printLnList t v = liftA4 printListDocD (state (printStr "[")) (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. (listSize)) #- (litInt 1))) ((&.++) "i") (bodyStatements [print t (v $. (listAccess (var "i"))), printStr ","])) (state (print t (v $. (listAccess ((v $. (listSize)) #- (litInt 1)))))) (printStrLn "]")
    printFileList f t v = liftA4 printListDocD (state (printFileStr f "[")) (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. (listSize)) #- (litInt 1))) ((&.++) "i") (bodyStatements [printFile f t (v $. (listAccess (var "i"))), printFileStr f ","])) (state (printFile f t (v $. (listAccess ((v $. (listSize)) #- (litInt 1)))))) (printFileStr f "]")
    printFileLnList f t v = liftA4 printListDocD (state (printFileStr f "[")) (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. (listSize)) #- (litInt 1))) ((&.++) "i") (bodyStatements [printFile f t (v $. (listAccess (var "i"))), printFileStr f ","])) (state (printFile f t (v $. (listAccess ((v $. (listSize)) #- (litInt 1)))))) (printFileStrLn f "]")

    getIntInput v = liftA2 (csInput "Int32.Parse") v inputFunc
    getFloatInput v = liftA2 (csInput "Double.Parse") v inputFunc
    getBoolInput _ = error "Boolean input not yet implemented for C#"
    getStringInput v = liftA2 (csInput "") v inputFunc
    getCharInput _ = error "Char input not yet implemented for C#"
    discardInput = liftA csDiscardInput inputFunc

    getIntFileInput f v = liftA2 (csInput "Int32.Parse") v (liftA csFileInput f)
    getFloatFileInput f v = liftA2 (csInput "Double.Parse") v (liftA   csFileInput f)
    getBoolFileInput _ _ = error "Boolean input not yet implemented for C#"
    getStringFileInput f v = liftA2 (csInput "") v (liftA csFileInput f)
    getCharFileInput _ _ = error "Char input not yet implemented for C#"
    discardFileInput f = liftA csFileInput f

    openFileR f n = liftA3 csOpenFile f n infile
    openFileW f n = liftA3 csOpenFile f n outfile
    closeFile f = valState $ objMethodCall f "Close" []

    getFileInputLine f v = getStringFileInput f v
    discardFileLine f = liftA csFileInput f
    stringSplit d vnew s = liftA2 csStringSplit vnew (s $. (func "Split" [litChar d]))

    break = return breakDocD
    continue = return continueDocD

    returnState v = liftA returnDocD v
    returnVar l = liftA returnDocD (var l)

    valState v = v

    comment cmt = liftA (commentDocD cmt) commentStart

    free _ = error "Cannot free variables in C#" -- could set variable to null? Might be misleading.

    throw errMsg = liftA csThrowDoc (litString errMsg)

    initState fsmName initialState = varDecDef fsmName string (litString initialState)
    changeState fsmName toState = fsmName &.= (litString toState)

    initObserverList t os = listDecDef observerListName t os
    addObserver t o = valState $ obsList $. listAdd lastelem o
        where obsList = observerListName `listOf` t
              lastelem = obsList $. listSize

    state s = liftA2 statementDocD s endStatement
    loopState s = liftA2 statementDocD s endStatementLoop
    multi s = lift1List multiStateDocD endStatement s

instance ControlStatementSym CSharpCode where
    ifCond bs b = lift4Pair ifCondDocD ifBodyStart elseIf blockEnd b bs
    ifNoElse bs = ifCond bs $ body []
    switch v cs c = lift3Pair switchDocD (state break) v c cs
    switchAsIf v cs c = ifCond cases c
        where cases = map (\(l, b) -> (v ?== l, b)) cs

    ifExists v ifBody elseBody = ifCond [(notNull v, ifBody)] elseBody

    for sInit vGuard sUpdate b = liftA6 forDocD blockStart blockEnd (loopState sInit) vGuard (loopState sUpdate) b
    forRange i initv finalv stepv b = for (varDecDef i int initv) ((var i) ?<= finalv) (i &.+= stepv) b
    forEach l t v b = liftA7 (forEachDocD l) blockStart blockEnd iterForEachLabel iterInLabel t v b
    while v b = liftA4 whileDocD blockStart blockEnd v b

    tryCatch tb cb = liftA2 csTryCatch tb cb

    checkState l cs c = switch (var l) cs c

    notifyObservers fn t ps = for initv (var index ?< (obsList $. listSize)) ((&.++) index) notify
        where obsList = observerListName `listOf` t
              index = "observerIndex"
              initv = varDecDef index int $ litInt 0
              notify = oneLiner $ valState $ (obsList $. at index) $. func fn ps

    getFileInputAll f v = while ((?!) f $. (func "EndOfStream" []))
        (oneLiner $ valState $ v $. (listAppend $ liftA csFileInput f))

instance ScopeSym CSharpCode where
    type Scope CSharpCode = Doc
    private = return privateDocD
    public = return publicDocD

    includeScope s = s

instance MethodTypeSym CSharpCode where
    type MethodType CSharpCode = Doc
    mState t = t
    void = return voidDocD
    construct n = return $ constructDocD n

instance ParameterSym CSharpCode where
    type Parameter CSharpCode = Doc
    stateParam n t = liftA (stateParamDocD n) t

instance MethodSym CSharpCode where
    type Method CSharpCode = Doc
    method n s p t ps b = liftA5 (methodDocD n) s p t (liftList (paramListDocD) ps) b
    getMethod n t = method (getterName n) public dynamic t [] getBody
        where getBody = oneLiner $ returnState (self $-> (var n))
    setMethod setLbl paramLbl t = method (setterName setLbl) public dynamic void [(stateParam paramLbl t)] setBody
        where setBody = oneLiner $ (self $-> (var setLbl)) &=. paramLbl
    mainMethod b = method "Main" public static void [return $ text "string[] args"] b
    privMethod n t ps b = method n private dynamic t ps b
    pubMethod n t ps b = method n public dynamic t ps b
    constructor n ps b = method n public dynamic (construct n) ps b

    function = method

instance StateVarSym CSharpCode where
    type StateVar CSharpCode = Doc
    stateVar _ l s p t = liftA4 (stateVarDocD l) (includeScope s) p t endStatement
    privMVar del l t = stateVar del l private dynamic t
    pubMVar del l t = stateVar del l public dynamic t
    pubGVar del l t = stateVar del l public static t

instance ClassSym CSharpCode where
    type Class CSharpCode = Doc
    buildClass n p s vs fs = liftA4 (classDocD n p) inherit s (liftList stateVarListDocD vs) (liftList methodListDocD fs)
    enum n es s = liftA2 (enumDocD n) (return $ enumElementsDocD es False) s
    mainClass n vs fs = buildClass n Nothing public vs fs
    privClass n p vs fs = buildClass n p private vs fs
    pubClass n p vs fs = buildClass n p public vs fs

instance ModuleSym CSharpCode where
    type Module CSharpCode = (Doc, Label)
    buildModule n _ _ _ cs = liftPairFst (liftList moduleDocD cs, n)

cstop :: Doc -> Doc -> Doc
cstop end inc = vcat [
    inc <+> text "System" <> end,
    inc <+> text "System.IO" <> end,
    inc <+> text "System.Collections" <> end,
    inc <+> text "System.Collections.Generic" <> end]

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
csInput it v inFn = v <+> equals <+> text it <> parens (inFn)

csFileInput :: Doc -> Doc
csFileInput f = f <> dot <> text "ReadLine()"

csOpenFile :: Doc -> Doc -> Doc -> Doc
csOpenFile f n rw = f <+> equals <+> new <+> rw <> parens n

csListExtend :: Doc -> Doc
csListExtend v = dot <> text "Add" <> parens v

csListExtendList :: Doc -> Doc
csListExtendList t = dot <> text "Add" <> parens (new <+> t <> parens (empty))

csStringSplit :: Doc -> Doc -> Doc
csStringSplit vnew s = vnew <+> equals <+> s