{-# LANGUAGE TypeFamilies #-}

-- | The logic to render C++ code is contained in this module
module Language.Drasil.Code.Imperative.LanguageRenderer.NewCppRenderer (
    -- * C++ Code Configuration -- defines syntax of all C++ code
    CppSrcCode(..), CppHdrCode(..)
) where

import Language.Drasil.Code.Imperative.New (Label,
    PackageSym(..), RenderSym(..), KeywordSym(..), PermanenceSym(..),
    BodySym(..), BlockSym(..), ControlBlockSym(..), StateTypeSym(..),
    UnaryOpSym(..), BinaryOpSym(..), ValueSym(..), NumericExpression(..), 
    BooleanExpression(..), ValueExpression(..), Selector(..), FunctionSym(..), 
    SelectorFunction(..), StatementSym(..), ControlStatementSym(..), ScopeSym(..),
    MethodTypeSym(..), ParameterSym(..), MethodSym(..), StateVarSym(..), 
    ClassSym(..), ModuleSym(..))
import Language.Drasil.Code.Imperative.NewLanguageRenderer (Terminator(..),
    fileDoc', enumElementsDocD, multiStateDocD, blockDocD, bodyDocD, 
    intTypeDocD, charTypeDocD, stringTypeDocD, typeDocD, listTypeDocD, voidDocD,
    constructDocD, stateParamDocD, paramListDocD, methodListDocD, stateVarDocD, 
    stateVarListDocD, alwaysDel, ifCondDocD, switchDocD, forDocD, whileDocD, 
    stratDocD, assignDocD, plusEqualsDocD, plusPlusDocD, varDecDocD, 
    varDecDefDocD, objDecDefDocD, constDecDefDocD, statementDocD,
    returnDocD, commentDocD, freeDocD, notOpDocD, negateOpDocD, sqrtOpDocD, 
    absOpDocD, expOpDocD, sinOpDocD, cosOpDocD, tanOpDocD, asinOpDocD, 
    acosOpDocD, atanOpDocD, unOpDocD, equalOpDocD, notEqualOpDocD, 
    greaterOpDocD, greaterEqualOpDocD, lessOpDocD, lessEqualOpDocD, plusOpDocD, 
    minusOpDocD, multOpDocD, divideOpDocD, moduloOpDocD, powerOpDocD, andOpDocD,
    orOpDocD, binOpDocD, binOpDocD', litTrueD, litFalseD, litCharD, litFloatD, 
    litIntD, litStringD, defaultCharD, defaultFloatD, defaultIntD, 
    defaultStringD, varDocD, selfDocD, argDocD, objVarDocD, inlineIfDocD, 
    funcAppDocD, funcDocD, castDocD, objAccessDocD, castObjDocD, breakDocD, 
    continueDocD, staticDocD, dynamicDocD, privateDocD, publicDocD, classDec, 
    dot, observerListName, doubleSlash, addCommentsDocD, callFuncParamList, 
    getterName, setterName, setEmpty)
import Language.Drasil.Code.Imperative.Helpers (angles, blank, doubleQuotedText,
  oneTab, oneTabbed, tripFst, tripSnd, tripThird, vibcat, liftA4, liftA5, 
  liftA6, liftA8, liftList, lift2Lists, lift1List, liftPair, lift3Pair, 
  lift4Pair, liftPairFst, liftTripFst, liftTrip)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor,const,log,exp)
import qualified Data.Map as Map (fromList,lookup)
import Control.Applicative (Applicative, liftA2, liftA3)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), braces, parens, comma,
  empty, equals, integer, semi, vcat, lbrace, rbrace, quotes, render, colon,
  isEmpty)

data CppCode a = CPPC {unSrc :: a, unHdr :: a}

instance PackageSym CppCode where
    type Package CppCode = ([(Doc, Label, Bool)], Label)
    packMods n ms = CPPC (unCPPSC $ 
        (packMods :: Label -> [CppSrcCode (RenderFile CppSrcCode)] ->
            CppSrcCode (Package CppSrcCode)) n (map (CPPSC . unSrc) ms)) 
        (unCPPHC $ (packMods :: Label -> [CppHdrCode (RenderFile CppHdrCode)] 
            -> CppHdrCode (Package CppHdrCode)) n (map (CPPHC . unHdr) ms))

instance RenderSym CppCode where
    type RenderFile CppCode = (Doc, Label, Bool)
    fileDoc code = CPPC (unCPPSC $ fileDoc code) (unCPPHC $ fileDoc code)
    top m = CPPC (unCPPSC $ top m) (unCPPHC $ top m)
    bottom = CPPC (unCPPSC bottom) (unCPPHC bottom)

instance KeywordSym CppCode where
    type Keyword CppCode = Doc
    endStatement = CPPC (unCPPSC endStatement) (unCPPHC endStatement)
    endStatementLoop = CPPC (unCPPSC endStatementLoop) (unCPPHC endStatementLoop)

    include n = CPPC (unCPPSC $ include n) (unCPPHC $ include n)
    inherit = CPPC (unCPPSC inherit) (unCPPHC inherit)

    list p = CPPC (unCPPSC $ list p) (unCPPHC $ list p)
    listObj = CPPC (unCPPSC listObj) (unCPPHC listObj)

    blockStart = CPPC (unCPPSC blockStart) (unCPPHC blockStart)
    blockEnd = CPPC (unCPPSC blockStart) (unCPPHC blockStart)

    ifBodyStart = CPPC (unCPPSC ifBodyStart) (unCPPHC ifBodyStart)
    elseIf = CPPC (unCPPSC elseIf) (unCPPHC elseIf)
    
    iterForEachLabel = CPPC (unCPPSC iterForEachLabel) (unCPPHC iterForEachLabel)
    iterInLabel = CPPC (unCPPSC iterInLabel) (unCPPHC iterInLabel)

    commentStart = CPPC (unCPPSC commentStart) (unCPPHC commentStart)
    
    printFunc = CPPC (unCPPSC printFunc) (unCPPHC printFunc)
    printLnFunc = CPPC (unCPPSC printLnFunc) (unCPPHC printLnFunc)
    printFileFunc v = CPPC (unCPPSC $ printFileFunc v) (unCPPHC $ printFileFunc v)
    printFileLnFunc v = CPPC (unCPPSC $ printFileLnFunc v) (unCPPHC $ printFileLnFunc v)

instance PermanenceSym CppCode where
    type Permanence CppCode = Doc
    static = CPPC (unCPPSC static) (unCPPHC static)
    dynamic = CPPC (unCPPSC dynamic) (unCPPHC dynamic)

instance BodySym CppCode where
    type Body CppCode = Doc
    body bs = CPPC (unCPPSC $ body bs) (unCPPHC $ body bs)
    bodyStatements sts = CPPC (unCPPSC $ bodyStatements sts) (unCPPHC $ bodyStatements sts)
    oneLiner s = CPPC (unCPPSC $ oneLiner s) (unCPPHC $ oneLiner s)

    addComments s b = CPPC (unCPPSC $ addComments s b) (unCPPHC $ addComments s b)

instance BlockSym CppCode where
    type Block CppCode = Doc
    block sts = CPPC (unCPPSC $ block sts) (unCPPHC $ block sts)

instance StateTypeSym CppCode where
    type StateType CppCode = Doc
    bool = CPPC (unCPPSC bool) (unCPPHC bool)
    int = CPPC (unCPPSC int) (unCPPHC int)
    float = CPPC (unCPPSC float) (unCPPHC float)
    char = CPPC (unCPPSC char) (unCPPHC char)
    string = CPPC (unCPPSC string) (unCPPHC string)
    infile = CPPC (unCPPSC infile) (unCPPHC infile)
    outfile = CPPC (unCPPSC outfile) (unCPPHC outfile)
    listType p st = CPPC (unCPPSC $ listType p st) (unCPPHC $ listType p st)
    intListType p = CPPC (unCPPSC $ intListType p) (unCPPHC $ intListType p)
    floatListType p = CPPC (unCPPSC $ floatListType p) (unCPPHC $ floatListType p)
    boolListType = CPPC (unCPPSC boolListType) (unCPPHC boolListType)
    obj t = CPPC (unCPPSC $ obj t) (unCPPHC $ obj t)
    enumType t = CPPC (unCPPSC $ enumType t) (unCPPHC $ enumType t)
    iterator t = CPPC (unCPPSC $ iterator t) (unCPPHC $ iterator t)

instance ControlBlockSym CppCode where
    runStrategy l strats rv av = CPPC (unCPPSC $ runStrategy l strats rv av) (unCPPHC $ runStrategy l strats rv av)

    listSlice t vnew vold b e s = CPPC (unCPPSC $ listSlice t vnew vold b e s) (unCPPHC $ listSlice t vnew vold b e s)

instance UnaryOpSym CppCode where
    type UnaryOp CppCode = Doc
    notOp = CPPC (unCPPSC notOp) (unCPPHC notOp)
    negateOp = CPPC (unCPPSC negateOp) (unCPPHC negateOp)
    sqrtOp = CPPC (unCPPSC sqrtOp) (unCPPHC sqrtOp)
    absOp = CPPC (unCPPSC absOp) (unCPPHC absOp)
    logOp = CPPC (unCPPSC logOp) (unCPPHC logOp)
    lnOp = CPPC (unCPPSC lnOp) (unCPPHC lnOp)
    expOp = CPPC (unCPPSC expOp) (unCPPHC expOp)
    sinOp = CPPC (unCPPSC sinOp) (unCPPHC sinOp)
    cosOp = CPPC (unCPPSC cosOp) (unCPPHC cosOp)
    tanOp = CPPC (unCPPSC tanOp) (unCPPHC tanOp)
    asinOp = CPPC (unCPPSC asinOp) (unCPPHC asinOp)
    acosOp = CPPC (unCPPSC acosOp) (unCPPHC acosOp)
    atanOp = CPPC (unCPPSC atanOp) (unCPPHC atanOp)
    floorOp = CPPC (unCPPSC floorOp) (unCPPHC floorOp)
    ceilOp = CPPC (unCPPSC ceilOp) (unCPPHC ceilOp)

instance BinaryOpSym CppCode where
    type BinaryOp CppCode = Doc
    equalOp = CPPC (unCPPSC equalOp) (unCPPHC equalOp)
    notEqualOp = CPPC (unCPPSC notEqualOp) (unCPPHC notEqualOp)
    greaterOp = CPPC (unCPPSC greaterOp) (unCPPHC greaterOp)
    greaterEqualOp = CPPC (unCPPSC greaterEqualOp) (unCPPHC greaterEqualOp)
    lessOp = CPPC (unCPPSC lessOp) (unCPPHC lessOp)
    lessEqualOp = CPPC (unCPPSC lessEqualOp) (unCPPHC lessEqualOp)
    plusOp = CPPC (unCPPSC plusOp) (unCPPHC plusOp)
    minusOp = CPPC (unCPPSC minusOp) (unCPPHC minusOp)
    multOp = CPPC (unCPPSC multOp) (unCPPHC multOp)
    divideOp = CPPC (unCPPSC divideOp) (unCPPHC divideOp)
    powerOp = CPPC (unCPPSC powerOp) (unCPPHC powerOp)
    moduloOp = CPPC (unCPPSC moduloOp) (unCPPHC moduloOp)
    andOp = CPPC (unCPPSC andOp) (unCPPHC andOp)
    orOp = CPPC (unCPPSC orOp) (unCPPHC orOp)

instance ValueSym CppCode where
    type Value CppCode = (Doc, Maybe String)
    litTrue = CPPC (unCPPSC litTrue) (unCPPHC litTrue)
    litFalse = CPPC (unCPPSC litFalse) (unCPPHC litFalse)
    litChar c = CPPC (unCPPSC $ litChar c) (unCPPHC $ litChar c)
    litFloat v = CPPC (unCPPSC $ litFloat v) (unCPPHC $ litFloat v)
    litInt v = CPPC (unCPPSC $ litInt v) (unCPPHC $ litInt v)
    litString s = CPPC (unCPPSC $ litString s) (unCPPHC $ litString s)

    defaultChar = CPPC (unCPPSC defaultChar) (unCPPHC defaultChar)
    defaultFloat = CPPC (unCPPSC defaultFloat) (unCPPHC defaultFloat)
    defaultInt = CPPC (unCPPSC defaultInt) (unCPPHC defaultInt)
    defaultString = CPPC (unCPPSC defaultString) (unCPPHC defaultString)
    defaultBool = CPPC (unCPPSC defaultBool) (unCPPHC defaultBool)

    ($->) v1 v2 = CPPC (unCPPSC $ ($->) v1 v2) (unCPPHC $ ($->) v1 v2)
    ($:) l1 l2 = CPPC (unCPPSC $ ($:) l1 l2) (unCPPHC $ ($:) l1 l2)

    const n = CPPC (unCPPSC $ const n) (unCPPHC $ const n)
    var n = CPPC (unCPPSC $ var n) (unCPPHC $ var n)
    extVar l n = CPPC (unCPPSC $ extVar l n) (unCPPHC $ extVar l n)
    self = CPPC (unCPPSC self) (unCPPHC self)
    arg n = CPPC (unCPPSC $ litInt v) (unCPPHC $ litInt v)
    enumElement en e = CPPC (unCPPSC $ enumElement en e) (unCPPHC $ enumElement en e)
    enumVar n = CPPC (unCPPSC $ enumVar n) (unCPPHC $ enumVar n)
    objVar o v = CPPC (unCPPSC $ objVar o v) (unCPPHC $ objVar o v)
    objVarSelf n = CPPC (unCPPSC $ objVarSelf n) (unCPPHC $ objVarSelf n)
    listVar n t = CPPC (unCPPSC $ listVar n t) (unCPPHC $ listVar n t)
    n `listOf` t = CPPC (unCPPSC $ n `listOf` t) (unCPPHC $ n `listOf` t)
    iterVar l = CPPC (unCPPSC $ iterVar l) (unCPPHC $ iterVar l)
    
    inputFunc = CPPC (unCPPSC inputFunc) (unCPPHC inputFunc)
    argsList = CPPC (unCPPSC argsList) (unCPPHC argsList)

    valName v = valName (v :: CppSrcCode (Value CppSrcCode))

instance NumericExpression CppCode where
    (#~) v = CPPC (unCPPSC $ (#~) v) (unCPPHC $ (#~) v)
    (#/^) v = CPPC (unCPPSC $ (#/^) v) (unCPPHC $ (#/^) v)
    (#|) v = CPPC (unCPPSC $ (#|) v) (unCPPHC $ (#|) v)
    (#+) v1 v2 = CPPC (unCPPSC $ (#+) v1 v2) (unCPPHC $ (#+) v1 v2)
    (#-) v1 v2 = CPPC (unCPPSC $ (#-) v1 v2) (unCPPHC $ (#-) v1 v2)
    (#*) v1 v2 = CPPC (unCPPSC $ (#*) v1 v2) (unCPPHC $ (#*) v1 v2)
    (#/) v1 v2 = CPPC (unCPPSC $ (#/) v1 v2) (unCPPHC $ (#/) v1 v2)
    (#%) v1 v2 = CPPC (unCPPSC $ (#%) v1 v2) (unCPPHC $ (#%) v1 v2)
    (#^) v1 v2 = CPPC (unCPPSC $ (#^) v1 v2) (unCPPHC $ (#^) v1 v2)

    log v = CPPC (unCPPSC $ log v) (unCPPHC $ log v)
    ln v = CPPC (unCPPSC $ ln v) (unCPPHC $ ln v)
    exp v = CPPC (unCPPSC $ exp v) (unCPPHC $ exp v)
    sin v = CPPC (unCPPSC $ sin v) (unCPPHC $ sin v)
    cos v = CPPC (unCPPSC $ cos v) (unCPPHC $ cos v)
    tan v = CPPC (unCPPSC $ tan v) (unCPPHC $ tan v)
    csc v = CPPC (unCPPSC $ csc v) (unCPPHC $ csc v)
    sec v = CPPC (unCPPSC $ sec v) (unCPPHC $ sec v)
    cot v = CPPC (unCPPSC $ cot v) (unCPPHC $ cot v)
    arcsin v = CPPC (unCPPSC $ arcsin v) (unCPPHC $ arcsin v)
    arccos v = CPPC (unCPPSC $ arccos v) (unCPPHC $ arccos v)
    arctan v = CPPC (unCPPSC $ arctan v) (unCPPHC $ arctan v)
    floor v = CPPC (unCPPSC $ floor v) (unCPPHC $ floor v)
    ceil v = CPPC (unCPPSC $ ceil v) (unCPPHC $ ceil v)

instance BooleanExpression CppCode where
    (?!) v = CPPC (unCPPSC $ (?!) v) (unCPPHC $ (?!) v)
    (?&&) v1 v2 = CPPC (unCPPSC $ (?&&) v1 v2) (unCPPHC $ (?&&) v1 v2)
    (?||) v1 v2 = CPPC (unCPPSC $ (?||) v1 v2) (unCPPHC $ (?||) v1 v2)

    (?<) v1 v2 = CPPC (unCPPSC $ (?<) v1 v2) (unCPPHC $ (?<) v1 v2)
    (?<=) v1 v2 = CPPC (unCPPSC $ (?<=) v1 v2) (unCPPHC $ (?<=) v1 v2)
    (?>) v1 v2 = CPPC (unCPPSC $ (?>) v1 v2) (unCPPHC $ (?>) v1 v2)
    (?>=) v1 v2 = CPPC (unCPPSC $ (?>=) v1 v2) (unCPPHC $ (?>=) v1 v2)
    (?==) v1 v2 = CPPC (unCPPSC $ (?==) v1 v2) (unCPPHC $ (?==) v1 v2)
    (?!=) v1 v2 = CPPC (unCPPSC $ (?!=) v1 v2) (unCPPHC $ (?!=) v1 v2)
    
instance ValueExpression CppCode where
    inlineIf b v1 v2 = CPPC (unCPPSC $ inlineIf b v1 v2) (unCPPHC $ inlineIf b v1 v2)
    funcApp n vs = CPPC (unCPPSC $ funcApp n vs) (unCPPHC $ funcApp n vs)
    selfFuncApp n vs = CPPC (unCPPSC $ selfFuncApp n vs) (unCPPHC $ selfFuncApp n vs)
    extFuncApp l n vs = CPPC (unCPPSC $ extFuncApp l n vs) (unCPPHC $ extFuncApp l n vs)
    stateObj t vs = CPPC (unCPPSC $ stateObj t vs) (unCPPHC $ stateObj t vs)
    extStateObj l t vs = CPPC (unCPPSC $ extStateObj l t vs) (unCPPHC $ extStateObj l t vs)
    listStateObj t vs = CPPC (unCPPSC $ listStateObj t vs) (unCPPHC $ listStateObj t vs)

    exists v = CPPC (unCPPSC $ exists v) (unCPPHC $ exists v)
    notNull v = CPPC (unCPPSC $ notNull v) (unCPPHC $ notNull v)

instance Selector CppCode where
    objAccess v f = CPPC (unCPPSC $ objAccess v f) (unCPPHC $ objAccess v f)
    ($.) v f = CPPC (unCPPSC $ ($.) v f) (unCPPHC $ ($.) v f)

    objMethodCall o f ps = CPPC (unCPPSC $ objMethodCall o f ps) (unCPPHC $ objMethodCall o f ps)
    objMethodCallVoid o f = CPPC (unCPPSC $ objMethodCallVoid o f) (unCPPHC $ objMethodCallVoid o f)

    selfAccess f = CPPC (unCPPSC $ selfAccess f) (unCPPHC $ selfAccess f)

    listPopulateAccess v f = CPPC (unCPPSC $ listPopulateAccess v f) (unCPPHC $ listPopulateAccess v f)
    listSizeAccess v = CPPC (unCPPSC $ listSizeAccess v) (unCPPHC $ listSizeAccess v)

    listIndexExists v i = CPPC (unCPPSC $ listIndexExists v i) (unCPPHC $ listIndexExists v i)
    argExists i = CPPC (unCPPSC $ argExists i) (unCPPHC $ argExists i)
    
    indexOf l v = CPPC (unCPPSC $ indexOf l v) (unCPPHC $ indexOf l v)

    stringEqual v1 v2 = CPPC (unCPPSC $ stringEqual v1 v2) (unCPPHC $ stringEqual v1 v2)

    castObj f v = CPPC (unCPPSC $ castObj f v) (unCPPHC $ castObj f v)
    castStrToFloat v = CPPC (unCPPSC $ castStrToFloat v) (unCPPHC $ castStrToFloat v)

instance FunctionSym CppCode where
    type Function CppCode = Doc
    func l vs = CPPC (unCPPSC $ func l vs) (unCPPHC $ func l vs)
    cast targT srcT = CPPC (unCPPSC $ cast targT srcT) (unCPPHC $ cast targT srcT)
    castListToInt = CPPC (unCPPSC castListToInt) (unCPPHC castListToInt)
    get n = CPPC (unCPPSC $ get n) (unCPPHC $ get n)
    set n v = CPPC (unCPPSC $ set n v) (unCPPHC $ set n v)

    listSize = CPPC (unCPPSC listSize) (unCPPHC listSize)
    listAdd i v = CPPC (unCPPSC $ listAdd i v) (unCPPHC $ listAdd i v)
    listPopulateInt v = CPPC (unCPPSC $ listPopulateInt v) (unCPPHC $ listPopulateInt v)
    listPopulateFloat v = CPPC (unCPPSC $ listPopulateFloat v) (unCPPHC $ listPopulateFloat v)
    listPopulateChar v = CPPC (unCPPSC $ listPopulateChar v) (unCPPHC $ listPopulateChar v)
    listPopulateBool v = CPPC (unCPPSC $ listPopulateBool v) (unCPPHC $ listPopulateBool v)
    listPopulateString v = CPPC (unCPPSC $ listPopulateString v) (unCPPHC $ listPopulateString v)
    listAppend v = CPPC (unCPPSC $ listAppend v) (unCPPHC $ listAppend v)
    listExtendInt = CPPC (unCPPSC listExtendInt) (unCPPHC listExtendInt)
    listExtendFloat = CPPC (unCPPSC listExtendFloat) (unCPPHC listExtendFloat)
    listExtendChar = CPPC (unCPPSC listExtendChar) (unCPPHC listExtendChar)
    listExtendBool = CPPC (unCPPSC listExtendBool) (unCPPHC listExtendBool)
    listExtendString = CPPC (unCPPSC listExtendString) (unCPPHC listExtendString)
    listExtendList i t = CPPC (unCPPSC $ listExtendList i t) (unCPPHC $ listExtendList i t)

    iterBegin = CPPC (unCPPSC iterBegin) (unCPPHC iterBegin)
    iterEnd = CPPC (unCPPSC iterEnd) (unCPPHC iterEnd)

instance SelectorFunction CppCode where
    listAccess v = CPPC (unCPPSC $ listAccess v) (unCPPHC $ listAccess v)
    listSet i v = CPPC (unCPPSC $ listSet i v) (unCPPHC $ listSet i v)

    listAccessEnum t v = CPPC (unCPPSC $ listAccessEnum t v) (unCPPHC $ listAccessEnum t v)
    listSetEnum t i v = CPPC (unCPPSC $ listSetEnum t i v) (unCPPHC $ listSetEnum t i v)

    at l = CPPC (unCPPSC $ at l) (unCPPHC $ at l)

instance StatementSym CppCode where
    type Statement CppCode = (Doc, Terminator)
    assign v1 v2 = CPPC (unCPPSC $ assign v1 v2) (unCPPHC $ assign v1 v2)
    assignToListIndex lst index v = CPPC (unCPPSC $ assignToListIndex lst index v) (unCPPHC $ assignToListIndex lst index v)
    (&=) v1 v2 = CPPC (unCPPSC $ (&=) v1 v2) (unCPPHC $ (&=) v1 v2)
    (&.=) l v = CPPC (unCPPSC $ (&.=) l v) (unCPPHC $ (&.=) l v)
    (&=.) v l = CPPC (unCPPSC $ (&=.) v l) (unCPPHC $ (&=.) v l)
    (&-=) v1 v2 = CPPC (unCPPSC $ (&-=) v1 v2) (unCPPHC $ (&-=) v1 v2)
    (&.-=) l v = CPPC (unCPPSC $ (&.-=) l v) (unCPPHC $ (&.-=) l v)
    (&+=) v1 v2 = CPPC (unCPPSC $ (&+=) v1 v2) (unCPPHC $ (&+=) v1 v2)
    (&.+=) l v = CPPC (unCPPSC $ (&.+=) l v) (unCPPHC $ (&.+=) l v)
    (&++) v = CPPC (unCPPSC $ (&++) v) (unCPPHC $ (&++) v)
    (&.++) l = CPPC (unCPPSC $ (&.++) l) (unCPPHC $ (&.++) l)
    (&~-) v = CPPC (unCPPSC $ (&~-) v) (unCPPHC $ (&~-) v)
    (&.~-) l = CPPC (unCPPSC $ (&.~-) l) (unCPPHC $ (&.~-) l)

    varDec l t = CPPC (unCPPSC $ varDec l t) (unCPPHC $ varDec l t)
    varDecDef l t v = CPPC (unCPPSC $ varDecDef l t v) (unCPPHC $ varDecDef l t v)
    listDec l n t = CPPC (unCPPSC $ listDec l n t) (unCPPHC $ listDec l n t)
    listDecDef l t vs = CPPC (unCPPSC $ listDecDef l t vs) (unCPPHC $ listDecDef l t vs)
    objDecDef l t v = CPPC (unCPPSC $ objDecDef l t v) (unCPPHC $ objDecDef l t v)
    objDecNew l t vs = CPPC (unCPPSC $ objDecNew l t vs) (unCPPHC $ objDecNew l t vs)
    extObjDecNew l lib t vs = CPPC (unCPPSC $ extObjDecNew l lib t vs) (unCPPHC $ extObjDecNew l lib t vs)
    objDecNewVoid l t = CPPC (unCPPSC $ objDecNewVoid l t) (unCPPHC $ objDecNewVoid l t)
    extObjDecNewVoid l lib t = CPPC (unCPPSC $ extObjDecNewVoid l lib t) (unCPPHC $ extObjDecNewVoid l lib t)
    constDecDef l t v = CPPC (unCPPSC $ constDecDef l t v) (unCPPHC $ constDecDef l t v)

    print t v = CPPC (unCPPSC $ print t v) (unCPPHC $ print t v)
    printLn t v = CPPC (unCPPSC $ printLn t v) (unCPPHC $ printLn t v)
    printStr s = CPPC (unCPPSC $ printStr s) (unCPPHC $ printStr s)
    printStrLn s = CPPC (unCPPSC $ printStrLn s) (unCPPHC $ printStrLn s)

    printFile f t v = CPPC (unCPPSC $ printFile f t v) (unCPPHC $ printFile f t v)
    printFileLn f t v = CPPC (unCPPSC $ printFileLn f t v) (unCPPHC $ printFileLn f t v)
    printFileStr f s =CPPC (unCPPSC $ printFileStr f s) (unCPPHC $ printFileStr f s)
    printFileStrLn f s = CPPC (unCPPSC $ printFileStrLn f s) (unCPPHC $ printFileStrLn f s)

    printList t v = CPPC (unCPPSC $ printList t v) (unCPPHC $ printList t v)
    printLnList t v = CPPC (unCPPSC $ printLnList t v) (unCPPHC $ printLnList t v)
    printFileList f t v = CPPC (unCPPSC $ printFileList f t v) (unCPPHC $ printFileList f t v)
    printFileLnList f t v = CPPC (unCPPSC $ printFileLnList f t v) (unCPPHC $ printFileLnList f t v)

    getIntInput v = CPPC (unCPPSC $ getIntInput v) (unCPPHC $ getIntInput v)
    getFloatInput v = CPPC (unCPPSC $ getFloatInput v) (unCPPHC $ getFloatInput v)
    getBoolInput v = CPPC (unCPPSC $ getBoolInput v) (unCPPHC $ getBoolInput v)
    getStringInput v = CPPC (unCPPSC $ getStringInput v) (unCPPHC $ getStringInput v)
    getCharInput v = CPPC (unCPPSC $ getCharInput v) (unCPPHC $ getCharInput v)
    discardInput = CPPC (unCPPSC discardInput) (unCPPHC discardInput)

    getIntFileInput f v = CPPC (unCPPSC $ getIntFileInput f v) (unCPPHC $ getIntFileInput f v)
    getFloatFileInput f v = CPPC (unCPPSC $ getFloatFileInput f v) (unCPPHC $ getFloatFileInput f v)
    getBoolFileInput f v = CPPC (unCPPSC $ getBoolFileInput f v) (unCPPHC $ getBoolFileInput f v)
    getStringFileInput f v = CPPC (unCPPSC $ getStringFileInput f v) (unCPPHC $ getStringFileInput f v)
    getCharFileInput f v = CPPC (unCPPSC $ getCharFileInput f v) (unCPPHC $ getCharFileInput f v)
    discardFileInput f = CPPC (unCPPSC $ discardFileInput f) (unCPPHC $ discardFileInput f)

    openFileR f n = CPPC (unCPPSC $ openFileR f n) (unCPPHC $ openFileR f n)
    openFileW f n = CPPC (unCPPSC $ openFileW f n) (unCPPHC $ openFileW f n)
    openFileA f n = CPPC (unCPPSC $ openFileA f n) (unCPPHC $ openFileA f n)
    closeFile f = CPPC (unCPPSC $ closeFile f) (unCPPHC $ closeFile f)

    getFileInputLine f v = CPPC (unCPPSC $ getFileInputLine f v) (unCPPHC $ getFileInputLine f v)
    discardFileLine f = CPPC (unCPPSC $ discardFileLine f) (unCPPHC $ discardFileLine f)
    stringSplit d vnew s = CPPC (unCPPSC $ stringSplit d vnew s) (unCPPHC $ stringSplit d vnew s)

    break = CPPC (unCPPSC break) (unCPPHC break)
    continue = CPPC (unCPPSC continue) (unCPPHC continue)

    returnState v = CPPC (unCPPSC $ returnState v) (unCPPHC $ returnState v)
    returnVar l = CPPC (unCPPSC $ returnVar l) (unCPPHC $ returnVar l)

    valState v = CPPC (unCPPSC $ valState v) (unCPPHC $ valState v)

    comment cmt = CPPC (unCPPSC $ comment cmt) (unCPPHC $ comment cmt)

    free v = CPPC (unCPPSC $ free v) (unCPPHC $ free v)

    throw errMsg = CPPC (unCPPSC $ throw errMsg) (unCPPHC $ throw errMsg)

    initState fsmName initialState = CPPC (unCPPSC $ initState fsmName initialState) (unCPPHC $ initState fsmName initialState)
    changeState fsmName toState = CPPC (unCPPSC $ changeState fsmName toState) (unCPPHC $ changeState fsmName toState)

    initObserverList t vs = CPPC (unCPPSC $ initObserverList t vs) (unCPPHC $ initObserverList t vs)
    addObserver t o = CPPC (unCPPSC $ addObserver t o) (unCPPHC $ addObserver t o)

    state s = CPPC (unCPPSC $ state s) (unCPPHC $ state s)
    loopState s = CPPC (unCPPSC $ loopState s) (unCPPHC $ loopState s)
    multi ss = CPPC (unCPPSC $ multi ss) (unCPPHC $ multi ss)

instance ControlStatementSym CppCode where
    ifCond bs b = CPPC (unCPPSC $ ifCond bs b) (unCPPHC $ ifCond bs b)
    ifNoElse bs = CPPC (unCPPSC $ ifNoElse bs) (unCPPHC $ ifNoElse bs)
    switch v cs c = CPPC (unCPPSC $ switch v cs c) (unCPPHC $ switch v cs c)
    switchAsIf v cs = CPPC (unCPPSC $ switchAsIf v cs) (unCPPHC $ switchAsIf v cs)

    ifExists cond ifBody elseBody = CPPC (unCPPSC $ ifExists cond ifBody elseBody) (unCPPHC $ ifExists cond ifBody elseBody)

    for sInit vGuard sUpdate b = CPPC (unCPPSC $ for sInit vGuard sUpdate b) (unCPPHC $ for sInit vGuard sUpdate b)
    forRange i initv finalv stepv b = CPPC (unCPPSC $ forRange i initv finalv stepv b) (unCPPHC $ forRange i initv finalv stepv b)
    forEach l t v b = CPPC (unCPPSC $ forEach l t v b) (unCPPHC $ forEach l t v b)
    while v b = CPPC (unCPPSC $ while v b) (unCPPHC $ while v b)

    tryCatch tb cb = CPPC (unCPPSC $ tryCatch tb cb) (unCPPHC $ tryCatch tb cb)

    checkState l vs b = CPPC (unCPPSC $ checkState l vs b) (unCPPHC $ checkState l vs b)

    notifyObservers fn t ps = CPPC (unCPPSC $ notifyObservers fn t ps) (unCPPHC $ notifyObservers fn t ps)

    getFileInputAll f v = CPPC (unCPPSC $ getFileInputAll f v) (unCPPHC $ getFileInputAll f v)

instance ScopeSym CppCode where
    type Scope CppCode = Doc
    private = CPPC (unCPPSC private) (unCPPHC private)
    public = CPPC (unCPPSC public) (unCPPHC public)

    includeScope s = CPPC (unCPPSC $ includeScope s) (unCPPHC $ includeScope s)

instance MethodTypeSym CppCode where
    type MethodType CppCode = Doc
    mState t = CPPC (unCPPSC $ mState t) (unCPPHC $ mState t)
    void = CPPC (unCPPSC void) (unCPPHC void)
    construct n = CPPC (unCPPSC $ construct n) (unCPPHC $ construct n)

instance ParameterSym CppCode where
    type Parameter CppCode = Doc
    stateParam n t = CPPC (unCPPSC $ stateParam n t) (unCPPHC $ stateParam n t)
    pointerParam n t = CPPC (unCPPSC $ pointerParam n t) (unCPPHC $ pointerParam n t)

instance MethodSym CppCode where
    -- Bool is True if the method is a main method, False otherwise
    type Method CppCode = (Doc, Bool)
    method n c s p t ps b = CPPC (unCPPSC $ method n c s p t ps b) (unCPPHC $ method n c s p t ps b)
    getMethod n c t = CPPC (unCPPSC $ getMethod n c t) (unCPPHC $ getMethod n c t)
    setMethod setLbl c paramLbl t = CPPC (unCPPSC $ setMethod setLbl c paramLbl t) (unCPPHC $ setMethod setLbl c paramLbl t)
    mainMethod l b = CPPC (unCPPSC $ mainMethod l b) (unCPPHC $ mainMethod l b)
    privMethod n c t ps b = CPPC (unCPPSC $ privMethod n c t ps b) (unCPPHC $ privMethod n c t ps b)
    pubMethod n c t ps b = CPPC (unCPPSC $ pubMethod n c t ps b) (unCPPHC $ pubMethod n c t ps b)
    constructor n ps b = CPPC (unCPPSC $ constructor n ps b) (unCPPHC $ constructor n ps b)
    destructor n vs = CPPC (unCPPSC $ destructor n vs) (unCPPHC $ destructor n vs)

    function n s p t ps b = CPPC (unCPPSC $ function n s p t ps b) (unCPPHC $ function n s p t ps b)

instance StateVarSym CppCode where
    -- (Doc, Bool) is the corresponding destructor code for the stateVar
    type StateVar CppCode = (Doc, (Doc, Terminator))
    stateVar del l s p t = CPPC (unCPPSC $ stateVar del l s p t) (unCPPHC $ stateVar del l s p t)
    privMVar del l t = CPPC (unCPPSC $ privMVar del l t) (unCPPHC $ privMVar del l t)
    pubMVar del l t = CPPC (unCPPSC $ pubMVar del l t) (unCPPHC $ pubMVar del l t)
    pubGVar del l t = CPPC (unCPPSC $ pubGVar del l t) (unCPPHC $ pubGVar del l t)
    listStateVar del l s p t = CPPC (unCPPSC $ listStateVar del l s p t) (unCPPHC $ listStateVar del l s p t)

instance ClassSym CppCode where
    -- Bool is True if the class is a main class, False otherwise
    type Class CppCode = (Doc, Bool)
    buildClass n p s vs fs = CPPC (unCPPSC $ buildClass n p s vs fs) (unCPPHC $ buildClass n p s vs fs)
    enum l ls s = CPPC (unCPPSC $ enum l ls s) (unCPPHC $ enum l ls s)
    mainClass l vs fs = CPPC (unCPPSC $ mainClass l vs fs) (unCPPHC $ mainClass l vs fs)
    privClass n p vs fs = CPPC (unCPPSC $ privClass n p vs fs) (unCPPHC $ privClass n p vs fs)
    pubClass n p vs fs = CPPC (unCPPSC $ pubClass n p vs fs) (unCPPHC $ pubClass n p vs fs)

instance ModuleSym CppCode where
    -- Label is module name
    -- Bool is True if the method is a main method, False otherwise
    type Module CppCode = (Doc, Label, Bool)
    buildModule n l vs ms cs = CPPC (unCPPSC $ buildModule n l vs ms cs) (unCPPHC $ buildModule n l vs ms cs)

-----------------
-- Source File --
-----------------

newtype CppSrcCode a = CPPSC {unCPPSC :: a}

instance Functor CppSrcCode where
    fmap f (CPPSC x) = CPPSC (f x)

instance Applicative CppSrcCode where
    pure = CPPSC
    (CPPSC f) <*> (CPPSC x) = CPPSC (f x)

instance Monad CppSrcCode where
    return = CPPSC
    CPPSC x >>= f = f x

instance PackageSym CppSrcCode where
    type Package CppSrcCode = ([(Doc, Label, Bool)], Label)
    packMods n ms = liftPairFst (sequence ms, n)

instance RenderSym CppSrcCode where
    type RenderFile CppSrcCode = (Doc, Label, Bool)
    fileDoc code = liftTripFst (liftA3 fileDoc' (top code) (fmap tripFst code) bottom, tripSnd $ unCPPSC code, tripThird $ unCPPSC code)
    top m = liftA3 cppstop m (list dynamic) endStatement
    bottom = return empty

instance KeywordSym CppSrcCode where
    type Keyword CppSrcCode = Doc
    endStatement = return semi
    endStatementLoop = return empty

    include n = return $ text "#include" <+> doubleQuotedText (n ++ cppHeaderExt)
    inherit = return colon

    list _ = return $ text "vector"
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
    printFileFunc = fmap fst -- is this right?
    printFileLnFunc = fmap fst

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
    bool = return $ cppBoolTypeDoc
    int = return $ intTypeDocD
    float = return $ cppFloatTypeDoc
    char = return $ charTypeDocD
    string = return $ stringTypeDocD
    infile = return $ cppInfileTypeDoc
    outfile = return $ cppOutfileTypeDoc
    listType p st = liftA2 listTypeDocD st (list p)
    intListType p = listType p int
    floatListType p = listType p float
    boolListType = listType dynamic bool
    obj t = return $ typeDocD t
    enumType t = return $ typeDocD t
    iterator t = fmap cppIterTypeDoc (listType dynamic t)

instance ControlBlockSym CppSrcCode where
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
        block [
            (listDec l_temp 0 t),
            for (varDecDef l_i int (getB b)) (v_i ?< getE e) (getS s v_i)
                (oneLiner $ valState $ v_temp $. (listAppend (vold $. (listAccess v_i)))),
            (vnew &= v_temp)]
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
    sqrtOp = return $ sqrtOpDocD
    absOp = return $ absOpDocD
    logOp = return $ text "log10"
    lnOp = return $ text "log"
    expOp = return $ expOpDocD
    sinOp = return $ sinOpDocD
    cosOp = return $ cosOpDocD
    tanOp = return $ tanOpDocD
    asinOp = return $ asinOpDocD
    acosOp = return $ acosOpDocD
    atanOp = return $ atanOpDocD
    floorOp = return $ text "floor"
    ceilOp = return $ text "ceil"

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
    powerOp = return $ powerOpDocD
    moduloOp = return $ moduloOpDocD
    andOp = return $ andOpDocD
    orOp = return $ orOpDocD

instance ValueSym CppSrcCode where
    type Value CppSrcCode = (Doc, Maybe String)
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
    extVar _ = var
    self = return $ (selfDocD, Just "this")
    arg n = liftPairFst (liftA2 argDocD (litInt (n+1)) argsList, Nothing)
    enumElement _ e = return $ (text e, Just e)
    enumVar = var
    objVar o v = liftPairFst (liftA2 objVarDocD o v, Just $ valName o ++ "." ++ valName v)
    objVarSelf = var
    listVar n _ = var n
    n `listOf` t = listVar n t
    iterVar l = return $ (text $ "(*" ++ l ++ ")", Nothing)
    
    inputFunc = return $ (text "std::cin", Nothing)
    argsList = return $ (text "argv", Nothing)

    valName (CPPSC (v, s)) = case s of Nothing -> error $ "Attempt to print unprintable Value (" ++ render v ++ ")"
                                       Just valstr -> valstr

instance NumericExpression CppSrcCode where
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

instance BooleanExpression CppSrcCode where
    (?!) v = liftPairFst (liftA2 unOpDocD notOp v, Nothing)
    (?&&) v1 v2 = liftPairFst (liftA3 binOpDocD andOp v1 v2, Nothing)
    (?||) v1 v2 = liftPairFst (liftA3 binOpDocD orOp v1 v2, Nothing)

    (?<) v1 v2 = liftPairFst (liftA3 binOpDocD lessOp v1 v2, Nothing)
    (?<=) v1 v2 = liftPairFst (liftA3 binOpDocD lessEqualOp v1 v2, Nothing)
    (?>) v1 v2 = liftPairFst (liftA3 binOpDocD greaterOp v1 v2, Nothing)
    (?>=) v1 v2 = liftPairFst (liftA3 binOpDocD greaterEqualOp v1 v2, Nothing)
    (?==) v1 v2 = liftPairFst (liftA3 binOpDocD equalOp v1 v2, Nothing)
    (?!=) v1 v2 = liftPairFst (liftA3 binOpDocD notEqualOp v1 v2, Nothing)
   
instance ValueExpression CppSrcCode where
    inlineIf b v1 v2 = liftPairFst (liftA3 inlineIfDocD b v1 v2, Nothing)
    funcApp n vs = liftPairFst (liftList (funcAppDocD n) vs, Nothing)
    selfFuncApp = funcApp
    extFuncApp _ = funcApp
    stateObj t vs = liftPairFst (liftA2 cppStateObjDoc t (liftList callFuncParamList vs), Nothing)
    extStateObj _ = stateObj
    listStateObj = stateObj

    exists = notNull
    notNull v = v

instance Selector CppSrcCode where
    objAccess v f = liftPairFst (liftA2 objAccessDocD v f, Nothing)
    ($.) = objAccess

    objMethodCall o f ps = objAccess o (func f ps)
    objMethodCallVoid o f = objMethodCall o f []

    selfAccess = objAccess self

    listPopulateAccess _ _ = return (empty, Nothing)
    listSizeAccess v = objAccess v listSize

    listIndexExists v i = listSizeAccess v ?> i
    argExists i = objAccess argsList (listAccess (litInt $ fromIntegral i))
    
    indexOf l v = funcApp "find" [l $. iterBegin, l $. iterEnd, v] #- l $. iterBegin

    stringEqual v1 v2 = v1 ?== v2

    castObj f v = liftPairFst (liftA2 castObjDocD f v, Nothing)
    castStrToFloat v = funcApp "std::stod" [v]

instance FunctionSym CppSrcCode where
    type Function CppSrcCode = Doc
    func l vs = fmap funcDocD (funcApp l vs)
    cast targT _ = fmap castDocD targT
    castListToInt = cast (listType static int) int
    get n = fmap funcDocD (funcApp (getterName n) [])
    set n v = fmap funcDocD (funcApp (setterName n) [v])

    listSize = func "size" []
    listAdd _ v = fmap funcDocD (funcApp "push_back" [v])
    listPopulateInt _ = return empty
    listPopulateFloat _ = return empty
    listPopulateChar _ = return empty
    listPopulateBool _ = return empty
    listPopulateString _ = return empty
    listAppend v = fmap funcDocD (funcApp "push_back" [v])
    listExtendInt = listAppend defaultInt 
    listExtendFloat = listAppend defaultFloat 
    listExtendChar = listAppend defaultChar 
    listExtendBool = listAppend defaultBool
    listExtendString = listAppend defaultString
    listExtendList _ = fmap cppListExtendList

    iterBegin = fmap funcDocD (funcApp "begin" [])
    iterEnd = fmap funcDocD (funcApp "end" [])

instance SelectorFunction CppSrcCode where
    listAccess v = fmap funcDocD (funcApp "at" [v])
    listSet = liftA2 cppListSetDoc

    listAccessEnum t v = listAccess (castObj (cast int t) v)
    listSetEnum t i = listSet (castObj (cast int t) i)

    at l = listAccess (var l) 

instance StatementSym CppSrcCode where
    type Statement CppSrcCode = (Doc, Terminator)
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
    listDec l n t = liftPairFst (liftA2 (cppListDecDoc l) (litInt n) t, Semi) -- this means that the type you declare must already be a list. Not sure how I feel about this. On the bright side, it also means you don't need to pass permanence
    listDecDef l t vs = liftPairFst (liftA2 (cppListDecDefDoc l) t (liftList callFuncParamList vs), Semi)
    objDecDef l t v = liftPairFst (liftA2 (objDecDefDocD l) t v, Semi)
    objDecNew l t vs = liftPairFst (liftA2 (objDecDefDocD l) t (stateObj t vs), Semi)
    extObjDecNew l _ = objDecNew l
    objDecNewVoid l t = liftPairFst (liftA2 (objDecDefDocD l) t (stateObj t []), Semi)
    extObjDecNewVoid l _ = objDecNewVoid l
    constDecDef l t v = liftPairFst (liftA2 (constDecDefDocD l) t v, Semi)

    print _ v = liftPairFst (liftA2 (cppPrintDocD False) printFunc v, Semi)
    printLn _ v = liftPairFst (liftA2 (cppPrintDocD True) printLnFunc v, Semi)
    printStr s = liftPairFst (liftA2 (cppPrintDocD False) printFunc (litString s), Semi)
    printStrLn s = liftPairFst (liftA2 (cppPrintDocD True) printLnFunc (litString s), Semi)

    printFile f _ v = liftPairFst (liftA2 (cppPrintDocD False) (printFileFunc f) v, Semi)
    printFileLn f _ v = liftPairFst (liftA2 (cppPrintDocD True) (printFileLnFunc f) v, Semi)
    printFileStr f s = liftPairFst (liftA2 (cppPrintDocD False) (printFileFunc f) (litString s), Semi)
    printFileStrLn f s = liftPairFst (liftA2 (cppPrintDocD True) (printFileLnFunc f) (litString s), Semi)

    -- Keep this block as is for now, I think this should work. Consult CppRenderer.hs if not
    printList t v = multi [(state (printStr "[")), (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. listSize) #- (litInt 1))) ((&.++) "i") (bodyStatements [print t (v $. (listAccess (var "i"))), printStr ","])), (state (print t (v $. (listAccess ((v $. listSize) #- (litInt 1)))))), (printStr "]")]
    printLnList t v = multi [(state (printStr "[")), (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. listSize) #- (litInt 1))) ((&.++) "i") (bodyStatements [print t (v $. (listAccess (var "i"))), printStr ","])), (state (print t (v $. (listAccess ((v $. listSize) #- (litInt 1)))))), (printStrLn "]")]
    printFileList f t v = multi [(state (printFileStr f "[")), (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. listSize) #- (litInt 1))) ((&.++) "i") (bodyStatements [printFile f t (v $. (listAccess (var "i"))), printFileStr f ","])), (state (printFile f t (v $. (listAccess ((v $. listSize) #- (litInt 1)))))), (printFileStr f "]")]
    printFileLnList f t v = multi [(state (printFileStr f "[")), (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. listSize) #- (litInt 1))) ((&.++) "i") (bodyStatements [printFile f t (v $. (listAccess (var "i"))), printFileStr f ","])), (state (printFile f t (v $. (listAccess ((v $. listSize) #- (litInt 1)))))), (printFileStrLn f "]")]

    getIntInput v = liftPairFst (liftA3 cppInput v inputFunc endStatement, Semi)
    getFloatInput v = liftPairFst (liftA3 cppInput v inputFunc endStatement, Semi)
    getBoolInput v = liftPairFst (liftA3 cppInput v inputFunc endStatement, Semi)
    getStringInput v = liftPairFst (liftA3 cppInput v inputFunc endStatement, Semi)
    getCharInput v = liftPairFst (liftA3 cppInput v inputFunc endStatement, Semi)
    discardInput = liftPairFst (fmap (cppDiscardInput "\\n") inputFunc, Semi)

    getIntFileInput f v = liftPairFst (liftA3 cppInput v f endStatement, Semi)
    getFloatFileInput f v = liftPairFst (liftA3 cppInput v f endStatement, Semi)
    getBoolFileInput f v = liftPairFst (liftA3 cppInput v f endStatement, Semi)
    getStringFileInput f v = liftPairFst (liftA3 cppInput v f endStatement, Semi)
    getCharFileInput f v = liftPairFst (liftA3 cppInput v f endStatement, Semi)
    discardFileInput f = liftPairFst (fmap (cppDiscardInput " ") f, Semi)

    openFileR f n = liftPairFst (liftA2 (cppOpenFile "std::fstream::in") f n, Semi)
    openFileW f n = liftPairFst (liftA2 (cppOpenFile "std::fstream::out") f n, Semi)
    openFileA f n = liftPairFst (liftA2 (cppOpenFile "std::fstream::app") f n, Semi)
    closeFile f = valState $ objMethodCall f "close" []

    getFileInputLine f v = valState $ funcApp "std::getline" [f, v]
    discardFileLine f = liftPairFst (fmap (cppDiscardInput "\\n") f, Semi)
    stringSplit d vnew s = let l_ss = "ss"
                               v_ss = var l_ss
                               l_word = "word"
                               v_word = var l_word
                           in
        multi [
            valState $ vnew $. (func "clear" []),
            varDec l_ss (obj "std::stringstream"),
            valState $ objMethodCall v_ss "str" [s],
            varDec l_word string,
            while (funcApp "std::getline" [v_ss, v_word, litChar d]) (oneLiner $ valState $ vnew $. (listAppend v_word))
        ]

    break = return (breakDocD, Semi)
    continue = return (continueDocD, Semi)

    returnState v = liftPairFst (fmap returnDocD v, Semi)
    returnVar l = liftPairFst (fmap returnDocD (var l), Semi)

    valState v = liftPairFst (fmap fst v, Semi)

    comment cmt = liftPairFst (fmap (commentDocD cmt) commentStart, Empty)

    free v = liftPairFst (fmap freeDocD v, Semi)

    throw errMsg = liftPairFst (fmap cppThrowDoc (litString errMsg), Semi)

    initState fsmName initialState = varDecDef fsmName string (litString initialState)
    changeState fsmName toState = fsmName &.= (litString toState)

    initObserverList = listDecDef observerListName
    addObserver t o = valState $ obsList $. listAdd lastelem o
        where obsList = observerListName `listOf` t
              lastelem = obsList $. listSize

    state = fmap statementDocD
    loopState = fmap (statementDocD . setEmpty)
    multi = lift1List multiStateDocD endStatement

instance ControlStatementSym CppSrcCode where
    ifCond bs b = liftPairFst (lift4Pair ifCondDocD ifBodyStart elseIf blockEnd b bs, Empty)
    ifNoElse bs = ifCond bs $ body []
    switch v cs c = liftPairFst (lift3Pair switchDocD (state break) v c cs, Empty)
    switchAsIf v cs = ifCond cases
        where cases = map (\(l, b) -> (v ?== l, b)) cs

    ifExists _ ifBody _ = liftPairFst (ifBody, Empty) -- All variables are initialized in C++

    for sInit vGuard sUpdate b = liftPairFst (liftA6 forDocD blockStart blockEnd (loopState sInit) vGuard (loopState sUpdate) b, Empty)
    forRange i initv finalv stepv = for (varDecDef i int initv) ((var i) ?< finalv) (i &.+= stepv)
    forEach l t v = for (varDecDef l (iterator t) (v $. iterBegin)) (var l ?!= v $. iterEnd) ((&.++) l)
    while v b = liftPairFst (liftA4 whileDocD blockStart blockEnd v b, Empty)

    tryCatch tb cb = liftPairFst (liftA2 cppTryCatch tb cb, Empty)

    checkState l = switchAsIf (var l) 

    notifyObservers fn t ps = for initv (var index ?< (obsList $. listSize)) ((&.++) index) notify
        where obsList = observerListName `listOf` t
              index = "observerIndex"
              initv = varDecDef index int $ litInt 0
              notify = oneLiner $ valState $ (obsList $. at index) $. func fn ps

    getFileInputAll f v = let l_line = "nextLine"
                              v_line = var l_line
                          in
        multi [varDec l_line string,
            while (funcApp "std::getline" [f, v_line])
                (oneLiner $ valState $ v $. (listAppend $ v_line))]

instance ScopeSym CppSrcCode where
    type Scope CppSrcCode = (Doc, ScopeTag)
    private = return (privateDocD, Priv)
    public = return (publicDocD, Pub)

    includeScope _ = return (empty, Priv)

instance MethodTypeSym CppSrcCode where
    type MethodType CppSrcCode = Doc
    mState t = t
    void = return voidDocD
    construct n = return $ constructDocD n

instance ParameterSym CppSrcCode where
    type Parameter CppSrcCode = Doc
    stateParam n = fmap (stateParamDocD n)
    pointerParam n = fmap (cppPointerParamDoc n)

instance MethodSym CppSrcCode where
    -- Bool is True if the method is a main method, False otherwise
    type Method CppSrcCode = (Doc, Bool, ScopeTag)
    method n c s _ t ps b = liftTripFst (liftA5 (cppsMethod n c) t (liftList paramListDocD ps) b blockStart blockEnd, False, snd $ unCPPSC s)
    getMethod n c t = method (getterName n) c public dynamic t [] getBody
        where getBody = oneLiner $ returnState (self $-> (var n))
    setMethod setLbl c paramLbl t = method (setterName setLbl) c public dynamic void [(stateParam paramLbl t)] setBody
        where setBody = oneLiner $ (self $-> (var setLbl)) &=. paramLbl
    mainMethod _ b = liftTripFst (liftA4 cppMainMethod int b blockStart blockEnd, True, Pub)
    privMethod n c = method n c private dynamic
    pubMethod n c = method n c public dynamic
    constructor n = method n n public dynamic (construct n)
    destructor n vs = 
        let i = "i"
            deleteStatements = map (fmap tripSnd) vs
            loopIndexDec = varDec i int
            dbody = if all (isEmpty . fst . unCPPSC) deleteStatements then return empty else bodyStatements $ loopIndexDec : deleteStatements
        in pubMethod ('~':n) n void [] dbody

    function n s _ t ps b = liftTripFst (liftA5 (cppsFunction n) t (liftList paramListDocD ps) b blockStart blockEnd, False, snd $ unCPPSC s)

instance StateVarSym CppSrcCode where
    -- (Doc, Terminator) is the corresponding destructor code for the stateVar
    type StateVar CppSrcCode = (Doc, (Doc, Terminator), ScopeTag)
    stateVar del l s p t = liftTrip (liftA4 (stateVarDocD l) (fmap fst $ includeScope s) p t endStatement, if del < alwaysDel then return (empty, Empty) else free $ var l, fmap snd s)
    privMVar del l = stateVar del l private dynamic
    pubMVar del l = stateVar del l public dynamic
    pubGVar del l = stateVar del l public static
    listStateVar del l s p t = 
        let i = "i"
            guard = var i ?< (var l $. listSize)
            loopBody = oneLiner $ free (var l $. at i)
            initv = (i &.= litInt 0)
            deleteLoop = for initv guard ((&.++) i) loopBody
        in liftTrip (fmap tripFst $ stateVar del l s p t, if del < alwaysDel then return (empty, Empty) else deleteLoop, fmap snd s)

instance ClassSym CppSrcCode where
    -- Bool is True if the class is a main class, False otherwise
    type Class CppSrcCode = (Doc, Bool)
    buildClass n _ _ vs fs = liftPairFst (liftList methodListDocD (map (\x -> fmap (\(d,b,_) -> (d,b)) x) (fs ++ [destructor n vs])), any (tripSnd . unCPPSC) fs)
    enum _ _ _ = return (empty, False)
    mainClass _ vs fs = liftPairFst (liftA2 (cppMainClass (null vs)) (liftList stateVarListDocD (map (fmap tripFst) vs)) (liftList methodListDocD (map (\x -> fmap (\(d,b,_) -> (d,b)) x) fs)), True)
    privClass n p = buildClass n p private
    pubClass n p = buildClass n p public

instance ModuleSym CppSrcCode where
    -- Label is module name
    -- Bool is True if the method is a main method, False otherwise
    type Module CppSrcCode = (Doc, Label, Bool)
    buildModule n l _ ms cs = liftTripFst (liftA5 cppModuleDoc (liftList vcat (map include l)) (if not (null l) && any (not . isEmpty . fst . unCPPSC) cs then return blank else return empty) (liftList methodListDocD (map (\x -> fmap (\(d,b,_) -> (d,b)) x) ms)) (if ((any (not . isEmpty . fst . unCPPSC) cs) || (all (isEmpty . fst . unCPPSC) cs && not (null l))) && any (not . isEmpty . tripFst . unCPPSC) ms then return blank else return empty) (liftList vibcat (map (fmap fst) cs)), n, any (snd . unCPPSC) cs || any (tripSnd . unCPPSC) ms)

-----------------
-- Header File --
-----------------

newtype CppHdrCode a = CPPHC {unCPPHC :: a}

instance Functor CppHdrCode where
    fmap f (CPPHC x) = CPPHC (f x)

instance Applicative CppHdrCode where
    pure = CPPHC
    (CPPHC f) <*> (CPPHC x) = CPPHC (f x)

instance Monad CppHdrCode where
    return = CPPHC
    CPPHC x >>= f = f x

instance PackageSym CppHdrCode where
    type Package CppHdrCode = ([(Doc, Label, Bool)], Label)
    packMods n ms = liftPairFst (sequence mods, n)
        where mods = filter (not . isEmpty . tripFst . unCPPHC) ms

instance RenderSym CppHdrCode where
    type RenderFile CppHdrCode = (Doc, Label, Bool)
    fileDoc code = liftTripFst (if isEmpty (tripFst (unCPPHC code)) then return empty else liftA3 fileDoc' (top code) (fmap tripFst code) bottom, tripSnd $ unCPPHC code, tripThird $ unCPPHC code)
    top m = liftA3 cpphtop m (list dynamic) endStatement
    bottom = return $ text "#endif"

instance KeywordSym CppHdrCode where
    type Keyword CppHdrCode = Doc
    endStatement = return semi
    endStatementLoop = return empty

    include n = return $ text "#include" <+> doubleQuotedText (n ++ cppHeaderExt)
    inherit = return colon

    list _ = return $ text "vector"
    listObj = return empty

    blockStart = return lbrace
    blockEnd = return rbrace

    ifBodyStart = return empty
    elseIf = return empty
    
    iterForEachLabel = return empty
    iterInLabel = return empty

    commentStart = return empty
    
    printFunc = return empty
    printLnFunc = return empty
    printFileFunc _ = return empty
    printFileLnFunc _ = return empty

instance PermanenceSym CppHdrCode where
    type Permanence CppHdrCode = Doc
    static = return staticDocD
    dynamic = return dynamicDocD

instance BodySym CppHdrCode where
    type Body CppHdrCode = Doc
    body _ = return empty
    bodyStatements _ = return empty
    oneLiner _ = return empty

    addComments _ _ = return empty

instance BlockSym CppHdrCode where
    type Block CppHdrCode = Doc
    block _ = return empty

instance StateTypeSym CppHdrCode where
    type StateType CppHdrCode = Doc
    bool = return $ cppBoolTypeDoc
    int = return $ intTypeDocD
    float = return $ cppFloatTypeDoc
    char = return $ charTypeDocD
    string = return $ stringTypeDocD
    infile = return $ cppInfileTypeDoc
    outfile = return $ cppOutfileTypeDoc
    listType p st = liftA2 listTypeDocD st (list p)
    intListType p = listType p int
    floatListType p = listType p float
    boolListType = listType dynamic bool
    obj t = return $ typeDocD t
    enumType t = return $ typeDocD t
    iterator t = fmap cppIterTypeDoc (listType dynamic t)

instance ControlBlockSym CppHdrCode where
    runStrategy _ _ _ _ = return empty

    listSlice _ _ _ _ _ _ = return empty

instance UnaryOpSym CppHdrCode where
    type UnaryOp CppHdrCode = Doc
    notOp = return empty
    negateOp = return empty
    sqrtOp = return empty
    absOp = return empty
    logOp = return empty
    lnOp = return empty
    expOp = return empty
    sinOp = return empty
    cosOp = return empty
    tanOp = return empty
    asinOp = return empty
    acosOp = return empty
    atanOp = return empty
    floorOp = return empty
    ceilOp = return empty

instance BinaryOpSym CppHdrCode where
    type BinaryOp CppHdrCode = Doc
    equalOp = return empty
    notEqualOp = return empty
    greaterOp = return empty
    greaterEqualOp = return empty
    lessOp = return empty
    lessEqualOp = return empty
    plusOp = return empty
    minusOp = return empty
    multOp = return empty
    divideOp = return empty
    powerOp = return empty
    moduloOp = return empty
    andOp = return empty
    orOp = return empty

instance ValueSym CppHdrCode where
    type Value CppHdrCode = (Doc, Maybe String)
    litTrue = return (empty, Nothing)
    litFalse = return (empty, Nothing)
    litChar _ = return (empty, Nothing)
    litFloat _ = return (empty, Nothing)
    litInt _ = return (empty, Nothing)
    litString _ = return (empty, Nothing)

    defaultChar = return (empty, Nothing)
    defaultFloat = return (empty, Nothing)
    defaultInt = return (empty, Nothing)
    defaultString = return (empty, Nothing)
    defaultBool = return (empty, Nothing)

    ($->) _ _ = return (empty, Nothing)
    ($:) _ _ = return (empty, Nothing)

    const _ = return (empty, Nothing)
    var _ = return (empty, Nothing)
    extVar _ _ = return (empty, Nothing)
    self = return (empty, Nothing)
    arg _ = return (empty, Nothing)
    enumElement _ _ = return (empty, Nothing)
    enumVar _ = return (empty, Nothing)
    objVar _ _ = return (empty, Nothing)
    objVarSelf _ = return (empty, Nothing)
    listVar _ _ = return (empty, Nothing)
    listOf _ _ = return (empty, Nothing)
    iterVar _ = return (empty, Nothing)
    
    inputFunc = return (empty, Nothing)
    argsList = return (empty, Nothing)

    valName _ = error "Attempted to extract string from Value for C++ header file"

instance NumericExpression CppHdrCode where
    (#~) _ = return (empty, Nothing)
    (#/^) _ = return (empty, Nothing)
    (#|) _ = return (empty, Nothing)
    (#+) _ _ = return (empty, Nothing)
    (#-) _ _ = return (empty, Nothing)
    (#*) _ _ = return (empty, Nothing)
    (#/) _ _ = return (empty, Nothing)
    (#%) _ _ = return (empty, Nothing)
    (#^) _ _ = return (empty, Nothing)

    log _ = return (empty, Nothing)
    ln _ = return (empty, Nothing)
    exp _ = return (empty, Nothing)
    sin _ = return (empty, Nothing)
    cos _ = return (empty, Nothing)
    tan _ = return (empty, Nothing)
    csc _ = return (empty, Nothing)
    sec _ = return (empty, Nothing)
    cot _ = return (empty, Nothing)
    arcsin _ = return (empty, Nothing)
    arccos _ = return (empty, Nothing)
    arctan _ = return (empty, Nothing)
    floor _ = return (empty, Nothing)
    ceil _ = return (empty, Nothing)

instance BooleanExpression CppHdrCode where
    (?!) _ = return (empty, Nothing)
    (?&&) _ _ = return (empty, Nothing)
    (?||) _ _ = return (empty, Nothing)

    (?<) _ _ = return (empty, Nothing)
    (?<=) _ _ = return (empty, Nothing)
    (?>) _ _ = return (empty, Nothing)
    (?>=) _ _ = return (empty, Nothing)
    (?==) _ _ = return (empty, Nothing)
    (?!=) _ _ = return (empty, Nothing)
   
instance ValueExpression CppHdrCode where
    inlineIf _ _ _ = return (empty, Nothing)
    funcApp _ _ = return (empty, Nothing)
    selfFuncApp _ _ = return (empty, Nothing)
    extFuncApp _ _ _ = return (empty, Nothing)
    stateObj _ _ = return (empty, Nothing)
    extStateObj _ _ _ = return (empty, Nothing)
    listStateObj _ _ = return (empty, Nothing)

    exists _ = return (empty, Nothing)
    notNull _ = return (empty, Nothing)

instance Selector CppHdrCode where
    objAccess _ _ = return (empty, Nothing)
    ($.) _ _ = return (empty, Nothing)

    objMethodCall _ _ _ = return (empty, Nothing)
    objMethodCallVoid _ _ = return (empty, Nothing)

    selfAccess _ = return (empty, Nothing)

    listPopulateAccess _ _ = return (empty, Nothing)
    listSizeAccess _ = return (empty, Nothing)

    listIndexExists _ _ = return (empty, Nothing)
    argExists _ = return (empty, Nothing)
    
    indexOf _ _ = return (empty, Nothing)

    stringEqual _ _ = return (empty, Nothing)

    castObj _ _ = return (empty, Nothing)
    castStrToFloat _ = return (empty, Nothing)

instance FunctionSym CppHdrCode where
    type Function CppHdrCode = Doc
    func _ _ = return empty
    cast _ _ = return empty
    castListToInt = return empty
    get _ = return empty
    set _ _ = return empty

    listSize = return empty
    listAdd _ _ = return empty
    listPopulateInt _ = return empty
    listPopulateFloat _ = return empty
    listPopulateChar _ = return empty
    listPopulateBool _ = return empty
    listPopulateString _ = return empty
    listAppend _ = return empty
    listExtendInt = return empty
    listExtendFloat = return empty
    listExtendChar = return empty
    listExtendBool = return empty
    listExtendString = return empty
    listExtendList _ _ = return empty

    iterBegin = return empty
    iterEnd = return empty

instance SelectorFunction CppHdrCode where
    listAccess _ = return empty
    listSet _ _ = return empty

    listAccessEnum _ _ = return empty
    listSetEnum _ _ _ = return empty

    at _ = return empty

instance StatementSym CppHdrCode where
    type Statement CppHdrCode = (Doc, Terminator)
    assign _ _ = return (empty, Empty)
    assignToListIndex _ _ _ = return (empty, Empty)
    (&=) _ _ = return (empty, Empty)
    (&.=) _ _ = return (empty, Empty)
    (&=.) _ _ = return (empty, Empty)
    (&-=) _ _ = return (empty, Empty)
    (&.-=) _ _ = return (empty, Empty)
    (&+=) _ _ = return (empty, Empty)
    (&.+=) _ _ = return (empty, Empty)
    (&++) _ = return (empty, Empty)
    (&.++) _ = return (empty, Empty)
    (&~-) _ = return (empty, Empty)
    (&.~-) _ = return (empty, Empty)

    varDec _ _ = return (empty, Empty)
    varDecDef _ _ _ = return (empty, Empty)
    listDec _ _ _ = return (empty, Empty)
    listDecDef _ _ _ = return (empty, Empty)
    objDecDef _ _ _ = return (empty, Empty)
    objDecNew _ _ _ = return (empty, Empty)
    extObjDecNew _ _ _ _ = return (empty, Empty)
    objDecNewVoid _ _ = return (empty, Empty)
    extObjDecNewVoid _ _ _ = return (empty, Empty)
    constDecDef _ _ _ = return (empty, Empty)

    print _ _ = return (empty, Empty)
    printLn _ _ = return (empty, Empty)
    printStr _ = return (empty, Empty)
    printStrLn _ = return (empty, Empty)

    printFile _ _ _ = return (empty, Empty)
    printFileLn _ _ _ = return (empty, Empty)
    printFileStr _ _ = return (empty, Empty)
    printFileStrLn _ _ = return (empty, Empty)

    printList _ _ = return (empty, Empty)
    printLnList _ _ = return (empty, Empty)
    printFileList _ _ _ = return (empty, Empty)
    printFileLnList _ _ _ = return (empty, Empty)

    getIntInput _ = return (empty, Empty)
    getFloatInput _ = return (empty, Empty)
    getBoolInput _ = return (empty, Empty)
    getStringInput _ = return (empty, Empty)
    getCharInput _ = return (empty, Empty)
    discardInput = return (empty, Empty)

    getIntFileInput _ _ = return (empty, Empty)
    getFloatFileInput _ _ = return (empty, Empty)
    getBoolFileInput _ _ = return (empty, Empty)
    getStringFileInput _ _ = return (empty, Empty)
    getCharFileInput _ _ = return (empty, Empty)
    discardFileInput _ = return (empty, Empty)

    openFileR _ _ = return (empty, Empty)
    openFileW _ _ = return (empty, Empty)
    openFileA _ _ = return (empty, Empty)
    closeFile _ = return (empty, Empty)

    getFileInputLine _ _ = return (empty, Empty)
    discardFileLine _ = return (empty, Empty)
    stringSplit _ _ _ = return (empty, Empty)

    break = return (empty, Empty)
    continue = return (empty, Empty)

    returnState _ = return (empty, Empty)
    returnVar _ = return (empty, Empty)

    valState _ = return (empty, Empty)

    comment _ = return (empty, Empty)

    free _ = return (empty, Empty)

    throw _ = return (empty, Empty)

    initState _ _ = return (empty, Empty)
    changeState _ _ = return (empty, Empty)

    initObserverList _ _ = return (empty, Empty)
    addObserver _ _ = return (empty, Empty)

    state _ = return (empty, Empty)
    loopState _ = return (empty, Empty)
    multi _ = return (empty, Empty)

instance ControlStatementSym CppHdrCode where
    ifCond _ _ = return (empty, Empty)
    ifNoElse _ = return (empty, Empty)
    switch _ _ _ = return (empty, Empty)
    switchAsIf _ _ _ = return (empty, Empty)

    ifExists _ _ _ = return (empty, Empty)

    for _ _ _ _ = return (empty, Empty)
    forRange _ _ _ _ _ = return (empty, Empty)
    forEach _ _ _ _ = return (empty, Empty)
    while _ _ = return (empty, Empty)

    tryCatch _ _ = return (empty, Empty)

    checkState _ _ _ = return (empty, Empty)

    notifyObservers _ _ _ = return (empty, Empty)

    getFileInputAll _ _ = return (empty, Empty)

instance ScopeSym CppHdrCode where
    type Scope CppHdrCode = (Doc, ScopeTag)
    private = return (privateDocD, Priv)
    public = return (publicDocD, Pub)

    includeScope _ = return (empty, Priv)

instance MethodTypeSym CppHdrCode where
    type MethodType CppHdrCode = Doc
    mState t = t
    void = return voidDocD
    construct n = return $ constructDocD n

instance ParameterSym CppHdrCode where
    type Parameter CppHdrCode = Doc
    stateParam n = fmap (stateParamDocD n)
    pointerParam n = fmap (cppPointerParamDoc n)

instance MethodSym CppHdrCode where
    -- Bool is True if the method is a main method, False otherwise
    type Method CppHdrCode = (Doc, Bool, ScopeTag)
    method n _ s _ t ps _ = liftTripFst (liftA3 (cpphMethod n) t (liftList paramListDocD ps) endStatement, False, snd $ unCPPHC s)
    getMethod n c t = method (getterName n) c public dynamic t [] (return empty)
    setMethod setLbl c paramLbl t = method (setterName setLbl) c public dynamic void [(stateParam paramLbl t)] (return empty)
    mainMethod _ _ = return (empty, True, Pub)
    privMethod n c = method n c private dynamic
    pubMethod n c = method n c public dynamic
    constructor n = method n n public dynamic (construct n)
    destructor n _ = pubMethod ('~':n) n void [] (return empty)

    function n = method n ""

instance StateVarSym CppHdrCode where
    type StateVar CppHdrCode = (Doc, (Doc, Terminator), ScopeTag)
    stateVar _ l s p t = liftTrip (liftA4 (stateVarDocD l) (fmap fst (includeScope s)) p t endStatement, return (empty, Empty), fmap snd s)
    privMVar del l = stateVar del l private dynamic
    pubMVar del l = stateVar del l public dynamic
    pubGVar del l = stateVar del l public static
    listStateVar = stateVar

instance ClassSym CppHdrCode where
    -- Bool is True if the class is a main class, False otherwise
    type Class CppHdrCode = (Doc, Bool)
    -- do this with a do? avoids liftA8...
    buildClass n p _ vs fs = liftPairFst (liftA8 (cpphClass n p) (lift2Lists (cpphVarsFuncsList Pub) vs (fs ++ [destructor n vs])) (lift2Lists (cpphVarsFuncsList Priv) vs (fs ++ [destructor n vs])) (fmap fst  public) (fmap fst private) inherit blockStart blockEnd endStatement, any (tripSnd . unCPPHC) fs)
    enum n es _ = liftPairFst (liftA4 (cpphEnum n) (return $ enumElementsDocD es enumsEqualInts) blockStart blockEnd endStatement, False)
    mainClass _ _ _ = return (empty, True)
    privClass n p = buildClass n p private
    pubClass n p = buildClass n p public

instance ModuleSym CppHdrCode where
    -- Label is module name
    -- Bool is True if the method is a main method, False otherwise
    type Module CppHdrCode = (Doc, Label, Bool)
    buildModule n l _ ms cs = liftTripFst (if all (isEmpty . fst . unCPPHC) cs && all (isEmpty . tripFst . unCPPHC) ms then return empty else liftA5 cppModuleDoc (liftList vcat (map include l)) (if not (null l) && any (not . isEmpty . fst . unCPPHC) cs then return blank else return empty) (liftList methodListDocD methods) (if ((any (not . isEmpty . fst . unCPPHC) cs) || (all (isEmpty . fst . unCPPHC) cs && not (null l))) && any (not . isEmpty . tripFst . unCPPHC) ms then return blank else return empty)  (liftList vibcat (map (fmap fst) cs)), n, any (snd . unCPPHC) cs || any (snd . unCPPHC) methods)
        where methods = map (fmap (\(d, m, _) -> (d, m))) ms

data ScopeTag = Pub | Priv deriving Eq

-- helpers
isDtor :: Label -> Bool
isDtor ('~':_) = True
isDtor _ = False

-- convenience
enumsEqualInts :: Bool
enumsEqualInts = False

cppHeaderExt :: Label
cppHeaderExt = ".hpp"

cppstop :: (Doc, Label, Bool) -> Doc -> Doc -> Doc
cppstop (_, n, b) lst end = vcat [
    if b then empty else inc <+> doubleQuotedText (n ++ cppHeaderExt),
    if b then empty else blank,
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
    where inc = text "#include"

cpphtop :: (Doc, Label, Bool) -> Doc -> Doc -> Doc
cpphtop (_, n, _) lst end = vcat [
    text "#ifndef" <+> text n <> text "_h",
    text "#define" <+> text n <> text "_h",
    blank,
    inc <+> angles (text "string"),
    inc <+> angles lst,
    blank,
    usingNameSpace "std" (Just "string") end,
    usingNameSpace "std" (Just $ render lst) end,
    usingNameSpace "std" (Just "ifstream") end,
    usingNameSpace "std" (Just "ofstream") end]
    where inc = text "#include"

usingNameSpace :: Label -> Maybe Label -> Doc -> Doc
usingNameSpace n (Just m) end = text "using" <+> text n <> colon <> colon <> text m <> end
usingNameSpace n Nothing end = text "using namespace" <+> text n <> end

cppBoolTypeDoc :: Doc
cppBoolTypeDoc = text "bool"

cppFloatTypeDoc :: Doc
cppFloatTypeDoc = text "double"

cppInfileTypeDoc :: Doc
cppInfileTypeDoc = text "ifstream"

cppOutfileTypeDoc :: Doc
cppOutfileTypeDoc = text "ofstream"

cppIterTypeDoc :: Doc -> Doc
cppIterTypeDoc t = text "std::" <> t <> text "::iterator"

cppStateObjDoc :: Doc -> Doc -> Doc
cppStateObjDoc t ps = t <> parens ps

cppListSetDoc :: (Doc, Maybe String) -> (Doc, Maybe String) -> Doc
cppListSetDoc (i, _) (v, _) = dot <> text "at" <> parens i <+> equals <+> v

cppListDecDoc :: Label -> (Doc, Maybe String) -> Doc -> Doc
cppListDecDoc l (n, _) t = t <+> text l <> parens n

cppListDecDefDoc :: Label -> Doc -> Doc -> Doc
cppListDecDefDoc l t vs = t <+> text l <> braces vs

cppPrintDocD :: Bool -> Doc -> (Doc, Maybe String) -> Doc
cppPrintDocD newLn printFn (v, _) = printFn <+> text "<<" <+> v <+> end
    where end = if newLn then text "<<" <+> text "std::endl" else empty

cppThrowDoc :: (Doc, Maybe String) -> Doc
cppThrowDoc (errMsg, _) = text "throw" <+> errMsg

cppTryCatch :: Doc -> Doc -> Doc
cppTryCatch tb cb= vcat [
    text "try" <+> lbrace,
    oneTab $ tb,
    rbrace <+> text "catch" <+> parens (text "...") <+> lbrace,
    oneTab $ cb,
    rbrace]

cppDiscardInput :: Label -> (Doc, Maybe String) -> Doc
cppDiscardInput sep (inFn, _) = inFn <> dot <> text "ignore" <+> parens 
    (text "std::numeric_limits<std::streamsize>::max()" <> comma <+> quotes (text sep))

cppInput :: (Doc, Maybe String) -> (Doc, Maybe String) -> Doc -> Doc
cppInput (v, _) (inFn, _) end = vcat [
    inFn <+> text ">>" <+> v <> end,
    inFn <> dot <> text "ignore (std::numeric_limits<std::streamsize>::max(), '\\n')"]

cppOpenFile :: Label -> (Doc, Maybe String) -> (Doc, Maybe String) -> Doc
cppOpenFile mode (f, _) (n, _) = f <> dot <> text "open" <> parens (n <> comma <+> text mode)

cppListExtendList :: Doc -> Doc
cppListExtendList t = dot <> text "push_back" <> parens (t <> parens (integer 0))

cppPointerParamDoc :: Label -> Doc -> Doc
cppPointerParamDoc n t = t <+> text "&" <> text n

cppsMethod :: Label -> Label -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc
cppsMethod n c t ps b bStart bEnd = vcat [ttype <+> text c <> text "::" <> text n <> parens ps <+> bStart,
    oneTab b,
    bEnd]
    where ttype | isDtor n = empty
                | otherwise = t

cppsFunction :: Label -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc
cppsFunction n t ps b bStart bEnd = vcat [t <+> text n <> parens ps <+> bStart,
    oneTab b,
    bEnd]

cpphMethod :: Label -> Doc -> Doc -> Doc -> Doc
cpphMethod n t ps end | isDtor n = text n <> parens ps <> end
                      | otherwise = t <+> text n <> parens ps <> end

cppMainMethod :: Doc -> Doc -> Doc -> Doc -> Doc
cppMainMethod t b bStart bEnd = vcat [
    t <+> text "main" <> parens (text "int argc, const char *argv[]") <+> bStart,
    oneTab b,
    blank,
    text "return 0;",
    bEnd]

cpphVarsFuncsList :: ScopeTag -> [(Doc, (Doc, Terminator), ScopeTag)] -> [(Doc, Bool, ScopeTag)] -> Doc
cpphVarsFuncsList st vs fs = 
    let scopedVs = [tripFst v | v <- vs, tripThird v == st]
        scopedFs = [tripFst f | f <- fs, tripThird f == st]
    in vcat $ scopedVs ++ (if null scopedVs then empty else blank) : scopedFs

cpphClass :: Label -> Maybe Label -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc
cpphClass n p pubs privs pub priv inhrt bStart bEnd end =
    let baseClass = case p of Nothing -> empty
                              Just pn -> inhrt <+> pub <+> text pn
    in vcat [
        classDec <+> text n <+> baseClass <+> bStart,
        oneTabbed [
            pub <> colon,
            oneTab pubs,
            blank,
            priv <> colon,
            oneTab privs],
        bEnd <> end]

cppMainClass :: Bool -> Doc -> Doc -> Doc
cppMainClass b vs fs = vcat [
    vs,
    if b then empty else blank,
    fs]

cpphEnum :: Label -> Doc -> Doc -> Doc -> Doc -> Doc
cpphEnum n es bStart bEnd end = vcat [
    text "enum" <+> text n <+> bStart,
    oneTab es,
    bEnd <> end]

cppModuleDoc :: Doc -> Doc -> Doc -> Doc -> Doc -> Doc
cppModuleDoc ls blnk1 fs blnk2 cs = vcat [
    ls,
    blnk1,
    cs,
    blnk2,
    fs]