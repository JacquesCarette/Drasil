{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

-- | The logic to render C++ code is contained in this module
module Language.Drasil.Code.Imperative.LanguageRenderer.NewCppRenderer (
    -- * C++ Code Configuration -- defines syntax of all C++ code
    CppSrcCode(..), CppHdrCode(..), CppCode(..), unSrc, unHdr
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
  oneTab, oneTabbed, mapPairFst, mapPairSnd, tripFst, tripSnd, tripThird, 
  vibcat, liftA4, liftA5, liftA6, liftA8, liftList, lift2Lists, lift1List, 
  lift3Pair, lift4Pair, liftPairFst, liftTripFst, liftTrip)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor,const,log,exp)
import qualified Data.Map as Map (fromList,lookup)
import Control.Applicative (Applicative, liftA2, liftA3)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), braces, parens, comma,
  empty, equals, integer, semi, vcat, lbrace, rbrace, quotes, render, colon,
  isEmpty)

data CppCode x y a = CPPC {src :: x a, hdr :: y a}

class Pair p where
    pfst :: p x y a -> x a
    psnd :: p x y b -> y b
    pair :: x a -> y a -> p x y a

instance Pair CppCode where
    pfst (CPPC xa _) = xa
    psnd (CPPC _ yb) = yb
    pair = CPPC

unSrc :: CppCode CppSrcCode CppHdrCode a -> a
unSrc (CPPC (CPPSC a) _) = a

unHdr :: CppCode CppSrcCode CppHdrCode a -> a
unHdr (CPPC _ (CPPHC a)) = a

instance (Pair p) => PackageSym (p CppSrcCode CppHdrCode) where
    type Package (p CppSrcCode CppHdrCode) = ([(Doc, Label, Bool)], Label)
    packMods n ms = pair (packMods n (map pfst ms)) (packMods n (map psnd ms))

instance (Pair p) => RenderSym (p CppSrcCode CppHdrCode) where
    type RenderFile (p CppSrcCode CppHdrCode) = (Doc, Label, Bool)
    fileDoc code = pair (fileDoc $ pfst code) (fileDoc $ psnd code)
    top m = pair (top $ pfst m) (top $ psnd m)
    bottom = pair bottom bottom

instance (Pair p) => KeywordSym (p CppSrcCode CppHdrCode) where
    type Keyword (p CppSrcCode CppHdrCode) = Doc
    endStatement = pair endStatement endStatement
    endStatementLoop = pair endStatementLoop endStatementLoop

    include n = pair (include n) (include n)
    inherit = pair inherit inherit

    list p = pair (list $ pfst p) (list $ psnd p)
    listObj = pair listObj listObj

    blockStart = pair blockStart blockStart
    blockEnd = pair blockEnd blockEnd

    ifBodyStart = pair ifBodyStart ifBodyStart
    elseIf = pair elseIf elseIf
    
    iterForEachLabel = pair iterForEachLabel iterForEachLabel
    iterInLabel = pair iterInLabel iterInLabel

    commentStart = pair commentStart commentStart
    
    printFunc = pair printFunc printFunc
    printLnFunc = pair printLnFunc printLnFunc
    printFileFunc v = pair (printFileFunc $ pfst v) (printFileFunc $ psnd v)
    printFileLnFunc v = pair (printFileLnFunc $ pfst v) (printFileLnFunc $ psnd v)

instance (Pair p) => PermanenceSym (p CppSrcCode CppHdrCode) where
    type Permanence (p CppSrcCode CppHdrCode) = Doc
    static = pair static static
    dynamic = pair dynamic dynamic

instance (Pair p) => BodySym (p CppSrcCode CppHdrCode) where
    type Body (p CppSrcCode CppHdrCode) = Doc
    body bs = pair (body $ map pfst bs) (body $ map psnd bs)
    bodyStatements sts = pair (bodyStatements $ map pfst sts) (bodyStatements $ map psnd sts)
    oneLiner s = pair (oneLiner $ pfst s) (oneLiner $ psnd s)

    addComments s b = pair (addComments s $ pfst b) (addComments s $ psnd b)

instance (Pair p) => BlockSym (p CppSrcCode CppHdrCode) where
    type Block (p CppSrcCode CppHdrCode) = Doc
    block sts = pair (block $ map pfst sts) (block $ map psnd sts)

instance (Pair p) => StateTypeSym (p CppSrcCode CppHdrCode) where
    type StateType (p CppSrcCode CppHdrCode) = Doc
    bool = pair bool bool
    int = pair int int
    float = pair float float
    char = pair char char
    string = pair string string
    infile = pair infile infile
    outfile = pair outfile outfile
    listType p st = pair (listType (pfst p) (pfst st)) (listType (psnd p) (psnd st))
    intListType p = pair (intListType $ pfst p) (intListType $ psnd p)
    floatListType p = pair (floatListType $ pfst p) (floatListType $ psnd p)
    boolListType = pair boolListType boolListType
    obj t = pair (obj t) (obj t)
    enumType t = pair (enumType t) (enumType t)
    iterator t = pair (iterator $ pfst t) (iterator $ psnd t)

instance (Pair p) => ControlBlockSym (p CppSrcCode CppHdrCode) where
    runStrategy l strats rv av = pair (runStrategy l (map (mapPairSnd pfst) strats) (fmap pfst rv) (fmap pfst av)) (runStrategy l (map (mapPairSnd psnd) strats) (fmap psnd rv) (fmap psnd av))

    listSlice t vnew vold b e s = pair (listSlice (pfst t) (pfst vnew) (pfst vold) (fmap pfst b) (fmap pfst e) (fmap pfst s)) (listSlice (psnd t) (psnd vnew) (psnd vold) (fmap psnd b) (fmap psnd e) (fmap psnd s))

instance (Pair p) => UnaryOpSym (p CppSrcCode CppHdrCode) where
    type UnaryOp (p CppSrcCode CppHdrCode) = Doc
    notOp = pair notOp notOp
    negateOp = pair negateOp negateOp
    sqrtOp = pair sqrtOp sqrtOp
    absOp = pair absOp absOp
    logOp = pair logOp logOp
    lnOp = pair lnOp lnOp
    expOp = pair expOp expOp
    sinOp = pair sinOp sinOp
    cosOp = pair cosOp cosOp
    tanOp = pair tanOp tanOp
    asinOp = pair asinOp asinOp
    acosOp = pair acosOp acosOp
    atanOp = pair atanOp atanOp
    floorOp = pair floorOp floorOp
    ceilOp = pair ceilOp ceilOp

instance (Pair p) => BinaryOpSym (p CppSrcCode CppHdrCode) where
    type BinaryOp (p CppSrcCode CppHdrCode) = Doc
    equalOp = pair equalOp equalOp
    notEqualOp = pair notEqualOp notEqualOp
    greaterOp = pair greaterOp greaterOp
    greaterEqualOp = pair greaterEqualOp greaterEqualOp
    lessOp = pair lessOp lessOp
    lessEqualOp = pair lessEqualOp lessEqualOp
    plusOp = pair plusOp plusOp
    minusOp = pair minusOp minusOp
    multOp = pair multOp multOp
    divideOp = pair divideOp divideOp
    powerOp = pair powerOp powerOp
    moduloOp = pair moduloOp moduloOp
    andOp = pair andOp andOp
    orOp = pair orOp orOp

instance (Pair p) => ValueSym (p CppSrcCode CppHdrCode) where
    type Value (p CppSrcCode CppHdrCode) = (Doc, Maybe String)
    litTrue = pair litTrue litTrue
    litFalse = pair litFalse litFalse
    litChar c = pair (litChar c) (litChar c)
    litFloat v = pair (litFloat v) (litFloat v)
    litInt v = pair (litInt v) (litInt v)
    litString s = pair (litString s) (litString s)

    defaultChar = pair defaultChar defaultChar
    defaultFloat = pair defaultFloat defaultFloat
    defaultInt = pair defaultInt defaultInt
    defaultString = pair defaultString defaultString
    defaultBool = pair defaultBool defaultBool

    ($->) v1 v2 = pair (($->) (pfst v1) (pfst v2)) (($->) (psnd v1) (psnd v2))
    ($:) l1 l2 = pair (($:) l1 l2) (($:) l1 l2)

    const n = pair (const n) (const n)
    var n = pair (var n) (var n)
    extVar l n = pair (extVar l n) (extVar l n)
    self = pair self self
    arg n = pair (arg n) (arg n)
    enumElement en e = pair (enumElement en e) (enumElement en e)
    enumVar n = pair (enumVar n) (enumVar n)
    objVar o v = pair (objVar (pfst o) (pfst v)) (objVar (psnd o) (psnd v))
    objVarSelf n = pair (objVarSelf n) (objVarSelf n)
    listVar n t = pair (listVar n $ pfst t) (listVar n $ psnd t)
    n `listOf` t = pair (n `listOf` (pfst t)) (n `listOf` (psnd t))
    iterVar l = pair (iterVar l) (iterVar l)
    
    inputFunc = pair inputFunc inputFunc
    argsList = pair argsList argsList

    valName v = valName $ pfst v

instance (Pair p) => NumericExpression (p CppSrcCode CppHdrCode) where
    (#~) v = pair ((#~) $ pfst v) ((#~) $ psnd v)
    (#/^) v = pair ((#/^) $ pfst v) ((#/^) $ psnd v)
    (#|) v = pair ((#|) $ pfst v) ((#|) $ psnd v)
    (#+) v1 v2 = pair ((#+) (pfst v1) (pfst v2)) ((#+) (psnd v1) (psnd v2))
    (#-) v1 v2 = pair ((#-) (pfst v1) (pfst v2)) ((#-) (psnd v1) (psnd v2))
    (#*) v1 v2 = pair ((#*) (pfst v1) (pfst v2)) ((#*) (psnd v1) (psnd v2))
    (#/) v1 v2 = pair ((#/) (pfst v1) (pfst v2)) ((#/) (psnd v1) (psnd v2))
    (#%) v1 v2 = pair ((#%) (pfst v1) (pfst v2)) ((#%) (psnd v1) (psnd v2))
    (#^) v1 v2 = pair ((#^) (pfst v1) (pfst v2)) ((#^) (psnd v1) (psnd v2))

    log v = pair (log $ pfst v) (log $ psnd v)
    ln v = pair (ln $ pfst v) (ln $ psnd v)
    exp v = pair (exp $ pfst v) (exp $ psnd v)
    sin v = pair (sin $ pfst v) (sin $ psnd v)
    cos v = pair (cos $ pfst v) (cos $ psnd v)
    tan v = pair (tan $ pfst v) (tan $ psnd v)
    csc v = pair (csc $ pfst v) (csc $ psnd v)
    sec v = pair (sec $ pfst v) (sec $ psnd v)
    cot v = pair (cot $ pfst v) (cot $ psnd v)
    arcsin v = pair (arcsin $ pfst v) (arcsin $ psnd v)
    arccos v = pair (arccos $ pfst v) (arccos $ psnd v)
    arctan v = pair (arctan $ pfst v) (arctan $ psnd v)
    floor v = pair (floor $ pfst v) (floor $ psnd v)
    ceil v = pair (ceil $ pfst v) (ceil $ psnd v)

instance (Pair p) => BooleanExpression (p CppSrcCode CppHdrCode) where
    (?!) v = pair ((?!) $ pfst v) ((?!) $ psnd v)
    (?&&) v1 v2 = pair ((?&&) (pfst v1) (pfst v2)) ((?&&) (psnd v1) (psnd v2))
    (?||) v1 v2 = pair ((?||) (pfst v1) (pfst v2)) ((?||) (psnd v1) (psnd v2))

    (?<) v1 v2 = pair ((?<) (pfst v1) (pfst v2)) ((?<) (psnd v1) (psnd v2))
    (?<=) v1 v2 = pair ((?<=) (pfst v1) (pfst v2)) ((?<=) (psnd v1) (psnd v2))
    (?>) v1 v2 = pair ((?>) (pfst v1) (pfst v2)) ((?>) (psnd v1) (psnd v2))
    (?>=) v1 v2 = pair ((?>=) (pfst v1) (pfst v2)) ((?>=) (psnd v1) (psnd v2))
    (?==) v1 v2 = pair ((?==) (pfst v1) (pfst v2)) ((?==) (psnd v1) (psnd v2))
    (?!=) v1 v2 = pair ((?!=) (pfst v1) (pfst v2)) ((?!=) (psnd v1) (psnd v2))
    
instance (Pair p) => ValueExpression (p CppSrcCode CppHdrCode) where
    inlineIf b v1 v2 = pair (inlineIf (pfst b) (pfst v1) (pfst v2)) (inlineIf (psnd b) (psnd v1) (psnd v2))
    funcApp n vs = pair (funcApp n $ map pfst vs) (funcApp n $ map psnd vs)
    selfFuncApp n vs = pair (selfFuncApp n $ map pfst vs) (selfFuncApp n $ map psnd vs)
    extFuncApp l n vs = pair (extFuncApp l n $ map pfst vs) (extFuncApp l n $ map psnd vs)
    stateObj t vs = pair (stateObj (pfst t) (map pfst vs)) (stateObj (psnd t) (map psnd vs))
    extStateObj l t vs = pair (extStateObj l (pfst t) (map pfst vs)) (extStateObj l (psnd t) (map psnd vs))
    listStateObj t vs = pair (listStateObj (pfst t) (map pfst vs)) (listStateObj (psnd t) (map psnd vs))

    exists v = pair (exists $ pfst v) (exists $ psnd v)
    notNull v = pair (notNull $ pfst v) (notNull $ psnd v)

instance (Pair p) => Selector (p CppSrcCode CppHdrCode) where
    objAccess v f = pair (objAccess (pfst v) (pfst f)) (objAccess (psnd v) (psnd f))
    ($.) v f = pair (($.) (pfst v) (pfst f)) (($.) (psnd v) (psnd f))

    objMethodCall o f ps = pair (objMethodCall (pfst o) f (map pfst ps)) (objMethodCall (psnd o) f (map psnd ps))
    objMethodCallVoid o f = pair (objMethodCallVoid (pfst o) f) (objMethodCallVoid (psnd o) f)

    selfAccess f = pair (selfAccess $ pfst f) (selfAccess $ psnd f)

    listPopulateAccess v f = pair (listPopulateAccess (pfst v) (pfst f)) (listPopulateAccess (psnd v) (psnd f))
    listSizeAccess v = pair (listSizeAccess $ pfst v) (listSizeAccess $ psnd v)

    listIndexExists v i = pair (listIndexExists (pfst v) (pfst i)) (listIndexExists (psnd v) (psnd i))
    argExists i = pair (argExists i) (argExists i)
    
    indexOf l v = pair (indexOf (pfst l) (pfst v)) (indexOf (psnd l) (psnd v))

    stringEqual v1 v2 = pair (stringEqual (pfst v1) (pfst v2)) (stringEqual (psnd v1) (psnd v2))

    castObj f v = pair (castObj (pfst f) (pfst v)) (castObj (psnd f) (psnd v))
    castStrToFloat v = pair (castStrToFloat $ pfst v) (castStrToFloat $ psnd v)

instance (Pair p) => FunctionSym (p CppSrcCode CppHdrCode) where
    type Function (p CppSrcCode CppHdrCode) = Doc
    func l vs = pair (func l $ map pfst vs) (func l $ map psnd vs)
    cast targT srcT = pair (cast (pfst targT) (pfst srcT)) (cast (psnd targT) (psnd srcT))
    castListToInt = pair castListToInt castListToInt
    get n = pair (get n) (get n)
    set n v = pair (set n $ pfst v) (set n $ psnd v)

    listSize = pair listSize listSize
    listAdd i v = pair (listAdd (pfst i) (pfst v)) (listAdd (psnd i) (psnd v))
    listPopulateInt v = pair (listPopulateInt $ pfst v) (listPopulateInt $ psnd v)
    listPopulateFloat v = pair (listPopulateFloat $ pfst v) (listPopulateFloat $ psnd v)
    listPopulateChar v = pair (listPopulateChar $ pfst v) (listPopulateChar $ psnd v)
    listPopulateBool v = pair (listPopulateBool $ pfst v) (listPopulateBool $ psnd v)
    listPopulateString v = pair (listPopulateString $ pfst v) (listPopulateString $ psnd v)
    listAppend v = pair (listAppend $ pfst v) (listAppend $ psnd v)
    listExtendInt = pair listExtendInt listExtendInt
    listExtendFloat = pair listExtendFloat listExtendFloat
    listExtendChar = pair listExtendChar listExtendChar
    listExtendBool = pair listExtendBool listExtendBool
    listExtendString = pair listExtendString listExtendString
    listExtendList i t = pair (listExtendList i $ pfst t) (listExtendList i $ psnd t)

    iterBegin = pair iterBegin iterBegin
    iterEnd = pair iterEnd iterEnd

instance (Pair p) => SelectorFunction (p CppSrcCode CppHdrCode) where
    listAccess v = pair (listAccess $ pfst v) (listAccess $ psnd v)
    listSet i v = pair (listSet (pfst i) (pfst v)) (listSet (psnd i) (psnd v))

    listAccessEnum t v = pair (listAccessEnum (pfst t) (pfst v)) (listAccessEnum (psnd t) (psnd v))
    listSetEnum t i v = pair (listSetEnum (pfst t) (pfst i) (pfst v)) (listSetEnum (psnd t) (psnd i) (psnd v))

    at l = pair (at l) (at l)

instance (Pair p) => StatementSym (p CppSrcCode CppHdrCode) where
    type Statement (p CppSrcCode CppHdrCode) = (Doc, Terminator)
    assign v1 v2 = pair (assign (pfst v1) (pfst v2)) (assign (psnd v1) (psnd v2))
    assignToListIndex lst index v = pair (assignToListIndex (pfst lst) (pfst index) (pfst v)) (assignToListIndex (psnd lst) (psnd index) (psnd v))
    (&=) v1 v2 = pair ((&=) (pfst v1) (pfst v2)) ((&=) (psnd v1) (psnd v2))
    (&.=) l v = pair ((&.=) l $ pfst v) ((&.=) l $ psnd v)
    (&=.) v l = pair ((&=.) (pfst v) l) ((&=.) (psnd v) l)
    (&-=) v1 v2 = pair ((&-=) (pfst v1) (pfst v2)) ((&-=) (psnd v1) (psnd v2))
    (&.-=) l v = pair ((&.-=) l $ pfst v) ((&.-=) l $ psnd v)
    (&+=) v1 v2 = pair ((&+=) (pfst v1) (pfst v2)) ((&+=) (psnd v1) (psnd v2))
    (&.+=) l v = pair ((&.+=) l $ pfst v) ((&.+=) l $ psnd v)
    (&++) v = pair ((&++) $ pfst v) ((&++) $ psnd v)
    (&.++) l = pair ((&.++) l) ((&.++) l)
    (&~-) v = pair ((&~-) $ pfst v) ((&~-) $ psnd v)
    (&.~-) l = pair ((&.~-) l) ((&.~-) l)

    varDec l t = pair (varDec l $ pfst t) (varDec l $ psnd t)
    varDecDef l t v = pair (varDecDef l (pfst t) (pfst v)) (varDecDef l (psnd t) (psnd v))
    listDec l n t = pair (listDec l n $ pfst t) (listDec l n $ psnd t)
    listDecDef l t vs = pair (listDecDef l (pfst t) (map pfst vs)) (listDecDef l (psnd t) (map psnd vs))
    objDecDef l t v = pair (objDecDef l (pfst t) (pfst v)) (objDecDef l (psnd t) (psnd v))
    objDecNew l t vs = pair (objDecNew l (pfst t) (map pfst vs)) (objDecNew l (psnd t) (map psnd vs))
    extObjDecNew l lib t vs = pair (extObjDecNew l lib (pfst t) (map pfst vs)) (extObjDecNew l lib (psnd t) (map psnd vs))
    objDecNewVoid l t = pair (objDecNewVoid l $ pfst t) (objDecNewVoid l $ psnd t)
    extObjDecNewVoid l lib t = pair (extObjDecNewVoid l lib $ pfst t) (extObjDecNewVoid l lib $ psnd t)
    constDecDef l t v = pair (constDecDef l (pfst t) (pfst v)) (constDecDef l (psnd t) (psnd v))

    print t v = pair (print (pfst t) (pfst v)) (print (psnd t) (psnd v))
    printLn t v = pair (printLn (pfst t) (pfst v)) (printLn (psnd t) (psnd v))
    printStr s = pair (printStr s) (printStr s)
    printStrLn s = pair (printStrLn s) (printStrLn s)

    printFile f t v = pair (printFile (pfst f) (pfst t) (pfst v)) (printFile (psnd f) (psnd t) (psnd v))
    printFileLn f t v = pair (printFileLn (pfst f) (pfst t) (pfst v)) (printFileLn (psnd f) (psnd t) (psnd v))
    printFileStr f s = pair (printFileStr (pfst f) s) (printFileStr (psnd f) s)
    printFileStrLn f s = pair (printFileStrLn (pfst f) s) (printFileStrLn (psnd f) s)

    printList t v = pair (printList (pfst t) (pfst v)) (printList (psnd t) (psnd v))
    printLnList t v = pair (printLnList (pfst t) (pfst v)) (printLnList (psnd t) (psnd v))
    printFileList f t v = pair (printFileList (pfst f) (pfst t) (pfst v)) (printFileList (psnd f) (psnd t) (psnd v))
    printFileLnList f t v = pair (printFileLnList (pfst f) (pfst t) (pfst v)) (printFileLnList (psnd f) (psnd t) (psnd v))

    getIntInput v = pair (getIntInput $ pfst v) (getIntInput $ psnd v)
    getFloatInput v = pair (getFloatInput $ pfst v) (getFloatInput $ psnd v)
    getBoolInput v = pair (getBoolInput $ pfst v) (getBoolInput $ psnd v)
    getStringInput v = pair (getStringInput $ pfst v) (getStringInput $ psnd v)
    getCharInput v = pair (getCharInput $ pfst v) (getCharInput $ psnd v)
    discardInput = pair discardInput discardInput

    getIntFileInput f v = pair (getIntFileInput (pfst f) (pfst v)) (getIntFileInput (psnd f) (psnd v))
    getFloatFileInput f v = pair (getFloatFileInput (pfst f) (pfst v)) (getFloatFileInput (psnd f) (psnd v))
    getBoolFileInput f v = pair (getBoolFileInput (pfst f) (pfst v)) (getBoolFileInput (psnd f) (psnd v))
    getStringFileInput f v = pair (getStringFileInput (pfst f) (pfst v)) (getStringFileInput (psnd f) (psnd v))
    getCharFileInput f v = pair (getCharFileInput (pfst f) (pfst v)) (getCharFileInput (psnd f) (psnd v))
    discardFileInput f = pair (discardFileInput $ pfst f) (discardFileInput $ psnd f)

    openFileR f n = pair (openFileR (pfst f) (pfst n)) (openFileR (psnd f) (psnd n))
    openFileW f n = pair (openFileW (pfst f) (pfst n)) (openFileW (psnd f) (psnd n))
    openFileA f n = pair (openFileA (pfst f) (pfst n)) (openFileA (psnd f) (psnd n))
    closeFile f = pair (closeFile $ pfst f) (closeFile $ psnd f)

    getFileInputLine f v = pair (getFileInputLine (pfst f) (pfst v)) (getFileInputLine (psnd f) (psnd v))
    discardFileLine f = pair (discardFileLine $ pfst f) (discardFileLine $ psnd f)
    stringSplit d vnew s = pair (stringSplit d (pfst vnew) (pfst s)) (stringSplit d (psnd vnew) (psnd s))

    break = pair break break
    continue = pair continue continue

    returnState v = pair (returnState $ pfst v) (returnState $ psnd v)
    returnVar l = pair (returnVar l) (returnVar l)

    valState v = pair (valState $ pfst v) (valState $ psnd v)

    comment cmt = pair (comment cmt) (comment cmt)

    free v = pair (free $ pfst v) (free $ psnd v)

    throw errMsg = pair (throw errMsg) (throw errMsg)

    initState fsmName initialState = pair (initState fsmName initialState) (initState fsmName initialState)
    changeState fsmName toState = pair (changeState fsmName toState) (changeState fsmName toState)

    initObserverList t vs = pair (initObserverList (pfst t) (map pfst vs)) (initObserverList (psnd t) (map psnd vs))
    addObserver t o = pair (addObserver (pfst t) (pfst o)) (addObserver (psnd t) (psnd o))

    state s = pair (state $ pfst s) (state $ psnd s)
    loopState s = pair (loopState $ pfst s) (loopState $ psnd s)
    multi ss = pair (multi $ map pfst ss) (multi $ map psnd ss)

instance (Pair p) => ControlStatementSym (p CppSrcCode CppHdrCode) where
    ifCond bs b = pair (ifCond (map ((mapPairFst pfst) . (mapPairSnd pfst)) bs) (pfst b)) (ifCond (map ((mapPairFst psnd) . (mapPairSnd psnd)) bs) (psnd b))
    ifNoElse bs = pair (ifNoElse $ map ((mapPairFst pfst) . (mapPairSnd pfst)) bs) (ifNoElse $ map ((mapPairFst psnd) . (mapPairSnd psnd)) bs)
    switch v cs c = pair (switch (pfst v) (map ((mapPairFst pfst) . (mapPairSnd pfst)) cs) (pfst c)) (switch (psnd v) (map ((mapPairFst psnd) . (mapPairSnd psnd)) cs) (psnd c))
    switchAsIf v cs b = pair (switchAsIf (pfst v) (map ((mapPairFst pfst) . (mapPairSnd pfst)) cs) (pfst b)) (switchAsIf (psnd v) (map ((mapPairFst psnd) . (mapPairSnd psnd)) cs) (psnd b))

    ifExists cond ifBody elseBody = pair (ifExists (pfst cond) (pfst ifBody) (pfst elseBody)) (ifExists (psnd cond) (psnd ifBody) (psnd elseBody))

    for sInit vGuard sUpdate b = pair (for (pfst sInit) (pfst vGuard) (pfst sUpdate) (pfst b)) (for (psnd sInit) (psnd vGuard) (psnd sUpdate) (psnd b))
    forRange i initv finalv stepv b = pair (forRange i (pfst initv) (pfst finalv) (pfst stepv) (pfst b)) (forRange i (psnd initv) (psnd finalv) (psnd stepv) (psnd b))
    forEach l t v b = pair (forEach l (pfst t) (pfst v) (pfst b)) (forEach l (psnd t) (psnd v) (psnd b))
    while v b = pair (while (pfst v) (pfst b)) (while (psnd v) (psnd b))

    tryCatch tb cb = pair (tryCatch (pfst tb) (pfst cb)) (tryCatch (psnd tb) (psnd cb))

    checkState l vs b = pair (checkState l (map ((mapPairFst pfst) . (mapPairSnd pfst)) vs) (pfst b)) (checkState l (map ((mapPairFst psnd) . (mapPairSnd psnd)) vs) (psnd b))

    notifyObservers fn t ps = pair (notifyObservers fn (pfst t) (map pfst ps)) (notifyObservers fn (psnd t) (map psnd ps))

    getFileInputAll f v = pair (getFileInputAll (pfst f) (pfst v)) (getFileInputAll (psnd f) (psnd v))

instance (Pair p) => ScopeSym (p CppSrcCode CppHdrCode) where
    type Scope (p CppSrcCode CppHdrCode) = (Doc, ScopeTag)
    private = pair private private
    public = pair public public

    includeScope s = pair (includeScope $ pfst s) (includeScope $ psnd s)

instance (Pair p) => MethodTypeSym (p CppSrcCode CppHdrCode) where
    type MethodType (p CppSrcCode CppHdrCode) = Doc
    mState t = pair (mState $ pfst t) (mState $ psnd t)
    void = pair void void
    construct n = pair (construct n) (construct n)

instance (Pair p) => ParameterSym (p CppSrcCode CppHdrCode) where
    type Parameter (p CppSrcCode CppHdrCode) = Doc
    stateParam n t = pair (stateParam n $ pfst t) (stateParam n $ psnd t)
    pointerParam n t = pair (pointerParam n $ pfst t) (pointerParam n $ psnd t)

instance (Pair p) => MethodSym (p CppSrcCode CppHdrCode) where
    -- Bool is True if the method is a main method, False otherwise
    type Method (p CppSrcCode CppHdrCode) = (Doc, Bool, ScopeTag)
    method n c s p t ps b = pair (method n c (pfst s) (pfst p) (pfst t) (map pfst ps) (pfst b)) (method n c (psnd s) (psnd p) (psnd t) (map psnd ps) (psnd b))
    getMethod n c t = pair (getMethod n c $ pfst t) (getMethod n c $ psnd t) 
    setMethod setLbl c paramLbl t = pair (setMethod setLbl c paramLbl $ pfst t) (setMethod setLbl c paramLbl $ psnd t)
    mainMethod l b = pair (mainMethod l $ pfst b) (mainMethod l $ psnd b)
    privMethod n c t ps b = pair (privMethod n c (pfst t) (map pfst ps) (pfst b)) (privMethod n c (psnd t) (map psnd ps) (psnd b))
    pubMethod n c t ps b = pair (pubMethod n c (pfst t) (map pfst ps) (pfst b)) (pubMethod n c (psnd t) (map psnd ps) (psnd b))
    constructor n ps b = pair (constructor n (map pfst ps) (pfst b)) (constructor n (map psnd ps) (psnd b))
    destructor n vs = pair (destructor n $ map pfst vs) (destructor n $ map psnd vs)

    function n s p t ps b = pair (function n (pfst s) (pfst p) (pfst t) (map pfst ps) (pfst b)) (function n (psnd s) (psnd p) (psnd t) (map psnd ps) (psnd b))

instance (Pair p) => StateVarSym (p CppSrcCode CppHdrCode) where
    -- (Doc, Bool) is the corresponding destructor code for the stateVar
    type StateVar (p CppSrcCode CppHdrCode) = (Doc, (Doc, Terminator), ScopeTag)
    stateVar del l s p t = pair (stateVar del l (pfst s) (pfst p) (pfst t)) (stateVar del l (psnd s) (psnd p) (psnd t))
    privMVar del l t = pair (privMVar del l $ pfst t) (privMVar del l $ psnd t)
    pubMVar del l t = pair (pubMVar del l $ pfst t) (pubMVar del l $ psnd t)
    pubGVar del l t = pair (pubGVar del l $ pfst t) (pubGVar del l $ psnd t)
    listStateVar del l s p t = pair (listStateVar del l (pfst s) (pfst p) (pfst t)) (listStateVar del l (psnd s) (psnd p) (psnd t))

instance (Pair p) => ClassSym (p CppSrcCode CppHdrCode) where
    -- Bool is True if the class is a main class, False otherwise
    type Class (p CppSrcCode CppHdrCode) = (Doc, Bool)
    buildClass n p s vs fs = pair (buildClass n p (pfst s) (map pfst vs) (map pfst fs)) (buildClass n p (psnd s) (map psnd vs) (map psnd fs))
    enum l ls s = pair (enum l ls $ pfst s) (enum l ls $ psnd s)
    mainClass l vs fs = pair (mainClass l (map pfst vs) (map pfst fs)) (mainClass l (map psnd vs) (map psnd fs))
    privClass n p vs fs = pair (privClass n p (map pfst vs) (map pfst fs)) (privClass n p (map psnd vs) (map psnd fs))
    pubClass n p vs fs = pair (pubClass n p (map pfst vs) (map pfst fs)) (pubClass n p (map psnd vs) (map psnd fs))

instance (Pair p) => ModuleSym (p CppSrcCode CppHdrCode) where
    -- Label is module name
    -- Bool is True if the method is a main method, False otherwise
    type Module (p CppSrcCode CppHdrCode) = (Doc, Label, Bool)
    buildModule n l vs ms cs = pair (buildModule n l (map pfst vs) (map pfst ms) (map pfst cs)) (buildModule n l (map psnd vs) (map psnd ms) (map psnd cs))

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