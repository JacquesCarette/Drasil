{-# LANGUAGE TypeFamilies #-}

module New (
    -- Types
    Label, Library, 
    -- Typeclasses
    RenderSym(..), KeywordSym(..), PermanenceSym(..),
    BodySym(..), ControlBlockSym(..), BlockSym(..), StateTypeSym(..), 
    StatementSym(..),
    UnaryOpSym(..), BinaryOpSym(..), ValueSym(..), Selector(..), 
    FunctionSym(..), SelectorFunction(..), ScopeSym(..), MethodTypeSym(..),
    ParameterSym(..), MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..)
) where

import Text.PrettyPrint.HughesPJ (Doc)

type Declaration = Doc

type Label = String
type Library = String

class (ModuleSym repr, ControlBlockSym repr) => RenderSym repr where
    type RenderFile repr
    fileDoc :: repr (Module repr) -> repr (RenderFile repr)
    top :: repr (Block repr)
    bottom :: repr (Block repr)

class (ValueSym repr, PermanenceSym repr) => KeywordSym repr where
    type Keyword repr
    endStatement     :: repr (Keyword repr)
    endStatementLoop :: repr (Keyword repr)

    include :: Label -> repr (Keyword repr)
    inherit :: repr (Keyword repr)

    list     :: repr (Permanence repr) -> repr (Keyword repr)
    argsList :: repr (Keyword repr)
    listObj  :: repr (Keyword repr)

    blockStart :: repr (Keyword repr)
    blockEnd   :: repr (Keyword repr)

    ifBodyStart :: repr (Keyword repr)
    elseIf      :: repr (Keyword repr)

    iterForEachLabel :: repr (Keyword repr)
    iterInLabel      :: repr (Keyword repr)
    
    commentStart :: repr (Keyword repr)

    printFunc       :: repr (Keyword repr)
    printLnFunc     :: repr (Keyword repr)
    printFileFunc   :: repr (Value repr) -> repr (Keyword repr)
    printFileLnFunc :: repr (Value repr) -> repr (Keyword repr)

class PermanenceSym repr where
    type Permanence repr
    static  :: repr (Permanence repr)
    dynamic :: repr (Permanence repr)

class (BlockSym repr) => BodySym repr where
    type Body repr
    body           :: [repr (Block repr)] -> repr (Body repr)
    bodyStatements :: [repr (Statement repr)] -> repr (Body repr)
    oneLiner       :: repr (Statement repr) -> repr (Body repr)

    addComments :: Label -> repr (Body repr) -> repr (Body repr)

class (StatementSym repr) => BlockSym repr where
    type Block repr
    block   :: [repr (Statement repr)] -> repr (Block repr)

class (PermanenceSym repr) => StateTypeSym repr where
    type StateType repr
    bool          :: repr (StateType repr)
    int           :: repr (StateType repr)
    float         :: repr (StateType repr)
    char          :: repr (StateType repr)
    string        :: repr (StateType repr)
    infile        :: repr (StateType repr)
    outfile       :: repr (StateType repr)
    listType      :: repr (Permanence repr) -> repr (StateType repr) -> repr (StateType repr)
    intListType   :: repr (Permanence repr) -> repr (StateType repr)
    intListType p = listType p int
    floatListType :: repr (Permanence repr) -> repr (StateType repr)
    floatListType p = listType p float
    boolListType  :: repr (StateType repr)
    obj           :: Label -> repr (StateType repr)
    enumType      :: Label -> repr (StateType repr)

class (BodySym repr) => ControlBlockSym repr where
    ifCond     :: [(repr (Value repr), repr (Body repr))] -> repr (Body repr) -> repr (Block repr) 
    switch     :: repr (Value repr) -> [(repr (Value repr), repr (Body repr))] -> repr (Body repr) -> repr (Block repr) -- is there value in separating Literals into their own type?
    switchAsIf :: repr (Value repr) -> [(repr (Value repr), repr (Body repr))] -> repr (Body repr) -> repr (Block repr)

    ifExists :: repr (Value repr) -> repr (Body repr) -> repr (Body repr) -> repr (Block repr)

    for      :: repr (Statement repr) -> repr (Value repr) -> repr (Statement repr) -> repr (Body repr) -> repr (Block repr)
    forRange :: Label -> repr (Value repr) -> repr (Value repr) -> repr (Value repr) -> repr (Body repr) -> repr (Block repr)
    -- Had to add StateType to forEach because I can't extract the StateType from the value.
    forEach  :: Label -> repr (StateType repr) -> repr (Value repr) -> repr (Body repr) -> repr (Block repr)
    while    :: repr (Value repr) -> repr (Body repr) -> repr (Block repr) 

    tryCatch :: repr (Body repr) -> repr (Body repr) -> repr (Block repr)

    checkState      :: Label -> [(repr (Value repr), repr (Body repr))] -> repr (Body repr) -> repr (Block repr)
    runStrategy     :: Label -> [(Label, repr (Body repr))] -> Maybe (repr (Value repr)) -> Maybe (repr (Value repr)) -> repr (Block repr)
    notifyObservers :: Label -> repr (StateType repr) -> [repr (Value repr)] -> repr (Block repr)

    getFileInputAll  :: repr (Value repr) -> repr (Value repr) -> repr (Block repr)
    listSlice        :: repr (StateType repr) -> repr (Value repr) -> repr (Value repr) -> Maybe (repr (Value repr)) -> Maybe (repr (Value repr)) -> Maybe (repr (Value repr)) -> repr (Block repr)

class (ValueSym repr, Selector repr, SelectorFunction repr, FunctionSym repr) => StatementSym repr where
    type Statement repr
    (&=)   :: repr (Value repr) -> repr (Value repr) -> repr (Statement repr)
    (&.=)  :: Label -> repr (Value repr) -> repr (Statement repr)
    (&=.)  :: repr (Value repr) -> Label -> repr (Statement repr)
    (&-=)  :: repr (Value repr) -> repr (Value repr) -> repr (Statement repr)
    (&.-=) :: Label -> repr (Value repr) -> repr (Statement repr)
    (&+=)  :: repr (Value repr) -> repr (Value repr) -> repr (Statement repr)
    (&.+=) :: Label -> repr (Value repr) -> repr (Statement repr)
    (&++)  :: repr (Value repr) -> repr (Statement repr)
    (&.++) :: Label -> repr (Statement repr)
    (&~-)  :: repr (Value repr) -> repr (Statement repr)
    (&.~-) :: Label -> repr (Statement repr)

    assign            :: repr (Value repr) -> repr (Value repr) -> repr (Statement repr)
    assignToListIndex :: repr (Value repr) -> repr (Value repr) -> repr (Value repr) -> repr (Statement repr)

    varDec        :: Label -> repr (StateType repr) -> repr (Statement repr)
    varDecDef     :: Label -> repr (StateType repr) -> repr (Value repr) -> repr (Statement repr)
    listDec       :: Label -> Integer -> repr (StateType repr) -> repr (Statement repr)
    listDecDef    :: Label -> repr (StateType repr) -> [repr (Value repr)] -> repr (Statement repr)
    objDecDef     :: Label -> repr (StateType repr) -> repr (Value repr) -> repr (Statement repr)
    objDecNew     :: Label -> repr (StateType repr) -> [repr (Value repr)] -> repr (Statement repr)
    objDecNewVoid :: Label -> repr (StateType repr) -> repr (Statement repr)
    constDecDef   :: Label -> repr (StateType repr) -> repr (Value repr) -> repr (Statement repr)

    print      :: repr (StateType repr) -> repr (Value repr) -> repr (Statement repr)
    printLn    :: repr (StateType repr) -> repr (Value repr) -> repr (Statement repr)
    printStr   :: String -> repr (Statement repr)
    printStrLn :: String -> repr (Statement repr)

    printFile      :: repr (Value repr) -> repr (StateType repr) -> repr (Value repr) -> repr (Statement repr)
    printFileLn    :: repr (Value repr) -> repr (StateType repr) -> repr (Value repr) -> repr (Statement repr)
    printFileStr   :: repr (Value repr) -> String -> repr (Statement repr)
    printFileStrLn :: repr (Value repr) -> String -> repr (Statement repr)

    printList       :: repr (StateType repr) -> repr (Value repr) -> repr (Statement repr)
    printLnList     :: repr (StateType repr) -> repr (Value repr) -> repr (Statement repr)
    printFileList   :: repr (Value repr) -> repr (StateType repr) -> repr (Value repr) -> repr (Statement repr)
    printFileLnList :: repr (Value repr) -> repr (StateType repr) -> repr (Value repr) -> repr (Statement repr)

    getIntInput        :: repr (Value repr) -> repr (Statement repr)
    getFloatInput      :: repr (Value repr) -> repr (Statement repr)
    getBoolInput       :: repr (Value repr) -> repr (Statement repr)
    getStringInput     :: repr (Value repr) -> repr (Statement repr)
    getCharInput       :: repr (Value repr) -> repr (Statement repr)
    discardInput       :: repr (Statement repr)
    getIntFileInput    :: repr (Value repr) -> repr (Value repr) -> repr (Statement repr)
    getFloatFileInput  :: repr (Value repr) -> repr (Value repr) -> repr (Statement repr)
    getBoolFileInput   :: repr (Value repr) -> repr (Value repr) -> repr (Statement repr)
    getStringFileInput :: repr (Value repr) -> repr (Value repr) -> repr (Statement repr)
    getCharFileInput   :: repr (Value repr) -> repr (Value repr) -> repr (Statement repr)
    discardFileInput   :: repr (Value repr) -> repr (Statement repr)

    openFileR :: repr (Value repr) -> repr (Value repr) -> repr (Statement repr)
    openFileW :: repr (Value repr) -> repr (Value repr) -> repr (Statement repr)
    closeFile :: repr (Value repr) -> repr (Statement repr)

    getFileInputLine :: repr (Value repr) -> repr (Value repr) -> repr (Statement repr)
    discardFileLine  :: repr (Value repr) -> repr (Statement repr)
    stringSplit      :: Char -> repr (Value repr) -> repr (Value repr) -> repr (Statement repr)

    break :: repr (Statement repr)
    continue :: repr (Statement repr)

    returnState :: repr (Value repr) -> repr (Statement repr)
    returnVar :: Label -> repr (Statement repr)

    valState :: repr (Value repr) -> repr (Statement repr)

    comment :: Label -> repr (Statement repr)

    free :: repr (Value repr) -> repr (Statement repr)

    throw :: Label -> repr (Statement repr)

    initState   :: Label -> Label -> repr (Statement repr)
    changeState :: Label -> Label -> repr (Statement repr)

    initObserverList :: repr (StateType repr) -> [repr (Value repr)] -> repr (Statement repr)
    addObserver      :: repr (StateType repr) -> repr (Value repr) -> repr (Statement repr)

    state :: repr (Statement repr) -> repr (Statement repr)
    loopState :: repr (Statement repr) -> repr (Statement repr)

class UnaryOpSym repr where
    type UnaryOp repr
    notOp    :: repr (UnaryOp repr)
    negateOp :: repr (UnaryOp repr)
    sqrtOp   :: repr (UnaryOp repr)
    absOp    :: repr (UnaryOp repr)
    logOp    :: repr (UnaryOp repr)
    lnOp     :: repr (UnaryOp repr)
    expOp    :: repr (UnaryOp repr)
    sinOp    :: repr (UnaryOp repr)
    cosOp    :: repr (UnaryOp repr)
    tanOp    :: repr (UnaryOp repr)
    floorOp  :: repr (UnaryOp repr)
    ceilOp   :: repr (UnaryOp repr)

class BinaryOpSym repr where
    type BinaryOp repr
    equalOp        :: repr (BinaryOp repr)
    notEqualOp     :: repr (BinaryOp repr)
    greaterOp      :: repr (BinaryOp repr)
    greaterEqualOp :: repr (BinaryOp repr)
    lessOp         :: repr (BinaryOp repr)
    lessEqualOp    :: repr (BinaryOp repr)
    plusOp         :: repr (BinaryOp repr)
    minusOp        :: repr (BinaryOp repr)
    multOp         :: repr (BinaryOp repr)
    divideOp       :: repr (BinaryOp repr)
    powerOp        :: repr (BinaryOp repr)
    moduloOp       :: repr (BinaryOp repr)
    andOp          :: repr (BinaryOp repr)
    orOp           :: repr (BinaryOp repr)

class (StateTypeSym repr, StateVarSym repr) => ValueSym repr where
    type Value repr
    litTrue   :: repr (Value repr)
    litFalse  :: repr (Value repr)
    litChar   :: Char -> repr (Value repr)
    litFloat  :: Double -> repr (Value repr)
    litInt    :: Integer -> repr (Value repr)
    litString :: String -> repr (Value repr)

    defaultChar   :: repr (Value repr)
    defaultFloat  :: repr (Value repr)
    defaultInt    :: repr (Value repr)
    defaultString :: repr (Value repr)
    defaultBool   :: repr (Value repr)

    (?!)  :: repr (Value repr) -> repr (Value repr)  -- where to specify infix?
    (?<)  :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
    (?<=) :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
    (?>)  :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
    (?>=) :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
    (?==) :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
    (?!=) :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
    (?&&) :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
    (?||) :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)

    --arithmetic operators (#)
    (#~)  :: repr (Value repr) -> repr (Value repr)
    (#/^) :: repr (Value repr) -> repr (Value repr)
    (#|)  :: repr (Value repr) -> repr (Value repr)
    (#+)  :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
    (#-)  :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
    (#*)  :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
    (#/)  :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
    (#%)  :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
    (#^)  :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)

     --other operators ($)
    ($->) :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
    ($:)  :: Label -> Label -> repr (Value repr)

    log   :: repr (Value repr) -> repr (Value repr)
    ln    :: repr (Value repr) -> repr (Value repr)
    exp   :: repr (Value repr) -> repr (Value repr)
    sin   :: repr (Value repr) -> repr (Value repr)
    cos   :: repr (Value repr) -> repr (Value repr)
    tan   :: repr (Value repr) -> repr (Value repr)
    csc   :: repr (Value repr) -> repr (Value repr)
    sec   :: repr (Value repr) -> repr (Value repr)
    cot   :: repr (Value repr) -> repr (Value repr)
    floor :: repr (Value repr) -> repr (Value repr)
    ceil  :: repr (Value repr) -> repr (Value repr)

    const        :: Label -> repr (Value repr)
    var          :: Label -> repr (Value repr)
    extVar       :: Library -> Label -> repr (Value repr)
--    global       :: Label -> repr (Value repr)         -- not sure how this one works
    self         :: repr (Value repr)
    arg          :: Integer -> repr (Value repr)
    enumElement  :: Label -> Label -> repr (Value repr)
    enumVar      :: Label -> repr (Value repr)
    objVar       :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
    objVarSelf   :: Label -> repr (Value repr)
    listVar      :: Label -> repr (StateType repr) -> repr (Value repr)
    listOf       :: Label -> repr (StateType repr) -> repr (Value repr)
    inlineIf     :: repr (Value repr) -> repr (Value repr) -> repr (Value repr) -> repr (Value repr)
    funcApp      :: Label -> [repr (Value repr)] -> repr (Value repr)
    selfFuncApp  :: Label -> [repr (Value repr)] -> repr (Value repr)
    extFuncApp   :: Library -> Label -> [repr (Value repr)] -> repr (Value repr)
    stateObj     :: repr (StateType repr) -> [repr (Value repr)] -> repr (Value repr)
    extStateObj  :: Library -> repr (StateType repr) -> [repr (Value repr)] -> repr (Value repr)
    listStateObj :: repr (StateType repr) -> [repr (Value repr)] -> repr (Value repr)
    
    inputFunc :: repr (Value repr)

    stringEqual :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)

    exists          :: repr (Value repr) -> repr (Value repr)
    listIndexExists :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
    argExists       :: Integer -> repr (Value repr)
    notNull :: repr (Value repr) -> repr (Value repr)

class (FunctionSym repr, ValueSym repr) => Selector repr where
    objAccess :: repr (Value repr) -> repr (Function repr) -> repr (Value repr)
    ($.)      :: repr (Value repr) -> repr (Function repr) -> repr (Value repr)

    objMethodCall     :: repr (Value repr) -> Label -> [repr (Value repr)] -> repr (Value repr)
    objMethodCallVoid :: repr (Value repr) -> Label -> repr (Value repr)

    selfAccess :: repr (Function repr) -> repr (Value repr)

    listPopulateAccess :: repr (Value repr) -> repr (Function repr) -> repr (Value repr)
    listSizeAccess     :: repr (Value repr) -> repr (Value repr)

    castObj        :: repr (Function repr) -> repr (Value repr) -> repr (Value repr)
    castStrToFloat :: repr (Value repr) -> repr (Function repr)

class (ValueSym repr) => FunctionSym repr where
    type Function repr
    func           :: Label -> [repr (Value repr)] -> repr (Function repr)
    cast           :: repr (StateType repr) -> repr (StateType repr) -> repr (Function repr)
    castListToInt  :: repr (Function repr)
    get            :: Label -> repr (Function repr)
    set            :: Label -> repr (Value repr) -> repr (Function repr)

    indexOf :: repr (Value repr) -> repr (Function repr)

    listSize           :: repr (Function repr)
    listAdd            :: repr (Value repr) -> repr (Value repr) -> repr (Function repr)
    listPopulateInt    :: repr (Value repr) -> repr (Function repr)
    listPopulateFloat  :: repr (Value repr) -> repr (Function repr)
    listPopulateChar   :: repr (Value repr) -> repr (Function repr)
    listPopulateBool   :: repr (Value repr) -> repr (Function repr)
    listPopulateString :: repr (Value repr) -> repr (Function repr)
    listPopulateList   :: repr (Value repr) -> repr (Function repr)
    listAppend         :: repr (Value repr) -> repr (Function repr)
    listExtendInt      :: repr (Function repr)
    listExtendFloat    :: repr (Function repr)
    listExtendChar     :: repr (Function repr)
    listExtendBool     :: repr (Function repr)
    listExtendString   :: repr (Function repr)
    listExtendList     :: repr (StateType repr) -> repr (Function repr)

    iterBegin :: repr (Function repr)
    iterEnd   :: repr (Function repr)

class (ValueSym repr, FunctionSym repr, Selector repr) => SelectorFunction repr where
    listAccess :: repr (Value repr) -> repr (Function repr)
    listSet    :: repr (Value repr) -> repr (Value repr) -> repr (Function repr)

    listAccessEnum   :: repr(StateType repr) -> repr (Value repr) -> repr (Function repr)
    listSetEnum      :: repr (StateType repr) -> repr (Value repr) -> repr (Value repr) -> repr (Function repr)

    at :: Label -> repr (Function repr)

class ScopeSym repr where
    type Scope repr
    private :: repr (Scope repr)
    public  :: repr (Scope repr)

    includeScope :: repr (Scope repr) -> repr (Scope repr)

class MethodTypeSym repr where
    type MethodType repr
    mState    :: repr (StateType repr) -> repr (MethodType repr)
    void      :: repr (MethodType repr)
    construct :: Label -> repr (MethodType repr)

class ParameterSym repr where
    type Parameter repr
    stateParam :: Label -> repr (StateType repr) -> repr (Parameter repr)
    funcParam  :: Label -> repr (MethodType repr) -> [repr (Parameter repr)] -> repr (Parameter repr) 

class (ScopeSym repr, MethodTypeSym repr, ParameterSym repr, BodySym repr) => MethodSym repr where
    type Method repr
    method      :: Label -> repr (Scope repr) -> repr (Permanence repr) -> repr (MethodType repr) -> [repr (Parameter repr)] -> repr (Body repr) -> repr (Method repr)
    getMethod   :: Label -> repr (MethodType repr) -> repr (Method repr)
    setMethod   :: Label -> Label -> repr (StateType repr) -> repr (Method repr) 
    mainMethod  :: repr (Body repr) -> repr (Method repr)
    privMethod  :: Label -> repr (MethodType repr) -> [repr (Parameter repr)] -> repr (Body repr) -> repr (Method repr)
    pubMethod   :: Label -> repr (MethodType repr) -> [repr (Parameter repr)] -> repr (Body repr) -> repr (Method repr)
    constructor :: Label -> [repr (Parameter repr)] -> repr (Body repr) -> repr (Method repr)

    function :: Label -> repr (Scope repr) -> repr (Permanence repr) -> repr (MethodType repr) -> [repr (Parameter repr)] -> repr (Body repr) -> repr (Method repr)  -- For methods outside of classes, not sure if function is a good name

class (ScopeSym repr, PermanenceSym repr, StateTypeSym repr) => StateVarSym repr where
    type StateVar repr
    stateVar :: Int -> Label -> repr (Scope repr) -> repr (Permanence repr) -> repr (StateType repr) -> repr (StateVar repr)
    privMVar :: Int -> Label -> repr (StateType repr) -> repr (StateVar repr)
    pubMVar  :: Int -> Label -> repr (StateType repr) -> repr (StateVar repr)
    pubGVar  :: Int -> Label -> repr (StateType repr) -> repr (StateVar repr)

class (StateVarSym repr, MethodSym repr) => ClassSym repr where
    type Class repr
    buildClass :: Label -> Maybe Label -> repr (Scope repr) -> [repr (StateVar repr)] -> [repr (Method repr)] -> repr (Class repr)
    enum :: Label -> [Label] -> repr (Scope repr) -> repr (Class repr)
    mainClass :: Label -> [repr (StateVar repr)] -> [repr (Method repr)] -> repr (Class repr)
    privClass ::Label -> Maybe Label -> [repr (StateVar repr)] -> [repr (Method repr)] -> repr (Class repr)
    pubClass ::Label -> Maybe Label -> [repr (StateVar repr)] -> [repr (Method repr)] -> repr (Class repr)

class (ClassSym repr) => ModuleSym repr where
    type Module repr
    buildModule :: Label -> [Library] -> [repr (Statement repr)] -> [repr (Method repr)] -> [repr (Class repr)] -> repr (Module repr)