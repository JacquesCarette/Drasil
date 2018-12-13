{-# LANGUAGE TypeFamilies #-}

module New (
    -- Types
    Declaration, StateVar, Scope,
    Label, Library, 
    -- Typeclasses
    RenderSym(..), KeywordSym(..), InputTypeSym(..), PermanenceSym(..), ClassSym(..), MethodSym(..), 
    BodySym(..), ControlStatementSym(..), BlockSym(..), StateTypeSym(..), 
    PreStatementSym(..), StatementSym(..),
    IOStSym(..), UnaryOpSym(..), BinaryOpSym(..), ValueSym(..), Selector(..), 
    FunctionSym(..), SelectorFunction(..)
) where

import Text.PrettyPrint.HughesPJ (Doc)

-- type Class = Doc
-- type Method = Doc
-- type Body = Doc
-- type Block = Doc
-- type Control = Doc
-- type Statement = Doc
type Declaration = Doc
-- type Value = Doc
-- type StateType = Doc
-- type Function = Doc
type StateVar = Doc
-- type IOType = Doc
-- type IOSt = Doc
type Scope = Doc
-- type UnaryOp = Doc
-- type BinaryOp = Doc
-- type RenderFile = Doc
-- type Permanence = Doc
-- type Keyword = Doc

type Label = String
type Library = String
-- type VarDecl = Declaration
-- type FunctionDecl = Method

class (BodySym repr, ControlStatementSym repr) => RenderSym repr where
    type RenderFile repr
    fileDoc :: repr (Body repr) -> repr (RenderFile repr)
    top :: repr (Block repr) -- Block is a placeholder for all of these, should change
    codeBody :: repr (Class repr) -> repr (Block repr)
    bottom :: repr (Block repr)

class (ValueSym repr, PermanenceSym repr) => KeywordSym repr where
    type Keyword repr
    endStatement :: repr (Keyword repr)
    endStatementLoop :: repr (Keyword repr)
    include :: repr (Keyword repr)
    list :: repr (Permanence repr) -> repr (Keyword repr)
    argsList :: repr (Keyword repr)
    listObj :: repr (Keyword repr)
    blockStart :: repr (Keyword repr)
    blockEnd :: repr (Keyword repr)
    ifBodyStart :: repr (Keyword repr)
    elseIf :: repr (Keyword repr)
    iterForEachLabel :: repr (Keyword repr)
    iterInLabel :: repr (Keyword repr)
    commentStart :: repr (Keyword repr)

    inputFunc :: repr (Keyword repr)

    printFunc :: repr (Keyword repr)
    printLnFunc :: repr (Keyword repr)
    printFileFunc :: repr (Value repr) -> repr (Keyword repr)
    printFileLnFunc :: repr (Value repr) -> repr (Keyword repr)

class InputTypeSym repr where
    type InputType repr
    inputInt :: repr (InputType repr)
    inputFloat :: repr (InputType repr)
    inputBool :: repr (InputType repr)
    inputString :: repr (InputType repr)

class PermanenceSym repr where
    type Permanence repr
    static :: repr (Permanence repr)
    dynamic :: repr (Permanence repr)

class ClassSym repr where
    type Class repr
    buildClass :: Label -> Maybe Label -> repr Scope -> [repr StateVar] -> [repr (Method repr)] -> repr (Class repr)

class MethodSym repr where
    type Method repr
    mainMethod :: repr (Body repr) -> repr (Method repr)

class (BlockSym repr) => BodySym repr where
    type Body repr
    body :: [repr (Block repr)] -> repr (Body repr)
    bodyStatements :: [repr (PreStatement repr)] -> repr (Body repr)
    oneLiner :: repr (PreStatement repr) -> repr (Body repr)

    bodyBlockState :: [repr (Statement repr)] -> repr (Body repr) -- for statements that don't need semi colons and should be treated as a block

    addComments :: Label -> repr (Body repr) -> repr (Body repr)

-- Right now the Block is the top-level structure
class StatementSym repr => BlockSym repr where
    type Block repr
    block   :: [repr (PreStatement repr)] -> repr (Block repr)
    blockState :: [repr (Statement repr)] -> repr (Block repr) -- for when the statements don't need semi colons (i.e. an if or a for loop, etc.)

class StateTypeSym repr where
    type StateType repr
    bool   :: repr (StateType repr)
    int    :: repr (StateType repr)
    float  :: repr (StateType repr)
    char   :: repr (StateType repr)
    string :: repr (StateType repr)
    infile :: repr (StateType repr)
    outfile :: repr (StateType repr)
    listType :: repr (Permanence repr) -> repr (StateType repr) -> repr (StateType repr)
    intListType :: repr (Permanence repr) -> repr (StateType repr)
    intListType p = listType p int
    floatListType :: repr (Permanence repr) -> repr (StateType repr)
    floatListType p = listType p float
    obj :: Label -> repr (StateType repr)
    enumType :: Label -> repr (StateType repr)

class (BodySym repr, StatementSym repr) => ControlStatementSym repr where
    ifCond :: [(repr (Value repr), repr (Body repr))] -> repr (Body repr) -> repr (Statement repr) 
    switch :: repr (Value repr) -> [(repr (Value repr), repr (Body repr))] -> repr (Body repr) -> repr (Statement repr) -- is there value in separating Literals into their own type?

    ifExists :: repr (Value repr) -> repr (Body repr) -> repr (Body repr) -> repr (Statement repr)

    for     :: repr (PreStatement repr) -> repr (Value repr) -> repr (PreStatement repr) -> repr (Body repr) -> repr (Statement repr)
    -- Had to add StateType to forEach because I can't extract the StateType from the value.
    forEach :: Label -> repr (StateType repr) -> repr (Value repr) -> repr (Body repr) -> repr (Statement repr)
    while   :: repr (Value repr) -> repr (Body repr) -> repr (Statement repr) 

    tryCatch :: repr (Body repr) -> repr (Body repr) -> repr (Statement repr)

    checkState      :: Label -> [(repr (Value repr), repr (Body repr))] -> repr (Body repr) -> repr (Statement repr)
    runStrategy     :: Label -> [(Label, repr (Body repr))] -> Maybe (repr (Value repr)) -> Maybe (repr (Value repr)) -> repr (Statement repr)
    notifyObservers :: Label -> repr (StateType repr) -> [repr (Value repr)] -> repr (Statement repr)

    getFileInputAll  :: repr (Value repr) -> repr (Value repr) -> repr (Statement repr)
    listSlice        :: repr (StateType repr) -> repr (Value repr) -> repr (Value repr) -> Maybe (repr (Value repr)) -> Maybe (repr (Value repr)) -> Maybe (repr (Value repr)) -> repr (Statement repr)

    statement  :: repr (PreStatement repr) -> repr (Statement repr)
    statements :: [repr (PreStatement repr)] -> repr (Statement repr)

class (PermanenceSym repr, StateTypeSym repr, ValueSym repr, IOStSym repr, InputTypeSym repr, Selector repr, SelectorFunction repr, FunctionSym repr) => PreStatementSym repr where
    type PreStatement repr
    (&=)   :: repr (Value repr) -> repr (Value repr) -> repr (PreStatement repr)
    (&.=)  :: Label -> repr (Value repr) -> repr (PreStatement repr)
    (&=.)  :: repr (Value repr) -> Label -> repr (PreStatement repr)
    (&-=)  :: repr (Value repr) -> repr (Value repr) -> repr (PreStatement repr)
    (&.-=) :: Label -> repr (Value repr) -> repr (PreStatement repr)
    (&+=)  :: repr (Value repr) -> repr (Value repr) -> repr (PreStatement repr)
    (&.+=) :: Label -> repr (Value repr) -> repr (PreStatement repr)
    (&++)  :: repr (Value repr) -> repr (PreStatement repr)
    (&.++) :: Label -> repr (PreStatement repr)
    (&~-)  :: repr (Value repr) -> repr (PreStatement repr)
    (&.~-) :: Label -> repr (PreStatement repr)

    assign  :: repr (Value repr) -> repr (Value repr) -> repr (PreStatement repr)

    varDec        :: Label -> repr (StateType repr) -> repr (PreStatement repr)
    varDecDef     :: Label -> repr (StateType repr) -> repr (Value repr) -> repr (PreStatement repr)
    listDec       :: Label -> Integer -> repr (StateType repr) -> repr (PreStatement repr)
    listDecDef    :: Label -> repr (StateType repr) -> [repr (Value repr)] -> repr (PreStatement repr)
    objDecDef     :: Label -> repr (StateType repr) -> repr (Value repr) -> repr (PreStatement repr)
    objDecNew     :: Label -> repr (StateType repr) -> [repr (Value repr)] -> repr (PreStatement repr)
    objDecNewVoid :: Label -> repr (StateType repr) -> repr (PreStatement repr)
    constDecDef   :: Label -> repr (StateType repr) -> repr (Value repr) -> repr (PreStatement repr)

    -- Loop versions of same functions (better way to do this? I want to avoid
    -- making a GOOL user have to specify either loopState or state for every
    -- statement they write, and this is the shortest way to do that)

    -- Maybe have almost everything here be "PreStatement", and then StatementSym
    -- only has state and loopState. Block/Body/Prog etc. would apply state
    -- to all statements, whereas for would apply loopState

    print      :: repr (StateType repr) -> repr (Value repr) -> repr (PreStatement repr)
    printLn    :: repr (StateType repr) -> repr (Value repr) -> repr (PreStatement repr)
    printStr   :: String -> repr (PreStatement repr)
    printStrLn :: String -> repr (PreStatement repr)

    printFile      :: repr (Value repr) -> repr (StateType repr) -> repr (Value repr) -> repr (PreStatement repr)
    printFileLn    :: repr (Value repr) -> repr (StateType repr) -> repr (Value repr) -> repr (PreStatement repr)
    printFileStr   :: repr (Value repr) -> String -> repr (PreStatement repr)
    printFileStrLn :: repr (Value repr) -> String -> repr (PreStatement repr)

    printList       :: repr (StateType repr) -> repr (Value repr) -> repr (PreStatement repr)
    printLnList     :: repr (StateType repr) -> repr (Value repr) -> repr (PreStatement repr)
    printFileList   :: repr (Value repr) -> repr (StateType repr) -> repr (Value repr) -> repr (PreStatement repr)
    printFileLnList :: repr (Value repr) -> repr (StateType repr) -> repr (Value repr) -> repr (PreStatement repr)

    getInput         :: repr (InputType repr) -> repr (Value repr) -> repr (PreStatement repr)
    discardInput     :: repr (InputType repr) -> repr (PreStatement repr)
    getFileInput     :: repr (Value repr) -> repr (StateType repr) -> repr (Value repr) -> repr (PreStatement repr)
    discardFileInput :: repr (Value repr) -> repr (PreStatement repr)

    openFileR :: repr (Value repr) -> repr (Value repr) -> repr (PreStatement repr)
    openFileW :: repr (Value repr) -> repr (Value repr) -> repr (PreStatement repr)
    closeFile :: repr (Value repr) -> repr (PreStatement repr)

    getFileInputLine :: repr (Value repr) -> repr (Value repr) -> repr (PreStatement repr)
    discardFileLine  :: repr (Value repr) -> repr (PreStatement repr)
    stringSplit      :: Char -> repr (Value repr) -> repr (Value repr) -> repr (PreStatement repr)

    break :: repr (PreStatement repr)
    continue :: repr (PreStatement repr)

    returnState :: repr (Value repr) -> repr (PreStatement repr)
    returnVar :: Label -> repr (PreStatement repr)

    valState :: repr (Value repr) -> repr (PreStatement repr)

    comment :: Label -> repr (PreStatement repr)

    free :: repr (Value repr) -> repr (PreStatement repr)

    throw :: Label -> repr (PreStatement repr)

    initState   :: Label -> Label -> repr (PreStatement repr)
    changeState :: Label -> Label -> repr (PreStatement repr)

    initObserverList :: repr (StateType repr) -> [repr (Value repr)] -> repr (PreStatement repr)
    addObserver      :: repr (StateType repr) -> repr (Value repr) -> repr (PreStatement repr)

class (PreStatementSym repr, IOStSym repr) => StatementSym repr where
    type Statement repr
    ioState :: repr (IOSt repr) -> repr (Statement repr)

    state :: repr (PreStatement repr) -> repr (Statement repr)
    loopState :: repr (PreStatement repr) -> repr (Statement repr)

class (KeywordSym repr, ValueSym repr) => IOStSym repr where
    type IOSt repr
    out :: repr (Keyword repr) -> repr (Value repr) -> repr (IOSt repr) 

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

class (StateTypeSym repr) => ValueSym repr where
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
    floor :: repr (Value repr) -> repr (Value repr)
    ceil  :: repr (Value repr) -> repr (Value repr)

     --other operators ($)
    ($->) :: repr (Value repr) -> repr (Value repr) -> repr (Value repr)
    ($:)  :: Label -> Label -> repr (Value repr)

    log :: repr (Value repr) -> repr (Value repr)
    ln  :: repr (Value repr) -> repr (Value repr)
    exp :: repr (Value repr) -> repr (Value repr)
    sin :: repr (Value repr) -> repr (Value repr)
    cos :: repr (Value repr) -> repr (Value repr)
    tan :: repr (Value repr) -> repr (Value repr)
    csc :: repr (Value repr) -> repr (Value repr)
    sec :: repr (Value repr) -> repr (Value repr)
    cot :: repr (Value repr) -> repr (Value repr)

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
    extFuncApp   :: Library -> Label -> [repr (Value repr)] -> repr (Value repr)
    stateObj     :: repr (StateType repr) -> [repr (Value repr)] -> repr (Value repr) -- did not include version with Library because it gets ignored by default. May need to add later.
    listStateObj :: repr (StateType repr) -> [repr (Value repr)] -> repr (Value repr)

    exists  :: repr (Value repr) -> repr (Value repr)
    notNull :: repr (Value repr) -> repr (Value repr)

class (FunctionSym repr, ValueSym repr) => Selector repr where
    objAccess :: repr (Value repr) -> repr (Function repr) -> repr (Value repr)
    ($.)      :: repr (Value repr) -> repr (Function repr) -> repr (Value repr)

    objMethodCall     :: repr (Value repr) -> Label -> [repr (Value repr)] -> repr (Value repr)
    objMethodCallVoid :: repr (Value repr) -> Label -> repr (Value repr)

    selfAccess :: Label -> [repr (Value repr)] -> repr (Value repr)

    castObj        :: repr (Function repr) -> repr (Value repr) -> repr (Value repr)
    castStrToFloat :: repr (Value repr) -> repr (Function repr)

class (ValueSym repr, StateTypeSym repr) => FunctionSym repr where
    type Function repr
    func           :: Label -> [repr (Value repr)] -> repr (Function repr)
    cast           :: repr (StateType repr) -> repr (StateType repr) -> repr (Function repr)
    castListToInt  :: repr (Function repr)
    get            :: Label -> repr (Function repr)
    set            :: Label -> repr (Value repr) -> repr (Function repr)

    indexOf :: repr (Value repr) -> repr (Function repr)

    listSize         :: repr (Function repr)
    listAdd          :: repr (Value repr) -> repr (Value repr) -> repr (Function repr)
    listPopulate     :: repr (Value repr) -> repr (StateType repr) -> repr (Function repr)
    listAppend       :: repr (Value repr) -> repr (Function repr)
    listExtendInt    :: repr (Function repr)
    listExtendFloat  :: repr (Function repr)
    listExtendChar   :: repr (Function repr)
    listExtendBool   :: repr (Function repr)
    listExtendString :: repr (Function repr)
    listExtendList   :: repr (StateType repr) -> repr (Function repr)

    iterBegin :: repr (Function repr)
    iterEnd   :: repr (Function repr)

class (ValueSym repr, StateTypeSym repr, FunctionSym repr, Selector repr) => SelectorFunction repr where
    listAccess :: repr (Value repr) -> repr (Function repr)
    listSet    :: repr (Value repr) -> repr (Value repr) -> repr (Function repr)

    listAccessEnum   :: repr(StateType repr) -> repr (Value repr) -> repr (Function repr)
    listSetEnum      :: repr (StateType repr) -> repr (Value repr) -> repr (Value repr) -> repr (Function repr)

    at :: Label -> repr (Function repr)