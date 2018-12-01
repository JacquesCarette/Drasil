module New where

data Class
data Method
data Body
data Block
data Statement
data Declaration
data Value
data StateType
data Function
data IOType

type Label = String
type Library = String
type VarDecl = Declaration
type FunctionDecl = Method

data Module repr = Mod Label [Library] [repr VarDecl] [repr FunctionDecl] [repr Class]

class MethodSym repr where
    mainMethod :: repr Body -> repr Method

class BodySym repr where
    body :: [repr Statement] -> repr Body

-- Right now the Block is the top-level structure
class StatementSym repr => Symantics repr where
    block   :: [repr Statement] -> repr Block

class StateTypeSym repr where
    bool   :: repr StateType
    int    :: repr StateType
    float  :: repr StateType
    char   :: repr StateType
    string :: repr StateType

class (StateTypeSym repr, ValueSym repr, IOSym repr) => StatementSym repr where
    (&=)   :: repr Value -> repr Value -> repr Statement
    (&.=)  :: Label -> repr Value -> repr Statement
    (&=.)  :: repr Value -> Label -> repr Statement
    (&-=)  :: repr Value -> repr Value -> repr Statement
    (&.-=) :: Label -> repr Value -> repr Statement
    (&+=)  :: repr Value -> repr Value -> repr Statement
    (&.+=) :: Label -> repr Value -> repr Statement
    (&++)  :: repr Value -> repr Statement
    (&.++) :: Label -> repr Statement
    (&~-)  :: repr Value -> repr Statement
    (&.~-) :: Label -> repr Statement

    assign  :: repr Value -> repr Value -> repr Statement
    varDec  :: Label -> repr StateType -> repr Statement

    print      :: repr StateType -> repr Value -> repr Statement
    printLn    :: repr StateType -> repr Value -> repr Statement
    printStr   :: String -> repr Statement
    printStrLn :: String -> repr Statement

    print'      :: repr IOType -> repr StateType -> repr Value -> repr Statement
    printLn'    :: repr IOType -> repr StateType -> repr Value -> repr Statement
    printStr'   :: repr IOType -> String -> repr Statement
    printStrLn' :: repr IOType -> String -> repr Statement

    printFile      :: repr Value -> repr StateType -> repr Value -> repr Statement
    printFileLn    :: repr Value -> repr StateType -> repr Value -> repr Statement
    printFileStr   :: repr Value -> String -> repr Statement
    printFileStrLn :: repr Value -> String -> repr Statement

    getInput         :: repr StateType -> repr Value -> repr Statement
    getFileInput     :: repr Value -> repr StateType -> repr Value -> repr Statement
    discardFileInput :: repr Value -> repr Statement
    getFileInputLine :: repr Value -> repr Value -> repr Statement
    discardFileLine  :: repr Value -> repr Statement
    getFileInputAll  :: repr Value -> repr Value -> repr Statement

    openFileR :: repr Value -> repr Value -> repr Statement
    openFileW :: repr Value -> repr Value -> repr Statement
    closeFile :: repr Value -> repr Statement

    return    :: repr Value -> repr Statement
    returnVar :: Label -> repr Statement

class ValueSym repr => IOSym repr where
    console :: repr IOType
    file    :: repr Value -> repr IOType

class ValueSym repr where
    litBool   :: Bool -> repr Value
    litChar   :: Char -> repr Value
    litFloat  :: Double -> repr Value
    litInt    :: Integer -> repr Value
    litString :: String -> repr Value

    defaultValue :: repr StateType -> repr Value

    (?!)  :: repr Value -> repr Value
    (?<)  :: repr Value -> repr Value -> repr Value
    (?<=) :: repr Value -> repr Value -> repr Value
    (?>)  :: repr Value -> repr Value -> repr Value
    (?>=) :: repr Value -> repr Value -> repr Value
    (?==) :: repr Value -> repr Value -> repr Value
    (?!=) :: repr Value -> repr Value -> repr Value
    (?&&) :: repr Value -> repr Value -> repr Value
    (?||) :: repr Value -> repr Value -> repr Value

    --arithmetic operators (#)
    (#~)  :: repr Value -> repr Value
    (#/^) :: repr Value -> repr Value
    (#|)  :: repr Value -> repr Value
    (#+)  :: repr Value -> repr Value -> repr Value
    (#-)  :: repr Value -> repr Value -> repr Value
    (#*)  :: repr Value -> repr Value -> repr Value
    (#/)  :: repr Value -> repr Value -> repr Value
    (#%)  :: repr Value -> repr Value -> repr Value
    (#^)  :: repr Value -> repr Value -> repr Value

     --other operators ($)
    ($->) :: repr Value -> repr Value -> repr Value
    ($:)  :: Label -> Label -> repr Value

    log :: repr Value -> repr Value
    exp :: repr Value -> repr Value
    sin :: repr Value -> repr Value
    cos :: repr Value -> repr Value
    tan :: repr Value -> repr Value
    csc :: repr Value -> repr Value
    sec :: repr Value -> repr Value
    cot :: repr Value -> repr Value

class (FunctionSym repr, ValueSym repr) => Selector repr where
    ($.)  :: repr Value -> repr Function -> repr Value

class (ValueSym repr, StateTypeSym repr) => FunctionSym repr where
    func :: Label -> [repr Value] -> repr Function

    listSize   :: repr Function
    listAccess :: repr Value -> repr Function
    listAppend :: repr Value -> repr Function
    listExtend :: repr StateType -> repr Function

-- Functions

buildModule :: Label -> [Library] -> [repr VarDecl] -> [repr FunctionDecl] -> [repr Class] -> Module repr
buildModule = Mod