module Language.Drasil.Code.Imperative.New (
    -- Types
    Class, Method, Body, Block, Statement, Declaration, Value, StateType,
    Function, StateVar, IOType, IOSt, Scope, Keyword, Label, Library, VarDecl, 
    FunctionDecl,
    -- Typeclasses
    RenderSym(..), KeywordSym(..), ClassSym(..), MethodSym(..), 
    BodySym(..), Symantics(..), StateTypeSym(..), StatementSym(..), IOTypeSym(..),
    IOStSym(..), ValueSym(..), Selector(..), FunctionSym(..)
) where

import Text.PrettyPrint.HughesPJ (Doc)

type Class = Doc
type Method = Doc
type Body = Doc
type Block = Doc
type Statement = Doc
type Declaration = Doc
type Value = Doc
type StateType = Doc
type Function = Doc
type StateVar = Doc
type IOType = Doc
type IOSt = Doc
type Scope = Doc
type Keyword = Doc

type Label = String
type Library = String
type VarDecl = Declaration
type FunctionDecl = Method

class RenderSym repr where
    fileDoc :: repr Doc -> repr Doc
    top :: repr Block -- Block is a placeholder for all of these, should change
    codeBody :: repr Class -> repr Block
    bottom :: repr Block

class KeywordSym repr where
    endStatement :: repr Keyword
    include :: repr Keyword
    list :: repr Keyword -- Later may need to be repr Permanence -> repr Keyword
    printFunc :: repr Keyword
    printLnFunc :: repr Keyword
    printFileFunc :: repr Value -> repr Keyword
    printFileLnFunc :: repr Value -> repr Keyword

class ClassSym repr where
    buildClass :: Label -> Maybe Label -> repr Scope -> [repr StateVar] -> [repr Method] -> repr Class

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

class (StateTypeSym repr, ValueSym repr, IOStSym repr) => StatementSym repr where
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

    returnState    :: repr Value -> repr Statement
    returnVar :: Label -> repr Statement

    ioState :: repr IOSt -> repr Statement

class ValueSym repr => IOTypeSym repr where
    console :: repr IOType
    file    :: repr Value -> repr IOType

class IOStSym repr where
    out :: repr Keyword -> repr Value -> repr IOSt 

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