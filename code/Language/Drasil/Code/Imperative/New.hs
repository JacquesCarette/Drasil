{-# LANGUAGE TypeFamilies, TypeOperators, Rank2Types, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
{-# OPTIONS_GHC -W #-}

module New where

--import qualified Data.Function as F
--import Language.Haskell.TH

-- base types
--data BooleanT
--data IntegerT
--data FloatT 
--data CharacterT 
--data StringT

data Block
data Statement
data Value
data StateType
data Function
data IOType

type Label = String

class Symantics repr where
    block   :: (StatementSym reprS) => [reprS Statement] -> repr Block
    

class StateTypeSym repr where
    bool   :: repr StateType
    int    :: repr StateType
    float  :: repr StateType
    char   :: repr StateType
    string :: repr StateType
    

class StatementSym repr where
    (&=)   :: (ValueSym reprV) => reprV Value -> reprV Value -> repr Statement
    (&.=)  :: (ValueSym reprV) => Label -> reprV Value -> repr Statement
    (&=.)  :: (ValueSym reprV) => reprV Value -> Label -> repr Statement
    (&-=)  :: (ValueSym reprV) => reprV Value -> reprV Value -> repr Statement
    (&.-=) :: (ValueSym reprV) => Label -> reprV Value -> repr Statement
    (&+=)  :: (ValueSym reprV) => reprV Value -> reprV Value -> repr Statement
    (&.+=) :: (ValueSym reprV) => Label -> reprV Value -> repr Statement
    (&++)  :: (ValueSym reprV) => reprV Value -> repr Statement
    (&.++) :: Label -> repr Statement
    (&~-)  :: (ValueSym reprV) => reprV Value -> repr Statement
    (&.~-) :: Label -> repr Statement
    
    assign  :: (ValueSym reprV) => reprV Value -> reprV Value -> repr Statement
    varDec  :: (StateTypeSym reprST) => Label -> reprST StateType -> repr Statement

    print      :: (StateTypeSym reprST, ValueSym reprV) => 
                    reprST StateType -> reprV Value -> repr Statement
    printLn    :: (StateTypeSym reprST, ValueSym reprV) =>
                    reprST StateType -> reprV Value -> repr Statement
    printStr   :: String -> repr Statement
    printStrLn :: String -> repr Statement
    
    print'      :: (IOSym reprI, StateTypeSym reprST, ValueSym reprV) => 
                     reprI IOType -> reprST StateType -> reprV Value -> repr Statement
    printLn'    :: (IOSym reprI, StateTypeSym reprST, ValueSym reprV) => 
                     reprI IOType -> reprST StateType -> reprV Value -> repr Statement
    printStr'   :: (IOSym reprI) => reprI IOType -> String -> repr Statement
    printStrLn' :: (IOSym reprI) => reprI IOType -> String -> repr Statement
    
    printFile      :: (StateTypeSym reprST, ValueSym reprV) =>
                        reprV Value -> reprST StateType -> reprV Value -> repr Statement
    printFileLn    :: (StateTypeSym reprST, ValueSym reprV) =>
                        reprV Value -> reprST StateType -> reprV Value -> repr Statement
    printFileStr   :: (ValueSym reprV) => reprV Value -> String -> repr Statement
    printFileStrLn :: (ValueSym reprV) => reprV Value -> String -> repr Statement

    getInput         :: (StateTypeSym reprST, ValueSym reprV) =>
                          reprST StateType -> reprV Value -> repr Statement
    getFileInput     :: (StateTypeSym reprST, ValueSym reprV) =>
                          reprV Value -> reprST StateType -> reprV Value -> repr Statement
    discardFileInput :: (ValueSym reprV) => reprV Value -> repr Statement
    getFileInputLine :: (ValueSym reprV) => 
                          reprV Value -> reprV Value -> repr Statement
    discardFileLine  :: (ValueSym reprV) => reprV Value -> repr Statement
    getFileInputAll  :: (ValueSym reprV) => 
                          reprV Value -> reprV Value -> repr Statement

    openFileR :: (ValueSym reprV) => 
                   reprV Value -> reprV Value -> repr Statement
    openFileW :: (ValueSym reprV) => 
                   reprV Value -> reprV Value -> repr Statement
    closeFile :: (ValueSym reprV) => reprV Value -> repr Statement

    return    :: (ValueSym reprV) => reprV Value -> repr Statement
    returnVar :: Label -> repr Statement

    
class IOSym repr where
    console :: repr IOType
    file    :: (ValueSym reprV) => reprV Value -> repr IOType
    
    
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
    ($.)  :: (FunctionSym reprF) => repr Value -> reprF Function -> repr Value
    ($:)  :: Label -> Label -> repr Value
    
    
    log :: repr Value -> repr Value
    exp :: repr Value -> repr Value
    sin :: repr Value -> repr Value
    cos :: repr Value -> repr Value
    tan :: repr Value -> repr Value
    csc :: repr Value -> repr Value
    sec :: repr Value -> repr Value
    cot :: repr Value -> repr Value
    
    
class FunctionSym repr where
    func :: (ValueSym reprV) => Label -> [reprV Value] -> repr Function
    
    listSize   :: repr Function
    listAccess :: (ValueSym reprV) => reprV Value -> repr Function
    listAppend :: (ValueSym reprV) => reprV Value -> repr Function
    listExtend :: (StateTypeSym reprST) => reprST StateType -> repr Function