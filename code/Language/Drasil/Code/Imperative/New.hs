{-# LANGUAGE TypeFamilies, TypeOperators, Rank2Types, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
{-# OPTIONS_GHC -W #-}

module New where

--import qualified Data.Function as F
import Language.Haskell.TH

-- base types
data BooleanT
data IntegerT
data FloatT 
data CharacterT 
data StringT

data Block
data Statement
data Value
data StateType

type Label = String

class Symantics repr where
    block   :: repr [Statement] -> repr Block
    assign  :: repr Value -> repr Value -> repr Statement
    varDec  :: Label -> repr StateType -> repr Statement
    
    bool   :: repr StateType
    int    :: repr StateType
    float  :: repr StateType
    char   :: repr StateType
    string :: repr StateType
    
    litBool   :: Bool -> repr Value
    litChar   :: Char -> repr Value
    litFloat  :: Double -> repr Value
    litInt    :: Integer -> repr Value
    litString :: String -> repr Value