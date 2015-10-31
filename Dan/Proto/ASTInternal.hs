{-# OPTIONS -Wall #-} 
{-# LANGUAGE GADTs, FlexibleContexts, RankNTypes, AllowAmbiguousTypes #-}
module ASTInternal where

import qualified Data.Map.Strict as Map
import Data.List (nub)
import Unicode
import Format (Format)

--Field should be configurable, but currently not in config to avoid
  -- cyclic import.
data Field = Symbol | Equation | Description | SIU | Name | VarName | Dependencies
  deriving (Ord, Eq)

data Chunk mode = Chunk String (Map.Map FName (FDesc mode))

instance Eq (Chunk mode) where
  Chunk s1 _ == Chunk s2 _ = s1 == s2

type Chunks mode = [Chunk mode]
type FName = Field
type FDesc mode = Spec mode
type Dependency mode = Chunks mode

--Supported output formats for documentation.
data OutLang   = CLang

data Expr mode = V Variable
          | Dbl Double
          | Int Integer
          | Expr mode :^ Expr mode
          | Expr mode :* Expr mode
          | Expr mode :/ Expr mode
          | Expr mode :+ Expr mode
          | Expr mode :- Expr mode
          | C (Chunk mode)

type Variable = String

--For writing chunks in a specification language that can be converted to TeX
data Spec mode where
  E :: Expr mode-> Spec mode      -- Expressions
  S :: String -> Spec mode    -- Strings, used for Descriptions/Symbols in Chunks
  (:-:) :: Spec mode -> Spec mode -> Spec mode -- Subscripting (Spec :- Spec -> Spec_{Spec} in TeX)
  (:^:) :: Spec mode -> Spec mode -> Spec mode -- Superscript (Spec :^ Spec -> Spec^{Spec} in TeX)
  (:+:) :: Spec mode -> Spec mode -> Spec mode -- Concatenation of two Specs (e.g. delta :+: T -> deltaT)
  Empty :: Spec mode        -- Blank
  U :: (Unicode mode r, Format.Format mode) => r -> Spec mode     -- Unicode for special characters
  M :: Unit mode -> Spec mode      -- Measured in *
  F :: FormatC -> Spec mode -> Spec mode -- Special formatting for certain symbols & special chars
                                          --(e.g. hat, dot, etc.)
  CS :: Chunk mode -> Spec mode
  D :: Dependency mode -> Spec mode -- Should only be used for "Dependencies" field. Need a way to ensure it.

data Unit mode = Fundamental --Fundamental unit type (e.g. "m" for length)
          | Derived (Expr mode)--Derived unit type (e.g. "J" for power, from
                                --the expression kg m^2 / s^2

data FormatC = Hat
            | Vector
            | Grave
            | Acute
  deriving (Eq, Ord)
  
data LayoutObj mode = Table (Chunks mode) [Field]
               | Section (Title mode) [LayoutObj mode]
               | Paragraph (Contents mode)
               | EqnBlock (Contents mode)
               | Definition DType (Chunk mode)

data DType = Data
           | Literate
               
data Document mode = Document (Title mode) (Author mode) [LayoutObj mode] --[LayoutObj] -> DocContent

type Title    = Spec
type Contents = Spec
type Author   = Spec
               
--data Context = Pg | Eqn | Cd -- paragraph, equation, or code
-- ----------------------------------------------------------------
-- data CodeType = Calc
-- data Precision = Single | Double

data DocType = SRS
             | LPM
             | Code

data DocParams = DocClass String String --SqBracks vs. Braces
               | UsePackages [String] -- Package name list
               | ExDoc String String --SqBracks vs. Braces
             
--Get dependency from equation  
get_dep :: Expr m -> Dependency m
get_dep (a :/ b) = nub (get_dep a ++ get_dep b)
get_dep (a :* b) = nub (get_dep a ++ get_dep b)
get_dep (a :+ b) = nub (get_dep a ++ get_dep b)
get_dep (a :^ b) = nub (get_dep a ++ get_dep b)
get_dep (a :- b) = nub (get_dep a ++ get_dep b)
get_dep (C c) = [c]
get_dep (Int _) = []
get_dep (Dbl _) = []
get_dep (V _) = []
