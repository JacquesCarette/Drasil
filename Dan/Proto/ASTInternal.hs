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

type Chunk mode = Map.Map FName (FDesc mode)
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
  deriving Eq

type Variable = String

--For writing chunks in a specification language that can be converted to TeX
data Spec mode where
  E :: Expr mode-> Spec mode      -- Expressions
  S :: String -> Spec mode    -- Strings, used for Descriptions/Symbols in Chunks
  (:-:) :: Spec mode -> Spec mode -> Spec mode -- Subscripting (Spec :- Spec -> Spec_{Spec} in TeX)
  (:^:) :: Spec mode -> Spec mode -> Spec mode -- Superscript (Spec :^ Spec -> Spec^{Spec} in TeX)
  (:+:) :: Spec mode -> Spec mode -> Spec mode -- Concatenation of two Specs (e.g. delta :+: T -> deltaT)
  Empty :: Spec mode        -- Blank
  U :: (Unicode mode r, Format.Format mode) => mode -> r -> Spec mode     -- Unicode for special characters
  M :: Unit mode -> Spec mode      -- Measured in *
  F :: FormatC -> Spec mode -> Spec mode -- Special formatting for certain symbols & special chars
                                          --(e.g. hat, dot, etc.)
  CS :: Chunk mode -> Spec mode
  D :: Dependency mode -> Spec mode -- Should only be used for "Dependencies" field. Need a way to ensure it.

instance Eq (Spec mode) where
  (E x) == (E y) = x == y
  (S x) == (S y) = x == y
  (x :-: x') == (y :-: y') = x == y && x' == y'
  (x :^: x') == (y :^: y') = x == y && x' == y'
  (x :+: x') == (y :+: y') = x == y && x' == y'
  (Empty) == (Empty) = True
  (M x) == (M y) = x == y
  (F x x') == (F y y') = x == y && x' == y'
  (CS x) == (CS y) = x == y
  (D x) == (D y) = x == y
  (U m x) == (U n y) = render m x == render n y
  _ == _ = False
  
data Unit mode = Fundamental String --Fundamental unit type (e.g. "m" for length)
          | Derived String (Expr mode)--Derived unit type (e.g. "J" for power, from
                                --the expression kg m^2 / s^2
  deriving Eq

{-
data Unicode = Tau_L
             | Tau_U
             | Alpha_L
             | Alpha_U
             | Circle
             | Delta_U
             | Delta_L
             | Rho_U
             | Rho_L
             | Phi_U
             | Phi_L
  deriving (Eq,Ord)
-}

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
