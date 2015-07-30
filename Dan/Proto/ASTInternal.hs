{-# OPTIONS -Wall #-} 
module ASTInternal where

import qualified Data.Map.Strict as Map
import Data.List (nub)

--Field should be configurable, but currently not in config to avoid
  -- cyclic import.
data Field = Symbol | Equation | Description | SIU | Name | VarName
  deriving (Ord, Eq)

type Chunk = Map.Map FName FDesc
type Chunks = [Chunk]
type FName = Field
type FDesc = Spec
type Dependency = Chunks

--Supported output formats for documentation.
data OutFormat = TeX
               | Plain
data OutLang   = CLang

data Expr = V Variable
          | Dbl Double
          | Int Integer
          | Expr :^ Expr
          | Expr :* Expr
          | Expr :/ Expr
          | Expr :+ Expr
          | Expr :- Expr
          | C Chunk
  deriving (Eq, Ord)

type Variable = String

--For writing chunks in a specification language that can be converted to TeX
data Spec = E Expr        -- Expressions
          | S String      -- Strings, used for Descriptions/Symbols in Chunks
          | Spec :-: Spec -- Subscripting (Spec :- Spec -> Spec_{Spec} in TeX)
          | Spec :^: Spec -- Superscript (Spec :^ Spec -> Spec^{Spec} in TeX)
          | Spec :+: Spec -- Concatenation of two Specs (e.g. delta :+: T -> deltaT)
          | Empty         -- Blank
          | U Unicode     -- Unicode for special characters
          | M Unit        -- Measured in *
          | F Format Spec -- Special formatting for certain symbols 
                                --(e.g. hat, dot, etc.)
  deriving (Eq, Ord)

data Unit = Fundamental String --Fundamental unit type (e.g. "m" for length)
          | Derived String Expr --Derived unit type (e.g. "J" for power, from
                                --the expression kg m^2 / s^2
  deriving (Eq, Ord)

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

data Format = Hat
            | Vector
  deriving (Eq, Ord)
  
data LayoutObj = Table Chunks [Field]
  
data Context = Pg | Eqn | Code -- paragraph, equation, or code
----------------------------------------------------------------
data CodeType = Calc
data Precision = Single | Double

--Get dependency from equation  
get_dep :: Expr -> Dependency
get_dep (a :/ b) = nub (get_dep a ++ get_dep b)
get_dep (a :* b) = nub (get_dep a ++ get_dep b)
get_dep (a :+ b) = nub (get_dep a ++ get_dep b)
get_dep (a :^ b) = nub (get_dep a ++ get_dep b)
get_dep (a :- b) = nub (get_dep a ++ get_dep b)
get_dep (C c) = [c]
get_dep (Int _) = []
get_dep (Dbl _) = []
get_dep (V _) = []
