{-# OPTIONS -Wall #-} 
module ASTInternal where
import qualified Data.Map.Strict as Map
import Data.List

--Field should be configurable, but currently not in config to avoid
  -- cyclic import.
data Field = Symbol | Equation | Description | SIU | Name
  deriving (Ord, Eq)

type Chunk = Map.Map
type FName = Field
type FDesc = Spec
type Dependency = [Chunk FName FDesc]

--Supported output formats for documentation.
data OutFormat = TeX
               | Plain

data Expr = V Variable
          | Dbl Double
          | Int Integer
          | Expr :* Expr
          | Expr :/ Expr
          | Expr :+ Expr
          | Expr :^: Expr
          | C (Chunk FName FDesc)
  deriving (Eq, Ord)

type Variable = String

--For writing chunks in a specification language that can be converted to TeX
data Spec = E Expr          -- Expressions
            | S String      -- Strings, used for Descriptions/Symbols in Chunks
            | Spec :- Spec  -- Subscripting (Spec :- Spec -> Spec_{Spec} in TeX)
            | Spec :^ Spec  -- Superscript (Spec :^ Spec -> Spec^{Spec} in TeX)
            | Empty         -- Blank
            | U Unicode     -- Unicode for special characters
            | M Unit        -- Measured in *
  deriving (Eq, Ord)

data Unit = Fundamental String --Fundamental unit type (e.g. "m" for length)
          | Derived String Expr --Derived unit type (e.g. "J" for power, from
                                --the expression kg m^2 / s^2
  deriving (Eq, Ord)
data Unicode = Tau_L
               | Tau_U
               | Alpha_L
               | Alpha_U
  deriving (Eq,Ord)
               -- ... Greek letters, lower and uppercase.
data Context = Pg | Eqn | Code -- paragraph, equation, or code
----------------------------------------------------------------

--Get dependency from equation  
get_dep :: Expr -> Dependency
get_dep (a :/ b) = nub (get_dep a ++ get_dep b)
get_dep (a :* b) = nub (get_dep a ++ get_dep b)
get_dep (a :+ b) = nub (get_dep a ++ get_dep b)
get_dep (C c) = [c]
get_dep (Int _) = []
get_dep (V _) = []
get_dep _ = error "Unexpected use of get_dep"