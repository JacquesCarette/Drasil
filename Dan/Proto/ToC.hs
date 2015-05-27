{-# OPTIONS -Wall #-} 
module ToC where

import ASTInternal
import qualified ASTC as C
import qualified Data.Map.Strict as Map
import Data.Maybe
import PrintPlain

type Chnk = Chunk FName FDesc

expr :: Expr -> C.Expr
expr (V v)    = C.V v
expr (Dbl d)  = C.Dbl d
expr (Int i)  = C.Int i
expr (a :* b) = C.Mul (expr a) (expr b)
expr (a :+ b) = C.Add (expr a) (expr b)
expr (a :/ b) = C.Div (expr a) (expr b)
expr (a :^ b) = C.Pow (expr a) (expr b)
expr (a :- b) = C.Sub (expr a) (expr b)
expr (C c)    = chunk c 
-- How to handle chunks? Call Eqn / VarName?

-- Consider chunks as var and ensure they are calc'd in advance? Or inline a 
-- call?
chunk :: Chnk -> C.Expr
chunk = \c -> C.V $ textify c
  (fromMaybe (fromMaybe (Empty) (Map.lookup VarName c)) 
  (Map.lookup Equation c))
  
textify :: Chnk -> FDesc -> String
textify c (Empty) = error "Not a usable chunk"
textify c x       = plaintext c
  
-- data Spec = E Expr          -- Expressions
            -- | S String      -- Strings, used for Descriptions/Symbols in Chunks
            -- | Spec :-: Spec  -- Subscripting (Spec :- Spec -> Spec_{Spec} in TeX)
            -- | Spec :^: Spec  -- Superscript (Spec :^ Spec -> Spec^{Spec} in TeX)
            -- | Empty         -- Blank
            -- | U Unicode     -- Unicode for special characters
            -- | M Unit        -- Measured in *
  -- deriving (Eq, Ord)