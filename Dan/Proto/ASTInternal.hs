module ASTInternal where
import Chunk

data Expr = Chnk Variable
          | Dbl Double
          | Int Integer
          | Expr :* Expr
          | Expr :+ Expr
          -- | Expr :/ Expr
          -- | Expr :- Expr
          -- | Neg Expr
          | Frac Expr Expr

type Variable = Chunk FName FDesc

----------------------------------------------------------------
-- Make things prettier
v = Chnk

