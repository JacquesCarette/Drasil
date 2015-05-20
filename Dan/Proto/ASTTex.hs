{-# OPTIONS -Wall #-} 
module ASTTex where
import qualified ASTInternal as AST

data TExp = Chnk AST.Variable
          | Dbl Double
          | Int Integer
          | Mul TExp TExp
          | Add TExp TExp
          | Frac TExp TExp
          | Div TExp TExp
  deriving (Eq, Ord)