{-# OPTIONS -Wall #-} 
{-# LANGUAGE ImpredicativeTypes #-}
module ASTTeX where

import ASTInternal (Variable)
import Unit (Unit)
import Spec (DType)
import Format (TeX(..))
import Control.Lens

--Might want to create our own TeX chunk to avoid cascading modes since they're
--pointless once we've decided to use TeX.
data Expr c = Var Variable
          | Dbl Double
          | Int Integer
          | Mul (Expr c) (Expr c)
          | Add (Expr c) (Expr c)
          | Frac (Expr c) (Expr c)
          | Div (Expr c) (Expr c)
          | C c
          | Pow (Expr c) (Expr c)
          | Sub (Expr c) (Expr c)
  deriving Eq

--Probably needs to be converted to GADT
  
data Spec c = E (Expr c)
          | S String
          | (Spec c) :+: (Spec c)
          | (Spec c) :^: (Spec c)
          | (Spec c) :-: (Spec c)
          | M (Unit TeX)
          | CS c --No need for Format / Empty / Unicode here, they will be converted to TeX specific strings. As will Spec combinations.
          | D [c]
data Document c t = Document (Title c) (Author c) [LayoutObj c t]
type Title c = Spec c
type Author c = Spec c
type Contents c = Spec c

data LayoutObj c t = Table [c] [Getter c t] 
               | Section (Title c) [LayoutObj c t]
               | Paragraph (Contents c)
               | EqnBlock (Contents c)
               | Definition DType c [Getter c t]
--NOTE: TeX Context is pointless and should be removed. Anything converted to 
--  this AST will by definition be TeX. Will allow for cleanup.
               
data Context = Pg | Eqn | Code -- paragraph, equation, or code. This will affect
                               -- the formatting of the finished document.
