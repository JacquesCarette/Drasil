{-# OPTIONS -Wall #-} 
module ASTTeX where

import ASTInternal (Variable)
import Unit (Unit)
import Spec ()

--Might want to create our own TeX chunk to avoid cascading modes since they're
--pointless once we've decided to use TeX.
data Expr = Var Variable
          | Dbl Double
          | Int Integer
          | Mul Expr Expr
          | Add Expr Expr
          | Frac Expr Expr
          | Div Expr Expr
          -- | C Chunk
          | Pow Expr Expr
          | Sub Expr Expr
  deriving Eq

--Probably needs to be converted to GADT
  
data Spec = E Expr
          | S String
          | Spec :+: Spec
          | Spec :^: Spec
          | Spec :-: Spec
          | Spec :/: Spec
          -- | M Unit
          -- | CS Chunk --No need for Format / Empty / Unicode here, they will be converted to TeX specific strings. As will Spec combinations.
          -- | D [Chunk]
data Document = Document Title Author [LayoutObj]
type Title = Spec
type Author = Spec
type Contents = Spec

data LayoutObj = Table [Spec] [[Spec]] -- header then data
               | Section Title [LayoutObj]
               | Paragraph Contents
               | EqnBlock Contents
               -- | Definition DType c [c -> Spec]
--NOTE: TeX Context is pointless and should be removed. Anything converted to 
--  this AST will by definition be TeX. Will allow for cleanup.
               
data Context = Pg | Eqn | Code -- paragraph, equation, or code. This will affect
                               -- the formatting of the finished document.
