module Language.Drasil.HTML.AST where

import Language.Drasil.Expr (Variable)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Spec (USymb, RefType)
import Language.Drasil.CCode.AST (Code) -- this seems wrong!
import Language.Drasil.Document (DType (..))

data Expr = Var   Variable
          | Dbl   Double
          | Int   Integer
          | Mul   Expr Expr
          | Add   Expr Expr
          | Frac  Expr Expr
          | Div   Expr Expr
          | Pow   Expr Expr
          | Sub   Expr Expr
          | Sym   Symbol
          | Eq    Expr Expr
          | Dot   Expr Expr
          | Neg   Expr

infixr 5 :+:
data Spec = E Expr
          | S String
          | Spec :+: Spec -- concat
          | Spec :^: Spec -- superscript
          | Spec :-: Spec -- subscript
          | Spec :/: Spec -- frac
          | Sy USymb
          | N Symbol
          | HARDNL
          | Ref RefType Spec

data Document = Document Title Author [LayoutObj]
type Title    = Spec
type Author   = Spec
type Contents = Spec
type Items    = [Spec]
type Tags     = [String]
type Label    = Spec
type Filepath = String
type Caption  = Spec

data LayoutObj = Table Tags [[Spec]] Label Bool Caption
               | Header Int Contents
               | Paragraph Contents
               | HDiv Tags [LayoutObj] Label
               | Tagless Contents
               | CodeBlock Code
               | Definition DType [(String,LayoutObj)] Label
               | List ListType Items
               | Figure Label Caption Filepath
               -- | Span Tags Contents
               
data ListType = Ordered | Unordered | Simple

instance Show ListType where
  show Ordered   = "o"
  show Unordered = "u"
  show Simple    = error "Printing Simple list failed, see ASTHTML/PrintHTML"
