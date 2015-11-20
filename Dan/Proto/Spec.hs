{-# OPTIONS -Wall #-} 
{-# LANGUAGE GADTs #-}
module Spec where

import ASTInternal (FormatC(..))
import Unicode (Unicode)

--For writing chunks in a specification language that can be converted to TeX
data Spec where
--  E :: Chunk c => Expr c -> Spec -- Expressions
  S :: String -> Spec -- Strings, used for Descriptions/Symbols in Chunks
  (:-:) :: Spec -> Spec -> Spec -- Subscripting (Spec :- Spec -> Spec_{Spec} in TeX)
  (:^:) :: Spec -> Spec -> Spec -- Superscript (Spec :^ Spec -> Spec^{Spec} in TeX)
  (:+:) :: Spec -> Spec -> Spec -- Concatenation of two Specs (e.g. delta :+: T -> deltaT)
  Empty :: Spec -- Blank
  U :: (Unicode r) => r -> Spec -- Unicode for special characters
--  M :: Chunk c => Unit c -> Spec -- Measured in *
  F :: FormatC -> Spec -> Spec -- Special formatting for certain symbols & special chars
                                          --(e.g. hat, dot, etc.)
--  CS :: Chunk c => c -> Spec 
--  D :: Chunk c => [c] -> Spec -- Should only be used for "Dependencies" field. Need a way to ensure it.

type Title    = Spec
type Contents = Spec
type Author   = Spec

data Document c t = Document Title Author [LayoutObj c t]

data LayoutObj c t = Table [c] [t]
               | Section Title [LayoutObj c t]
               | Paragraph Contents
               | EqnBlock Contents
               | Definition DType c

data DType = Data
           | Literate
