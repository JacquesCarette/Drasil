{-# OPTIONS -Wall #-} 
{-# LANGUAGE GADTs #-}
module Spec where

import Format (FormatC)
import Unicode (Render)

--For writing chunks in a specification language that can be converted to TeX
data Spec where
  -- E :: Expr -> Spec -- Expressions
  S :: String -> Spec -- Strings, used for Descriptions/Symbols in Chunks
  (:-:) :: Spec -> Spec -> Spec -- Subscripting (Spec :- Spec -> Spec_{Spec} in TeX)
  (:^:) :: Spec -> Spec -> Spec -- Superscript (Spec :^ Spec -> Spec^{Spec} in TeX)
  (:+:) :: Spec -> Spec -> Spec -- Concatenation of two Specs (e.g. delta :+: T -> deltaT)
  (:/:) :: Spec -> Spec -> Spec -- frac
  Empty :: Spec -- Blank
  U :: (Render r) => r -> Spec -- Unicode for special characters
--  M :: Chunk c => Unit c -> Spec -- Measured in *
  F :: FormatC -> Spec -> Spec -- Special formatting for certain symbols & special chars
                                          --(e.g. hat, dot, etc.)
--  CS :: Chunk c => c -> Spec 
--  D :: Chunk c => [c] -> Spec -- Should only be used for "Dependencies" field. Need a way to ensure it.

type Title    = Spec
type Contents = Spec
type Author   = Spec

data Document = Document Title Author [LayoutObj]

data LayoutObj = Table [Spec] [[Spec]] -- header then data
               | Section Title [LayoutObj]
               | Paragraph Contents
               | EqnBlock Contents
--               | Definition DType c

data DType = Data
           | Literate
