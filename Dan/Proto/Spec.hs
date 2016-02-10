{-# OPTIONS -Wall #-} 
{-# LANGUAGE GADTs #-}
module Spec where

import Format (FormatC)
import Unicode (Render)
import Symbol
import Unit (USymb)
import ASTCode
import EqChunk

--For writing chunks in a specification language that can be converted to TeX
infixr 5 :+:
data Spec where
  N :: Symbol -> Spec
  Sy :: USymb -> Spec
  S :: String -> Spec           -- Strings, used for Descriptions in Chunks
  (:-:) :: Spec -> Spec -> Spec -- Subscripting (Spec :-: Spec -> Spec_{Spec} in TeX)
  (:^:) :: Spec -> Spec -> Spec -- Superscript (Spec :^: Spec -> Spec^{Spec} in TeX)
  (:+:) :: Spec -> Spec -> Spec -- Concatenation of two Specs (e.g. delta :+: T -> deltaT)
  (:/:) :: Spec -> Spec -> Spec -- Fractions (Spec :/: Spec -> frac{Spec}{Spec} in TeX)
  Empty :: Spec                 -- Blank
  U :: (Render r) => r -> Spec  -- Unicode for special characters
  F :: FormatC -> Spec -> Spec  -- Special formatting for certain symbols & special
                                -- chars (e.g. hat, dot, etc.)

type Title    = Spec
type Contents = Spec
type Author   = Spec

data Document = Document Title Author [LayoutObj]

--Types of layout objects we deal with explicitly
data LayoutObj = Table [Spec] [[Spec]] -- table header then data
               | Section Title [LayoutObj]
               | Paragraph Contents
               | EqnBlock Contents
               | CodeBlock Code
               | Definition DType (EqChunk)

-- Types of definitions
data DType = Data