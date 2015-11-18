{-# OPTIONS -Wall #-} 
{-# LANGUAGE GADTs, FlexibleContexts, RankNTypes, AllowAmbiguousTypes #-}
module Spec where

--For writing chunks in a specification language that can be converted to TeX
data Spec mode where
  E :: Chunk c => Expr c -> Spec mode      -- Expressions
  S :: String -> Spec mode    -- Strings, used for Descriptions/Symbols in Chunks
  (:-:) :: Spec mode -> Spec mode -> Spec mode -- Subscripting (Spec :- Spec -> Spec_{Spec} in TeX)
  (:^:) :: Spec mode -> Spec mode -> Spec mode -- Superscript (Spec :^ Spec -> Spec^{Spec} in TeX)
  (:+:) :: Spec mode -> Spec mode -> Spec mode -- Concatenation of two Specs (e.g. delta :+: T -> deltaT)
  Empty :: Spec mode        -- Blank
  U :: (Unicode mode r, Format.Format mode) => r -> Spec mode     -- Unicode for special characters
  M :: Unit mode -> Spec mode      -- Measured in *
  F :: FormatC -> Spec mode -> Spec mode -- Special formatting for certain symbols & special chars
                                          --(e.g. hat, dot, etc.)
  CS :: Chunk c => c -> Spec mode
  D :: Chunk c => [c] -> Spec mode -- Should only be used for "Dependencies" field. Need a way to ensure it.

type Title    = Spec
type Contents = Spec
type Author   = Spec

data Document c t mode = Document (Title mode) (Author mode) [LayoutObj c t mode] --[LayoutObj] -> DocContent
