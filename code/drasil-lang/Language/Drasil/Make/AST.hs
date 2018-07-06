-- | Makefile abstract syntax tree
module Language.Drasil.Make.AST where

data Makefile = M [Rule]

type Rule = (Type, Target, [Dependencies])

data Type = Phony
          | TeX
          | CCode

type Target = String
type Dependencies = Target
