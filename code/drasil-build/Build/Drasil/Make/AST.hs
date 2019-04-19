-- | Makefile abstract syntax tree
module Build.Drasil.Make.AST where

newtype Makefile = M [Rule]

type Rule = (Type, Target, [Dependencies])

data Type = Phony
          | TeX
          | Code

type Target = String
type Dependencies = Target
