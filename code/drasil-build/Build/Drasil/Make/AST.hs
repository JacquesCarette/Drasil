-- | Makefile abstract syntax tree
module Build.Drasil.Make.AST where

newtype Makefile = M [Rule]

data Rule = R Target [Dependencies] Type [Command]

data Command = C String [CommandOpts]

data CommandOpts =
  IgnoreReturnCode deriving Eq

data Type = Abstract
          | File

type Target = String
type Dependencies = Target
