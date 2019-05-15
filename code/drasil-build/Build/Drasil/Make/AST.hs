-- | Makefile abstract syntax tree
module Build.Drasil.Make.AST where

newtype Makefile = M [Rule]

data Rule = R Target [Dependencies] Type [Command]

data Command = C String [CommandOpts]

data CommandOpts =
  IgnoreReturnCode deriving Eq

data Type = Abstract
          | File deriving Eq

type Target = String
type Dependencies = Target

-- | Creates a Rule which results in a file being created
mkFile :: Target -> [Dependencies] -> [Command] -> Rule
mkFile t d = R t d File

-- | Creates an abstract Rule not associated to a specific file
mkRule :: Target -> [Dependencies] -> [Command] -> Rule
mkRule t d = R t d Abstract

-- | Creates a Command which fails the make process if it does not return zero
mkCheckedCommand :: String -> Command
mkCheckedCommand = flip C []

-- | Creates a command which executes and ignores the return code
mkCommand :: String -> Command
mkCommand = flip C [IgnoreReturnCode]
