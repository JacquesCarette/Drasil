-- | Defines a Makefile abstract syntax tree.
module Build.Drasil.Make.AST where
import Build.Drasil.Make.MakeString (MakeString)

-- * Types

-- | A Makefile is made up of Makefile rules.
newtype Makefile = M [Rule]

-- | A Makefile Rule needs a target, dependencies, type, and commands.
data Rule = R Target Dependencies Type [Command]

-- | A command is made up of 'MakeString's and command operators.
data Command = C MakeString [CommandOpts]

-- | Ignore the return code from the system.
data CommandOpts =
  IgnoreReturnCode deriving Eq

-- | Type of rule, either abstract or file-oriented.
data Type = Abstract
          | File deriving Eq

-- | A Makefile target is made from a 'MakeString'.
type Target = MakeString
-- | Dependencies are made up of 0 or more 'Target's.
type Dependencies = [Target]

-- * Constructors

-- | Creates a Rule which results in a file being created.
mkFile :: Target -> Dependencies -> [Command] -> Rule
mkFile t d = R t d File

-- | Creates an abstract Rule not associated to a specific file.
mkRule :: Target -> Dependencies -> [Command] -> Rule
mkRule t d = R t d Abstract

-- | Creates a Command which fails the make process if it does not return zero.
mkCheckedCommand :: MakeString -> Command
mkCheckedCommand = flip C []

-- | Creates a command which executes and ignores the return code.
mkCommand :: MakeString -> Command
mkCommand = flip C [IgnoreReturnCode]
