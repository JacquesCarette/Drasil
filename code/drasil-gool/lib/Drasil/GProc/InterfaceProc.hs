{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Drasil.GProc.InterfaceProc (
  -- Types
  Program, GSProgram, File, Module,
  -- Typeclasses
  ProcProg, ProgramSym(..), FileSym(..), ModuleSym(..)
  ) where

import Drasil.Shared.InterfaceCommon (Label, SharedProg, MethodSym(..))
import Drasil.Shared.State (GS, FS, MS)
import Drasil.Shared.AST (FileData, ModData, ProgData)

-- | Wrapper typeclass that bundles everything essential
-- for generating a procedural program.
class (SharedProg r stmt mthd, ProgramSym r stmt mthd prg)
  => ProcProg r stmt mthd prg

type Program = ProgData
type GSProgram a prg = GS (a prg)

-- | Class for representing a program.
-- Usually 'ProgData' is used for the representation.
class (FileSym r stmt mthd) => ProgramSym r stmt mthd prg | r -> prg where
  -- | Given program name, program purpose, and list of files,
  -- Generates a representation of a program.
  prog :: Label -> Label -> [FS (r File)] -> GSProgram r prg

type File = FileData

-- | Class for representing a file.
class (ModuleSym r stmt mthd) => FileSym r stmt mthd where
  -- | Given a module, generates a representation of a file.
  -- (Implicit assumption: exactly one module per file)
  fileDoc :: FS (r Module) -> FS (r File)

  -- | Given module description, watermark, list of author names,
  -- date as a String, and file to comment, creates a __documented module__
  -- (i.e. module with a header comment)
  docMod :: String -> String -> [String] -> String -> FS (r File) -> FS (r File)

type Module = ModData

-- | Class for representing a module.
class (MethodSym r stmt mthd) => ModuleSym r stmt mthd where
  -- | Given module name, list of import names, and list of module functions,
  -- generates a representation of a module.
  buildModule :: Label -> [Label] -> [MS (r mthd)] -> FS (r Module)
