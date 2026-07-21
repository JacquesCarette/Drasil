{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Drasil.GProc.InterfaceProc (
  -- Types
  Program, GSProgram, File, Module, FSModule,
  -- Typeclasses
  ProcProg, ProgramSym(..), FileSym(..), ModuleSym(..)
  ) where

import Drasil.Shared.InterfaceCommon (Label, SharedProg, MethodSym(..))
import Drasil.Shared.State (GS, FS, MS)
import Drasil.Shared.AST (FileData, ModData, ProgData)

class (SharedProg r vis smt md, ProgramSym r vis smt md prg)
  => ProcProg r vis smt md prg

type Program = ProgData
type GSProgram a prg = GS (a prg)

class (FileSym r vis smt md) => ProgramSym r vis smt md prg | r -> prg where
  prog :: Label -> Label -> [FS (r File)] -> GSProgram r prg

type File = FileData

class (ModuleSym r vis smt md) => FileSym r vis smt md where
  fileDoc :: FSModule r -> FS (r File)

  -- Module description, watermark, list of author names, date as a String, file to comment
  docMod :: String -> String -> [String] -> String -> FS (r File) -> FS (r File)

type Module = ModData
type FSModule a = FS (a Module)

class (MethodSym r vis smt md) => ModuleSym r vis smt md where
  -- Module name, import names, module functions
  buildModule :: Label -> [Label] -> [MS (r md)] -> FSModule r
