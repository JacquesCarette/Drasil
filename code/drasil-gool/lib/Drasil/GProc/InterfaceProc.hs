{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Drasil.GProc.InterfaceProc (
  -- Types
  Program, GSProgram, File, SFile, Module, FSModule,
  -- Typeclasses
  ProcProg, ProgramSym(..), FileSym(..), ModuleSym(..)
  ) where

import Drasil.Shared.InterfaceCommon (Label, SharedProg, MethodSym(..),
  NativeVector)
import Drasil.Shared.State (GS, FS, MS)
import Drasil.Shared.AST (FileData, ModData, ProgData)

class (SharedProg r vis smt md, ProgramSym r vis smt md prg,
  NativeVector r) => ProcProg r vis smt md prg

type Program = ProgData
type GSProgram a prg = GS (a prg)

class (FileSym r vis smt md) => ProgramSym r vis smt md prg | r -> prg where
  prog :: Label -> Label -> [SFile r] -> GSProgram r prg

type File = FileData
type SFile a = FS (a File)

class (ModuleSym r vis smt md) => FileSym r vis smt md where
  fileDoc :: FSModule r -> SFile r

  -- Module description, watermark, list of author names, date as a String, file to comment
  docMod :: String -> String -> [String] -> String -> SFile r -> SFile r

type Module = ModData
type FSModule a = FS (a Module)

class (MethodSym r vis smt md) => ModuleSym r vis smt md where
  -- Module name, import names, module functions
  buildModule :: Label -> [Label] -> [MS (r md)] -> FSModule r
