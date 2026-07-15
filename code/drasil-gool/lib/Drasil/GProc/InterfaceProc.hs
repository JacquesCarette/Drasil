{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Drasil.GProc.InterfaceProc (
  -- Types
  GSProgram, File, SFile, Module, FSModule,
  -- Typeclasses
  ProcProg, ProgramSym(..), FileSym(..), ModuleSym(..)
  ) where

import Drasil.Shared.InterfaceCommon (Label, SharedProg, MethodSym(..))
import Drasil.Shared.State (GS, FS, MS)
import Drasil.Shared.AST (FileData, ModData)

class (SharedProg r vis smt md, ProgramSym r vis smt md) => ProcProg r vis smt md

type GSProgram a = GS (a (Program a))

class (FileSym r vis smt md) => ProgramSym r vis smt md where
  type Program r
  prog :: Label -> Label -> [SFile r] -> GSProgram r

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
