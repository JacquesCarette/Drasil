{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Drasil.GProc.InterfaceProc (
  -- Types
  GSProgram, SFile, FSModule,
  -- Typeclasses
  ProcProg, ProgramSym(..), FileSym(..), ModuleSym(..)
  ) where

import Drasil.Shared.InterfaceCommon (Label, SMethod, SharedProg,
  MethodSym)
import Drasil.Shared.State (GS, FS)

class (SharedProg r s, ProgramSym r s
  ) => ProcProg r s

type GSProgram a = GS (a (Program a))

class (FileSym r s) => ProgramSym r s where
  type Program r
  prog :: Label -> Label -> [SFile r] -> GSProgram r

type SFile a = FS (a (File a))

class (ModuleSym r s) => FileSym r s where
  type File r
  fileDoc :: FSModule r -> SFile r

  -- Module description, watermark, list of author names, date as a String, file to comment
  docMod :: String -> String -> [String] -> String -> SFile r -> SFile r

type FSModule a = FS (a (Module a))

class (MethodSym r s) => ModuleSym r s where
  type Module r
  -- Module name, import names, module functions
  buildModule :: Label -> [Label] -> [SMethod r] -> FSModule r
