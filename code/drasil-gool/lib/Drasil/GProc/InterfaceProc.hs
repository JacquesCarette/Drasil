{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Drasil.GProc.InterfaceProc (
  -- Types
  GSProgram, SFile, FSModule,
  -- Typeclasses
  ProcProg, ProgramSym(..), FileSym(..), ModuleSym(..)
  ) where

import Drasil.Shared.InterfaceCommon (Label, SharedProg, MethodSym(..),
  NativeVector)
import Drasil.Shared.State (GS, FS, MS)

class (SharedProg r vis smt md, ProgramSym r vis smt md,
  NativeVector r) => ProcProg r vis smt md

type GSProgram a = GS (a (Program a))

class (FileSym r vis smt md) => ProgramSym r vis smt md where
  type Program r
  prog :: Label -> Label -> [SFile r] -> GSProgram r

type SFile a = FS (a (File a))

class (ModuleSym r vis smt md) => FileSym r vis smt md where
  type File r
  fileDoc :: FSModule r -> SFile r

  -- Module description, watermark, list of author names, date as a String, file to comment
  docMod :: String -> String -> [String] -> String -> SFile r -> SFile r

type FSModule a = FS (a (Module a))

class (MethodSym r vis smt md) => ModuleSym r vis smt md where
  type Module r
  -- Module name, import names, module functions
  buildModule :: Label -> [Label] -> [MS (r md)] -> FSModule r
