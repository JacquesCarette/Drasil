{-# LANGUAGE TypeFamilyDependencies #-}

module Drasil.GProc.InterfaceProc (
  -- Typeclasses
  ProcProg, ProgramSym(..), FileSym(..), ModuleSym(..)
  ) where

import Drasil.Shared.InterfaceCommon (Label, MethodSym(Method), SharedProg,
  MethodSym)

class (SharedProg r, ProgramSym r
  ) => ProcProg r

class (FileSym r) => ProgramSym r where
  type Program r = t | t -> r
  prog :: Label -> Label -> [File r] -> Program r

class (ModuleSym r) => FileSym r where 
  type File r = t | t -> r
  fileDoc :: Module r -> File r

  -- Module description, list of author names, date as a String, file to comment
  docMod :: String -> [String] -> String -> File r -> File r

class (MethodSym r) => ModuleSym r where
  type Module r = t | t -> r
  -- Module name, import names, module functions
  buildModule :: Label -> [Label] -> [Method r] -> Module r
