{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Drasil.GProc.InterfaceProc (
  -- Types
  Program, GSProgram, File, Module,
  -- Typeclasses
  ProcProg, ProgramSym(..), FileSym(..), ModuleSym(..)
  ) where

import Drasil.Shared.InterfaceCommon (Label, MethodSym(..), Array, StatementSym,
  AssignStatement, Argument, BooleanExpression, CommandLineArgs, DeclStatement,
  CommentStatement, Comparison, ControlStatement, FuncAppStatement, PrintConsole,
  ReadConsole, FileHandling, PrintFile, ReadFile, List, Literal, MathConstant,
  NumericExpression, ParameterSym, Reference, Set, StringStatement,
  ValueExpression, VariableValue, UnRepr, FunctionSym, ScopeSym, BinderSym,
  InternalList, TypeElim, VariableElim)
import Drasil.Shared.State (GS, FS, MS)
import Drasil.Shared.AST (FileData, ModData, ProgData, TypeData)

-- | Wrapper typeclass that bundles everything essential
-- for generating a procedural program.
class (UnRepr r TypeData, FunctionSym r, VariableValue r, ScopeSym r,
  BinderSym r, InternalList r, MethodSym r vis stmt mthd, TypeElim r,
  VariableElim r, Array r, StatementSym r stmt, AssignStatement r stmt,
  Argument r, BooleanExpression r, CommandLineArgs r, CommentStatement r stmt,
  Comparison r, ControlStatement r stmt, DeclStatement r stmt,
  FuncAppStatement r stmt, PrintConsole r stmt, ReadConsole r stmt,
  FileHandling r stmt, PrintFile r stmt, ReadFile r stmt, List r stmt, Literal r,
  MathConstant r, NumericExpression r, ParameterSym r, Reference r, Set r,
  StringStatement r stmt, ValueExpression r, VariableValue r,
  ProgramSym r vis stmt mthd prg)
  => ProcProg r vis stmt mthd prg

type Program = ProgData
type GSProgram a prg = GS (a prg)

-- | Class for representing a program.
-- Usually 'ProgData' is used for the representation.
class (FileSym r vis stmt mthd) => ProgramSym r vis stmt mthd prg | r -> prg where
  -- | Given program name, program purpose, and list of files,
  -- Generates a representation of a program.
  prog :: Label -> Label -> [FS (r File)] -> GSProgram r prg

type File = FileData

-- | Class for representing a file.
class (ModuleSym r vis stmt mthd) => FileSym r vis stmt mthd where
  -- | Given a module, generates a representation of a file.
  -- (Implicit assumption: exactly one module per file)
  fileDoc :: FS (r Module) -> FS (r File)

  -- | Given module description, watermark, list of author names,
  -- date as a String, and file to comment, creates a __documented module__
  -- (i.e. module with a header comment)
  docMod :: String -> String -> [String] -> String -> FS (r File) -> FS (r File)

type Module = ModData

-- | Class for representing a module.
class (MethodSym r vis stmt mthd) => ModuleSym r vis stmt mthd where
  -- | Given module name, list of import names, and list of module functions,
  -- generates a representation of a module.
  buildModule :: Label -> [Label] -> [MS (r mthd)] -> FS (r Module)
