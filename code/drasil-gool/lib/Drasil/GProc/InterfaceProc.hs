{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Drasil.GProc.InterfaceProc (
  -- Types
  Program, GSProgram,
  -- Typeclasses
  ProcProg, ProgramSym(..), FileSym(..), ModuleSym(..)
  ) where

import Drasil.Shared.InterfaceCommon (Label, VisibilitySym, MethodSym(..),
  IndexTranslator, Array, EmptyStatement, MultiStatement, ValueStatement,
  AssignStatement, Argument, BooleanExpression, CommandLineArgs, DeclStatement,
  CommentStatement, Comparison, ControlStatement, FuncAppStatement, PrintConsole,
  ReadConsole, FileHandling, PrintFile, ReadFile, List, ListStatement, Literal,
  MathConstant, NumericExpression, ParameterSym, Reference, Set, StringStatement,
  ValueExpression, ValueSym, VariableSym, VariableValue, UnRepr, ScopeSym,
  BinderSym, InternalList, TypeSym, TypeElim, VariableElim, BodySym, BlockSym)
import Drasil.Shared.State (GS, FS, MS)
import Drasil.Shared.AST (ProgData, TypeData)

-- | Wrapper typeclass that bundles everything essential
-- for generating a procedural program.
class (UnRepr r TypeData, BodySym r bod block, BlockSym r block stmt,
  ValueSym r typ val, VariableSym r typ, VariableValue r val, ScopeSym r,
  BinderSym r typ, InternalList r val block, VisibilitySym r vis,
  MethodSym r vis typ mthd bod, TypeSym r typ, TypeElim r typ,
  VariableElim r typ, IndexTranslator r val, Array r val, EmptyStatement r stmt,
  MultiStatement r stmt, ValueStatement r val stmt, AssignStatement r val stmt,
  Argument r val, BooleanExpression r val, CommandLineArgs r val,
  CommentStatement r stmt, Comparison r val, ControlStatement r val stmt bod,
  DeclStatement r val stmt bod, FuncAppStatement r val stmt,
  PrintConsole r val stmt, ReadConsole r stmt, FileHandling r val stmt,
  PrintFile r val stmt, ReadFile r val stmt, List r val,
  ListStatement r val stmt, Literal r typ val, MathConstant r val,
  NumericExpression r val, ParameterSym r, Reference r val, Set r val,
  StringStatement r val stmt, ValueExpression r typ val, VariableValue r val,
  ModuleSym r mod mthd, FileSym r file mod, ProgramSym r prg file)
  => ProcProg r vis typ val stmt mthd prg file mod bod block

type Program = ProgData
type GSProgram a prg = GS (a prg)

-- | Class for representing a program.
-- Usually 'ProgData' is used for the representation.
class ProgramSym r prg file | r -> prg file where
  -- | Given program name, program purpose, and list of files,
  -- Generates a representation of a program.
  prog :: Label -> Label -> [FS (r file)] -> GSProgram r prg

-- | Class for representing a file.
class FileSym r file mod | r -> file mod where
  -- | Given a module, generates a representation of a file.
  -- (Implicit assumption: exactly one module per file)
  fileDoc :: FS (r mod) -> FS (r file)

  -- | Given module description, watermark, list of author names,
  -- date as a String, and file to comment, creates a __documented module__
  -- (i.e. module with a header comment)
  docMod :: String -> String -> [String] -> String -> FS (r file) -> FS (r file)

-- | Class for representing a module.
class ModuleSym r mod mthd | r -> mod mthd where
  -- | Given module name, list of import names, and list of module functions,
  -- generates a representation of a module.
  buildModule :: Label -> [Label] -> [MS (r mthd)] -> FS (r mod)
