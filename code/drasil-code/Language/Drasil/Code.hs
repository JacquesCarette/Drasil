-- | re-export smart constructors for external code writing
module Language.Drasil.Code (
  makeCode, createCodeFiles, 
  generator, generateCode,
  readWithDataDesc, sampleInputDD,
  ($:=), Choices(..), CodeSpec(..), CodeSystInfo(..), Comments(..), 
  ConstraintBehaviour(..), Func, FuncStmt(..), ImplementationType(..), Lang(..),
  Logging(LogNone, LogAll), Mod(Mod), Structure(..), ConstantStructure(..), 
  ConstantRepr(..), InputModule(..), AuxFile(..), Visibility(..),
  asExpr, asExpr', asVC, asVC', codeSpec, fdec, ffor, funcData, funcDef, 
  packmod, relToQD,
  junkLine, multiLine, repeated, singleLine, singleton,
  PackageSym(..), ProgramSym(..), RenderSym(..), 
  PermanenceSym(..), BodySym(..), BlockSym(..), ControlBlockSym(..), 
  StateTypeSym(..), StatementSym(..), ControlStatementSym(..), VariableSym(..),
  ValueSym(..), NumericExpression(..), BooleanExpression(..), 
  ValueExpression(..), Selector(..), FunctionSym(..), SelectorFunction(..),
  MethodSym(..), ModuleSym(..), BlockCommentSym(..), ModData(..),
  JavaCode(..), PythonCode(..), CSharpCode(..), CppSrcCode(..), CppHdrCode(..),
  unCPPC,
  quantvar
) where

import Prelude hiding (break, print, return, log, exp)

import Language.Drasil.Code.Imperative.Generator (generator, generateCode)

import Language.Drasil.Code.Imperative.ReadInput (readWithDataDesc, 
  sampleInputDD)

import Language.Drasil.Code.CodeGeneration (makeCode, createCodeFiles)

import Language.Drasil.Code.DataDesc (junkLine, multiLine, repeated, singleLine,
  singleton)

import Language.Drasil.CodeSpec (($:=), Choices(..), CodeSpec(..), 
  CodeSystInfo(..), Comments(..), ConstraintBehaviour(..), Func, FuncStmt(..),
  ImplementationType(..), Lang(..), Logging(..), Mod(Mod), Structure(..), 
  ConstantStructure(..), ConstantRepr(..), InputModule(..), AuxFile(..), 
  Visibility(..), asExpr, asExpr', asVC, asVC', codeSpec, fdec, ffor, funcData, 
  funcDef, packmod, relToQD,
  )

import Language.Drasil.Chunk.Code (quantvar)

