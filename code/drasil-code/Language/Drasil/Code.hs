-- | re-export smart constructors for external code writing
module Language.Drasil.Code (
  makeCode, createCodeFiles, 
  generator, generateCode,
  readWithDataDesc, sampleInputDD,
  ($:=), Choices(..), CodeSpec(..), CodeSystInfo(..), Comments(..), 
  Verbosity(..), ConstraintBehaviour(..), Func, FuncStmt(..), 
  ImplementationType(..), Lang(..), Logging(LogNone, LogAll), Mod(Mod), 
  Structure(..), ConstantStructure(..), ConstantRepr(..), InputModule(..), 
  CodeConcept(..), matchConcepts, AuxFile(..), Visibility(..),
  asExpr, asExpr', asVC, asVC', codeSpec, fdec, ffor, funcData, funcDef, 
  packmod, relToQD,
  junkLine, multiLine, repeated, singleLine, singleton,
  ExternalLibrary, FunctionInterface, Argument, externalLib, choiceSteps, 
  choiceStep, mandatoryStep, libFunction, libMethod, libFunctionWithResult,
  libMethodWithResult, loopConditionFunction, loopConditionMethod,
  loopedFunction, loopedMethod, loopedFunctionWithResult, 
  loopedMethodWithResult, libConstructor, lockedArg, lockedNamedArg, inlineArg, 
  inlineNamedArg, preDefinedArg, preDefinedNamedArg, functionArg, customObjArg, 
  recordArg, lockedParam, unnamedParam, customClass, implementation, 
  constructorInfo, methodInfo, iterateStep, statementStep, lockedStatement,
  PackageSym(..),
  CodeChunk, codevar, quantvar, ccObjVar, CodeQuantityDict, implCQD,
  unPP, unJP, unCSP, unCPPP
) where

import Prelude hiding (break, print, return, log, exp)

import Language.Drasil.Code.Imperative.Generator (generator, generateCode)

import Language.Drasil.Code.Imperative.ReadInput (readWithDataDesc, 
  sampleInputDD)

import Language.Drasil.Code.CodeGeneration (makeCode, createCodeFiles)

import Language.Drasil.Code.DataDesc (junkLine, multiLine, repeated, singleLine,
  singleton)

import Language.Drasil.Code.ExternalLibrary (ExternalLibrary, 
  FunctionInterface, Argument, externalLib, choiceSteps, choiceStep, 
  mandatoryStep, libFunction, libMethod, libFunctionWithResult, 
  libMethodWithResult, loopConditionFunction, loopConditionMethod, 
  loopedFunction, loopedMethod, loopedFunctionWithResult, 
  loopedMethodWithResult, libConstructor, lockedArg, lockedNamedArg, inlineArg, 
  inlineNamedArg, preDefinedArg, preDefinedNamedArg, functionArg, customObjArg, 
  recordArg, lockedParam, unnamedParam, customClass, implementation, 
  constructorInfo, methodInfo, iterateStep, statementStep, lockedStatement)

import Language.Drasil.CodeSpec (($:=), Choices(..), CodeSpec(..), 
  CodeSystInfo(..), Comments(..), Verbosity(..), ConstraintBehaviour(..), Func, 
  FuncStmt(..), ImplementationType(..), Lang(..), Logging(..), Mod(Mod), 
  Structure(..), ConstantStructure(..), ConstantRepr(..), InputModule(..), 
  CodeConcept(..), matchConcepts, AuxFile(..), Visibility(..), asExpr, asExpr', 
  asVC, asVC', codeSpec, fdec, ffor, funcData, funcDef, packmod, relToQD)

import Language.Drasil.Code.Imperative.GOOL.Symantics (PackageSym(..))

import Language.Drasil.Chunk.Code (CodeChunk, codevar, quantvar, ccObjVar)
import Language.Drasil.Chunk.CodeQuantity (CodeQuantityDict, implCQD)

import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.PythonRenderer (unPP)
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.JavaRenderer (unJP)
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.CSharpRenderer (unCSP)
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.CppRenderer (unCPPP)
