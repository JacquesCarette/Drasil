-- | re-export smart constructors for external code writing
module Language.Drasil.Code (
  makeCode, createCodeFiles, 
  generator, generateCode,
  readWithDataDesc, sampleInputDD,
  Choices(..), CodeSpec(..), CodeSystInfo(..), Comments(..), Verbosity(..), 
  ConstraintBehaviour(..), ImplementationType(..), Lang(..), 
  Logging(LogNone, LogAll), Modularity(..), Structure(..), 
  ConstantStructure(..), ConstantRepr(..), InputModule(..), CodeConcept(..), 
  matchConcepts, AuxFile(..), Visibility(..), asExpr, asExpr', asVC, asVC', 
  codeSpec, relToQD,
  ($:=), Mod(Mod), Func, FuncStmt(..), fdec, ffor, funcData, funcDef, 
  packmod,
  junkLine, multiLine, repeated, singleLine, singleton,
  ExternalLibrary, Step, FunctionInterface, Argument, externalLib, choiceSteps, 
  choiceStep, mandatoryStep, mandatorySteps, callStep, callRequiresJust, 
  callRequires, libFunction, libMethod, libFunctionWithResult, 
  libMethodWithResult, libConstructor, lockedArg, lockedNamedArg, inlineArg, 
  inlineNamedArg, preDefinedArg, preDefinedNamedArg, functionArg, customObjArg, 
  recordArg, lockedParam, unnamedParam, customClass, implementation, 
  constructorInfo, methodInfo, appendCurrSol, populateSolList, 
  assignArrayIndex, assignSolFromObj, initSolListFromArray, initSolListWithVal, 
  solveAndPopulateWhile, fixedReturn,
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

import Language.Drasil.Code.ExternalLibrary (ExternalLibrary, Step,
  FunctionInterface, Argument, externalLib, choiceSteps, choiceStep, 
  mandatoryStep, mandatorySteps, callStep, callRequiresJust, callRequires, 
  libFunction, libMethod, libFunctionWithResult, libMethodWithResult, 
  libConstructor, lockedArg, lockedNamedArg, inlineArg, inlineNamedArg, 
  preDefinedArg, preDefinedNamedArg, functionArg, customObjArg, recordArg, 
  lockedParam, unnamedParam, customClass, implementation, constructorInfo, 
  methodInfo, appendCurrSol, populateSolList, assignArrayIndex, 
  assignSolFromObj, initSolListFromArray, initSolListWithVal, 
  solveAndPopulateWhile, fixedReturn)

import Language.Drasil.CodeSpec (Choices(..), CodeSpec(..), CodeSystInfo(..), 
  Comments(..), Verbosity(..), ConstraintBehaviour(..), ImplementationType(..), 
  Lang(..), Logging(..), Modularity(..), Structure(..), ConstantStructure(..), 
  ConstantRepr(..), InputModule(..), CodeConcept(..), matchConcepts, 
  AuxFile(..), Visibility(..), asExpr, asExpr', asVC, asVC', codeSpec, relToQD)
import Language.Drasil.Mod (($:=), Mod(Mod), Func, FuncStmt(..), fdec, ffor, 
  funcData, funcDef, packmod)

import Language.Drasil.Code.Imperative.GOOL.Symantics (PackageSym(..))

import Language.Drasil.Chunk.Code (CodeChunk, codevar, quantvar, ccObjVar)
import Language.Drasil.Chunk.CodeQuantity (CodeQuantityDict, implCQD)

import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.PythonRenderer (unPP)
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.JavaRenderer (unJP)
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.CSharpRenderer (unCSP)
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.CppRenderer (unCPPP)
