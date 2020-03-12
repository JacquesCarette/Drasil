-- | re-export smart constructors for external code writing
module Language.Drasil.Code (
  spaceToCodeType,
  makeCode, createCodeFiles, 
  generator, generateCode,
  readWithDataDesc, sampleInputDD,
  Choices(..), CodeSpec(..), CodeSystInfo(..), Comments(..), Verbosity(..), 
  ConstraintBehaviour(..), ImplementationType(..),
  Logging(LogNone, LogAll), Modularity(..), Structure(..), 
  ConstantStructure(..), ConstantRepr(..), InputModule(..), CodeConcept(..), 
  matchConcepts, SpaceMatch, matchSpaces, AuxFile(..), Visibility(..), 
  defaultChoices, funcUID, funcUID', asVC, asVC', codeSpec, relToQD,
  ($:=), Mod(Mod), Func, FuncStmt(..), fdec, ffor, funcData, funcDef, 
  packmod,
  junkLine, multiLine, repeated, singleLine, singleton,
  ExternalLibrary, Step, FunctionInterface, Argument, externalLib, choiceSteps, 
  choiceStep, mandatoryStep, mandatorySteps, callStep, callRequiresJust, 
  callRequires, libFunction, libMethod, libFunctionWithResult, 
  libMethodWithResult, libConstructor, constructAndReturn, lockedArg, 
  lockedNamedArg, inlineArg, inlineNamedArg, preDefinedArg, preDefinedNamedArg, 
  functionArg, customObjArg, recordArg, lockedParam, unnamedParam, customClass, 
  implementation, constructorInfo, methodInfo, methodInfoNoReturn, 
  appendCurrSol, populateSolList, assignArrayIndex, assignSolFromObj, 
  initSolListFromArray, initSolListWithVal, solveAndPopulateWhile, 
  returnExprList, fixedReturn,
  ExternalLibraryCall, StepGroupFill(..), StepFill(..), FunctionIntFill(..), 
  ArgumentFill(..), ParameterFill(..), ClassInfoFill(..), MethodInfoFill(..),
  externalLibCall, choiceStepsFill, choiceStepFill, mandatoryStepFill, 
  mandatoryStepsFill, callStepFill, libCallFill, basicArgFill, functionArgFill,
  customObjArgFill, recordArgFill,  unnamedParamFill, userDefinedParamFill, 
  customClassFill, implementationFill, constructorInfoFill, methodInfoFill, 
  appendCurrSolFill, populateSolListFill, assignArrayIndexFill, 
  assignSolFromObjFill, initSolListFromArrayFill, initSolListWithValFill, 
  solveAndPopulateWhileFill, returnExprListFill, fixedStatementFill,
  Lang(..),
  PackageSym(..),
  CodeChunk, CodeVarChunk, CodeFuncChunk, codevar, codefunc, quantvar, 
  ccObjVar, CodeQuantityDict, implCQD,
  ODEInfo(..), odeInfo, ODEOptions(..), odeOptions, ODEMethod(..), 
  ODELibPckg(..), mkODELib,
  unPP, unJP, unCSP, unCPPP
) where

import Prelude hiding (break, print, return, log, exp)

import Language.Drasil.Code.Imperative.Generator (generator, generateCode)

import Language.Drasil.Code.Imperative.ReadInput (readWithDataDesc, 
  sampleInputDD)

import Language.Drasil.Code.Code (spaceToCodeType)

import Language.Drasil.Code.CodeGeneration (makeCode, createCodeFiles)

import Language.Drasil.Code.DataDesc (junkLine, multiLine, repeated, singleLine,
  singleton)

import Language.Drasil.Code.ExternalLibrary (ExternalLibrary, Step,
  FunctionInterface, Argument, externalLib, choiceSteps, choiceStep, 
  mandatoryStep, mandatorySteps, callStep, callRequiresJust, callRequires, 
  libFunction, libMethod, libFunctionWithResult, libMethodWithResult, 
  libConstructor, constructAndReturn, lockedArg, lockedNamedArg, inlineArg, 
  inlineNamedArg, preDefinedArg, preDefinedNamedArg, functionArg, customObjArg, 
  recordArg, lockedParam, unnamedParam, customClass, implementation, 
  constructorInfo, methodInfo, methodInfoNoReturn, appendCurrSol, 
  populateSolList, assignArrayIndex, assignSolFromObj, initSolListFromArray, 
  initSolListWithVal, solveAndPopulateWhile, returnExprList, fixedReturn)
import Language.Drasil.Code.ExternalLibraryCall (ExternalLibraryCall,
  StepGroupFill(..), StepFill(..), FunctionIntFill(..), ArgumentFill(..),
  ParameterFill(..), ClassInfoFill(..), MethodInfoFill(..), externalLibCall, 
  choiceStepsFill, choiceStepFill, mandatoryStepFill, mandatoryStepsFill, 
  callStepFill, libCallFill, basicArgFill, functionArgFill, customObjArgFill, 
  recordArgFill, unnamedParamFill, userDefinedParamFill, customClassFill, 
  implementationFill, constructorInfoFill, methodInfoFill, appendCurrSolFill, 
  populateSolListFill, assignArrayIndexFill, assignSolFromObjFill, 
  initSolListFromArrayFill, initSolListWithValFill, solveAndPopulateWhileFill, 
  returnExprListFill, fixedStatementFill)

import Language.Drasil.Code.Lang (Lang(..))

import Language.Drasil.CodeSpec (Choices(..), CodeSpec(..), CodeSystInfo(..), 
  Comments(..), Verbosity(..), ConstraintBehaviour(..), ImplementationType(..), 
  Logging(..), Modularity(..), Structure(..), ConstantStructure(..), 
  ConstantRepr(..), InputModule(..), CodeConcept(..), matchConcepts, SpaceMatch,
  matchSpaces, AuxFile(..), Visibility(..), defaultChoices, funcUID, funcUID', 
  asVC, asVC', codeSpec, relToQD)
import Language.Drasil.Mod (($:=), Mod(Mod), Func, FuncStmt(..), fdec, ffor, 
  funcData, funcDef, packmod)

import Language.Drasil.Code.Imperative.GOOL.Symantics (PackageSym(..))

import Language.Drasil.Chunk.Code (CodeChunk, CodeVarChunk, CodeFuncChunk, 
  codevar, codefunc, quantvar, ccObjVar)
import Language.Drasil.Chunk.CodeQuantity (CodeQuantityDict, implCQD)

import Language.Drasil.Data.ODEInfo (ODEInfo(..), odeInfo, ODEOptions(..), 
  odeOptions, ODEMethod(..), ODELibPckg(..), mkODELib)

import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.PythonRenderer (unPP)
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.JavaRenderer (unJP)
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.CSharpRenderer (unCSP)
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.CppRenderer (unCPPP)
