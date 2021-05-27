-- | re-export smart constructors for external code writing
module Language.Drasil.Code (
  makeCode, createCodeFiles, 
  generator, generateCode,
  readWithDataDesc, sampleInputDD,
  Choices(..), Comments(..), Verbosity(..), ConstraintBehaviour(..), 
  ImplementationType(..), Logging(..), Modularity(..), Structure(..), 
  ConstantStructure(..), ConstantRepr(..), InputModule(..), CodeConcept(..), 
  matchConcepts, SpaceMatch, matchSpaces, AuxFile(..), getSampleData, 
  Visibility(..), defaultChoices, 
  CodeSpec(..), funcUID, funcUID', asVC, asVC', codeSpec, relToQD,
  ($:=), Mod(Mod), StateVariable, Func, FuncStmt(..), pubStateVar, 
  privStateVar, fDecDef, ffor, funcData, funcDef, packmod,
  junkLine, multiLine, repeated, singleLine, singleton,
  ExternalLibrary, Step, FunctionInterface, Argument, externalLib, choiceSteps, 
  choiceStep, mandatoryStep, mandatorySteps, callStep, libFunction, libMethod, 
  libFunctionWithResult, libMethodWithResult, libConstructor, 
  libConstructorMultiReqs, constructAndReturn, lockedArg, lockedNamedArg, 
  inlineArg, inlineNamedArg, preDefinedArg, preDefinedNamedArg, functionArg, 
  customObjArg, recordArg, lockedParam, unnamedParam, customClass, 
  implementation, constructorInfo, methodInfo, methodInfoNoReturn, 
  appendCurrSol, populateSolList, assignArrayIndex, assignSolFromObj, 
  initSolListFromArray, initSolListWithVal, solveAndPopulateWhile, 
  returnExprList, fixedReturn, initSolWithVal,
  ExternalLibraryCall, StepGroupFill(..), StepFill(..), FunctionIntFill(..), 
  ArgumentFill(..), ParameterFill(..), ClassInfoFill(..), MethodInfoFill(..),
  externalLibCall, choiceStepsFill, choiceStepFill, mandatoryStepFill, 
  mandatoryStepsFill, callStepFill, libCallFill, userDefinedArgFill, 
  basicArgFill, functionArgFill, customObjArgFill, recordArgFill,  
  unnamedParamFill, unnamedParamPBVFill, userDefinedParamFill, customClassFill, 
  implementationFill, constructorInfoFill, methodInfoFill, appendCurrSolFill, 
  populateSolListFill, assignArrayIndexFill, assignSolFromObjFill, 
  initSolListFromArrayFill, initSolListWithValFill, solveAndPopulateWhileFill, 
  returnExprListFill, fixedStatementFill, initSolWithValFill,
  Lang(..),
  PackageSym(..), AuxiliarySym(..),
  AuxData(..), PackData(..),
  CodeChunk, CodeVarChunk, CodeFuncChunk, quantvar, quantfunc, ccObjVar, 
  listToArray,
  field,
  ODEInfo(..), odeInfo, ODEOptions(..), odeOptions, ODEMethod(..), 
  ODELibPckg(..), mkODELib, mkODELibNoPath,
  unPP, unJP, unCSP, unCPPP, unSP
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
  mandatoryStep, mandatorySteps, callStep, libFunction, libMethod, 
  libFunctionWithResult, libMethodWithResult, libConstructor, 
  libConstructorMultiReqs, constructAndReturn, lockedArg, lockedNamedArg, 
  inlineArg, inlineNamedArg, preDefinedArg, preDefinedNamedArg, functionArg, 
  customObjArg, recordArg, lockedParam, unnamedParam, customClass, 
  implementation, constructorInfo, methodInfo, methodInfoNoReturn, 
  appendCurrSol, populateSolList, assignArrayIndex, assignSolFromObj, 
  initSolListFromArray, initSolListWithVal, solveAndPopulateWhile, 
  returnExprList, fixedReturn, initSolWithVal)
import Language.Drasil.Code.ExternalLibraryCall (ExternalLibraryCall,
  StepGroupFill(..), StepFill(..), FunctionIntFill(..), ArgumentFill(..),
  ParameterFill(..), ClassInfoFill(..), MethodInfoFill(..), externalLibCall, 
  choiceStepsFill, choiceStepFill, mandatoryStepFill, mandatoryStepsFill, 
  callStepFill, libCallFill, userDefinedArgFill, basicArgFill, functionArgFill, 
  customObjArgFill, recordArgFill, unnamedParamFill, unnamedParamPBVFill, 
  userDefinedParamFill, customClassFill, implementationFill, 
  constructorInfoFill, methodInfoFill, appendCurrSolFill, populateSolListFill, 
  assignArrayIndexFill, assignSolFromObjFill, initSolListFromArrayFill, 
  initSolListWithValFill, solveAndPopulateWhileFill, returnExprListFill, 
  fixedStatementFill, initSolWithValFill)

import Language.Drasil.Code.Lang (Lang(..))

import Language.Drasil.Choices (Choices(..), Comments(..), Verbosity(..), 
  ConstraintBehaviour(..), ImplementationType(..), Logging(..), Modularity(..), 
  Structure(..), ConstantStructure(..), ConstantRepr(..), InputModule(..), 
  CodeConcept(..), matchConcepts, SpaceMatch, matchSpaces, AuxFile(..), 
  getSampleData, Visibility(..), defaultChoices,)
import Language.Drasil.CodeSpec (CodeSpec(..), funcUID, funcUID', asVC, asVC', 
  codeSpec, relToQD)
import Language.Drasil.Mod (($:=), Mod(Mod), StateVariable, Func, FuncStmt(..), 
  pubStateVar, privStateVar, fDecDef, ffor, funcData, funcDef, packmod)

import Language.Drasil.Code.Imperative.GOOL.ClassInterface (PackageSym(..), 
  AuxiliarySym(..))

import Language.Drasil.Code.Imperative.GOOL.Data (AuxData(..), PackData(..))

import Language.Drasil.Chunk.Code (CodeChunk, CodeVarChunk, CodeFuncChunk, 
  quantvar, quantfunc, ccObjVar, listToArray)

import Language.Drasil.CodeExpr (field)

import Language.Drasil.Data.ODEInfo (ODEInfo(..), odeInfo, ODEOptions(..), 
  odeOptions, ODEMethod(..))
import Language.Drasil.Data.ODELibPckg (ODELibPckg(..), mkODELib, 
  mkODELibNoPath)

import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.PythonRenderer (unPP)
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.JavaRenderer (unJP)
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.CSharpRenderer (unCSP)
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.CppRenderer (unCPPP)
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.SwiftRenderer (unSP)
