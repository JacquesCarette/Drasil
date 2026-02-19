-- | Re-export code-related smart constructors for external code writing and generation.
module Language.Drasil.Code (
  generator, generateCode, generateCodeProc,
  readWithDataDesc, sampleInputDD,
  Choices(..), Comments(..), Verbosity(..), ConstraintBehaviour(..), makeArchit,
  Architecture(..), DataInfo(..), makeData, Maps(..), makeMaps, spaceToCodeType,
  makeConstraints, makeODE, makeDocConfig, makeLogConfig, LogConfig(..),
  OptionalFeatures(..), makeOptFeats, ExtLib(..), ImplementationType(..), Logging(..),
  Modularity(..), Structure(..), ConstantStructure(..), ConstantRepr(..),
  CodeConcept(..), matchConcepts, SpaceMatch, matchSpaces, AuxFile(..),
  getSampleData, Visibility(..), defaultChoices, CodeSpec(..), OldCodeSpec(..), codeSpec,
  HasOldCodeSpec(..), funcUID, asVC, ($:=), Mod(Mod), StateVariable, Func, FuncStmt(..), pubStateVar,
  privStateVar, fDecDef, ffor, fforRange, funcData, funcDef, packmod,
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
  returnExprList, fixedReturn, fixedReturn', initSolWithVal,
  ExternalLibraryCall, StepGroupFill(..), StepFill(..), FunctionIntFill(..),
  ArgumentFill(..), ParameterFill(..), ClassInfoFill(..), MethodInfoFill(..),
  externalLibCall, choiceStepsFill, choiceStepFill, mandatoryStepFill,
  mandatoryStepsFill, callStepFill, libCallFill, userDefinedArgFill,
  basicArgFill, functionArgFill, customObjArgFill, recordArgFill,
  unnamedParamFill, unnamedParamPBVFill, userDefinedParamFill, customClassFill,
  implementationFill, constructorInfoFill, methodInfoFill, appendCurrSolFill,
  populateSolListFill, assignArrayIndexFill, assignSolFromObjFill,
  initSolListFromArrayFill, initSolListWithValFill, solveAndPopulateWhileFill,
  returnExprListFill, fixedStatementFill, fixedStatementFill', initSolWithValFill,
  Lang(..),
  CodeChunk, CodeVarChunk, CodeFuncChunk, quantvar, quantfunc, ccObjVar,
  listToArray, field, SoftwareDossierState, makeSds,
  ODEInfo(..), odeInfo, odeInfo', ODEOptions(..), odeOptions, ODEMethod(..),
  ODELibPckg(..), mkODELib, mkODELibNoPath,
  -- Language.Drasil.Chunk.NamedArgument
  NamedArgument, narg
  -- Language.Drasil.Code.CodeQuantityDicts
  , codeDQDs
) where

import Prelude hiding (break, print, return, log, exp)

import Drasil.Code.CodeExpr (field)
import Language.Drasil.SoftwareDossier.SoftwareDossierSym (
  SoftwareDossierState, makeSds)
import Language.Drasil.Code.Imperative.Generator (generator, generateCode,
  generateCodeProc)
import Language.Drasil.Code.Imperative.ReadInput (readWithDataDesc,
  sampleInputDD)

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
  returnExprList, fixedReturn, fixedReturn', initSolWithVal)
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
  fixedStatementFill, fixedStatementFill', initSolWithValFill)
import Language.Drasil.Code.Lang (Lang(..))
import Language.Drasil.Choices (Choices(..), Comments(..), Verbosity(..),
  ConstraintBehaviour(..), ImplementationType(..), Logging(..), Modularity(..),
  Structure(..), ConstantStructure(..), ConstantRepr(..), CodeConcept(..),
  matchConcepts, SpaceMatch, matchSpaces, AuxFile(..), getSampleData,
  Visibility(..), defaultChoices, makeArchit, Architecture(..), DataInfo(..),
  makeData, Maps(..), makeMaps, spaceToCodeType, makeConstraints, makeODE,
  makeDocConfig, makeLogConfig, LogConfig(..), OptionalFeatures(..),
  makeOptFeats, ExtLib(..))
import Language.Drasil.CodeSpec (CodeSpec(..), OldCodeSpec(..), HasOldCodeSpec(..),
  codeSpec, funcUID, asVC)
import Language.Drasil.Mod (($:=), Mod(Mod), StateVariable, Func, FuncStmt(..),
  pubStateVar, privStateVar, fDecDef, ffor, fforRange, funcData, funcDef, packmod)
import Language.Drasil.Chunk.Code (CodeChunk, CodeVarChunk, CodeFuncChunk,
  quantvar, quantfunc, ccObjVar, listToArray)
import Language.Drasil.Chunk.NamedArgument (NamedArgument, narg)
import Language.Drasil.Data.ODEInfo (ODEInfo(..), odeInfo, odeInfo', ODEOptions(..),
  odeOptions, ODEMethod(..))
import Language.Drasil.Data.ODELibPckg (ODELibPckg(..), mkODELib,
  mkODELibNoPath)
import Language.Drasil.Code.CodeQuantityDicts (codeDQDs)
