-- | Re-export code-related smart constructors for external code writing and generation.
module Language.Drasil.Code (
  module Drasil.Code.CodeExpr,
  module Drasil.Code.CodeVar,
  module Language.Drasil.SoftwareDossier.SoftwareDossierSym,
  module Language.Drasil.Code.Imperative.Generator,
  module Language.Drasil.Code.Imperative.ReadInput,
  module Language.Drasil.Code.DataDesc,
  module Language.Drasil.Code.ExternalLibrary,
  module Language.Drasil.Code.ExternalLibraryCall,
  module Language.Drasil.Code.Lang,
  module Language.Drasil.Choices,
  module Language.Drasil.CodeSpec,
  module Language.Drasil.Mod,
  module Language.Drasil.Chunk.Code,
  module Language.Drasil.Chunk.NamedArgument,
  module Language.Drasil.Data.ODEInfo,
  module Language.Drasil.Data.ODELibPckg,
  module Language.Drasil.Code.CodeQuantityDicts
) where

import Prelude hiding (break, print, return, log, exp)

import Drasil.Code.CodeExpr (field)
import Drasil.Code.CodeVar (CodeChunk, CodeVarChunk, CodeFuncChunk, quantvar,
  quantfunc, listToArray)
import Language.Drasil.SoftwareDossier.SoftwareDossierSym (
  SoftwareDossierState, makeSds)
import Language.Drasil.Code.Imperative.Generator
  ( generator, generateCode
  , generateCodeProc, toFileLayout
  , SomeProgGenerator(..)
  )
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
  matchConcepts, SpaceMatch, matchSpaces, SoftwareDossierFile(..), getSampleData,
  Visibility(..), defaultChoices, makeArchit, Architecture(..), DataInfo(..),
  makeData, Maps(..), makeMaps, spaceToCodeType, makeConstraints, makeODE,
  makeDocConfig, makeLogConfig, LogConfig(..), OptionalFeatures(..),
  makeOptFeats, ExtLib(..))
import Language.Drasil.CodeSpec (CodeSpec, OldCodeSpec(..), HasOldCodeSpec(..),
  codeSpec, funcUID, asVC)
import Language.Drasil.Mod (($:=), Mod(Mod), StateVariable, Func, FuncStmt(..),
  pubStateVar, privStateVar, fDecDef, ffor, fforRange, funcData, funcDef, packmod)
import Language.Drasil.Chunk.Code (ccObjVar)
import Language.Drasil.Chunk.NamedArgument (NamedArgument, narg)
import Language.Drasil.Data.ODEInfo (ODEInfo(..), odeInfo, odeInfo', ODEOptions(..),
  odeOptions, ODEMethod(..))
import Language.Drasil.Data.ODELibPckg (ODELibPckg(..), mkODELib,
  mkODELibNoPath)
import Language.Drasil.Code.CodeQuantityDicts (codeDQDs)
