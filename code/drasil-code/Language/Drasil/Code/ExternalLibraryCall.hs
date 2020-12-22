-- | Defines an AST to be paired with an ExternalLibrary for a specific use-case
module Language.Drasil.Code.ExternalLibraryCall (ExternalLibraryCall,
  StepGroupFill(..), StepFill(..), FunctionIntFill(..), ArgumentFill(..),
  ParameterFill(..), ClassInfoFill(..), MethodInfoFill(..), externalLibCall, 
  choiceStepsFill, choiceStepFill, mandatoryStepFill, mandatoryStepsFill, 
  callStepFill, libCallFill, userDefinedArgFill, basicArgFill, functionArgFill, 
  customObjArgFill, recordArgFill, unnamedParamFill, unnamedParamPBVFill, 
  userDefinedParamFill, customClassFill, implementationFill, 
  constructorInfoFill, methodInfoFill, appendCurrSolFill, populateSolListFill, 
  assignArrayIndexFill, assignSolFromObjFill, initSolListFromArrayFill, 
  initSolListWithValFill, solveAndPopulateWhileFill, returnExprListFill, 
  fixedStatementFill, initSolWithValFill
) where

import Language.Drasil

import Language.Drasil.Chunk.Code (CodeVarChunk)
import Language.Drasil.Chunk.Parameter (ParameterChunk, pcAuto, pcVal)
import Language.Drasil.Mod (Initializer, StateVariable)

import Data.List.NonEmpty (NonEmpty(..), fromList)

type ExternalLibraryCall = [StepGroupFill]

-- Convention is to end types with "Fill" because they fill in the use-case 
-- specific information for an ExternalLibrary

-- This AST mirrors the ExternalLibrary AST by intention.

data StepGroupFill = SGF Int [StepFill] -- Int is to "choose" from the options in ExternalLibrary

data StepFill = CallF FunctionIntFill
  | LoopF (NonEmpty FunctionIntFill) [Expr] (NonEmpty StepFill)
  | StatementF [CodeVarChunk] [Expr]

newtype FunctionIntFill = FIF [ArgumentFill]

data ArgumentFill = UserDefinedArgF (Maybe NamedArgument) Expr -- For arguments that are completely dependent on use case
  | BasicF Expr 
  | FnF [ParameterFill] StepFill -- Fills in the names for the unnamed parameters
  | ClassF [StateVariable] ClassInfoFill -- List of CodeChunk for state variables
  | RecordF [Expr] -- Fills in the field values

data ParameterFill = NameableParamF ParameterChunk | UserDefined ParameterChunk

data ClassInfoFill = RegularF [MethodInfoFill] | ImplementsF [MethodInfoFill]

data MethodInfoFill = CIF [ParameterFill] [Initializer] [StepFill]
  | MIF [ParameterFill] (NonEmpty StepFill)

-- Constructs an ExternalLibraryCall specification
externalLibCall :: [StepGroupFill] -> ExternalLibraryCall
externalLibCall = id

-- Corresponds to ExternalLibrary's choiceSteps. Provides the index of the 
-- steps that should be used for the current use case.
choiceStepsFill :: Int -> [StepFill] -> StepGroupFill
choiceStepsFill = SGF

-- Corresponds to ExternalLibrary's choiceStep. Provides the index of the 
-- step that should be used for the current use case.
choiceStepFill :: Int -> StepFill -> StepGroupFill
choiceStepFill i s = SGF i [s]

-- Corresponds to ExternalLibrary's mandatorySteps.
mandatoryStepFill :: StepFill -> StepGroupFill
mandatoryStepFill s = SGF 0 [s]

-- Corresponds to ExternalLibrary's mandatoryStep.
mandatoryStepsFill :: [StepFill] -> StepGroupFill
mandatoryStepsFill = SGF 0

-- Corresponds to ExternalLibrary's callStep.
callStepFill :: FunctionIntFill -> StepFill
callStepFill = CallF

-- Corresponds to ExternalLibrary's loopStep.
loopStepFill :: [FunctionIntFill] -> [Expr] -> [StepFill] -> StepFill
loopStepFill [] _ _ = error "loopStepFill should be called with a non-empty list of FunctionInterfaceFill"
loopStepFill _ _ [] = error "loopStepFill should be called with a non-empty list of StepFill"
loopStepFill fifs cdchs sfs = LoopF (fromList fifs) cdchs (fromList sfs)

-- Corresponds to any of ExternalLibrary's FunctionInterface constructors.
libCallFill :: [ArgumentFill] -> FunctionIntFill
libCallFill = FIF

-- Does not correspond to anything in ExternalLibrary. To be used when the 
-- presence of an argument is only a consequence of the use case.
userDefinedArgFill :: Expr -> ArgumentFill
userDefinedArgFill = UserDefinedArgF Nothing

-- Corresponds to ExternalLibrary's inlineArg, inlineNamedArg, preDefinedArg, 
-- and preDefinedNamedArg. Provides the Expr for the argument's value.
basicArgFill :: Expr -> ArgumentFill
basicArgFill = BasicF

-- Corresponds to ExternalLibrary's functionArg. 
functionArgFill :: [ParameterFill] -> StepFill -> ArgumentFill
functionArgFill = FnF

-- Corresponds to ExternalLibrary's customObjArg. Provides the list of state 
-- variables for the class that must be written in the calling program.
customObjArgFill :: [StateVariable] -> ClassInfoFill -> ArgumentFill
customObjArgFill = ClassF

-- Corresponds to ExternalLibrary's recordArg. Provides the list of Exprs for 
-- the values of the fields that must be set by the calling program.
recordArgFill :: [Expr] -> ArgumentFill
recordArgFill = RecordF

-- Corresponds to ExternalLibrary's unnamedParam. Provides the CodeVarChunk 
-- representing the parameter.
unnamedParamFill :: CodeVarChunk -> ParameterFill
unnamedParamFill = NameableParamF . pcAuto

-- Corresponds to ExternalLibrary's unnamedParam. Provides the CodeVarChunk 
-- representing the parameter. Specifies that the parameter is passed by value.
unnamedParamPBVFill :: CodeVarChunk -> ParameterFill
unnamedParamPBVFill = NameableParamF . pcVal

-- Does not correspond to anything in ExternalLibrary. To be used when the 
-- presence of a parameter is only a consequence of the use case.
userDefinedParamFill :: CodeVarChunk -> ParameterFill
userDefinedParamFill = UserDefined . pcAuto

-- Corresponds to ExternalLibrary's customClass.
customClassFill :: [MethodInfoFill] -> ClassInfoFill
customClassFill = RegularF

-- Corresponds to ExternalLibrary's implementation.
implementationFill :: [MethodInfoFill] -> ClassInfoFill
implementationFill = ImplementsF

-- Corresponds to ExternalLibrary's constructorInfo. Provides Variable-Value 
-- pairs for variables initialized by the constructor.
constructorInfoFill :: [ParameterFill] -> [Initializer] -> [StepFill] -> 
  MethodInfoFill
constructorInfoFill = CIF

-- Corresponds to ExternalLibrary's methodInfo.
methodInfoFill :: [ParameterFill] -> [StepFill] -> MethodInfoFill
methodInfoFill _ [] = error "methodInfoFill should be called with non-empty list of StepFill"
methodInfoFill pfs sfs = MIF pfs (fromList sfs)

-- Corresponds to ExternalLibrary's appendCurrSol. Provides the CodeVarChunk 
-- for the solution list.
appendCurrSolFill :: CodeVarChunk -> StepFill
appendCurrSolFill s = statementStepFill [s] []

-- Corresponds to ExternalLibrary's populateSolList. Provides the CodeVarChunk
-- for the solution list.
populateSolListFill :: CodeVarChunk -> [StepFill]
populateSolListFill s = replicate 2 (statementStepFill [s] [])

-- Corresponds to ExternalLibrary's assignArrayIndex. Provides the CodeVarChunk
-- for the array variable. Provides the Exprs for the values to assign to each 
-- array index.
assignArrayIndexFill :: CodeVarChunk-> [Expr] -> StepFill
assignArrayIndexFill a = statementStepFill [a]

-- Corresponds to ExternalLibrary's assignSolFromObj. Provides the CodeVarChunk
-- for the variable that the solution should be assigned to.
assignSolFromObjFill :: CodeVarChunk -> StepFill
assignSolFromObjFill s = statementStepFill [s] []

-- Corresponds to ExternalLibrary's initSolListFromArray. Provides the 
-- CodeVarChunk for the solution list.
initSolListFromArrayFill :: CodeVarChunk -> StepFill
initSolListFromArrayFill s = statementStepFill [s] []

-- Corresponds to ExternalLibrary's initSolListWithVal. Provides the 
-- CodeVarChunk for the solution list and the Expr for the initial element of 
-- the solution list
initSolListWithValFill :: CodeVarChunk -> Expr -> StepFill
initSolListWithValFill s v = statementStepFill [s] [v]

-- Corresponds to ExternalLibrary's solveAndPopulateWhile. Provides the Expr 
-- for the upper bound in the while loop condition and the CodeVarChunk for the 
-- solution list.
solveAndPopulateWhileFill :: FunctionIntFill -> Expr -> FunctionIntFill -> 
  CodeVarChunk -> StepFill
solveAndPopulateWhileFill lcf ub slvf s = loopStepFill [lcf] [ub] 
  [callStepFill slvf, appendCurrSolFill s]

-- Corresponds to ExternalLibrary's returnExprList. Provides the list of Exprs 
-- to return.
returnExprListFill :: [Expr] -> StepFill
returnExprListFill = statementStepFill []

-- Corresponds to ExternalLibrary's statementStep. Provides the 
-- use-case-specific CodeVarChunks and Exprs that parameterize the statement.
statementStepFill :: [CodeVarChunk] -> [Expr] -> StepFill
statementStepFill = StatementF

-- Corresponds to ExternalLibrary's fixedStatement.
-- No parameters because the statement is not use-case-dependent.
fixedStatementFill :: StepFill
fixedStatementFill = StatementF [] []

-- Corresponds to ExternalLibrary's initSolWithVal. Provides the 
-- CodeVarChunk for one solution and one Expr for the initial element of 
-- the solution list
initSolWithValFill :: CodeVarChunk -> Expr -> StepFill
initSolWithValFill s v = statementStepFill [s] [v]