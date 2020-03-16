-- | Defines an AST to be paired with an ExternalLibrary for a specific use-case
module Language.Drasil.Code.ExternalLibraryCall (ExternalLibraryCall,
  StepGroupFill(..), StepFill(..), FunctionIntFill(..), ArgumentFill(..),
  ParameterFill(..), ClassInfoFill(..), MethodInfoFill(..), externalLibCall, 
  choiceStepsFill, choiceStepFill, mandatoryStepFill, mandatoryStepsFill, 
  callStepFill, libCallFill, basicArgFill, functionArgFill, customObjArgFill, 
  recordArgFill, unnamedParamFill, userDefinedParamFill, customClassFill, 
  implementationFill, constructorInfoFill, methodInfoFill, appendCurrSolFill, 
  populateSolListFill, assignArrayIndexFill, assignSolFromObjFill, 
  initSolListFromArrayFill, initSolListWithValFill, solveAndPopulateWhileFill, 
  returnExprListFill, fixedStatementFill
) where

import Language.Drasil

import Language.Drasil.Chunk.Code (CodeVarChunk)
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

data ArgumentFill = BasicF Expr 
  | FnF [ParameterFill] StepFill -- Fills in the names for the unnamed parameters
  | ClassF [StateVariable] ClassInfoFill -- List of CodeChunk for state variables
  | RecordF [Expr] -- Fills in the field values

data ParameterFill = NameableParamF CodeVarChunk | UserDefined CodeVarChunk

data ClassInfoFill = RegularF [MethodInfoFill] | ImplementsF [MethodInfoFill]

data MethodInfoFill = CIF [ParameterFill] [Initializer] [StepFill]
  | MIF [ParameterFill] (NonEmpty StepFill)

externalLibCall :: [StepGroupFill] -> ExternalLibraryCall
externalLibCall = id

choiceStepsFill :: Int -> [StepFill] -> StepGroupFill
choiceStepsFill = SGF

choiceStepFill :: Int -> StepFill -> StepGroupFill
choiceStepFill i s = SGF i [s]

mandatoryStepFill :: StepFill -> StepGroupFill
mandatoryStepFill s = SGF 0 [s]

mandatoryStepsFill :: [StepFill] -> StepGroupFill
mandatoryStepsFill = SGF 0

callStepFill :: FunctionIntFill -> StepFill
callStepFill = CallF

loopStepFill :: [FunctionIntFill] -> [Expr] -> [StepFill] -> StepFill
loopStepFill [] _ _ = error "loopStepFill should be called with a non-empty list of FunctionInterfaceFill"
loopStepFill _ _ [] = error "loopStepFill should be called with a non-empty list of StepFill"
loopStepFill fifs cdchs sfs = LoopF (fromList fifs) cdchs (fromList sfs)

libCallFill :: [ArgumentFill] -> FunctionIntFill
libCallFill = FIF

basicArgFill :: Expr -> ArgumentFill
basicArgFill = BasicF

functionArgFill :: [ParameterFill] -> StepFill -> ArgumentFill
functionArgFill = FnF

customObjArgFill :: [StateVariable] -> ClassInfoFill -> ArgumentFill
customObjArgFill = ClassF

recordArgFill :: [Expr] -> ArgumentFill
recordArgFill = RecordF

unnamedParamFill :: CodeVarChunk -> ParameterFill
unnamedParamFill = NameableParamF

userDefinedParamFill :: CodeVarChunk -> ParameterFill
userDefinedParamFill = UserDefined

customClassFill :: [MethodInfoFill] -> ClassInfoFill
customClassFill = RegularF

implementationFill :: [MethodInfoFill] -> ClassInfoFill
implementationFill = ImplementsF

constructorInfoFill :: [ParameterFill] -> [Initializer] -> [StepFill] -> 
  MethodInfoFill
constructorInfoFill = CIF

methodInfoFill :: [ParameterFill] -> [StepFill] -> MethodInfoFill
methodInfoFill _ [] = error "methodInfoFill should be called with non-empty list of StepFill"
methodInfoFill pfs sfs = MIF pfs (fromList sfs)

appendCurrSolFill :: CodeVarChunk -> StepFill
appendCurrSolFill s = statementStepFill [s] []

populateSolListFill :: CodeVarChunk -> StepFill
populateSolListFill s = statementStepFill [s] []

assignArrayIndexFill :: CodeVarChunk-> [Expr] -> StepFill
assignArrayIndexFill a = statementStepFill [a]

assignSolFromObjFill :: CodeVarChunk -> StepFill
assignSolFromObjFill s = statementStepFill [s] []

initSolListFromArrayFill :: CodeVarChunk -> StepFill
initSolListFromArrayFill s = statementStepFill [s] []

initSolListWithValFill :: CodeVarChunk -> Expr -> StepFill
initSolListWithValFill s v = statementStepFill [s] [v]

solveAndPopulateWhileFill :: FunctionIntFill -> Expr -> FunctionIntFill -> 
  CodeVarChunk -> StepFill
solveAndPopulateWhileFill lcf ub slvf s = loopStepFill [lcf] [ub] 
  [callStepFill slvf, appendCurrSolFill s]

returnExprListFill :: [Expr] -> StepFill
returnExprListFill = statementStepFill []

statementStepFill :: [CodeVarChunk] -> [Expr] -> StepFill
statementStepFill = StatementF

fixedStatementFill :: StepFill
fixedStatementFill = StatementF [] []
