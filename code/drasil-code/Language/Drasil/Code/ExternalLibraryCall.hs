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

import Language.Drasil.Chunk.Code (CodeChunk)

import Data.List.NonEmpty (NonEmpty(..), fromList)

type ExternalLibraryCall = [StepGroupFill]

-- Convention is to end types with "Fill" because they fill in the use-case 
-- specific information for an ExternalLibrary

-- This AST mirrors the ExternalLibrary AST by intention.

data StepGroupFill = SGF Integer [StepFill] -- Integer is to "choose" from the options in ExternalLibrary

data StepFill = CallF FunctionIntFill
  | LoopF (NonEmpty FunctionIntFill) [Expr] (NonEmpty StepFill)
  | StatementF [CodeChunk] [Expr]

newtype FunctionIntFill = FIF [ArgumentFill]

data ArgumentFill = BasicF Expr 
  | FnF [ParameterFill] StepFill -- Fills in the names for the unnamed parameters
  | ClassF [CodeChunk] ClassInfoFill -- List of CodeChunk for state variables
  | RecordF [Expr] -- Fills in the field values

data ParameterFill = NameableParamF CodeChunk | UserDefined CodeChunk

data ClassInfoFill = RegularF [MethodInfoFill] | ImplementsF [MethodInfoFill]

data MethodInfoFill = CIF [ParameterFill] [Initializer] [StepFill]
  | MIF [ParameterFill] (NonEmpty StepFill)

type Initializer = (CodeChunk, Expr)

externalLibCall :: [StepGroupFill] -> ExternalLibraryCall
externalLibCall = id

choiceStepsFill :: Integer -> [StepFill] -> StepGroupFill
choiceStepsFill = SGF

choiceStepFill :: Integer -> StepFill -> StepGroupFill
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

customObjArgFill :: [CodeChunk] -> ClassInfoFill -> ArgumentFill
customObjArgFill = ClassF

recordArgFill :: [Expr] -> ArgumentFill
recordArgFill = RecordF

unnamedParamFill :: CodeChunk -> ParameterFill
unnamedParamFill = NameableParamF

userDefinedParamFill :: CodeChunk -> ParameterFill
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

appendCurrSolFill :: CodeChunk -> StepFill
appendCurrSolFill s = statementStepFill [s] []

populateSolListFill :: CodeChunk -> StepFill
populateSolListFill s = statementStepFill [s] []

assignArrayIndexFill :: CodeChunk-> [Expr] -> StepFill
assignArrayIndexFill a = statementStepFill [a]

assignSolFromObjFill :: CodeChunk -> StepFill
assignSolFromObjFill s = statementStepFill [s] []

initSolListFromArrayFill :: CodeChunk -> StepFill
initSolListFromArrayFill s = statementStepFill [s] []

initSolListWithValFill :: CodeChunk -> Expr -> StepFill
initSolListWithValFill s v = statementStepFill [s] [v]

solveAndPopulateWhileFill :: FunctionIntFill -> Expr -> FunctionIntFill -> 
  CodeChunk -> StepFill
solveAndPopulateWhileFill lcf ub slvf s = loopStepFill [lcf] [ub] 
  [callStepFill slvf, appendCurrSolFill s]

returnExprListFill :: [Expr] -> StepFill
returnExprListFill = statementStepFill []

statementStepFill :: [CodeChunk] -> [Expr] -> StepFill
statementStepFill = StatementF

fixedStatementFill :: StepFill
fixedStatementFill = StatementF [] []
