{-# LANGUAGE LambdaCase #-}
module Language.Drasil.Code.ExternalLibrary (ExternalLibrary, Step,
  FunctionInterface, Argument, externalLib, choiceSteps, choiceStep, 
  mandatoryStep, mandatorySteps, callStep, callRequiresJust, callRequires, 
  libFunction, libMethod, libFunctionWithResult, libMethodWithResult, 
  libConstructor, lockedArg, lockedNamedArg, inlineArg, inlineNamedArg, 
  preDefinedArg, preDefinedNamedArg, functionArg, customObjArg, recordArg, 
  lockedParam, unnamedParam, customClass, implementation, constructorInfo, 
  methodInfo, appendCurrSol, populateSolList, assignArrayIndex, 
  assignSolFromObj, initSolListFromArray, initSolListWithVal, 
  solveAndPopulateWhile, fixedReturn
) where

import Language.Drasil
import Language.Drasil.Chunk.Code (CodeChunk, codeType, ccObjVar)
import Language.Drasil.Mod (FuncStmt(..))

import GOOL.Drasil (CodeType)

import Data.List.NonEmpty (NonEmpty(..), fromList)

type FuncName = String
type FieldName = String
type Condition = Expr
type Requires = String

type ExternalLibrary = [StepGroup]

type StepGroup = NonEmpty [Step]

data Step = Call [Requires] FunctionInterface
  -- A while loop -- function calls in the condition, other conditions, steps for the body
  | Loop (NonEmpty FunctionInterface) ([CodeChunk] -> Condition) (NonEmpty Step)
  -- For when a statement is needed, but does not interface with the external library
  | Statement ([CodeChunk] -> [Expr] -> FuncStmt)

data FunctionInterface = FI FuncType FuncName [Argument] (Maybe CodeChunk) -- Maybe CodeChunk to assign to

data Argument = 
  -- Not dependent on use case, Maybe is name for the argument
  LockedArg (Maybe CodeChunk) Expr 
  -- First Maybe is name for the argument (needed for named parameters)
  -- Second Maybe is the variable if it needs to be declared and defined prior to calling
  | Basic (Maybe CodeChunk) CodeType (Maybe CodeChunk)
  | Fn CodeChunk [Parameter] ([Expr] -> FuncStmt)
  | Class [Requires] CodeChunk ClassInfo
  | Record FuncName CodeChunk [FieldName]

data Parameter = LockedParam CodeChunk | NameableParam CodeType

data ClassInfo = Regular [MethodInfo] | Implements String [MethodInfo]

-- Constructor: known parameters, body
data MethodInfo = CI [Parameter] (NonEmpty Step)
  -- Method name, parameters, return type, body
  | MI FuncName [Parameter] CodeType (NonEmpty Step)

data FuncType = Function | Method CodeChunk | Constructor

externalLib :: [StepGroup] -> ExternalLibrary
externalLib = id

choiceSteps :: [[Step]] -> StepGroup
choiceSteps [] = error "choiceSteps should be called with a non-empty list"
choiceSteps sg = fromList sg

choiceStep :: [Step] -> StepGroup
choiceStep [] = error "choiceStep should be called with a non-empty list"
choiceStep ss = fromList $ map (: []) ss

mandatoryStep :: Step -> StepGroup
mandatoryStep f = [f] :| []

mandatorySteps :: [Step] -> StepGroup
mandatorySteps fs = fs :| []

callStep :: FunctionInterface -> Step
callStep = Call []

callRequiresJust :: Requires -> FunctionInterface -> Step
callRequiresJust i = Call [i]

callRequires :: [Requires] -> FunctionInterface -> Step
callRequires = Call

loopStep :: [FunctionInterface] -> ([CodeChunk] -> Condition) -> [Step] -> Step
loopStep [] _ _ = error "loopStep should be called with a non-empty list of FunctionInterface"
loopStep _ _ [] = error "loopStep should be called with a non-empty list of Step"
loopStep fis c ss = Loop (fromList fis) c (fromList ss)

libFunction :: FuncName -> [Argument] -> FunctionInterface
libFunction n ps = FI Function n ps Nothing

libMethod :: CodeChunk -> FuncName -> [Argument] -> FunctionInterface
libMethod o n ps = FI (Method o) n ps Nothing

libFunctionWithResult :: FuncName -> [Argument] -> CodeChunk -> 
  FunctionInterface
libFunctionWithResult n ps r = FI Function n ps (Just r)

libMethodWithResult :: CodeChunk -> FuncName -> [Argument] -> CodeChunk -> 
  FunctionInterface
libMethodWithResult o n ps r = FI (Method o) n ps (Just r)

libConstructor :: FuncName -> [Argument] -> CodeChunk -> FunctionInterface
libConstructor n as c = FI Constructor n as (Just c)

lockedArg :: Expr -> Argument
lockedArg = LockedArg Nothing

lockedNamedArg :: CodeChunk -> Expr -> Argument
lockedNamedArg n = LockedArg (Just n)

inlineArg :: CodeType -> Argument
inlineArg t = Basic Nothing t Nothing

inlineNamedArg :: CodeChunk ->  CodeType -> Argument
inlineNamedArg n t = Basic (Just n) t Nothing

preDefinedArg :: CodeChunk -> Argument
preDefinedArg v = Basic Nothing (codeType v) (Just v)

preDefinedNamedArg :: CodeChunk -> CodeChunk -> Argument
preDefinedNamedArg n v = Basic (Just n) (codeType v) (Just v)

functionArg :: CodeChunk -> [Parameter] -> ([Expr] -> FuncStmt) -> Argument
functionArg = Fn

customObjArg :: [Requires] -> CodeChunk -> ClassInfo -> Argument
customObjArg = Class

recordArg :: FuncName -> CodeChunk -> [FieldName] -> Argument
recordArg = Record

lockedParam :: CodeChunk -> Parameter
lockedParam = LockedParam

unnamedParam :: CodeType -> Parameter
unnamedParam = NameableParam

customClass :: [MethodInfo] -> ClassInfo
customClass = Regular

implementation :: String -> [MethodInfo] -> ClassInfo
implementation = Implements

constructorInfo :: [Parameter] -> [Step] -> MethodInfo
constructorInfo _ [] = error "constructorInfo should be called with a non-empty list of Step"
constructorInfo ps ss = CI ps (fromList ss)

methodInfo :: FuncName -> [Parameter] -> CodeType -> [Step] -> MethodInfo
methodInfo _ _ _ [] = error "methodInfo should be called with a non-empty list of Step"
methodInfo n ps t ss = MI n ps t (fromList ss)

appendCurrSol :: CodeChunk -> Step
appendCurrSol curr = statementStep (\cdchs es -> case (cdchs, es) of
    ([s], []) -> appendCurrSolFS curr s
    (_,_) -> error "Fill for appendCurrSol should provide one CodeChunk and no Exprs")
  
populateSolList :: CodeChunk -> CodeChunk -> CodeChunk -> [Step]
populateSolList arr el fld = [statementStep (\cdchs es -> case (cdchs, es) of
    ([s], []) -> FDecDef s (Matrix [[]])
    (_,_) -> error popErr),
  statementStep (\cdchs es -> case (cdchs, es) of
    ([s], []) -> FForEach el (sy arr) [appendCurrSolFS (ccObjVar el fld) s]
    (_,_) -> error popErr)]
  where popErr = "Fill for populateSolList should provide one CodeChunk and no Exprs"

assignArrayIndex :: Integer -> Step
assignArrayIndex i = statementStep (\cdchs es -> case (cdchs, es) of
  ([a],[e]) -> FAsgIndex a i e
  (_,_) -> error "Fill for assignArrayIndex should provide one CodeChunk and one Expr")

assignSolFromObj :: CodeChunk -> Step
assignSolFromObj o = statementStep (\cdchs es -> case (cdchs, es) of
  ([s],[]) -> FAsg s (sy $ ccObjVar o s)
  (_,_) -> error "Fill for assignSolFromObj should provide one CodeChunk and no Exprs")

initSolListFromArray :: CodeChunk -> Step
initSolListFromArray a = statementStep (\cdchs es -> case (cdchs, es) of
  ([s],[]) -> FAsg s (Matrix [[idx (sy a) (int 0)]])
  (_,_) -> error "Fill for initSolListFromArray should provide one CodeChunk and no Exprs")

initSolListWithVal :: Step
initSolListWithVal = statementStep (\cdchs es -> case (cdchs, es) of
  ([s],[v]) -> FDecDef s (Matrix [[v]])
  (_,_) -> error "Fill for initSolListWithVal should provide one CodeChunk and one Expr")

-- FunctionInterface for loop condition, CodeChunk for independent var,
-- FunctionInterface for solving, CodeChunk for soln array to populate with
solveAndPopulateWhile :: FunctionInterface -> CodeChunk -> FunctionInterface -> 
  CodeChunk -> Step
solveAndPopulateWhile lc iv slv popArr = loopStep [lc] (\case 
  [ub] -> sy iv $< sy ub
  _ -> error "Fill for solveAndPopulateWhile should provide one CodeChunk") 
  [callStep slv, appendCurrSol popArr]

appendCurrSolFS :: CodeChunk -> CodeChunk -> FuncStmt
appendCurrSolFS cs s = FAppend (sy s) (idx (sy cs) (int 0))

fixedReturn :: Expr -> Step
fixedReturn = lockedStatement . FRet

statementStep :: ([CodeChunk] -> [Expr] -> FuncStmt) -> Step
statementStep = Statement

lockedStatement :: FuncStmt -> Step
lockedStatement s = Statement (\_ _ -> s)
