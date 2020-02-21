{-# LANGUAGE LambdaCase #-}
module Language.Drasil.Code.ExternalLibrary (ExternalLibrary, Step,
  FunctionInterface, Argument, externalLib, choiceSteps, choiceStep, 
  mandatoryStep, mandatorySteps, callStep, callWithImport, callWithImports, 
  libFunction, libMethod, libFunctionWithResult, libMethodWithResult, 
  libConstructor, constructAndReturn, lockedArg, lockedNamedArg, inlineArg, 
  inlineNamedArg, preDefinedArg, preDefinedNamedArg, functionArg, customObjArg, 
  recordArg, lockedParam, unnamedParam, customClass, implementation, 
  constructorInfo, methodInfo, appendCurrSol, populateSolList, 
  assignArrayIndex, assignSolFromObj, initSolListFromArray, initSolListWithVal, 
  solveAndPopulateWhile, returnExprList, fixedReturn
) where

import Language.Drasil
import Language.Drasil.Chunk.Code (CodeChunk, codeType, ccObjVar)
import Language.Drasil.Mod (FuncStmt(..))

import GOOL.Drasil (CodeType)

type VarName = String
type FuncName = String
type FieldName = String
type Condition = Expr
type Import = String

type ExternalLibrary = [StepGroup]

type StepGroup = [[Step]]

data Step = Call [Import] FunctionInterface
  -- A while loop -- function calls in the condition, other conditions, steps for the body
  | Loop [FunctionInterface] ([CodeChunk] -> Condition) [Step]
  -- For when a statement is needed, but does not interface with the external library
  | Statement ([CodeChunk] -> [Expr] -> FuncStmt)

data FunctionInterface = FI FuncType CodeChunk [Argument] (Maybe Result)

data Result = Assign CodeChunk | Return 

data Argument = 
  -- Not dependent on use case, Maybe is name for the argument
  LockedArg (Maybe VarName) Expr 
  -- First Maybe is name for the argument (needed for named parameters)
  -- Second Maybe is the variable if it needs to be declared and defined prior to calling
  | Basic (Maybe VarName) CodeType (Maybe CodeChunk) 
  | Fn CodeChunk [Parameter] Step
  | Class [Import] CodeChunk ClassInfo
  | Record FuncName CodeChunk [FieldName]

data Parameter = LockedParam CodeChunk | NameableParam CodeType

data ClassInfo = Regular [MethodInfo] | Implements String [MethodInfo]

-- Constructor: known parameters, body
data MethodInfo = CI [Parameter] [Step]
  -- Method name, parameters, return type, body
  | MI FuncName [Parameter] CodeType [Step]

data FuncType = Function | Method CodeChunk | Constructor

externalLib :: [StepGroup] -> ExternalLibrary
externalLib = id

choiceSteps :: [[Step]] -> StepGroup
choiceSteps = id

choiceStep :: [Step] -> StepGroup
choiceStep = map (: [])

mandatoryStep :: Step -> StepGroup
mandatoryStep f = [[f]]

mandatorySteps :: [Step] -> StepGroup
mandatorySteps fs = [fs]

callStep :: FunctionInterface -> Step
callStep = Call []

callWithImport :: Import -> FunctionInterface -> Step
callWithImport i = Call [i]

callWithImports :: [Import] -> FunctionInterface -> Step
callWithImports = Call

loopStep :: [FunctionInterface] -> ([CodeChunk] -> Condition) -> [Step] -> Step
loopStep = Loop

libFunction :: CodeChunk -> [Argument] -> FunctionInterface
libFunction f ps = FI Function f ps Nothing

libMethod :: CodeChunk -> CodeChunk -> [Argument] -> FunctionInterface
libMethod o m ps = FI (Method o) m ps Nothing

libFunctionWithResult :: CodeChunk -> [Argument] -> CodeChunk -> 
  FunctionInterface
libFunctionWithResult f ps r = FI Function f ps (Just $ Assign r)

libMethodWithResult :: CodeChunk -> CodeChunk -> [Argument] -> CodeChunk -> 
  FunctionInterface
libMethodWithResult o m ps r = FI (Method o) m ps (Just $ Assign r)

libConstructor :: CodeChunk -> [Argument] -> CodeChunk -> FunctionInterface
libConstructor c as r = FI Constructor c as (Just $ Assign r)

constructAndReturn :: CodeChunk -> [Argument] -> FunctionInterface
constructAndReturn c as = FI Constructor c as (Just Return)

lockedArg :: Expr -> Argument
lockedArg = LockedArg Nothing

lockedNamedArg :: VarName -> Expr -> Argument
lockedNamedArg n = LockedArg (Just n)

inlineArg :: CodeType -> Argument
inlineArg t = Basic Nothing t Nothing

inlineNamedArg :: VarName ->  CodeType -> Argument
inlineNamedArg n t = Basic (Just n) t Nothing

preDefinedArg :: CodeChunk -> Argument
preDefinedArg v = Basic Nothing (codeType v) (Just v)

preDefinedNamedArg :: VarName -> CodeChunk -> Argument
preDefinedNamedArg n v = Basic (Just n) (codeType v) (Just v)

functionArg :: CodeChunk -> [Parameter] -> Step -> Argument
functionArg = Fn

customObjArg :: [Import] -> CodeChunk -> ClassInfo -> Argument
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
constructorInfo = CI

methodInfo :: FuncName -> [Parameter] -> CodeType -> [Step] -> MethodInfo
methodInfo = MI

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

returnExprList :: Step
returnExprList = statementStep (\cdchs es -> case (cdchs, es) of
  ([], _) -> FRet $ Matrix [es]
  (_,_) -> error "Fill for returnExprList should provide no CodeChunks")

appendCurrSolFS :: CodeChunk -> CodeChunk -> FuncStmt
appendCurrSolFS cs s = FAppend (sy s) (idx (sy cs) (int 0))

fixedReturn :: Expr -> Step
fixedReturn = lockedStatement . FRet

statementStep :: ([CodeChunk] -> [Expr] -> FuncStmt) -> Step
statementStep = Statement

lockedStatement :: FuncStmt -> Step
lockedStatement s = Statement (\_ _ -> s)
