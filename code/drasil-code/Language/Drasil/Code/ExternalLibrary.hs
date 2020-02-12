module Language.Drasil.Code.ExternalLibrary (ExternalLibrary, 
  FunctionInterface, Argument, externalLib, choiceStep, mandatoryStep, 
  libFunction, libMethod, libFunctionWithResult, libMethodWithResult, 
  loopConditionFunction, loopConditionMethod, loopedFunction, loopedMethod, 
  loopedFunctionWithResult, loopedMethodWithResult, libConstructor, lockedArg, 
  lockedNamedArg, inlineArg, inlineNamedArg, preDefinedArg, preDefinedNamedArg, 
  functionArg, customObjArg, recordArg, lockedParam, unnamedParam, customClass, 
  implementation, constructorInfo, methodInfo, iterateStep, statementStep,
  lockedStatement
) where

import Language.Drasil
import Language.Drasil.Chunk.Code (CodeChunk)
import Language.Drasil.CodeSpec (FuncStmt)

import GOOL.Drasil (CodeType)

type VarName = String
type FuncName = String
type FieldName = String
type Condition = Expr

type ExternalLibrary = [Step]

data Step = Call [FunctionInterface]
  -- A foreach loop - CodeChunk to iterate through, CodeChunk for iteration variable, loop body
  | Iterate CodeChunk CodeChunk ([CodeChunk] -> [FuncStmt])
  -- For when a statement is needed, but does not interface with the external library
  | Statement ([CodeChunk] -> [Expr] -> FuncStmt)

data FunctionInterface = FI FuncType FuncName [Argument] [ContextAttribute]

data ContextAttribute = Assignment CodeChunk -- CodeChunk is variable to assign
  | LoopCondition ([CodeChunk] -> Condition) -- Parameter is other conditions
  | LoopBody ([CodeChunk] -> [FuncStmt]) -- Other statements to do in the loop, need to pass list of CodeChunk for use-case-specific variables

data Argument = 
  -- Not dependent on use case, Maybe is name for the argument
  LockedArg (Maybe VarName) Expr 
  -- First Maybe is name for the argument (needed for named parameters)
  -- Second Maybe is the variable name if it needs to be declared and defined prior to calling
  | Basic (Maybe VarName) CodeType (Maybe CodeChunk) 
  | Fn CodeChunk [Parameter] ([Expr] -> FuncStmt)
  | Class CodeChunk ClassInfo
  | Record FuncName CodeChunk [FieldName]

data Parameter = LockedParam CodeChunk | NameableParam CodeType

data ClassInfo = Regular [MethodInfo] | Implements String [MethodInfo]

-- Constructor body
data MethodInfo = CI [Step]
  -- Method name, parameters, return type, body
  | MI FuncName [Parameter] CodeType [Step]

data FuncType = Function | Method CodeChunk | Constructor

externalLib :: [Step] -> ExternalLibrary
externalLib = id

choiceStep :: [FunctionInterface] -> Step
choiceStep = Call

mandatoryStep :: FunctionInterface -> Step
mandatoryStep f = Call [f]

libFunction :: FuncName -> [Argument] -> FunctionInterface
libFunction n ps = FI Function n ps []

libMethod :: CodeChunk -> FuncName -> [Argument] -> FunctionInterface
libMethod o n ps = FI (Method o) n ps []

libFunctionWithResult :: FuncName -> [Argument] -> CodeChunk -> 
  FunctionInterface
libFunctionWithResult n ps r = FI Function n ps [Assignment r]

libMethodWithResult :: CodeChunk -> FuncName -> [Argument] -> CodeChunk -> 
  FunctionInterface
libMethodWithResult o n ps r = FI (Method o) n ps [Assignment r]

loopConditionFunction :: FuncName -> [Argument] -> 
  ([CodeChunk] -> Condition) -> FunctionInterface
loopConditionFunction n ps c = FI Function n ps [LoopCondition c]

loopConditionMethod :: CodeChunk -> FuncName -> [Argument] -> 
  ([CodeChunk] -> Condition) -> FunctionInterface
loopConditionMethod o n ps c = FI (Method o) n ps [LoopCondition c]

loopedFunction :: FuncName -> [Argument] -> 
  ([CodeChunk] -> [FuncStmt]) -> FunctionInterface
loopedFunction n ps loop = FI Function n ps [LoopBody loop]

loopedMethod :: CodeChunk -> FuncName -> [Argument] -> 
  ([CodeChunk] -> [FuncStmt]) -> FunctionInterface
loopedMethod o n ps loop = FI (Method o) n ps [LoopBody loop]

loopedFunctionWithResult :: FuncName -> [Argument] -> CodeChunk ->
  ([CodeChunk] -> [FuncStmt]) -> FunctionInterface
loopedFunctionWithResult n ps r loop = FI Function n ps [Assignment r, 
  LoopBody loop]

loopedMethodWithResult :: CodeChunk -> FuncName -> [Argument] -> CodeChunk ->
  ([CodeChunk] -> [FuncStmt]) -> FunctionInterface
loopedMethodWithResult o n ps r loop = FI (Method o) n ps [Assignment r, 
  LoopBody loop]

libConstructor :: FuncName -> [Argument] -> CodeChunk -> FunctionInterface
libConstructor n as c = FI Constructor n as [Assignment c] 

lockedArg :: Expr -> Argument
lockedArg = LockedArg Nothing

lockedNamedArg :: VarName -> Expr -> Argument
lockedNamedArg n = LockedArg (Just n)

inlineArg :: CodeType -> Argument
inlineArg t = Basic Nothing t Nothing

inlineNamedArg :: VarName ->  CodeType -> Argument
inlineNamedArg n t = Basic (Just n) t Nothing

preDefinedArg :: CodeType -> CodeChunk -> Argument
preDefinedArg t v = Basic Nothing t (Just v)

preDefinedNamedArg :: VarName ->  CodeType -> CodeChunk -> Argument
preDefinedNamedArg n t v = Basic (Just n) t (Just v)

functionArg :: CodeChunk -> [Parameter] -> ([Expr] -> FuncStmt) -> Argument
functionArg = Fn

customObjArg :: CodeChunk -> ClassInfo -> Argument
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

constructorInfo :: [Step] -> MethodInfo
constructorInfo = CI

methodInfo :: FuncName -> [Parameter] -> CodeType -> [Step] -> MethodInfo
methodInfo = MI

iterateStep :: CodeChunk -> CodeChunk -> ([CodeChunk] -> [FuncStmt]) -> Step
iterateStep = Iterate

statementStep :: ([CodeChunk] -> [Expr] -> FuncStmt) -> Step
statementStep = Statement

lockedStatement :: FuncStmt -> Step
lockedStatement s = Statement (\_ _ -> s)