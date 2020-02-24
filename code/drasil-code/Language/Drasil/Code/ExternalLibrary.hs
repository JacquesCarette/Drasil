module Language.Drasil.Code.ExternalLibrary (ExternalLibrary, Step,
  FunctionInterface, Argument, externalLib, choiceSteps, choiceStep, 
  mandatoryStep, callStep, callRequiresJust, callRequires, loopStep, 
  libFunction, libMethod, libFunctionWithResult, libMethodWithResult, 
  libConstructor, lockedArg, lockedNamedArg, inlineArg, inlineNamedArg, 
  preDefinedArg, preDefinedNamedArg, functionArg, customObjArg, recordArg, 
  lockedParam, unnamedParam, customClass, implementation, constructorInfo, 
  methodInfo, iterateStep, statementStep, lockedStatement
) where

import Language.Drasil
import Language.Drasil.Chunk.Code (CodeChunk, codeType)
import Language.Drasil.CodeSpec (FuncStmt)

import GOOL.Drasil (CodeType)

type VarName = String
type FuncName = String
type FieldName = String
type Condition = Expr
type Requires = String

type ExternalLibrary = [StepGroup]

type StepGroup = [[Step]]

data Step = Call [Requires] FunctionInterface
  -- A while loop -- function calls in the condition, other conditions, steps for the body
  | Loop [FunctionInterface] ([CodeChunk] -> Condition) [Step]
  -- A foreach loop - CodeChunk to iterate through, CodeChunk for iteration variable, loop body
  -- FIXME: This Node should be merged with Statement. Will do soon.
  | Iterate CodeChunk CodeChunk ([CodeChunk] -> [FuncStmt])
  -- For when a statement is needed, but does not interface with the external library
  | Statement ([CodeChunk] -> [Expr] -> FuncStmt)

data FunctionInterface = FI FuncType FuncName [Argument] (Maybe CodeChunk) -- Maybe CodeChunk to assign to

data Argument = 
  -- Not dependent on use case, Maybe is name for the argument
  LockedArg (Maybe VarName) Expr 
  -- First Maybe is name for the argument (needed for named parameters)
  -- Second Maybe is the variable if it needs to be declared and defined prior to calling
  | Basic (Maybe VarName) CodeType (Maybe CodeChunk)
  | Fn CodeChunk [Parameter] ([Expr] -> FuncStmt)
  | Class [Requires] CodeChunk ClassInfo
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

callStep :: FunctionInterface -> Step
callStep = Call []

callRequiresJust :: Requires -> FunctionInterface -> Step
callRequiresJust i = Call [i]

callRequires :: [Requires] -> FunctionInterface -> Step
callRequires = Call

loopStep :: [FunctionInterface] -> ([CodeChunk] -> Condition) -> [Step] -> Step
loopStep = Loop

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
constructorInfo = CI

methodInfo :: FuncName -> [Parameter] -> CodeType -> [Step] -> MethodInfo
methodInfo = MI

iterateStep :: CodeChunk -> CodeChunk -> ([CodeChunk] -> [FuncStmt]) -> Step
iterateStep = Iterate

statementStep :: ([CodeChunk] -> [Expr] -> FuncStmt) -> Step
statementStep = Statement

lockedStatement :: FuncStmt -> Step
lockedStatement s = Statement (\_ _ -> s)
