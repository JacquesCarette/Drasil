module Language.Drasil.Code.ExternalLibrary (ExternalLibrary, 
  FunctionInterface, Argument, externalLib, choiceStep, mandatoryStep, 
  libFunction, libMethod, libFunctionWithResult, libMethodWithResult, 
  loopConditionFunction, loopConditionMethod, loopedFunction, loopedMethod, 
  loopedFunctionWithResult, loopedMethodWithResult, lockedArg, lockedNamedArg, 
  inlineArg, inlineNamedArg, preDefinedArg, preDefinedNamedArg, functionArg, 
  customObjArg, recordArg, unknown, interface, methodInterface
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

type Step = [FunctionInterface]

data FunctionInterface = FI FuncType FuncName [Argument] [ContextAttribute]

data ContextAttribute = Assignment CodeChunk -- CodeChunk is variable to assign
  | LoopCondition ([CodeChunk] -> Condition) -- Parameter is other conditions
  | LoopBody ([CodeChunk] -> [FuncStmt]) -- Other statements to do in the loop, need to pass list of CodeChunk for use-case-specific variables

data Argument = 
  -- Not dependent on use case, Maybe is name for the argument
  Locked (Maybe VarName) Expr 
  -- First Maybe is name for the argument (needed for named parameters)
  -- Second Maybe is the variable name if it needs to be declared and defined prior to calling
  | Basic (Maybe VarName) CodeType (Maybe CodeChunk) 
  | Fn CodeChunk [CodeType] ([Expr] -> FuncStmt)
  | Class CodeChunk Interface
  | Record FuncName CodeChunk [FieldName]

data Interface = Unknown | Implements String [MethodInterface]

data MethodInterface = MI FuncName [CodeChunk] CodeType -- Name, parameters, return type

data FuncType = Function | Method CodeChunk

externalLib :: [Step] -> ExternalLibrary
externalLib = id

choiceStep :: [FunctionInterface] -> Step
choiceStep = id

mandatoryStep :: FunctionInterface -> Step
mandatoryStep f = [f]

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

lockedArg :: Expr -> Argument
lockedArg = Locked Nothing

lockedNamedArg :: VarName -> Expr -> Argument
lockedNamedArg n = Locked (Just n)

inlineArg :: CodeType -> Argument
inlineArg t = Basic Nothing t Nothing

inlineNamedArg :: VarName ->  CodeType -> Argument
inlineNamedArg n t = Basic (Just n) t Nothing

preDefinedArg :: CodeType -> CodeChunk -> Argument
preDefinedArg t v = Basic Nothing t (Just v)

preDefinedNamedArg :: VarName ->  CodeType -> CodeChunk -> Argument
preDefinedNamedArg n t v = Basic (Just n) t (Just v)

functionArg :: CodeChunk -> [CodeType] -> ([Expr] -> FuncStmt) -> Argument
functionArg = Fn

customObjArg :: CodeChunk -> Interface -> Argument
customObjArg = Class

recordArg :: FuncName -> CodeChunk -> [FieldName] -> Argument
recordArg = Record

unknown :: Interface
unknown = Unknown

interface :: String -> [MethodInterface] -> Interface
interface = Implements

methodInterface :: FuncName -> [CodeChunk] -> CodeType -> MethodInterface
methodInterface = MI