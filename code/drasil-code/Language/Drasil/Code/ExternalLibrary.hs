{-# LANGUAGE LambdaCase #-}
-- | Defines a language for specifying external library use scenarios
module Language.Drasil.Code.ExternalLibrary (ExternalLibrary, Step(..), 
  FunctionInterface(..), Result(..), Argument(..), ArgumentInfo(..), 
  Parameter(..), ClassInfo(..), MethodInfo(..), FuncType(..), externalLib, 
  choiceSteps, choiceStep, mandatoryStep, mandatorySteps, callStep, 
  libFunction, libMethod, libFunctionWithResult, libMethodWithResult, 
  libConstructor, libConstructorMultiReqs, constructAndReturn, lockedArg, 
  lockedNamedArg, inlineArg, inlineNamedArg, preDefinedArg, preDefinedNamedArg, 
  functionArg, customObjArg, recordArg, lockedParam, unnamedParam, customClass, 
  implementation, constructorInfo, methodInfo, methodInfoNoReturn, 
  appendCurrSol, populateSolList, assignArrayIndex, assignSolFromObj, 
  initSolListFromArray, initSolListWithVal, solveAndPopulateWhile, 
  returnExprList, fixedReturn, initSolWithVal
) where

import Language.Drasil
import Language.Drasil.Chunk.Code (CodeVarChunk, CodeFuncChunk, codeName)
import Language.Drasil.Chunk.Parameter (ParameterChunk, pcAuto)
import Language.Drasil.CodeExpr (field)
import Language.Drasil.Mod (FuncStmt(..), Description)

import Control.Lens ((^.))
import Data.List.NonEmpty (NonEmpty(..), fromList)

type Condition = Expr
type Requires = String

type ExternalLibrary = [StepGroup]

type StepGroup = NonEmpty [Step]

-- A step can be a call to an external library function or method.
data Step = Call FunctionInterface 
  -- A while loop -- function calls in the condition, other conditions, steps for the body
  | Loop (NonEmpty FunctionInterface) ([Expr] -> Condition) (NonEmpty Step)
  -- For when a statement is needed, but does not interface with the external library
  | Statement ([CodeVarChunk] -> [Expr] -> FuncStmt)

-- The first item in the requires list should be where the function being called is defined
data FunctionInterface = FI (NonEmpty Requires) FuncType CodeFuncChunk [Argument] (Maybe Result)

-- The result of a function call can be assigned to a variable or returned
data Result = Assign CodeVarChunk | Return 

data Argument = Arg (Maybe NamedArgument) ArgumentInfo -- Maybe named argument

data ArgumentInfo = 
  -- An argument not dependent on use case
  LockedArg Expr 
  -- An argument dependent on the use case. Maybe is the variable if it needs to be declared and defined prior to calling
  | Basic Space (Maybe CodeVarChunk) 
  -- A function-type argument, with a single Step for the body.
  | Fn CodeFuncChunk [Parameter] Step
  -- An argument that is an object of a class that must be implemented in the 
  -- calling program.
  -- Parameters: Requires, description, object, constructor, class info
  | Class [Requires] Description CodeVarChunk CodeFuncChunk ClassInfo
  -- An argument that is an object of a record class defined by the external
  -- library, where some fields need to be set by the calling program.
  -- Parameters: Requires, constructor, object, fields. 
  -- First Require should be where the record type is defined.
  | Record (NonEmpty Requires) CodeFuncChunk CodeVarChunk [CodeVarChunk]

data Parameter = LockedParam ParameterChunk | NameableParam Space

-- For classes that need to be generated in the calling program. May be a 
-- regular class or a class that implements an interface from the external 
-- library.
data ClassInfo = Regular [MethodInfo] | Implements String [MethodInfo]

-- Constructor: description, known parameters, body. (CodeFuncChunk for constructor is not here because it is higher up in the AST, at the Class node)
data MethodInfo = CI Description [Parameter] [Step]
  -- Method, description, known parameters, maybe return description, body
  | MI CodeFuncChunk Description [Parameter] (Maybe Description) (NonEmpty Step)

data FuncType = Function | Method CodeVarChunk | Constructor

-- Specifies an external library
externalLib :: [StepGroup] -> ExternalLibrary
externalLib = id

-- To be used when there are multiple options for a group of consecutive steps, 
-- where a single use-case-specific factor decides which step group to use
choiceSteps :: [[Step]] -> StepGroup
choiceSteps [] = error "choiceSteps should be called with a non-empty list"
choiceSteps sg = fromList sg

-- To be used when there are multiple options for a single step, where a 
-- use-case-specific factor decides which step to use.
choiceStep :: [Step] -> StepGroup
choiceStep [] = error "choiceStep should be called with a non-empty list"
choiceStep ss = fromList $ map (: []) ss

-- Specifies a step which must exist in some form in every use case.
mandatoryStep :: Step -> StepGroup
mandatoryStep f = [f] :| []

-- Specifies multiple consecutive steps that all must exist in some form in 
-- every use case.
mandatorySteps :: [Step] -> StepGroup
mandatorySteps fs = fs :| []

-- Specifies a step that includes a call to an external library function or method.
callStep :: FunctionInterface -> Step
callStep = Call

-- Specifies a step where an external library function or method is called in a 
-- while-loop condition and in the loop body.
loopStep :: [FunctionInterface] -> ([Expr] -> Condition) -> [Step] -> Step
loopStep [] _ _ = error "loopStep should be called with a non-empty list of FunctionInterface"
loopStep _ _ [] = error "loopStep should be called with a non-empty list of Step"
loopStep fis c ss = Loop (fromList fis) c (fromList ss)

-- Specifies a call to an external library function.
libFunction :: Requires -> CodeFuncChunk -> [Argument] -> FunctionInterface
libFunction rq f ps = FI (rq :| []) Function f ps Nothing

-- Specifies a call to an external library method.
libMethod :: Requires -> CodeVarChunk -> CodeFuncChunk -> [Argument] -> 
  FunctionInterface
libMethod rq o m ps = FI (rq :| []) (Method o) m ps Nothing

-- Specifies a call to an external library function, where the result is 
-- assigned to a variable.
libFunctionWithResult :: Requires -> CodeFuncChunk -> [Argument] -> 
  CodeVarChunk -> FunctionInterface
libFunctionWithResult rq f ps r = FI (rq :| []) Function f ps (Just $ Assign r)

-- Specifies a call to an external library method, where the result is 
-- assigned to a variable.
libMethodWithResult :: Requires -> CodeVarChunk -> CodeFuncChunk -> [Argument] 
  -> CodeVarChunk -> FunctionInterface
libMethodWithResult rq o m ps r = FI (rq :| []) (Method o) m ps (Just $ Assign r)

-- Specifies a call to an external library constructor, where the result is 
-- assigned to a variable.
libConstructor :: Requires -> CodeFuncChunk -> [Argument] -> CodeVarChunk -> 
  FunctionInterface
libConstructor rq c as r = FI (rq :| []) Constructor c as (Just $ Assign r)

-- Specifies a call to an external library function, where multiple modules from
-- the external library are required, and the result is assigned to a variable.
libConstructorMultiReqs :: [Requires] -> CodeFuncChunk -> [Argument] -> 
  CodeVarChunk -> FunctionInterface
libConstructorMultiReqs [] _ _ _ = error $ "libConstructorMultiReqs should" ++
  " be called with a non-empty list of Requires"
libConstructorMultiReqs rqs c as r = FI (fromList rqs) Constructor c as 
  (Just $ Assign r)

-- Specifies a call to an external library constructor, where the result is returned.
constructAndReturn :: Requires -> CodeFuncChunk -> [Argument] -> 
  FunctionInterface
constructAndReturn rq c as = FI (rq :| []) Constructor c as (Just Return)

-- Specifies an argument that is not use-case-dependent.
lockedArg :: Expr -> Argument
lockedArg = Arg Nothing . LockedArg

-- Specifies a named argument that is not use-case-dependent.
lockedNamedArg :: NamedArgument -> Expr -> Argument
lockedNamedArg n = Arg (Just n) . LockedArg

-- Specifies a use-case-dependent argument whose value can be inlined in the 
-- call.
inlineArg :: Space -> Argument
inlineArg t = Arg Nothing $ Basic t Nothing

-- Specifies a use-case-dependent named argument whose value can be inlined in 
-- the call.
inlineNamedArg :: NamedArgument ->  Space -> Argument
inlineNamedArg n t = Arg (Just n) $ Basic t Nothing

-- Specifies use-case-dependent argument whose value must be assigned to a 
-- variable before being passed in the call.
preDefinedArg :: CodeVarChunk -> Argument
preDefinedArg v = Arg Nothing $ Basic (v ^. typ) (Just v)

-- Specifies use-case-dependent named argument whose value must be assigned to 
-- a variable before being passed in the call.
preDefinedNamedArg :: NamedArgument -> CodeVarChunk -> Argument
preDefinedNamedArg n v = Arg (Just n) $ Basic (v ^. typ) (Just v)

-- Specifies a function type argument, where the body consists of a single step.
functionArg :: CodeFuncChunk -> [Parameter] -> Step -> Argument
functionArg f ps b = Arg Nothing (Fn f ps b)

-- Specifies an argument that is an object of a class that must be defined in 
-- the calling program.
customObjArg :: [Requires] -> Description -> CodeVarChunk -> CodeFuncChunk -> 
  ClassInfo -> Argument
customObjArg rs d o c ci = Arg Nothing (Class rs d o c ci)

-- Specifies an argument that is an object of a class from the external library.
-- The list of [CodeVarChunk] represents fields of the object that must be set 
-- in the calling program.
recordArg :: Requires -> CodeFuncChunk -> CodeVarChunk -> [CodeVarChunk] -> 
  Argument
recordArg rq c o fs = Arg Nothing (Record (rq :| []) c o fs)

-- Specifies a use-case-independent parameter
lockedParam :: CodeVarChunk -> Parameter
lockedParam = LockedParam . pcAuto

-- Specifies a parameter whose name depends on the use case.
unnamedParam :: Space -> Parameter
unnamedParam = NameableParam

-- Specifies a class that must be implemented in the calling program.
customClass :: [MethodInfo] -> ClassInfo
customClass = Regular

-- Specifies an implementation of an interface from the external library.
implementation :: String -> [MethodInfo] -> ClassInfo
implementation = Implements

-- Specifies a constructor.
constructorInfo :: CodeFuncChunk -> [Parameter] -> [Step] -> MethodInfo
constructorInfo c = CI ("Constructor for " ++ codeName c ++ " objects")

-- Specifies a method.
methodInfo :: CodeFuncChunk -> Description -> [Parameter] -> Description -> 
  [Step] -> MethodInfo
methodInfo _ _ _ _ [] = error "methodInfo should be called with a non-empty list of Step"
methodInfo m d ps rd ss = MI m d ps (Just rd) (fromList ss)

-- Specifies a method that does not return anything.
methodInfoNoReturn :: CodeFuncChunk -> Description -> [Parameter] -> [Step] -> 
  MethodInfo
methodInfoNoReturn _ _ _ [] = error "methodInfoNoReturn should be called with a non-empty list of Step"
methodInfoNoReturn m d ps ss = MI m d ps Nothing (fromList ss)

-- Specifies a statement where a current solution is appended to a solution list.
appendCurrSol :: Expr -> Step
appendCurrSol curr = statementStep (\cdchs es -> case (cdchs, es) of
    ([s], []) -> appendCurrSolFS curr s
    (_,_) -> error "Fill for appendCurrSol should provide one CodeChunk and no Exprs")
  
-- Specifies a statement where a solution list is populated by iterating 
-- through a solution array.
populateSolList :: CodeVarChunk -> CodeVarChunk -> CodeVarChunk -> [Step]
populateSolList arr el fld = [statementStep (\cdchs es -> case (cdchs, es) of
    ([s], []) -> FAsg s (Matrix [[]])
    (_,_) -> error popErr),
  statementStep (\cdchs es -> case (cdchs, es) of
    ([s], []) -> FForEach el (sy arr) [appendCurrSolFS (field el fld) s]
    (_,_) -> error popErr)]
  where popErr = "Fill for populateSolList should provide one CodeChunk and no Exprs"

-- Specifies statements where every index of an array is assigned a value.
assignArrayIndex :: Step
assignArrayIndex = statementStep (\cdchs es -> case (cdchs, es) of
  ([a],vs) -> FMulti $ zipWith (FAsgIndex a) [0..] vs
  (_,_) -> error "Fill for assignArrayIndex should provide one CodeChunk")

-- Specifies a statement where a solution is assigned from the field of an 
-- object.
assignSolFromObj :: CodeVarChunk -> Step
assignSolFromObj o = statementStep (\cdchs es -> case (cdchs, es) of
  ([s],[]) -> FAsg s (field o s)
  (_,_) -> error "Fill for assignSolFromObj should provide one CodeChunk and no Exprs")

-- Specifies a statement where a solution list is initialized with the first 
-- element of an array.
initSolListFromArray :: CodeVarChunk -> Step
initSolListFromArray a = statementStep (\cdchs es -> case (cdchs, es) of
  ([s],[]) -> FAsg s (Matrix [[idx (sy a) (int 0)]])
  (_,_) -> error "Fill for initSolListFromArray should provide one CodeChunk and no Exprs")

-- Specifies a statement where a solution list is initialized with a value.
initSolListWithVal :: Step
initSolListWithVal = statementStep (\cdchs es -> case (cdchs, es) of
  ([s],[v]) -> FDecDef s (Matrix [[v]])
  (_,_) -> error "Fill for initSolListWithVal should provide one CodeChunk and one Expr")

-- FunctionInterface for loop condition, CodeChunk for solution object, 
-- CodeChunk for independent var, FunctionInterface for solving, 
-- CodeChunk for soln array to populate with
solveAndPopulateWhile :: FunctionInterface -> CodeVarChunk -> CodeVarChunk -> 
  FunctionInterface -> CodeVarChunk -> Step
solveAndPopulateWhile lc ob iv slv popArr = loopStep [lc] (\case 
  [ub] -> field ob iv $< ub
  _ -> error "Fill for solveAndPopulateWhile should provide one Expr") 
  [callStep slv, appendCurrSol (field ob popArr)]

-- Specifies a statement where a list is returned, where each value of the list -- is explicitly defined.
returnExprList :: Step
returnExprList = statementStep (\cdchs es -> case (cdchs, es) of
  ([], _) -> FRet $ Matrix [es]
  (_,_) -> error "Fill for returnExprList should provide no CodeChunks")

-- A statement where a current solution is appended to a solution list
appendCurrSolFS :: Expr -> CodeVarChunk -> FuncStmt
appendCurrSolFS cs s = FAppend (sy s) (idx cs (int 0))

-- Specifies a use-case-independent statement that returns a value.
fixedReturn :: Expr -> Step
fixedReturn = lockedStatement . FRet

-- Specifies a statement step
statementStep :: ([CodeVarChunk] -> [Expr] -> FuncStmt) -> Step
statementStep = Statement

-- Specifies a statement that is not use-case-dependent
lockedStatement :: FuncStmt -> Step
lockedStatement s = Statement (\_ _ -> s)

-- Specifies a statement where a single solution is initialized with a value.
initSolWithVal :: Step
initSolWithVal = statementStep (\cdchs es -> case (cdchs, es) of
  ([s],[v]) -> FDecDef s v
  (_,_) -> error "Fill for initSolWithVal should provide one CodeChunk and one Expr")
