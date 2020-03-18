{-# LANGUAGE LambdaCase #-}
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
  returnExprList, fixedReturn
) where

import Language.Drasil
import Language.Drasil.Chunk.Code (CodeVarChunk, CodeFuncChunk, codeName)
import Language.Drasil.Chunk.Parameter (ParameterChunk, pcAuto)
import Language.Drasil.CodeExpr (field)
import Language.Drasil.Mod (FuncStmt(..))

import Control.Lens ((^.))
import Data.List.NonEmpty (NonEmpty(..), fromList)

type Description = String
type Condition = Expr
type Requires = String

type ExternalLibrary = [StepGroup]

type StepGroup = NonEmpty [Step]

data Step = Call FunctionInterface 
  -- A while loop -- function calls in the condition, other conditions, steps for the body
  | Loop (NonEmpty FunctionInterface) ([Expr] -> Condition) (NonEmpty Step)
  -- For when a statement is needed, but does not interface with the external library
  | Statement ([CodeVarChunk] -> [Expr] -> FuncStmt)

-- The first item in the requires list should be where the function being called is defined
data FunctionInterface = FI (NonEmpty Requires) FuncType CodeFuncChunk [Argument] (Maybe Result)

data Result = Assign CodeVarChunk | Return 

data Argument = Arg (Maybe NamedArgument) ArgumentInfo -- Maybe named argument

data ArgumentInfo = 
  -- Not dependent on use case, Maybe is name for the argument
  LockedArg Expr 
  -- Maybe is the variable if it needs to be declared and defined prior to calling
  | Basic Space (Maybe CodeVarChunk) 
  | Fn CodeFuncChunk [Parameter] Step
  -- Requires, description, object, constructor, class info
  | Class [Requires] Description CodeVarChunk CodeFuncChunk ClassInfo
  -- Requires, constructor, object, fields. First Require should be where the record type is defined.
  | Record (NonEmpty Requires) CodeFuncChunk CodeVarChunk [CodeVarChunk]

data Parameter = LockedParam ParameterChunk | NameableParam Space

data ClassInfo = Regular [MethodInfo] | Implements String [MethodInfo]

-- Constructor: description, known parameters, body. (CodeFuncChunk for constructor is not here because it is higher up in the AST, at the Class node)
data MethodInfo = CI Description [Parameter] [Step]
  -- Method, description, known parameters, maybe return description, body
  | MI CodeFuncChunk Description [Parameter] (Maybe Description) (NonEmpty Step)

data FuncType = Function | Method CodeVarChunk | Constructor

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
callStep = Call

loopStep :: [FunctionInterface] -> ([Expr] -> Condition) -> [Step] -> Step
loopStep [] _ _ = error "loopStep should be called with a non-empty list of FunctionInterface"
loopStep _ _ [] = error "loopStep should be called with a non-empty list of Step"
loopStep fis c ss = Loop (fromList fis) c (fromList ss)

libFunction :: Requires -> CodeFuncChunk -> [Argument] -> FunctionInterface
libFunction rq f ps = FI (rq :| []) Function f ps Nothing

libMethod :: Requires -> CodeVarChunk -> CodeFuncChunk -> [Argument] -> 
  FunctionInterface
libMethod rq o m ps = FI (rq :| []) (Method o) m ps Nothing

libFunctionWithResult :: Requires -> CodeFuncChunk -> [Argument] -> 
  CodeVarChunk -> FunctionInterface
libFunctionWithResult rq f ps r = FI (rq :| []) Function f ps (Just $ Assign r)

libMethodWithResult :: Requires -> CodeVarChunk -> CodeFuncChunk -> [Argument] 
  -> CodeVarChunk -> FunctionInterface
libMethodWithResult rq o m ps r = FI (rq :| []) (Method o) m ps (Just $ Assign r)

libConstructor :: Requires -> CodeFuncChunk -> [Argument] -> CodeVarChunk -> 
  FunctionInterface
libConstructor rq c as r = FI (rq :| []) Constructor c as (Just $ Assign r)

libConstructorMultiReqs :: [Requires] -> CodeFuncChunk -> [Argument] -> 
  CodeVarChunk -> FunctionInterface
libConstructorMultiReqs [] _ _ _ = error $ "libConstructorMultiReqs should" ++
  " be called with a non-empty list of Requires"
libConstructorMultiReqs rqs c as r = FI (fromList rqs) Constructor c as 
  (Just $ Assign r)

constructAndReturn :: Requires -> CodeFuncChunk -> [Argument] -> 
  FunctionInterface
constructAndReturn rq c as = FI (rq :| []) Constructor c as (Just Return)

lockedArg :: Expr -> Argument
lockedArg = Arg Nothing . LockedArg

lockedNamedArg :: NamedArgument -> Expr -> Argument
lockedNamedArg n = Arg (Just n) . LockedArg

inlineArg :: Space -> Argument
inlineArg t = Arg Nothing $ Basic t Nothing

inlineNamedArg :: NamedArgument ->  Space -> Argument
inlineNamedArg n t = Arg (Just n) $ Basic t Nothing

preDefinedArg :: CodeVarChunk -> Argument
preDefinedArg v = Arg Nothing $ Basic (v ^. typ) (Just v)

preDefinedNamedArg :: NamedArgument -> CodeVarChunk -> Argument
preDefinedNamedArg n v = Arg (Just n) $ Basic (v ^. typ) (Just v)

functionArg :: CodeFuncChunk -> [Parameter] -> Step -> Argument
functionArg f ps b = Arg Nothing (Fn f ps b)

customObjArg :: [Requires] -> Description -> CodeVarChunk -> CodeFuncChunk -> 
  ClassInfo -> Argument
customObjArg rs d o c ci = Arg Nothing (Class rs d o c ci)

recordArg :: Requires -> CodeFuncChunk -> CodeVarChunk -> [CodeVarChunk] -> 
  Argument
recordArg rq c o fs = Arg Nothing (Record (rq :| []) c o fs)

lockedParam :: CodeVarChunk -> Parameter
lockedParam = LockedParam . pcAuto

unnamedParam :: Space -> Parameter
unnamedParam = NameableParam

customClass :: [MethodInfo] -> ClassInfo
customClass = Regular

implementation :: String -> [MethodInfo] -> ClassInfo
implementation = Implements

constructorInfo :: CodeFuncChunk -> [Parameter] -> [Step] -> MethodInfo
constructorInfo c = CI ("Constructor for " ++ codeName c ++ " objects")

methodInfo :: CodeFuncChunk -> Description -> [Parameter] -> Description -> 
  [Step] -> MethodInfo
methodInfo _ _ _ _ [] = error "methodInfo should be called with a non-empty list of Step"
methodInfo m d ps rd ss = MI m d ps (Just rd) (fromList ss)

methodInfoNoReturn :: CodeFuncChunk -> Description -> [Parameter] -> [Step] -> 
  MethodInfo
methodInfoNoReturn _ _ _ [] = error "methodInfoNoReturn should be called with a non-empty list of Step"
methodInfoNoReturn m d ps ss = MI m d ps Nothing (fromList ss)

appendCurrSol :: Expr -> Step
appendCurrSol curr = statementStep (\cdchs es -> case (cdchs, es) of
    ([s], []) -> appendCurrSolFS curr s
    (_,_) -> error "Fill for appendCurrSol should provide one CodeChunk and no Exprs")
  
populateSolList :: CodeVarChunk -> CodeVarChunk -> CodeVarChunk -> [Step]
populateSolList arr el fld = [statementStep (\cdchs es -> case (cdchs, es) of
    ([s], []) -> FAsg s (Matrix [[]])
    (_,_) -> error popErr),
  statementStep (\cdchs es -> case (cdchs, es) of
    ([s], []) -> FForEach el (sy arr) [appendCurrSolFS (field el fld) s]
    (_,_) -> error popErr)]
  where popErr = "Fill for populateSolList should provide one CodeChunk and no Exprs"

assignArrayIndex :: Step
assignArrayIndex = statementStep (\cdchs es -> case (cdchs, es) of
  ([a],vs) -> FMulti $ zipWith (FAsgIndex a) [0..] vs
  (_,_) -> error "Fill for assignArrayIndex should provide one CodeChunk")

assignSolFromObj :: CodeVarChunk -> Step
assignSolFromObj o = statementStep (\cdchs es -> case (cdchs, es) of
  ([s],[]) -> FAsg s (field o s)
  (_,_) -> error "Fill for assignSolFromObj should provide one CodeChunk and no Exprs")

initSolListFromArray :: CodeVarChunk -> Step
initSolListFromArray a = statementStep (\cdchs es -> case (cdchs, es) of
  ([s],[]) -> FAsg s (Matrix [[idx (sy a) (int 0)]])
  (_,_) -> error "Fill for initSolListFromArray should provide one CodeChunk and no Exprs")

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

returnExprList :: Step
returnExprList = statementStep (\cdchs es -> case (cdchs, es) of
  ([], _) -> FRet $ Matrix [es]
  (_,_) -> error "Fill for returnExprList should provide no CodeChunks")

appendCurrSolFS :: Expr -> CodeVarChunk -> FuncStmt
appendCurrSolFS cs s = FAppend (sy s) (idx cs (int 0))

fixedReturn :: Expr -> Step
fixedReturn = lockedStatement . FRet

statementStep :: ([CodeVarChunk] -> [Expr] -> FuncStmt) -> Step
statementStep = Statement

lockedStatement :: FuncStmt -> Step
lockedStatement s = Statement (\_ _ -> s)
