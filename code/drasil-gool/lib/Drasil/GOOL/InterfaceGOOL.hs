{-# LANGUAGE TypeFamilyDependencies #-}

module Drasil.GOOL.InterfaceGOOL (
  -- Types
  Initializers,
  -- Typeclasses
  OOProg, ProgramSym(..), FileSym(..), ModuleSym(..), ClassSym(..),
  OOTypeSym(..), OOVariableSym(..), staticVar, staticConst, ($->), OOValueSym,
  OOVariableValue, OOValueExpression(..), selfFuncApp, newObj, extNewObj,
  libNewObj, OODeclStatement(..), objDecNewNoParams, extObjDecNewNoParams,
  OOFuncAppStatement(..), GetSet(..), InternalValueExp(..), objMethodCall,
  objMethodCallNamedArgs, objMethodCallMixedArgs, objMethodCallNoParams,
  OOMethodSym(..), privMethod, pubMethod, initializer, nonInitConstructor,
  StateVarSym(..), privDVar, pubDVar, pubSVar, PermanenceSym(..),
  OOFunctionSym(..), ($.), selfAccess, ObserverPattern(..), observerListName,
  initObserverList, addObserver, StrategyPattern(..), convTypeOO
  ) where

import Drasil.Shared.InterfaceCommon (
  -- Types
  Label, Library, NamedArgs, MixedCall, MixedCtorCall,
  PosCall, PosCtorCall, InOutCall, InOutFunc, DocInOutFunc,
  -- Typeclasses
  SharedProg, BodySym(Body, body), BlockSym(Block), TypeSym(Type, listType),
  FunctionSym(Function), MethodSym(Method), VariableSym(Variable, var),
  ScopeSym(..), ValueSym(Value, valueType), VariableValue(valueOf),
  ParameterSym(Parameter), ValueExpression, List(listSize, listAdd), listOf,
  StatementSym(Statement, valStmt), DeclStatement(listDecDef), FuncAppStatement,
  VisibilitySym(..), convType)
import Drasil.Shared.CodeType (CodeType(..), ClassName)

class (SharedProg r, ProgramSym r, OOVariableValue r, OODeclStatement r,
  OOFuncAppStatement r, OOValueExpression r, InternalValueExp r, GetSet r,
  ObserverPattern r, StrategyPattern r
  ) => OOProg r

class (FileSym r) => ProgramSym r where
  type Program r = t | t -> r
  prog :: Label -> Label -> [File r] -> Program r

class (ModuleSym r) => FileSym r where 
  type File r = t | t -> r
  fileDoc :: Module r -> File r

  -- Module description, list of author names, date as a String, file to comment
  docMod :: String -> [String] -> String -> File r -> File r

class (ClassSym r) => ModuleSym r where
  type Module r = t | t -> r
  -- Module name, import names, module functions, module classes
  buildModule :: Label -> [Label] -> [Method r] -> [Class r] -> Module r

class (OOMethodSym r, StateVarSym r) => ClassSym r where
  type Class r = t | t -> r
  -- | Main external method for creating a class.
  --   Inputs: parent class, variables, constructor(s), methods
  buildClass :: Maybe Label -> [StateVar r] -> [Method r] -> 
    [Method r] -> Class r
  -- | Creates an extra class.
  --   Inputs: class name, the rest are the same as buildClass.
  extraClass :: Label -> Maybe Label -> [StateVar r] -> [Method r] -> 
    [Method r] -> Class r
  -- | Creates a class implementing interfaces.
  --   Inputs: class name, interface names, variables, constructor(s), methods
  implementingClass :: Label -> [Label] -> [StateVar r] -> [Method r] -> 
    [Method r] -> Class r

  docClass :: String -> Class r -> Class r

type Initializers r = [(Variable r, Value r)]

class (MethodSym r, PermanenceSym r) => OOMethodSym r where
  method      :: Label -> Visibility r -> Permanence r -> Type r -> 
    [Parameter r] -> Body r -> Method r
  getMethod   :: Variable r -> Method r
  setMethod   :: Variable r -> Method r 
  constructor :: [Parameter r] -> Initializers r -> Body r -> Method r

  -- inOutMethod and docInOutMethod both need the Permanence parameter
  inOutMethod :: Label -> Visibility r -> Permanence r -> InOutFunc r
  docInOutMethod :: Label -> Visibility r -> Permanence r -> DocInOutFunc r

privMethod :: (OOMethodSym r) => Label -> Type r -> [Parameter r] -> Body r 
  -> Method r
privMethod n = method n private dynamic

pubMethod :: (OOMethodSym r) => Label -> Type r -> [Parameter r] -> Body r 
  -> Method r
pubMethod n = method n public dynamic

initializer :: (OOMethodSym r) => [Parameter r] -> Initializers r -> Method r
initializer ps is = constructor ps is (body [])

nonInitConstructor :: (OOMethodSym r) => [Parameter r] -> Body r -> Method r
nonInitConstructor ps = constructor ps []

class (VisibilitySym r, PermanenceSym r, VariableSym r) => StateVarSym r where
  type StateVar r = t | t -> r
  stateVar :: Visibility r -> Permanence r -> Variable r -> StateVar r
  stateVarDef :: Visibility r -> Permanence r -> Variable r -> 
    Value r -> StateVar r
  constVar :: Visibility r ->  Variable r -> Value r -> StateVar r

privDVar :: (StateVarSym r) => Variable r -> StateVar r
privDVar = stateVar private dynamic

pubDVar :: (StateVarSym r) => Variable r -> StateVar r
pubDVar = stateVar public dynamic

pubSVar :: (StateVarSym r) => Variable r -> StateVar r
pubSVar = stateVar public static

class PermanenceSym r where
  type Permanence r = t | t -> r
  static  :: Permanence r
  dynamic :: Permanence r

class (TypeSym r) => OOTypeSym r where
  obj :: ClassName -> Type r

class (ValueSym r, OOTypeSym r) => OOValueSym r

class (VariableSym r, OOTypeSym r) => OOVariableSym r where
  -- Bool: False for variable, True for constant.  Required by the Python renderer.
  staticVar'    :: Bool -> Label -> Type r -> Variable r
  self         :: Variable r
  classVar     :: Type r -> Variable r -> Variable r
  extClassVar  :: Type r -> Variable r -> Variable r
  objVar       :: Variable r -> Variable r -> Variable r
  objVarSelf   :: Variable r -> Variable r

staticVar :: (OOVariableSym r) => Label -> Type r -> Variable r
staticVar = staticVar' False

staticConst :: (OOVariableSym r) => Label -> Type r -> Variable r
staticConst = staticVar' True

($->) :: (OOVariableSym r) => Variable r -> Variable r -> Variable r
infixl 9 $->
($->) = objVar

class (VariableValue r, OOVariableSym r) => OOVariableValue r

-- for values that can include expressions
class (ValueExpression r, OOVariableSym r, OOValueSym r) => OOValueExpression r where
  selfFuncAppMixedArgs ::            MixedCall r
  newObjMixedArgs      ::            MixedCtorCall r
  extNewObjMixedArgs   :: Library -> MixedCtorCall r
  libNewObjMixedArgs   :: Library -> MixedCtorCall r

selfFuncApp      :: (OOValueExpression r) =>            PosCall r
selfFuncApp n t vs = selfFuncAppMixedArgs n t vs []

newObj           :: (OOValueExpression r) =>            PosCtorCall r
newObj t vs = newObjMixedArgs t vs []

extNewObj        :: (OOValueExpression r) => Library -> PosCtorCall r
extNewObj l t vs = extNewObjMixedArgs l t vs []

libNewObj        :: (OOValueExpression r) => Library -> PosCtorCall r
libNewObj l t vs = libNewObjMixedArgs l t vs []

class (ValueSym r) => InternalValueExp r where
  -- | Generic function for calling a method.
  --   Takes the function name, the return type, the object, a list of 
  --   positional arguments, and a list of named arguments.
  objMethodCallMixedArgs' :: Label -> Type r -> Value r -> [Value r] -> 
    NamedArgs r -> Value r

-- | Calling a method. t is the return type of the method, o is the
--   object, f is the method name, and ps is a list of positional arguments.
objMethodCall :: (InternalValueExp r) => Type r -> Value r -> Label -> 
  [Value r] -> Value r
objMethodCall t o f ps = objMethodCallMixedArgs' f t o ps []

-- | Calling a method with named arguments.
objMethodCallNamedArgs :: (InternalValueExp r) => Type r -> Value r -> Label 
  -> NamedArgs r -> Value r
objMethodCallNamedArgs t o f = objMethodCallMixedArgs' f t o []

-- | Calling a method with a mix of positional and named arguments.
objMethodCallMixedArgs :: (InternalValueExp r) => Type r -> Value r -> Label 
  -> [Value r] -> NamedArgs r -> Value r
objMethodCallMixedArgs t o f = objMethodCallMixedArgs' f t o

-- | Calling a method with no parameters.
objMethodCallNoParams :: (InternalValueExp r) => Type r -> Value r -> Label 
  -> Value r
objMethodCallNoParams t o f = objMethodCall t o f []

class (DeclStatement r, OOVariableSym r) => OODeclStatement r where
  objDecDef    :: Variable r -> Scope r -> Value r -> Statement r
  -- Parameters: variable to store the object, scope of the variable,
  --             constructor arguments.  Object type is not needed,
  --             as it is inferred from the variable's type.
  objDecNew    :: Variable r -> Scope r -> [Value r] -> Statement r
  extObjDecNew :: Library -> Variable r -> Scope r -> [Value r]
    -> Statement r

objDecNewNoParams :: (OODeclStatement r) => Variable r -> Scope r
  -> Statement r
objDecNewNoParams v s = objDecNew v s []

extObjDecNewNoParams :: (OODeclStatement r) => Library -> Variable r -> 
  Scope r -> Statement r
extObjDecNewNoParams l v s = extObjDecNew l v s []

class (FuncAppStatement r, OOVariableSym r) => OOFuncAppStatement r where
  selfInOutCall :: InOutCall r

class (StatementSym r, OOFunctionSym r) => ObserverPattern r where
  notifyObservers :: Function r -> Type r -> Statement r

observerListName :: Label
observerListName = "observerList"

initObserverList :: (DeclStatement r) => Type r -> [Value r] -> Scope r
  -> Statement r
initObserverList t os scp = listDecDef (var observerListName (listType t)) scp os

addObserver :: (StatementSym r, OOVariableValue r, List r) => Value r
  -> Statement r
addObserver o = valStmt $ listAdd obsList lastelem o
  where obsList = valueOf $ listOf observerListName (valueType o)
        lastelem = listSize obsList

class (BodySym r, VariableSym r) => StrategyPattern r where
  runStrategy :: Label -> [(Label, Body r)] -> Maybe (Value r) ->
    Maybe (Variable r) -> Block r

class (FunctionSym r) => OOFunctionSym r where
  func :: Label -> Type r -> [Value r] -> Function r
  objAccess :: Value r -> Function r -> Value r

($.) :: (OOFunctionSym r) => Value r -> Function r -> Value r
infixl 9 $.
($.) = objAccess

selfAccess :: (OOVariableValue r, OOFunctionSym r) => Function r -> Value r
selfAccess = objAccess (valueOf self)

class (ValueSym r, VariableSym r) => GetSet r where
  get :: Value r -> Variable r -> Value r
  set :: Value r -> Variable r -> Value r -> Value r

convTypeOO :: (OOTypeSym r) => CodeType -> Type r
convTypeOO (Object n) = obj n
convTypeOO t = convType t
