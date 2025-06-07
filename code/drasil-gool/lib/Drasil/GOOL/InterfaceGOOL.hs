{-# LANGUAGE TypeFamilies #-}

module Drasil.GOOL.InterfaceGOOL (
  -- Types
  GSProgram, SFile, FSModule, SClass, CSStateVar, Initializers,
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
  Label, Library, MSBody, MSBlock, VSFunction, VSType, SVariable, SValue,
  MSStatement, NamedArgs, MSParameter, SMethod, MixedCall, MixedCtorCall,
  PosCall, PosCtorCall, InOutCall, InOutFunc, DocInOutFunc,
  -- Typeclasses
  SharedProg, BodySym(body), TypeSym(listType), FunctionSym, MethodSym,
  VariableSym(var), ScopeSym(..), ValueSym(valueType), VariableValue(valueOf),
  ValueExpression, List(listSize, listAdd), listOf, StatementSym(valStmt),
  DeclStatement(listDecDef), FuncAppStatement, VisibilitySym(..), convType)
import Drasil.Shared.CodeType (CodeType(..), ClassName)
import Drasil.Shared.Helpers (onStateValue)
import Drasil.Shared.State (GS, FS, CS)

class (SharedProg r, ProgramSym r, OOVariableValue r, OODeclStatement r,
  OOFuncAppStatement r, OOValueExpression r, InternalValueExp r, GetSet r,
  ObserverPattern r, StrategyPattern r
  ) => OOProg r

type GSProgram a = GS (a (Program a))

class (FileSym r) => ProgramSym r where
  type Program r
  prog :: Label -> Label -> [SFile r] -> GSProgram r

type SFile a = FS (a (File a))

class (ModuleSym r) => FileSym r where 
  type File r
  fileDoc :: FSModule r -> SFile r

  -- Module description, list of author names, date as a String, file to comment
  docMod :: String -> [String] -> String -> SFile r -> SFile r

type FSModule a = FS (a (Module a))

class (ClassSym r) => ModuleSym r where
  type Module r
  -- Module name, import names, module functions, module classes
  buildModule :: Label -> [Label] -> [SMethod r] -> [SClass r] -> FSModule r

type SClass a = CS (a (Class a))

class (OOMethodSym r, StateVarSym r) => ClassSym r where
  type Class r
  -- | Main external method for creating a class.
  --   Inputs: parent class, variables, constructor(s), methods
  buildClass :: Maybe Label -> [CSStateVar r] -> [SMethod r] -> 
    [SMethod r] -> SClass r
  -- | Creates an extra class.
  --   Inputs: class name, the rest are the same as buildClass.
  extraClass :: Label -> Maybe Label -> [CSStateVar r] -> [SMethod r] -> 
    [SMethod r] -> SClass r
  -- | Creates a class implementing interfaces.
  --   Inputs: class name, interface names, variables, constructor(s), methods
  implementingClass :: Label -> [Label] -> [CSStateVar r] -> [SMethod r] -> 
    [SMethod r] -> SClass r

  docClass :: String -> SClass r -> SClass r

type Initializers r = [(SVariable r, SValue r)]

class (MethodSym r, PermanenceSym r) => OOMethodSym r where
  method      :: Label -> r (Visibility r) -> r (Permanence r) -> VSType r -> 
    [MSParameter r] -> MSBody r -> SMethod r
  getMethod   :: SVariable r -> SMethod r
  setMethod   :: SVariable r -> SMethod r 
  constructor :: [MSParameter r] -> Initializers r -> MSBody r -> SMethod r

  -- inOutMethod and docInOutMethod both need the Permanence parameter
  inOutMethod :: Label -> r (Visibility r) -> r (Permanence r) -> InOutFunc r
  docInOutMethod :: Label -> r (Visibility r) -> r (Permanence r) -> DocInOutFunc r

privMethod :: (OOMethodSym r) => Label -> VSType r -> [MSParameter r] -> MSBody r 
  -> SMethod r
privMethod n = method n private dynamic

pubMethod :: (OOMethodSym r) => Label -> VSType r -> [MSParameter r] -> MSBody r 
  -> SMethod r
pubMethod n = method n public dynamic

initializer :: (OOMethodSym r) => [MSParameter r] -> Initializers r -> SMethod r
initializer ps is = constructor ps is (body [])

nonInitConstructor :: (OOMethodSym r) => [MSParameter r] -> MSBody r -> SMethod r
nonInitConstructor ps = constructor ps []

type CSStateVar a = CS (a (StateVar a))

class (VisibilitySym r, PermanenceSym r, VariableSym r) => StateVarSym r where
  type StateVar r
  stateVar :: r (Visibility r) -> r (Permanence r) -> SVariable r -> CSStateVar r
  stateVarDef :: r (Visibility r) -> r (Permanence r) -> SVariable r -> 
    SValue r -> CSStateVar r
  constVar :: r (Visibility r) ->  SVariable r -> SValue r -> CSStateVar r

privDVar :: (StateVarSym r) => SVariable r -> CSStateVar r
privDVar = stateVar private dynamic

pubDVar :: (StateVarSym r) => SVariable r -> CSStateVar r
pubDVar = stateVar public dynamic

pubSVar :: (StateVarSym r) => SVariable r -> CSStateVar r
pubSVar = stateVar public static

class PermanenceSym r where
  type Permanence r
  static  :: r (Permanence r)
  dynamic :: r (Permanence r)

class (TypeSym r) => OOTypeSym r where
  obj :: ClassName -> VSType r

class (ValueSym r, OOTypeSym r) => OOValueSym r

class (VariableSym r, OOTypeSym r) => OOVariableSym r where
  -- Bool: False for variable, True for constant.  Required by the Python renderer.
  staticVar'    :: Bool -> Label -> VSType r -> SVariable r
  self         :: SVariable r
  classVar     :: VSType r -> SVariable r -> SVariable r
  extClassVar  :: VSType r -> SVariable r -> SVariable r
  objVar       :: SVariable r -> SVariable r -> SVariable r
  objVarSelf   :: SVariable r -> SVariable r

staticVar :: (OOVariableSym r) => Label -> VSType r -> SVariable r
staticVar = staticVar' False

staticConst :: (OOVariableSym r) => Label -> VSType r -> SVariable r
staticConst = staticVar' True

($->) :: (OOVariableSym r) => SVariable r -> SVariable r -> SVariable r
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
  objMethodCallMixedArgs' :: Label -> VSType r -> SValue r -> [SValue r] -> 
    NamedArgs r -> SValue r

-- | Calling a method. t is the return type of the method, o is the
--   object, f is the method name, and ps is a list of positional arguments.
objMethodCall :: (InternalValueExp r) => VSType r -> SValue r -> Label -> 
  [SValue r] -> SValue r
objMethodCall t o f ps = objMethodCallMixedArgs' f t o ps []

-- | Calling a method with named arguments.
objMethodCallNamedArgs :: (InternalValueExp r) => VSType r -> SValue r -> Label 
  -> NamedArgs r -> SValue r
objMethodCallNamedArgs t o f = objMethodCallMixedArgs' f t o []

-- | Calling a method with a mix of positional and named arguments.
objMethodCallMixedArgs :: (InternalValueExp r) => VSType r -> SValue r -> Label 
  -> [SValue r] -> NamedArgs r -> SValue r
objMethodCallMixedArgs t o f = objMethodCallMixedArgs' f t o

-- | Calling a method with no parameters.
objMethodCallNoParams :: (InternalValueExp r) => VSType r -> SValue r -> Label 
  -> SValue r
objMethodCallNoParams t o f = objMethodCall t o f []

class (DeclStatement r, OOVariableSym r) => OODeclStatement r where
  objDecDef    :: SVariable r -> r (Scope r) -> SValue r -> MSStatement r
  -- Parameters: variable to store the object, scope of the variable,
  --             constructor arguments.  Object type is not needed,
  --             as it is inferred from the variable's type.
  objDecNew    :: SVariable r -> r (Scope r) -> [SValue r] -> MSStatement r
  extObjDecNew :: Library -> SVariable r -> r (Scope r) -> [SValue r]
    -> MSStatement r

objDecNewNoParams :: (OODeclStatement r) => SVariable r -> r (Scope r)
  -> MSStatement r
objDecNewNoParams v s = objDecNew v s []

extObjDecNewNoParams :: (OODeclStatement r) => Library -> SVariable r -> 
  r (Scope r) -> MSStatement r
extObjDecNewNoParams l v s = extObjDecNew l v s []

class (FuncAppStatement r, OOVariableSym r) => OOFuncAppStatement r where
  selfInOutCall :: InOutCall r

class (StatementSym r, OOFunctionSym r) => ObserverPattern r where
  notifyObservers :: VSFunction r -> VSType r -> MSStatement r

observerListName :: Label
observerListName = "observerList"

initObserverList :: (DeclStatement r) => VSType r -> [SValue r] -> r (Scope r)
  -> MSStatement r
initObserverList t os scp = listDecDef (var observerListName (listType t)) scp os

addObserver :: (StatementSym r, OOVariableValue r, List r) => SValue r
  -> MSStatement r
addObserver o = valStmt $ listAdd obsList lastelem o
  where obsList = valueOf $ listOf observerListName (onStateValue valueType o)
        lastelem = listSize obsList

class (BodySym r, VariableSym r) => StrategyPattern r where
  runStrategy :: Label -> [(Label, MSBody r)] -> Maybe (SValue r) ->
    Maybe (SVariable r) -> MSBlock r

class (FunctionSym r) => OOFunctionSym r where
  func :: Label -> VSType r -> [SValue r] -> VSFunction r
  objAccess :: SValue r -> VSFunction r -> SValue r

($.) :: (OOFunctionSym r) => SValue r -> VSFunction r -> SValue r
infixl 9 $.
($.) = objAccess

selfAccess :: (OOVariableValue r, OOFunctionSym r) => VSFunction r -> SValue r
selfAccess = objAccess (valueOf self)

class (ValueSym r, VariableSym r) => GetSet r where
  get :: SValue r -> SVariable r -> SValue r
  set :: SValue r -> SVariable r -> SValue r -> SValue r

convTypeOO :: (OOTypeSym r) => CodeType -> VSType r
convTypeOO (Object n) = obj n
convTypeOO t = convType t
