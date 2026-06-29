{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Drasil.GOOL.InterfaceGOOL (
  -- Types
  GSProgram, SFile, FSModule, SClass, CSStateVar, Initializers,
  -- Typeclasses
  OOProg, ProgramSym(..), FileSym(..), ModuleSym(..), ClassSym(..),
  OOTypeSym(..), OOVariableSym(..), ($->), SelfSym(..), instanceVarSelf,
  OOValueSym, OOVariableValue, OOValueExpression(..), selfMethodCall, newObj,
  extNewObj, libNewObj, OODeclStatement(..), objDecNewNoParams,
  extObjDecNewNoParams, OOFuncAppStatement(..), GetSet(..), InternalValueExp(..),
  objMethodCall, objMethodCallNamedArgs, objMethodCallMixedArgs,
  objMethodCallNoParams, classMethodCall, classMethodCallNamedArgs,
  classMethodCallMixedArgs, classMethodCallNoParams, OOMethodSym(..), privMethod,
  pubMethod, initializer, nonInitConstructor, StateVarSym(..), privDVar, pubDVar,
  pubSVar, AttachmentSym(..), OOFunctionSym(..), ($.), selfAccess,
  ObserverPattern(..), observerListName, initObserverList, addObserver,
  StrategyPattern(..), convTypeOO
  ) where

import Drasil.Shared.InterfaceCommon (
  -- Types
  Label, Library, MSBody, MSBlock, VSFunction, SVariable, SValue, MSStatement,
  NamedArgs, MSParameter, SMethod, MixedCtorCall, PosCall, PosCtorCall,
  InOutCall, InOutFunc, DocInOutFunc,
  -- Typeclasses
  SharedProg, BodySym(body), TypeSym(..), FunctionSym, MethodSym,
  VariableSym(var), ValueSym(valueType), VariableValue(valueOf),
  ValueExpression, List(listSize, listAdd), listOf, StatementSym,
  DeclStatement(listDecDef), FuncAppStatement, VisibilitySym(..), convType)
import Drasil.Shared.CodeType (CodeType(..), ClassName)
import Drasil.Shared.Helpers (onStateValue)
import Drasil.Shared.State (GS, FS, CS, VS)
import Drasil.Shared.AST (ScopeData)

class (SharedProg r s, ProgramSym r s, OOVariableValue r s, OODeclStatement r s,
  OOFuncAppStatement r s, OOValueExpression r s, InternalValueExp r s, GetSet r s,
  ObserverPattern r s, StrategyPattern r s
  ) => OOProg r s

type GSProgram a = GS (a (Program a))

class (FileSym r s) => ProgramSym r s where
  type Program r
  prog :: Label -> Label -> [SFile r] -> GSProgram r

type SFile a = FS (a (File a))

class (ModuleSym r s) => FileSym r s where
  type File r
  fileDoc :: FSModule r -> SFile r

  -- Module description, watermark, list of author names, date as a String, file to comment
  docMod :: String -> String -> [String] -> String -> SFile r -> SFile r

type FSModule a = FS (a (Module a))

class (ClassSym r s) => ModuleSym r s where
  type Module r
  -- Module name, import names, module functions, module classes
  buildModule :: Label -> [Label] -> [SMethod r] -> [SClass r] -> FSModule r

type SClass a = CS (a (Class a))

class (OOMethodSym r s, StateVarSym r s) => ClassSym r s where
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

type Initializers r s = [(SVariable r, SValue r)]

class (MethodSym r s, AttachmentSym r) => OOMethodSym r s where
  method      :: Label -> r (Visibility r) -> r (Attachment r) -> VS (r s) ->
    [MSParameter r] -> MSBody r -> SMethod r
  getMethod   :: SVariable r -> SMethod r
  setMethod   :: SVariable r -> SMethod r
  constructor :: [MSParameter r] -> Initializers r s -> MSBody r -> SMethod r

  -- inOutMethod and docInOutMethod both need the Attachment parameter
  inOutMethod :: Label -> r (Visibility r) -> r (Attachment r) -> InOutFunc r
  docInOutMethod :: Label -> r (Visibility r) -> r (Attachment r) -> DocInOutFunc r

privMethod :: (OOMethodSym r s) => Label -> VS (r s) -> [MSParameter r] -> MSBody r
  -> SMethod r
privMethod n = method n private instanceLevel

pubMethod :: (OOMethodSym r s) => Label -> VS (r s) -> [MSParameter r] -> MSBody r
  -> SMethod r
pubMethod n = method n public instanceLevel

initializer :: (OOMethodSym r s) => [MSParameter r] -> Initializers r s -> SMethod r
initializer ps is = constructor ps is (body [])

nonInitConstructor :: (OOMethodSym r s) => [MSParameter r] -> MSBody r -> SMethod r
nonInitConstructor ps = constructor ps []

type CSStateVar a = CS (a (StateVar a))

class (VisibilitySym r, AttachmentSym r, VariableSym r s) => StateVarSym r s where
  type StateVar r
  stateVar :: r (Visibility r) -> r (Attachment r) -> SVariable r -> CSStateVar r
  stateVarDef :: r (Visibility r) -> r (Attachment r) -> SVariable r ->
    SValue r -> CSStateVar r
  constVar :: r (Visibility r) ->  SVariable r -> SValue r -> CSStateVar r

privDVar :: (StateVarSym r s) => SVariable r -> CSStateVar r
privDVar = stateVar private instanceLevel

pubDVar :: (StateVarSym r s) => SVariable r -> CSStateVar r
pubDVar = stateVar public instanceLevel

pubSVar :: (StateVarSym r s) => SVariable r -> CSStateVar r
pubSVar = stateVar public classLevel

-- | Used to differentiate whether a member is attached to the class or the instance
class AttachmentSym r where
  type Attachment r
  classLevel  :: r (Attachment r)
  instanceLevel :: r (Attachment r)

class (TypeSym r s) => OOTypeSym r s where
  obj :: ClassName -> VS (r s)

class (ValueSym r s, OOTypeSym r s) => OOValueSym r s

class (VariableSym r s, OOTypeSym r s) => OOVariableSym r s where
  -- | A class-level variable, separate from its class (i.e. `v`, not `C.v`)
  classVar          :: Label -> VS (r s) -> SVariable r
  -- | A class-level constant, separate from its class (i.e. `v`, not `C.v`)
  classConst        :: Label -> VS (r s) -> SVariable r
  -- | Given a class `C` and a class-level variable `v`, creates `C.v`
  classVarAccess    :: VS (r s) -> SVariable r -> SVariable r
  -- | Given a class `C` from an external module and a class-level variable `v`,
  -- performs any necessary imports and creates `C.v`
  extClassVarAccess :: VS (r s) -> SVariable r -> SVariable r
  -- | Given an instance `i` and an instance-level variable `v`, creates `i.v`
  instanceVarAccess :: SValue r -> SVariable r -> SVariable r

($->) :: (OOVariableSym r s) => SValue r -> SVariable r -> SVariable r
infixl 9 $->
($->) = instanceVarAccess

class (OOVariableSym r s) => SelfSym r s where
  -- | `self` keyword
  self              :: SVariable r

-- | Given a variable `v`, creates `self.v`
instanceVarSelf   :: (SelfSym r s, VariableValue r s) => SVariable r -> SVariable r
instanceVarSelf = instanceVarAccess (valueOf self)

class (VariableValue r s, OOVariableSym r s, SelfSym r s) => OOVariableValue r s

-- for values that can include expressions
class (ValueExpression r s, OOVariableSym r s, OOValueSym r s) => OOValueExpression r s where
  newObjMixedArgs         ::            MixedCtorCall r s
  extNewObjMixedArgs      :: Library -> MixedCtorCall r s
  libNewObjMixedArgs      :: Library -> MixedCtorCall r s

selfMethodCall   :: (InternalValueExp r s, VariableValue r s, SelfSym r s) => PosCall r s
selfMethodCall n t = objMethodCall t (valueOf self) n

newObj           :: (OOValueExpression r s) =>            PosCtorCall r s
newObj t vs = newObjMixedArgs t vs []

extNewObj        :: (OOValueExpression r s) => Library -> PosCtorCall r s
extNewObj l t vs = extNewObjMixedArgs l t vs []

libNewObj        :: (OOValueExpression r s) => Library -> PosCtorCall r s
libNewObj l t vs = libNewObjMixedArgs l t vs []

class (ValueSym r s) => InternalValueExp r s where
  -- | Generic function for calling a method.
  --   Takes the function name, the return type, the object, a list of
  --   positional arguments, and a list of named arguments.
  objMethodCallMixedArgs' :: Label -> VS (r s) -> SValue r -> [SValue r] ->
    NamedArgs r s -> SValue r
  -- | Generic function for calling a class method.
  --   Takes the function name, the return type, the class type,
  --   a list of positional arguments, and a list of named arguments.
  classMethodCallMixedArgs' :: Label -> VS (r s) -> VS (r s) -> [SValue r] ->
    NamedArgs r s -> SValue r

-- | Calling a method. t is the return type of the method, o is the
--   object, f is the method name, and ps is a list of positional arguments.
objMethodCall :: (InternalValueExp r s) => VS (r s) -> SValue r -> Label ->
  [SValue r] -> SValue r
objMethodCall t o f ps = objMethodCallMixedArgs' f t o ps []

-- | Calling a method with named arguments.
objMethodCallNamedArgs :: (InternalValueExp r s) => VS (r s) -> SValue r -> Label
  -> NamedArgs r s -> SValue r
objMethodCallNamedArgs t o f = objMethodCallMixedArgs' f t o []

-- | Calling a method with a mix of positional and named arguments.
objMethodCallMixedArgs :: (InternalValueExp r s) => VS (r s) -> SValue r -> Label
  -> [SValue r] -> NamedArgs r s -> SValue r
objMethodCallMixedArgs t o f = objMethodCallMixedArgs' f t o

-- | Calling a method with no parameters.
objMethodCallNoParams :: (InternalValueExp r s) => VS (r s) -> SValue r -> Label
  -> SValue r
objMethodCallNoParams t o f = objMethodCall t o f []

-- | Calling a class method. t is the return type of the method, c is the
--   class, f is the method name, and ps is a list of positional arguments.
classMethodCall :: (InternalValueExp r s) => VS (r s) -> VS (r s) -> Label ->
  [SValue r] -> SValue r
classMethodCall t c f ps = classMethodCallMixedArgs' f t c ps []

-- | Calling a class method with named arguments.
classMethodCallNamedArgs :: (InternalValueExp r s) => VS (r s) -> VS (r s) -> Label
  -> NamedArgs r s -> SValue r
classMethodCallNamedArgs t c f = classMethodCallMixedArgs' f t c []

-- | Calling a class method with a mix of positional and named arguments.
classMethodCallMixedArgs :: (InternalValueExp r s) => VS (r s) -> VS (r s) -> Label
  -> [SValue r] -> NamedArgs r s -> SValue r
classMethodCallMixedArgs t c f = classMethodCallMixedArgs' f t c

-- | Calling a class method with no parameters.
classMethodCallNoParams :: (InternalValueExp r s) => VS (r s) -> VS (r s) -> Label
  -> SValue r
classMethodCallNoParams t c f = classMethodCall t c f []

class (DeclStatement r s, OOVariableSym r s) => OODeclStatement r s where
  objDecDef    :: SVariable r -> r ScopeData -> SValue r -> MSStatement r
  -- Parameters: variable to store the object, scope of the variable,
  --             constructor arguments.  Object type is not needed,
  --             as it is inferred from the variable's type.
  objDecNew    :: SVariable r -> r ScopeData -> [SValue r] -> MSStatement r
  extObjDecNew :: Library -> SVariable r -> r ScopeData -> [SValue r]
    -> MSStatement r

objDecNewNoParams :: (OODeclStatement r s) => SVariable r -> r ScopeData
  -> MSStatement r
objDecNewNoParams v s = objDecNew v s []

extObjDecNewNoParams :: (OODeclStatement r s) => Library -> SVariable r ->
  r ScopeData -> MSStatement r
extObjDecNewNoParams l v s = extObjDecNew l v s []

class (FuncAppStatement r s, OOVariableSym r s) => OOFuncAppStatement r s where
  selfInOutCall :: InOutCall r

class (StatementSym r s, OOFunctionSym r s) => ObserverPattern r s where
  notifyObservers :: VSFunction r -> VS (r s) -> MSStatement r

observerListName :: Label
observerListName = "observerList"

initObserverList :: (DeclStatement r s) => VS (r s) -> [SValue r] -> r ScopeData
  -> MSStatement r
initObserverList t os scp = listDecDef (var observerListName (listType t)) scp os

addObserver :: (StatementSym r s, OOVariableValue r s, List r s) => SValue r
  -> MSStatement r
addObserver o = listAdd obsList lastelem o
  where obsList = valueOf $ listOf observerListName (onStateValue valueType o)
        lastelem = listSize obsList

class (BodySym r s, VariableSym r s) => StrategyPattern r s where
  runStrategy :: Label -> [(Label, MSBody r)] -> Maybe (SValue r) ->
    Maybe (SVariable r) -> MSBlock r

class (FunctionSym r s) => OOFunctionSym r s where
  func :: Label -> VS (r s) -> [SValue r] -> VSFunction r
  objAccess :: SValue r -> VSFunction r -> SValue r

($.) :: (OOFunctionSym r s) => SValue r -> VSFunction r -> SValue r
infixl 9 $.
($.) = objAccess

selfAccess :: (OOVariableValue r s, OOFunctionSym r s) => VSFunction r -> SValue r
selfAccess = objAccess (valueOf self)

class (ValueSym r s, VariableSym r s) => GetSet r s where
  get :: SValue r -> SVariable r -> SValue r
  set :: SValue r -> SVariable r -> SValue r -> SValue r

convTypeOO :: (OOTypeSym r s) => CodeType -> VS (r s)
convTypeOO (Object n) = obj n
convTypeOO (Reference t) = referenceType (convTypeOO t)
convTypeOO t = convType t
