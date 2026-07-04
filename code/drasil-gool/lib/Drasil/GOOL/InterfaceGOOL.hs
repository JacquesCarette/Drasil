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
  Label, Library, MSBody, MSBlock, VSFunction, SVariable, SValue, NamedArgs,
  MSParameter, SMethod, MixedCtorCall, PosCall, PosCtorCall, InOutCall,
  InOutFunc, DocInOutFunc,
  -- Typeclasses
  SharedProg, BodySym(body), TypeSym(..), FunctionSym, MethodSym,
  VariableSym(var), ValueSym(valueType), VariableValue(valueOf),
  ValueExpression, List(listSize, listAdd), listOf, StatementSym(..),
  DeclStatement(listDecDef), FuncAppStatement, VisibilitySym(..), convType)
import Drasil.Shared.CodeType (CodeType(..), ClassName)
import Drasil.Shared.Helpers (onStateValue)
import Drasil.Shared.State (GS, FS, CS, MS, VS)
import Drasil.Shared.AST (ScopeData)

class (SharedProg r tp vis smt, ProgramSym r tp vis smt, OOVariableValue r tp,
  OODeclStatement r tp smt, OOFuncAppStatement r tp smt, OOValueExpression r tp,
  InternalValueExp r tp, GetSet r tp, ObserverPattern r tp smt,
  StrategyPattern r tp smt
  ) => OOProg r tp vis smt

type GSProgram a = GS (a (Program a))

class (FileSym r tp vis smt) => ProgramSym r tp vis smt where
  type Program r
  prog :: Label -> Label -> [SFile r] -> GSProgram r

type SFile a = FS (a (File a))

class (ModuleSym r tp vis smt) => FileSym r tp vis smt where
  type File r
  fileDoc :: FSModule r -> SFile r

  -- Module description, watermark, list of author names, date as a String, file to comment
  docMod :: String -> String -> [String] -> String -> SFile r -> SFile r

type FSModule a = FS (a (Module a))

class (ClassSym r tp vis smt) => ModuleSym r tp vis smt where
  type Module r
  -- Module name, import names, module functions, module classes
  buildModule :: Label -> [Label] -> [SMethod r] -> [SClass r] -> FSModule r

type SClass a = CS (a (Class a))

class (OOMethodSym r tp vis smt, StateVarSym r tp vis) => ClassSym r tp vis smt where
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

type Initializers r tp = [(SVariable r, SValue r)]

class (MethodSym r tp vis smt, AttachmentSym r) => OOMethodSym r tp vis smt where
  method      :: Label -> r vis -> r (Attachment r) -> VS (r tp) ->
    [MSParameter r] -> MSBody r -> SMethod r
  getMethod   :: SVariable r -> SMethod r
  setMethod   :: SVariable r -> SMethod r
  constructor :: [MSParameter r] -> Initializers r tp -> MSBody r -> SMethod r

  -- inOutMethod and docInOutMethod both need the Attachment parameter
  inOutMethod :: Label -> r vis -> r (Attachment r) -> InOutFunc r
  docInOutMethod :: Label -> r vis -> r (Attachment r) -> DocInOutFunc r

privMethod :: (OOMethodSym r tp vis smt) => Label -> VS (r tp) ->
  [MSParameter r] -> MSBody r -> SMethod r
privMethod n = method n private instanceLevel

pubMethod :: (OOMethodSym r tp vis smt) => Label -> VS (r tp) ->
  [MSParameter r] -> MSBody r -> SMethod r
pubMethod n = method n public instanceLevel

initializer :: (OOMethodSym r tp vis smt) => [MSParameter r] ->
  Initializers r tp -> SMethod r
initializer ps is = constructor ps is (body [])

nonInitConstructor :: (OOMethodSym r tp vis smt) => [MSParameter r] ->
  MSBody r -> SMethod r
nonInitConstructor ps = constructor ps []

type CSStateVar a = CS (a (StateVar a))

class (VisibilitySym r vis, AttachmentSym r, VariableSym r tp) => StateVarSym r tp vis where
  type StateVar r
  stateVar :: r vis -> r (Attachment r) -> SVariable r -> CSStateVar r
  stateVarDef :: r vis -> r (Attachment r) -> SVariable r -> SValue r -> CSStateVar r
  constVar :: r vis ->  SVariable r -> SValue r -> CSStateVar r

privDVar :: (StateVarSym r tp vis) => SVariable r -> CSStateVar r
privDVar = stateVar private instanceLevel

pubDVar :: (StateVarSym r tp vis) => SVariable r -> CSStateVar r
pubDVar = stateVar public instanceLevel

pubSVar :: (StateVarSym r tp vis) => SVariable r -> CSStateVar r
pubSVar = stateVar public classLevel

-- | Used to differentiate whether a member is attached to the class or the instance
class AttachmentSym r where
  type Attachment r
  classLevel  :: r (Attachment r)
  instanceLevel :: r (Attachment r)

class (TypeSym r tp) => OOTypeSym r tp where
  obj :: ClassName -> VS (r tp)

class (ValueSym r tp, OOTypeSym r tp) => OOValueSym r tp

class (VariableSym r tp, OOTypeSym r tp) => OOVariableSym r tp where
  -- | A class-level variable, separate from its class (i.e. `v`, not `C.v`)
  classVar          :: Label -> VS (r tp) -> SVariable r
  -- | A class-level constant, separate from its class (i.e. `v`, not `C.v`)
  classConst        :: Label -> VS (r tp) -> SVariable r
  -- | Given a class `C` and a class-level variable `v`, creates `C.v`
  classVarAccess    :: VS (r tp) -> SVariable r -> SVariable r
  -- | Given a class `C` from an external module and a class-level variable `v`,
  -- performs any necessary imports and creates `C.v`
  extClassVarAccess :: VS (r tp) -> SVariable r -> SVariable r
  -- | Given an instance `i` and an instance-level variable `v`, creates `i.v`
  instanceVarAccess :: SValue r -> SVariable r -> SVariable r

($->) :: (OOVariableSym r tp) => SValue r -> SVariable r -> SVariable r
infixl 9 $->
($->) = instanceVarAccess

class (OOVariableSym r tp) => SelfSym r tp where
  -- | `self` keyword
  self              :: SVariable r

-- | Given a variable `v`, creates `self.v`
instanceVarSelf   :: (SelfSym r tp, VariableValue r tp) => SVariable r -> SVariable r
instanceVarSelf = instanceVarAccess (valueOf self)

class (VariableValue r tp, OOVariableSym r tp, SelfSym r tp) => OOVariableValue r tp

-- for values that can include expressions
class (ValueExpression r tp, OOVariableSym r tp, OOValueSym r tp) => OOValueExpression r tp where
  newObjMixedArgs         ::            MixedCtorCall r tp
  extNewObjMixedArgs      :: Library -> MixedCtorCall r tp
  libNewObjMixedArgs      :: Library -> MixedCtorCall r tp

selfMethodCall   :: (InternalValueExp r tp, VariableValue r tp, SelfSym r tp) => PosCall r tp
selfMethodCall n t = objMethodCall t (valueOf self) n

newObj           :: (OOValueExpression r tp) =>            PosCtorCall r tp
newObj t vs = newObjMixedArgs t vs []

extNewObj        :: (OOValueExpression r tp) => Library -> PosCtorCall r tp
extNewObj l t vs = extNewObjMixedArgs l t vs []

libNewObj        :: (OOValueExpression r tp) => Library -> PosCtorCall r tp
libNewObj l t vs = libNewObjMixedArgs l t vs []

class (ValueSym r tp) => InternalValueExp r tp where
  -- | Generic function for calling a method.
  --   Takes the function name, the return type, the object, a list of
  --   positional arguments, and a list of named arguments.
  objMethodCallMixedArgs' :: Label -> VS (r tp) -> SValue r -> [SValue r] ->
    NamedArgs r tp -> SValue r
  -- | Generic function for calling a class method.
  --   Takes the function name, the return type, the class type,
  --   a list of positional arguments, and a list of named arguments.
  classMethodCallMixedArgs' :: Label -> VS (r tp) -> VS (r tp) -> [SValue r] ->
    NamedArgs r tp -> SValue r

-- | Calling a method. t is the return type of the method, o is the
--   object, f is the method name, and ps is a list of positional arguments.
objMethodCall :: (InternalValueExp r tp) => VS (r tp) -> SValue r -> Label ->
  [SValue r] -> SValue r
objMethodCall t o f ps = objMethodCallMixedArgs' f t o ps []

-- | Calling a method with named arguments.
objMethodCallNamedArgs :: (InternalValueExp r tp) => VS (r tp) -> SValue r ->
  Label -> NamedArgs r tp -> SValue r
objMethodCallNamedArgs t o f = objMethodCallMixedArgs' f t o []

-- | Calling a method with a mix of positional and named arguments.
objMethodCallMixedArgs :: (InternalValueExp r tp) => VS (r tp) -> SValue r ->
  Label -> [SValue r] -> NamedArgs r tp -> SValue r
objMethodCallMixedArgs t o f = objMethodCallMixedArgs' f t o

-- | Calling a method with no parameters.
objMethodCallNoParams :: (InternalValueExp r tp) => VS (r tp) -> SValue r ->
  Label -> SValue r
objMethodCallNoParams t o f = objMethodCall t o f []

-- | Calling a class method. t is the return type of the method, c is the
--   class, f is the method name, and ps is a list of positional arguments.
classMethodCall :: (InternalValueExp r tp) => VS (r tp) -> VS (r tp) -> Label ->
  [SValue r] -> SValue r
classMethodCall t c f ps = classMethodCallMixedArgs' f t c ps []

-- | Calling a class method with named arguments.
classMethodCallNamedArgs :: (InternalValueExp r tp) => VS (r tp) -> VS (r tp) ->
  Label -> NamedArgs r tp -> SValue r
classMethodCallNamedArgs t c f = classMethodCallMixedArgs' f t c []

-- | Calling a class method with a mix of positional and named arguments.
classMethodCallMixedArgs :: (InternalValueExp r tp) => VS (r tp) -> VS (r tp) ->
  Label -> [SValue r] -> NamedArgs r tp -> SValue r
classMethodCallMixedArgs t c f = classMethodCallMixedArgs' f t c

-- | Calling a class method with no parameters.
classMethodCallNoParams :: (InternalValueExp r tp) => VS (r tp) -> VS (r tp) ->
  Label -> SValue r
classMethodCallNoParams t c f = classMethodCall t c f []

class (DeclStatement r tp smt, OOVariableSym r tp) => OODeclStatement r tp smt where
  objDecDef    :: SVariable r -> r ScopeData -> SValue r -> MS (r smt)
  -- Parameters: variable to store the object, scope of the variable,
  --             constructor arguments.  Object type is not needed,
  --             as it is inferred from the variable's type.
  objDecNew    :: SVariable r -> r ScopeData -> [SValue r] -> MS (r smt)
  extObjDecNew :: Library -> SVariable r -> r ScopeData -> [SValue r]
    -> MS (r smt)

objDecNewNoParams :: (OODeclStatement r tp smt) => SVariable r -> r ScopeData
  -> MS (r smt)
objDecNewNoParams v tp = objDecNew v tp []

extObjDecNewNoParams :: (OODeclStatement r tp smt) => Library -> SVariable r ->
  r ScopeData -> MS (r smt)
extObjDecNewNoParams l v tp = extObjDecNew l v tp []

class (FuncAppStatement r tp smt, OOVariableSym r tp) => OOFuncAppStatement r tp smt where
  selfInOutCall :: InOutCall r smt

class (StatementSym r tp smt, OOFunctionSym r tp) => ObserverPattern r tp smt where
  notifyObservers :: VSFunction r -> VS (r tp) -> MS (r smt)

observerListName :: Label
observerListName = "observerList"

initObserverList :: (DeclStatement r tp smt) => VS (r tp) -> [SValue r] ->
  r ScopeData -> MS (r smt)
initObserverList t os scp = listDecDef (var observerListName (listType t)) scp os

addObserver :: (OOVariableValue r tp, List r tp smt) => SValue r -> MS (r smt)
addObserver o = listAdd obsList lastelem o
  where obsList = valueOf $ listOf observerListName (onStateValue valueType o)
        lastelem = listSize obsList

class (BodySym r tp smt, VariableSym r tp) => StrategyPattern r tp smt where
  runStrategy :: Label -> [(Label, MSBody r)] -> Maybe (SValue r) ->
    Maybe (SVariable r) -> MSBlock r

class (FunctionSym r tp) => OOFunctionSym r tp where
  func :: Label -> VS (r tp) -> [SValue r] -> VSFunction r
  objAccess :: SValue r -> VSFunction r -> SValue r

($.) :: (OOFunctionSym r tp) => SValue r -> VSFunction r -> SValue r
infixl 9 $.
($.) = objAccess

selfAccess :: (OOVariableValue r tp, OOFunctionSym r tp) => VSFunction r -> SValue r
selfAccess = objAccess (valueOf self)

class (ValueSym r tp, VariableSym r tp) => GetSet r tp where
  get :: SValue r -> SVariable r -> SValue r
  set :: SValue r -> SVariable r -> SValue r -> SValue r

convTypeOO :: (OOTypeSym r tp) => CodeType -> VS (r tp)
convTypeOO (Object n) = obj n
convTypeOO (Reference t) = referenceType (convTypeOO t)
convTypeOO t = convType t
