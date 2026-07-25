{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Drasil.GOOL.InterfaceGOOL (
  -- Types
  Program, GSProgram, File, Module, Class, StateVar, CSStateVar, Initializers,
  -- Typeclasses
  OOProg, OOStatement, ProgramSym(..), FileSym(..), ModuleSym(..), ClassSym(..),
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
  Label, Library, Body, Block, SVariable, SValue, NamedArgs, MixedCtorCall,
  PosCall, PosCtorCall, InOutCall, InOutFunc, DocInOutFunc,
  -- Typeclasses
  SharedProg, BodySym(body), TypeSym(..), FunctionSym, MethodSym(..),
  VariableSym(var), ValueSym(valueType), VariableValue(valueOf),
  ValueExpression, Array, List(listSize, listAdd), listOf, StatementSym(..),
  AssignStatement, DeclStatement(listDecDef), FuncAppStatement,
  VisibilitySym(..), Argument, BooleanExpression, CommandLineArgs,
  CommentStatement, Comparison, ControlStatement, PrintConsole, ReadConsole,
  FileHandling, PrintFile, ReadFile, Literal, MathConstant, NumericExpression,
  ParameterSym, Reference, Set, StringStatement, convType)

import Drasil.Shared.CodeType (CodeType(..), ClassName)
import Drasil.Shared.Helpers (onStateValue)
import Drasil.Shared.State (GS, FS, CS, MS, VS)
import Drasil.Shared.AST (ScopeData, TypeData, ParamData, FileData, FuncData,
  ModData, ProgData)

import Text.PrettyPrint.HughesPJ (Doc)

-- | Wrapper typeclass that bundles everything essential
-- for generating an object-oriented program.
class (SharedProg r vis stmt mthd, OOStatement r stmt,
  ProgramSym r vis stmt mthd stvr attch prg
  ) => OOProg r vis stmt mthd stvr attch prg

class (Array r, AssignStatement r stmt, Argument r, BooleanExpression r,
  CommandLineArgs r, CommentStatement r stmt, Comparison r,
  ControlStatement r stmt, DeclStatement r stmt, FuncAppStatement r stmt,
  PrintConsole r stmt, ReadConsole r stmt, FileHandling r stmt, PrintFile r stmt,
  ReadFile r stmt, List r stmt, Literal r, MathConstant r, NumericExpression r,
  ParameterSym r, Reference r, Set r, StringStatement r stmt, ValueExpression r,
  VariableValue r, GetSet r, InternalValueExp r, OOFuncAppStatement r stmt,
  OOVariableValue r, OODeclStatement r stmt, OOFuncAppStatement r stmt,
  OOFunctionSym r, OOValueExpression r
  ) => OOStatement r stmt

type Program = ProgData
type GSProgram a prg = GS (a prg)

-- | Class for representing a program.
-- Usually 'ProgData' is used for the representation.
class (FileSym r vis stmt mthd stvr attch) => ProgramSym r vis stmt mthd stvr attch prg | r -> prg where
  -- | Given program name, program purpose, and list of files,
  -- Generates a representation of a program.
  prog :: Label -> Label -> [FS (r File)] -> GSProgram r prg

type File = FileData

-- | Class for representing a file.
class (ModuleSym r vis stmt mthd stvr attch) => FileSym r vis stmt mthd stvr attch where
  -- | Given a module, generates a representation of a file.
  -- (Implicit assumption: exactly one module per file)
  fileDoc :: FS (r Module) -> FS (r File)

  -- | Given module description, watermark, list of author names,
  -- date as a String, and file to comment, creates a __documented module__
  -- (i.e. module with a header comment)
  docMod :: String -> String -> [String] -> String -> FS (r File) -> FS (r File)

type Module = ModData

-- | Class for representing a module.
class (ClassSym r vis stmt mthd stvr attch) => ModuleSym r vis stmt mthd stvr attch where
  -- | Given module name, list of import names, list of module functions,
  -- and list of module classes, generates a representation of a module.
  buildModule :: Label -> [Label] -> [MS (r mthd)] -> [CS (r Class)] -> FS (r Module)

type Class = Doc

-- | Class for representing an OO class.
class (OOMethodSym r vis stmt mthd attch, StateVarSym r vis stvr attch) => ClassSym r vis stmt mthd stvr attch where
  -- | Main external method for creating a class.
  -- Inputs: parent class, variables, constructor(s), methods
  buildClass :: Maybe Label -> [CSStateVar r stvr] -> [MS (r mthd)] ->
    [MS (r mthd)] -> CS (r Class)
  -- | Creates an extra class, i.e. with a different name than the module name.
  -- Inputs: class name, the rest are the same as buildClass.
  extraClass :: Label -> Maybe Label -> [CSStateVar r stvr] -> [MS (r mthd)] ->
    [MS (r mthd)] -> CS (r Class)
  -- | Creates a class implementing a list of interfaces.
  -- Inputs: class name, interface names, variables, constructor(s), methods
  implementingClass :: Label -> [Label] -> [CSStateVar r stvr] -> [MS (r mthd)] ->
    [MS (r mthd)] -> CS (r Class)

  docClass :: String -> CS (r Class) -> CS (r Class)

type Initializers r = [(SVariable r, SValue r)]

class (MethodSym r vis stmt mthd, AttachmentSym r attch) => OOMethodSym r vis stmt mthd attch where
  method      :: Label -> r vis -> r attch -> VS (r TypeData) ->
    [MS (r ParamData)] -> MS (r Body) -> MS (r mthd)
  getMethod   :: SVariable r -> MS (r mthd)
  setMethod   :: SVariable r -> MS (r mthd)
  constructor :: [MS (r ParamData)] -> Initializers r -> MS (r Body) -> MS (r mthd)

  -- inOutMethod and docInOutMethod both need AttachmentSym
  inOutMethod :: Label -> r vis -> r attch -> InOutFunc r mthd
  docInOutMethod :: Label -> r vis -> r attch -> DocInOutFunc r mthd

privMethod :: (OOMethodSym r vis stmt mthd attch) => Label -> VS (r TypeData) ->
  [MS (r ParamData)] -> MS (r Body) -> MS (r mthd)
privMethod n = method n private instanceLevel

pubMethod :: (OOMethodSym r vis stmt mthd attch) => Label -> VS (r TypeData) ->
  [MS (r ParamData)] -> MS (r Body) -> MS (r mthd)
pubMethod n = method n public instanceLevel

initializer :: (OOMethodSym r vis stmt mthd attch) => [MS (r ParamData)] ->
  Initializers r -> MS (r mthd)
initializer ps is = constructor ps is (body [])

nonInitConstructor :: (OOMethodSym r vis stmt mthd attch) => [MS (r ParamData)] ->
  MS (r Body) -> MS (r mthd)
nonInitConstructor ps = constructor ps []

type StateVar = Doc
type CSStateVar r stvr = CS (r stvr)

-- | Class for representing class variables, both instance- and class-level.
-- Used when creating a class, to hold extra information about `Attachment`
-- and `Visibility`.
-- Usually 'Doc' is used for the representation.
class (VisibilitySym r vis, AttachmentSym r attch, VariableSym r) => StateVarSym r vis stvr attch | r -> stvr where
  -- | Given a visibility, attachment, and variable, represent the declaration
  -- of a state variable with no initial value.
  stateVar :: r vis -> r attch -> SVariable r -> CSStateVar r stvr
  -- | Given a visibility, attachment, variable, and initial value,
  -- represent the declaration of a state variable with the given initial value.
  stateVarDef :: r vis -> r attch -> SVariable r -> SValue r -> CSStateVar r stvr
  -- | Given a visibility, variable, and value, represent the declaration of
  -- a state constant with the given value.
  constVar :: r vis ->  SVariable r -> SValue r -> CSStateVar r stvr

privDVar :: (StateVarSym r vis stvr attch) => SVariable r -> CSStateVar r stvr
privDVar = stateVar private instanceLevel

pubDVar :: (StateVarSym r vis stvr attch) => SVariable r -> CSStateVar r stvr
pubDVar = stateVar public instanceLevel

pubSVar :: (StateVarSym r vis stvr attch) => SVariable r -> CSStateVar r stvr
pubSVar = stateVar public classLevel

-- | Used to differentiate whether a member is attached to the class or the instance
class AttachmentSym r attch | r -> attch where
  classLevel  :: r attch
  instanceLevel :: r attch

class (TypeSym r) => OOTypeSym r where
  obj :: ClassName -> VS (r TypeData)

class (ValueSym r, OOTypeSym r) => OOValueSym r

class (VariableSym r, OOTypeSym r) => OOVariableSym r where
  -- | A class-level variable, separate from its class (i.e. `v`, not `C.v`)
  classVar          :: Label -> VS (r TypeData) -> SVariable r
  -- | A class-level constant, separate from its class (i.e. `v`, not `C.v`)
  classConst        :: Label -> VS (r TypeData) -> SVariable r
  -- | Given a class `C` and a class-level variable `v`, creates `C.v`
  classVarAccess    :: VS (r TypeData) -> SVariable r -> SVariable r
  -- | Given a class `C` from an external module and a class-level variable `v`,
  -- performs any necessary imports and creates `C.v`
  extClassVarAccess :: VS (r TypeData) -> SVariable r -> SVariable r
  -- | Given an instance `i` and an instance-level variable `v`, creates `i.v`
  instanceVarAccess :: SValue r -> SVariable r -> SVariable r

($->) :: (OOVariableSym r) => SValue r -> SVariable r -> SVariable r
infixl 9 $->
($->) = instanceVarAccess

class (OOVariableSym r) => SelfSym r where
  -- | `self` keyword
  self              :: SVariable r

-- | Given a variable `v`, creates `self.v`
instanceVarSelf   :: (SelfSym r, VariableValue r) => SVariable r -> SVariable r
instanceVarSelf = instanceVarAccess (valueOf self)

class (VariableValue r, OOVariableSym r, SelfSym r) => OOVariableValue r

-- for values that can include expressions
class (ValueExpression r, OOVariableSym r, OOValueSym r) => OOValueExpression r where
  newObjMixedArgs         ::            MixedCtorCall r
  extNewObjMixedArgs      :: Library -> MixedCtorCall r
  libNewObjMixedArgs      :: Library -> MixedCtorCall r

selfMethodCall   :: (InternalValueExp r, VariableValue r, SelfSym r) => PosCall r
selfMethodCall n t = objMethodCall t (valueOf self) n

newObj           :: (OOValueExpression r) =>            PosCtorCall r
newObj t vs = newObjMixedArgs t vs []

extNewObj        :: (OOValueExpression r) => Library -> PosCtorCall r
extNewObj l t vs = extNewObjMixedArgs l t vs []

libNewObj        :: (OOValueExpression r) => Library -> PosCtorCall r
libNewObj l t vs = libNewObjMixedArgs l t vs []

-- TODO [Brandon Bosman, 07/22/2026]: Give this a better name
-- | A class for representing method calls, both instance- and class-level
class (ValueSym r) => InternalValueExp r where
  -- TODO [Brandon Bosman, 07/22/2026]: rename this to `instanceMethodCallMixedArgs'`
  -- | Generic function for calling a method.
  --   Takes the function name, the return type, the object, a list of
  --   positional arguments, and a list of named arguments.
  objMethodCallMixedArgs' :: Label -> VS (r TypeData) -> SValue r -> [SValue r] ->
    NamedArgs r -> SValue r
  -- | Generic function for calling a class method.
  --   Takes the function name, the return type, the class type,
  --   a list of positional arguments, and a list of named arguments.
  classMethodCallMixedArgs' :: Label -> VS (r TypeData) -> VS (r TypeData) -> [SValue r] ->
    NamedArgs r -> SValue r

-- | Calling a method. t is the return type of the method, o is the
--   object, f is the method name, and ps is a list of positional arguments.
objMethodCall :: (InternalValueExp r) => VS (r TypeData) -> SValue r -> Label ->
  [SValue r] -> SValue r
objMethodCall t o f ps = objMethodCallMixedArgs' f t o ps []

-- | Calling a method with named arguments.
objMethodCallNamedArgs :: (InternalValueExp r) => VS (r TypeData) -> SValue r ->
  Label -> NamedArgs r -> SValue r
objMethodCallNamedArgs t o f = objMethodCallMixedArgs' f t o []

-- | Calling a method with a mix of positional and named arguments.
objMethodCallMixedArgs :: (InternalValueExp r) => VS (r TypeData) -> SValue r ->
  Label -> [SValue r] -> NamedArgs r -> SValue r
objMethodCallMixedArgs t o f = objMethodCallMixedArgs' f t o

-- | Calling a method with no parameters.
objMethodCallNoParams :: (InternalValueExp r) => VS (r TypeData) -> SValue r ->
  Label -> SValue r
objMethodCallNoParams t o f = objMethodCall t o f []

-- | Calling a class method. t is the return type of the method, c is the
--   class, f is the method name, and ps is a list of positional arguments.
classMethodCall :: (InternalValueExp r) => VS (r TypeData) -> VS (r TypeData) -> Label ->
  [SValue r] -> SValue r
classMethodCall t c f ps = classMethodCallMixedArgs' f t c ps []

-- | Calling a class method with named arguments.
classMethodCallNamedArgs :: (InternalValueExp r) => VS (r TypeData) -> VS (r TypeData) ->
  Label -> NamedArgs r -> SValue r
classMethodCallNamedArgs t c f = classMethodCallMixedArgs' f t c []

-- | Calling a class method with a mix of positional and named arguments.
classMethodCallMixedArgs :: (InternalValueExp r) => VS (r TypeData) -> VS (r TypeData) ->
  Label -> [SValue r] -> NamedArgs r -> SValue r
classMethodCallMixedArgs t c f = classMethodCallMixedArgs' f t c

-- | Calling a class method with no parameters.
classMethodCallNoParams :: (InternalValueExp r) => VS (r TypeData) -> VS (r TypeData) ->
  Label -> SValue r
classMethodCallNoParams t c f = classMethodCall t c f []

class (DeclStatement r stmt, OOVariableSym r) => OODeclStatement r stmt where
  objDecDef    :: SVariable r -> r ScopeData -> SValue r -> MS (r stmt)
  -- Parameters: variable to store the object, scope of the variable,
  --             constructor arguments.  Object type is not needed,
  --             as it is inferred from the variable's type.
  objDecNew    :: SVariable r -> r ScopeData -> [SValue r] -> MS (r stmt)
  extObjDecNew :: Library -> SVariable r -> r ScopeData -> [SValue r]
    -> MS (r stmt)

objDecNewNoParams :: (OODeclStatement r stmt) => SVariable r -> r ScopeData
  -> MS (r stmt)
objDecNewNoParams v tp = objDecNew v tp []

extObjDecNewNoParams :: (OODeclStatement r stmt) => Library -> SVariable r ->
  r ScopeData -> MS (r stmt)
extObjDecNewNoParams l v tp = extObjDecNew l v tp []

class (FuncAppStatement r stmt, OOVariableSym r) => OOFuncAppStatement r stmt where
  selfInOutCall :: InOutCall r stmt

class (StatementSym r stmt, OOFunctionSym r) => ObserverPattern r stmt where
  notifyObservers :: VS (r FuncData) -> VS (r TypeData) -> MS (r stmt)

observerListName :: Label
observerListName = "observerList"

initObserverList :: (DeclStatement r stmt) => VS (r TypeData) -> [SValue r] ->
  r ScopeData -> MS (r stmt)
initObserverList t os scp = listDecDef (var observerListName (listType t)) scp os

addObserver :: (OOVariableValue r, List r stmt) => SValue r -> MS (r stmt)
addObserver o = listAdd obsList lastelem o
  where obsList = valueOf $ listOf observerListName (onStateValue valueType o)
        lastelem = listSize obsList

class (BodySym r stmt, VariableSym r) => StrategyPattern r stmt where
  runStrategy :: Label -> [(Label, MS (r Body))] -> Maybe (SValue r) ->
    Maybe (SVariable r) -> MS (r Block)

class (FunctionSym r) => OOFunctionSym r where
  func :: Label -> VS (r TypeData) -> [SValue r] -> VS (r FuncData)
  objAccess :: SValue r -> VS (r FuncData) -> SValue r

($.) :: (OOFunctionSym r) => SValue r -> VS (r FuncData) -> SValue r
infixl 9 $.
($.) = objAccess

selfAccess :: (OOVariableValue r, OOFunctionSym r) => VS (r FuncData) -> SValue r
selfAccess = objAccess (valueOf self)

class (ValueSym r, VariableSym r) => GetSet r where
  get :: SValue r -> SVariable r -> SValue r
  set :: SValue r -> SVariable r -> SValue r -> SValue r

convTypeOO :: (OOTypeSym r) => CodeType -> VS (r TypeData)
convTypeOO (Object n) = obj n
convTypeOO (Reference t) = referenceType (convTypeOO t)
convTypeOO t = convType t
