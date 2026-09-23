{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Drasil.GOOL.InterfaceGOOL (
  -- Types
  Program, GSProgram, Class, StateVar, CSStateVar, Initializers,
  -- Typeclasses
  OOProg, ProgramSym(..), FileSym(..), ModuleSym(..), ClassSym(..),
  OOTypeSym(..), OOVariableSym(..), ($->), SelfSym(..), instanceVarSelf,
  OOValueExpression(..), selfMethodCall, newObj, extNewObj, libNewObj,
  OODeclStatement(..), objDecNewNoParams, extObjDecNewNoParams,
  OOFuncAppStatement(..), GetSet(..), InternalValueExp(..), objMethodCall,
  objMethodCallNamedArgs, objMethodCallMixedArgs, objMethodCallNoParams,
  classMethodCall, classMethodCallNamedArgs, classMethodCallMixedArgs,
  classMethodCallNoParams, OOMethodSym(..), privMethod, pubMethod, initializer,
  nonInitConstructor, StateVarSym(..), privDVar, pubDVar, pubSVar,
  AttachmentSym(..), OOFunctionSym(..), ($.), selfAccess, ObserverPattern(..),
  observerListName, initObserverList, addObserver, StrategyPattern(..),
  convTypeOO
  ) where

import Drasil.Shared.InterfaceCommon (
  -- Types
  Label, Library, SVariable, Value, NamedArgs, MixedCtorCall, PosCall,
  PosCtorCall, InOutCall, InOutFunc, DocInOutFunc,
  -- Typeclasses
  BodySym(body), BlockSym, TypeSym(..), MethodSym(..), VariableSym(var),
  ValueSym(valueType), VariableValue(valueOf), ValueExpression, IndexTranslator,
  Array, List(listSize), ListStatement(listAdd), listOf, EmptyStatement,
  MultiStatement, ValueStatement, AssignStatement, DeclStatement(listDecDef),
  FuncAppStatement, VisibilitySym(..), Argument, BooleanExpression,
  CommandLineArgs, CommentStatement, Comparison, ControlStatement, PrintConsole,
  ReadConsole, FileHandling, PrintFile, ReadFile, Literal, MathConstant,
  NumericExpression, ParameterSym, Reference, Set, StringStatement, convType,
  UnRepr, ScopeSym, BinderSym, InternalList, TypeElim, VariableElim)

import Drasil.Shared.CodeType (CodeType(..), ClassName)
import Drasil.Shared.Helpers (onStateValue)
import Drasil.Shared.State (GS, FS, CS, MS, VS)
import Drasil.Shared.AST (ScopeData, ParamData, FuncData, ProgData)

import Text.PrettyPrint.HughesPJ (Doc)

-- | Wrapper typeclass that bundles everything essential
-- for generating an object-oriented program.
class (UnRepr r typ, Argument r, BodySym r bod block, BlockSym r block stmt,
  CommandLineArgs r, Literal r typ, MathConstant r, VariableValue r,
  VariableSym r typ, TypeSym r typ, OOTypeSym r typ, OOVariableSym r typ,
  SelfSym r, BooleanExpression r, Comparison r, NumericExpression r,
  ValueSym r typ, InternalValueExp r typ, ValueExpression r typ,
  OOValueExpression r typ, IndexTranslator r, Array r, List r,
  ListStatement r stmt, Reference r, Set r, OOFunctionSym r typ, ParameterSym r,
  ScopeSym r, BinderSym r typ, InternalList r block,
  MethodSym r vis typ mthd bod, OOMethodSym r vis typ mthd attch bod,
  AttachmentSym r attch, VisibilitySym r vis, StateVarSym r vis stvr attch,
  ClassSym r mthd stvr, TypeElim r typ, VariableElim r typ,
  EmptyStatement r stmt, MultiStatement r stmt, ValueStatement r stmt,
  CommentStatement r stmt, DeclStatement r stmt bod, OODeclStatement r stmt,
  AssignStatement r stmt, FuncAppStatement r stmt, OOFuncAppStatement r stmt,
  ControlStatement r stmt bod, StringStatement r stmt, PrintConsole r stmt,
  ReadConsole r stmt, FileHandling r stmt, PrintFile r stmt, ReadFile r stmt,
  ModuleSym r mod mthd, FileSym r file mod, ProgramSym r prg file
  ) => OOProg r vis typ stmt mthd stvr attch prg file mod bod block

type Program = ProgData
type GSProgram a prg = GS (a prg)

-- | Class for representing a program.
-- Usually 'ProgData' is used for the representation.
class ProgramSym r prg file | r -> prg file where
  -- | Given program name, program purpose, and list of files,
  -- Generates a representation of a program.
  prog :: Label -> Label -> [FS (r file)] -> GSProgram r prg

-- | Class for representing a file.
class FileSym r file mod | r -> file mod where
  -- | Given a module, generates a representation of a file.
  -- (Implicit assumption: exactly one module per file)
  fileDoc :: FS (r mod) -> FS (r file)

  -- | Given module description, watermark, list of author names,
  -- date as a String, and file to comment, creates a __documented module__
  -- (i.e. module with a header comment)
  docMod :: String -> String -> [String] -> String -> FS (r file) -> FS (r file)

-- | Class for representing a module.
class ModuleSym r mod mthd | r -> mod mthd where
  -- | Given module name, list of import names, list of module functions,
  -- and list of module classes, generates a representation of a module.
  buildModule :: Label -> [Label] -> [MS (r mthd)] -> [CS (r Class)] -> FS (r mod)

type Class = Doc

-- | Class for representing an OO class.
class ClassSym r mthd stvr | r -> mthd stvr where
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

type Initializers r = [(SVariable r, VS (r Value))]

class OOMethodSym r vis typ mthd attch bod | r -> vis typ mthd attch bod where
  method      :: Label -> r vis -> r attch -> VS (r typ) ->
    [MS (r ParamData)] -> MS (r bod) -> MS (r mthd)
  getMethod   :: SVariable r -> MS (r mthd)
  setMethod   :: SVariable r -> MS (r mthd)
  constructor :: [MS (r ParamData)] -> Initializers r -> MS (r bod) -> MS (r mthd)

  -- inOutMethod and docInOutMethod both need AttachmentSym
  inOutMethod :: Label -> r vis -> r attch -> InOutFunc r mthd bod
  docInOutMethod :: Label -> r vis -> r attch -> DocInOutFunc r mthd bod

privMethod
  ::
    ( OOMethodSym r vis typ mthd attch bod
    , AttachmentSym r attch
    , VisibilitySym r vis
    )
  => Label -> VS (r typ) -> [MS (r ParamData)] -> MS (r bod) -> MS (r mthd)
privMethod n = method n private instanceLevel

pubMethod
  ::
    ( OOMethodSym r vis typ mthd attch bod
    , AttachmentSym r attch
    , VisibilitySym r vis
    )
  => Label -> VS (r typ) -> [MS (r ParamData)] -> MS (r bod) -> MS (r mthd)
pubMethod n = method n public instanceLevel

initializer
  :: (OOMethodSym r vis typ mthd attch bod, BodySym r bod block)
  => [MS (r ParamData)] -> Initializers r -> MS (r mthd)
initializer ps is = constructor ps is (body [])

nonInitConstructor
  :: (OOMethodSym r vis typ mthd attch bod)
  => [MS (r ParamData)] -> MS (r bod) -> MS (r mthd)
nonInitConstructor ps = constructor ps []

type StateVar = Doc
type CSStateVar r stvr = CS (r stvr)

-- | Class for representing class variables, both instance- and class-level.
-- Used when creating a class, to hold extra information about `Attachment`
-- and `Visibility`.
-- Usually 'Doc' is used for the representation.
class StateVarSym r vis stvr attch | r -> vis stvr attch where
  -- | Given a visibility, attachment, and variable, represent the declaration
  -- of a state variable with no initial value.
  stateVar :: r vis -> r attch -> SVariable r -> CSStateVar r stvr
  -- | Given a visibility, attachment, variable, and initial value,
  -- represent the declaration of a state variable with the given initial value.
  stateVarDef :: r vis -> r attch -> SVariable r -> VS (r Value) -> CSStateVar r stvr
  -- | Given a visibility, variable, and value, represent the declaration of
  -- a state constant with the given value.
  constVar :: r vis ->  SVariable r -> VS (r Value) -> CSStateVar r stvr

privDVar
  :: (AttachmentSym r attch, VisibilitySym r vis, StateVarSym r vis stvr attch)
  => SVariable r -> CSStateVar r stvr
privDVar = stateVar private instanceLevel

pubDVar
  :: (AttachmentSym r attch, VisibilitySym r vis, StateVarSym r vis stvr attch)
  => SVariable r -> CSStateVar r stvr
pubDVar = stateVar public instanceLevel

pubSVar
  :: (AttachmentSym r attch, VisibilitySym r vis, StateVarSym r vis stvr attch)
  => SVariable r -> CSStateVar r stvr
pubSVar = stateVar public classLevel

-- | Used to differentiate whether a member is attached to the class or the instance
class AttachmentSym r attch | r -> attch where
  classLevel  :: r attch
  instanceLevel :: r attch

class OOTypeSym r typ | r -> typ where
  obj :: ClassName -> VS (r typ)

class OOVariableSym r typ | r -> typ where
  -- | A class-level variable, separate from its class (i.e. `v`, not `C.v`)
  classVar          :: Label -> VS (r typ) -> SVariable r
  -- | A class-level constant, separate from its class (i.e. `v`, not `C.v`)
  classConst        :: Label -> VS (r typ) -> SVariable r
  -- | Given a class `C` and a class-level variable `v`, creates `C.v`
  classVarAccess    :: VS (r typ) -> SVariable r -> SVariable r
  -- | Given a class `C` from an external module and a class-level variable `v`,
  -- performs any necessary imports and creates `C.v`
  extClassVarAccess :: VS (r typ) -> SVariable r -> SVariable r
  -- | Given an instance `i` and an instance-level variable `v`, creates `i.v`
  instanceVarAccess :: VS (r Value) -> SVariable r -> SVariable r

($->) :: (OOVariableSym r typ) => VS (r Value) -> SVariable r -> SVariable r
infixl 9 $->
($->) = instanceVarAccess

class SelfSym r where
  -- | `self` keyword
  self              :: SVariable r

-- | Given a variable `v`, creates `self.v`
instanceVarSelf
  :: (OOVariableSym r typ, SelfSym r, VariableValue r)
  => SVariable r -> SVariable r
instanceVarSelf = instanceVarAccess (valueOf self)

-- for values that can include expressions
class OOValueExpression r typ | r -> typ where
  newObjMixedArgs         ::            MixedCtorCall r typ
  extNewObjMixedArgs      :: Library -> MixedCtorCall r typ
  libNewObjMixedArgs      :: Library -> MixedCtorCall r typ

selfMethodCall
  :: (InternalValueExp r typ, VariableValue r, SelfSym r)
  => PosCall r typ
selfMethodCall n t = objMethodCall t (valueOf self) n

newObj           :: (OOValueExpression r typ) =>            PosCtorCall r typ
newObj t vs = newObjMixedArgs t vs []

extNewObj        :: (OOValueExpression r typ) => Library -> PosCtorCall r typ
extNewObj l t vs = extNewObjMixedArgs l t vs []

libNewObj        :: (OOValueExpression r typ) => Library -> PosCtorCall r typ
libNewObj l t vs = libNewObjMixedArgs l t vs []

-- TODO [Brandon Bosman, 07/22/2026]: Give this a better name
-- | A class for representing method calls, both instance- and class-level
class InternalValueExp r typ | r -> typ where
  -- TODO [Brandon Bosman, 07/22/2026]: rename this to `instanceMethodCallMixedArgs'`
  -- | Generic function for calling a method.
  --   Takes the function name, the return type, the object, a list of
  --   positional arguments, and a list of named arguments.
  objMethodCallMixedArgs' :: Label -> VS (r typ) -> VS (r Value) -> [VS (r Value)] ->
    NamedArgs r -> VS (r Value)
  -- | Generic function for calling a class method.
  --   Takes the function name, the return type, the class type,
  --   a list of positional arguments, and a list of named arguments.
  classMethodCallMixedArgs' :: Label -> VS (r typ) -> VS (r typ) -> [VS (r Value)] ->
    NamedArgs r -> VS (r Value)

-- | Calling a method. t is the return type of the method, o is the
--   object, f is the method name, and ps is a list of positional arguments.
objMethodCall
  :: (InternalValueExp r typ)
  => VS (r typ) -> VS (r Value) -> Label -> [VS (r Value)] -> VS (r Value)
objMethodCall t o f ps = objMethodCallMixedArgs' f t o ps []

-- | Calling a method with named arguments.
objMethodCallNamedArgs
  :: (InternalValueExp r typ)
  => VS (r typ) -> VS (r Value) -> Label -> NamedArgs r -> VS (r Value)
objMethodCallNamedArgs t o f = objMethodCallMixedArgs' f t o []

-- | Calling a method with a mix of positional and named arguments.
objMethodCallMixedArgs
  :: (InternalValueExp r typ)
  => VS (r typ) -> VS (r Value) -> Label -> [VS (r Value)] -> NamedArgs r -> VS (r Value)
objMethodCallMixedArgs t o f = objMethodCallMixedArgs' f t o

-- | Calling a method with no parameters.
objMethodCallNoParams
  :: (InternalValueExp r typ)
  => VS (r typ) -> VS (r Value) -> Label -> VS (r Value)
objMethodCallNoParams t o f = objMethodCall t o f []

-- | Calling a class method. t is the return type of the method, c is the
--   class, f is the method name, and ps is a list of positional arguments.
classMethodCall
  :: (InternalValueExp r typ)
  => VS (r typ) -> VS (r typ) -> Label -> [VS (r Value)] -> VS (r Value)
classMethodCall t c f ps = classMethodCallMixedArgs' f t c ps []

-- | Calling a class method with named arguments.
classMethodCallNamedArgs
  :: (InternalValueExp r typ)
  => VS (r typ) -> VS (r typ) -> Label -> NamedArgs r -> VS (r Value)
classMethodCallNamedArgs t c f = classMethodCallMixedArgs' f t c []

-- | Calling a class method with a mix of positional and named arguments.
classMethodCallMixedArgs
  :: (InternalValueExp r typ)
  => VS (r typ) -> VS (r typ) -> Label -> [VS (r Value)] -> NamedArgs r -> VS (r Value)
classMethodCallMixedArgs t c f = classMethodCallMixedArgs' f t c

-- | Calling a class method with no parameters.
classMethodCallNoParams
  :: (InternalValueExp r typ)
  => VS (r typ) -> VS (r typ) -> Label -> VS (r Value)
classMethodCallNoParams t c f = classMethodCall t c f []

class OODeclStatement r stmt | r -> stmt where
  objDecDef    :: SVariable r -> r ScopeData -> VS (r Value) -> MS (r stmt)
  -- Parameters: variable to store the object, scope of the variable,
  --             constructor arguments.  Object type is not needed,
  --             as it is inferred from the variable's type.
  objDecNew    :: SVariable r -> r ScopeData -> [VS (r Value)] -> MS (r stmt)
  extObjDecNew :: Library -> SVariable r -> r ScopeData -> [VS (r Value)]
    -> MS (r stmt)

objDecNewNoParams :: (OODeclStatement r stmt) => SVariable r -> r ScopeData
  -> MS (r stmt)
objDecNewNoParams v tp = objDecNew v tp []

extObjDecNewNoParams :: (OODeclStatement r stmt) => Library -> SVariable r ->
  r ScopeData -> MS (r stmt)
extObjDecNewNoParams l v tp = extObjDecNew l v tp []

class OOFuncAppStatement r stmt | r -> stmt where
  selfInOutCall :: InOutCall r stmt

class ObserverPattern r typ stmt | r -> typ stmt where
  notifyObservers :: VS (r FuncData) -> VS (r typ) -> MS (r stmt)

observerListName :: Label
observerListName = "observerList"

initObserverList
  :: (TypeSym r typ, VariableSym r typ, DeclStatement r stmt bod)
  => VS (r typ) -> [VS (r Value)] -> r ScopeData -> MS (r stmt)
initObserverList t os scp = listDecDef (var observerListName (listType t)) scp os

addObserver
  ::
    ( TypeSym r typ
    , VariableSym r typ
    , VariableValue r
    , ValueSym r typ
    , List r
    , ListStatement r stmt
    )
  => VS (r Value) -> MS (r stmt)
addObserver o = listAdd obsList lastelem o
  where obsList = valueOf $ listOf observerListName (onStateValue valueType o)
        lastelem = listSize obsList

class StrategyPattern r bod block | r -> bod block where
  runStrategy :: Label -> [(Label, MS (r bod))] -> Maybe (VS (r Value)) ->
    Maybe (SVariable r) -> MS (r block)

class OOFunctionSym r typ | r -> typ where
  func :: Label -> VS (r typ) -> [VS (r Value)] -> VS (r FuncData)
  objAccess :: VS (r Value) -> VS (r FuncData) -> VS (r Value)

($.) :: (OOFunctionSym r typ) => VS (r Value) -> VS (r FuncData) -> VS (r Value)
infixl 9 $.
($.) = objAccess

selfAccess
  :: (VariableValue r, SelfSym r, OOFunctionSym r typ)
  => VS (r FuncData) -> VS (r Value)
selfAccess = objAccess (valueOf self)

class GetSet r where
  get :: VS (r Value) -> SVariable r -> VS (r Value)
  set :: VS (r Value) -> SVariable r -> VS (r Value) -> VS (r Value)

convTypeOO :: (TypeSym r typ, OOTypeSym r typ) => CodeType -> VS (r typ)
convTypeOO (Object n) = obj n
convTypeOO (Reference t) = referenceType (convTypeOO t)
convTypeOO t = convType t
