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
  Label, Library, var, NamedArgs, MixedCtorCall, PosCall, PosCtorCall,
  InOutCall, InOutFunc, DocInOutFunc,
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
import Drasil.Shared.AST (FuncData, ProgData)

import Text.PrettyPrint.HughesPJ (Doc)

-- | Wrapper typeclass that bundles everything essential
-- for generating an object-oriented program.
class (UnRepr r typ, Argument r val, BodySym r bod block, BlockSym r block stmt,
  CommandLineArgs r val, Literal r val typ, MathConstant r val,
  VariableValue r var val, VariableSym r var typ, TypeSym r typ, OOTypeSym r typ,
  OOVariableSym r var val typ, SelfSym r var, BooleanExpression r val,
  Comparison r val, NumericExpression r val, ValueSym r val typ,
  InternalValueExp r var val typ, ValueExpression r var val binder typ,
  OOValueExpression r var val typ, IndexTranslator r val, Array r var val,
  List r val, ListStatement r stmt val, Reference r val, Set r val,
  OOFunctionSym r val typ, ParameterSym r param var, ScopeSym r scope,
  BinderSym r binder typ, InternalList r block var val,
  MethodSym r mthd vis param bod var typ,
  OOMethodSym r mthd attch vis param bod var val typ, AttachmentSym r attch,
  VisibilitySym r vis, StateVarSym r stvr attch vis var val,
  ClassSym r cls stvr mthd, TypeElim r typ, VariableElim r var typ,
  EmptyStatement r stmt, MultiStatement r stmt, ValueStatement r stmt val,
  CommentStatement r stmt, DeclStatement r bod stmt var scope val,
  OODeclStatement r stmt var scope val, AssignStatement r stmt var val,
  FuncAppStatement r stmt var val, OOFuncAppStatement r stmt var val,
  ControlStatement r bod stmt var val, StringStatement r stmt var val,
  PrintConsole r stmt val, ReadConsole r stmt var, FileHandling r stmt var val,
  PrintFile r stmt val, ReadFile r stmt var val, ModuleSym r mod cls mthd,
  FileSym r file mod, ProgramSym r prg file
  ) => OOProg r prg file mod cls stvr mthd attch vis param bod block stmt var scope val binder typ

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
class ModuleSym r mod cls mthd | r -> mod cls mthd where
  -- | Given module name, list of import names, list of module functions,
  -- and list of module classes, generates a representation of a module.
  buildModule :: Label -> [Label] -> [MS (r mthd)] -> [CS (r cls)] -> FS (r mod)

type Class = Doc

-- | Class for representing an OO class.
class ClassSym r cls stvr mthd | r -> cls stvr mthd where
  -- | Main external method for creating a class.
  -- Inputs: parent class, variables, constructor(s), methods
  buildClass :: Maybe Label -> [CSStateVar r stvr] -> [MS (r mthd)] ->
    [MS (r mthd)] -> CS (r cls)
  -- | Creates an extra class, i.e. with a different name than the module name.
  -- Inputs: class name, the rest are the same as buildClass.
  extraClass :: Label -> Maybe Label -> [CSStateVar r stvr] -> [MS (r mthd)] ->
    [MS (r mthd)] -> CS (r cls)
  -- | Creates a class implementing a list of interfaces.
  -- Inputs: class name, interface names, variables, constructor(s), methods
  implementingClass :: Label -> [Label] -> [CSStateVar r stvr] -> [MS (r mthd)] ->
    [MS (r mthd)] -> CS (r cls)

  docClass :: String -> CS (r cls) -> CS (r cls)

type Initializers r var val = [(VS (r var), VS (r val))]

class OOMethodSym r mthd attch vis param bod var val typ | r -> mthd attch vis param bod var val typ where
  method      :: Label -> r vis -> r attch -> VS (r typ) ->
    [MS (r param)] -> MS (r bod) -> MS (r mthd)
  getMethod   :: VS (r var) -> MS (r mthd)
  setMethod   :: VS (r var) -> MS (r mthd)
  constructor :: [MS (r param)] -> Initializers r var val -> MS (r bod) -> MS (r mthd)

  -- inOutMethod and docInOutMethod both need AttachmentSym
  inOutMethod :: Label -> r vis -> r attch -> InOutFunc r mthd bod var
  docInOutMethod :: Label -> r vis -> r attch -> DocInOutFunc r mthd bod var

privMethod
  ::
    ( OOMethodSym r mthd attch vis param bod var val typ
    , AttachmentSym r attch
    , VisibilitySym r vis
    )
  => Label -> VS (r typ) -> [MS (r param)] -> MS (r bod) -> MS (r mthd)
privMethod n = method n private instanceLevel

pubMethod
  ::
    ( OOMethodSym r mthd attch vis param bod var val typ
    , AttachmentSym r attch
    , VisibilitySym r vis
    )
  => Label -> VS (r typ) -> [MS (r param)] -> MS (r bod) -> MS (r mthd)
pubMethod n = method n public instanceLevel

initializer
  :: (OOMethodSym r mthd attch vis param bod var val typ, BodySym r bod block)
  => [MS (r param)] -> Initializers r var val -> MS (r mthd)
initializer ps is = constructor ps is (body [])

nonInitConstructor
  :: (OOMethodSym r mthd attch vis param bod var val typ)
  => [MS (r param)] -> MS (r bod) -> MS (r mthd)
nonInitConstructor ps = constructor ps []

type StateVar = Doc
type CSStateVar r stvr = CS (r stvr)

-- | Class for representing class variables, both instance- and class-level.
-- Used when creating a class, to hold extra information about `Attachment`
-- and `Visibility`.
-- Usually 'Doc' is used for the representation.
class StateVarSym r stvr attch vis var val | r -> stvr attch vis var val where
  -- | Given a visibility, attachment, and variable, represent the declaration
  -- of a state variable with no initial value.
  stateVar :: r vis -> r attch -> VS (r var) -> CSStateVar r stvr
  -- | Given a visibility, attachment, variable, and initial value,
  -- represent the declaration of a state variable with the given initial value.
  stateVarDef :: r vis -> r attch -> VS (r var) -> VS (r val) -> CSStateVar r stvr
  -- | Given a visibility, variable, and value, represent the declaration of
  -- a state constant with the given value.
  constVar :: r vis ->  VS (r var) -> VS (r val) -> CSStateVar r stvr

privDVar
  ::
    ( AttachmentSym r attch
    , VisibilitySym r vis
    , StateVarSym r stvr attch vis var val
    )
  => VS (r var) -> CSStateVar r stvr
privDVar = stateVar private instanceLevel

pubDVar
  ::
    ( AttachmentSym r attch
    , VisibilitySym r vis
    , StateVarSym r stvr attch vis var val
    )
  => VS (r var) -> CSStateVar r stvr
pubDVar = stateVar public instanceLevel

pubSVar
  ::
    ( AttachmentSym r attch
    , VisibilitySym r vis
    , StateVarSym r stvr attch vis var val
    )
  => VS (r var) -> CSStateVar r stvr
pubSVar = stateVar public classLevel

-- | Used to differentiate whether a member is attached to the class or the instance
class AttachmentSym r attch | r -> attch where
  classLevel  :: r attch
  instanceLevel :: r attch

class OOTypeSym r typ | r -> typ where
  obj :: ClassName -> VS (r typ)

class OOVariableSym r var val typ | r -> var val typ where
  -- | A class-level variable, separate from its class (i.e. `v`, not `C.v`)
  classVar          :: Label -> VS (r typ) -> VS (r var)
  -- | A class-level constant, separate from its class (i.e. `v`, not `C.v`)
  classConst        :: Label -> VS (r typ) -> VS (r var)
  -- | Given a class `C` and a class-level variable `v`, creates `C.v`
  classVarAccess    :: VS (r typ) -> VS (r var) -> VS (r var)
  -- | Given a class `C` from an external module and a class-level variable `v`,
  -- performs any necessary imports and creates `C.v`
  extClassVarAccess :: VS (r typ) -> VS (r var) -> VS (r var)
  -- | Given an instance `i` and an instance-level variable `v`, creates `i.v`
  instanceVarAccess :: VS (r val) -> VS (r var) -> VS (r var)

($->)
  :: (OOVariableSym r var val typ)
  => VS (r val) -> VS (r var) -> VS (r var)
infixl 9 $->
($->) = instanceVarAccess

class SelfSym r var | r -> var where
  -- | `self` keyword
  self              :: VS (r var)

-- | Given a variable `v`, creates `self.v`
instanceVarSelf
  :: (OOVariableSym r var val typ, SelfSym r var, VariableValue r var val)
  => VS (r var) -> VS (r var)
instanceVarSelf = instanceVarAccess (valueOf self)

-- for values that can include expressions
class OOValueExpression r var val typ | r -> var val typ where
  newObjMixedArgs         ::            MixedCtorCall r var val typ
  extNewObjMixedArgs      :: Library -> MixedCtorCall r var val typ
  libNewObjMixedArgs      :: Library -> MixedCtorCall r var val typ

selfMethodCall
  :: (InternalValueExp r var val typ, VariableValue r var val, SelfSym r var)
  => PosCall r val typ
selfMethodCall n t = objMethodCall t (valueOf self) n

newObj
  :: (OOValueExpression r var val typ) => PosCtorCall r val typ
newObj t vs = newObjMixedArgs t vs []

extNewObj
  :: (OOValueExpression r var val typ) => Library -> PosCtorCall r val typ
extNewObj l t vs = extNewObjMixedArgs l t vs []

libNewObj
  :: (OOValueExpression r var val typ) => Library -> PosCtorCall r val typ
libNewObj l t vs = libNewObjMixedArgs l t vs []

-- TODO [Brandon Bosman, 07/22/2026]: Give this a better name
-- | A class for representing method calls, both instance- and class-level
class InternalValueExp r var val typ | r -> var val typ where
  -- TODO [Brandon Bosman, 07/22/2026]: rename this to `instanceMethodCallMixedArgs'`
  -- | Generic function for calling a method.
  --   Takes the function name, the return type, the object, a list of
  --   positional arguments, and a list of named arguments.
  objMethodCallMixedArgs'
    :: Label -> VS (r typ) -> VS (r val) -> [VS (r val)] -> NamedArgs r var val -> VS (r val)
  -- | Generic function for calling a class method.
  --   Takes the function name, the return type, the class type,
  --   a list of positional arguments, and a list of named arguments.
  classMethodCallMixedArgs'
    :: Label -> VS (r typ) -> VS (r typ) -> [VS (r val)] -> NamedArgs r var val -> VS (r val)

-- | Calling a method. t is the return type of the method, o is the
--   object, f is the method name, and ps is a list of positional arguments.
objMethodCall
  :: (InternalValueExp r var val typ)
  => VS (r typ) -> VS (r val) -> Label -> [VS (r val)] -> VS (r val)
objMethodCall t o f ps = objMethodCallMixedArgs' f t o ps []

-- | Calling a method with named arguments.
objMethodCallNamedArgs
  :: (InternalValueExp r var val typ)
  => VS (r typ) -> VS (r val) -> Label -> NamedArgs r var val -> VS (r val)
objMethodCallNamedArgs t o f = objMethodCallMixedArgs' f t o []

-- | Calling a method with a mix of positional and named arguments.
objMethodCallMixedArgs
  :: (InternalValueExp r var val typ)
  => VS (r typ) -> VS (r val) -> Label -> [VS (r val)] -> NamedArgs r var val -> VS (r val)
objMethodCallMixedArgs t o f = objMethodCallMixedArgs' f t o

-- | Calling a method with no parameters.
objMethodCallNoParams
  :: (InternalValueExp r var val typ)
  => VS (r typ) -> VS (r val) -> Label -> VS (r val)
objMethodCallNoParams t o f = objMethodCall t o f []

-- | Calling a class method. t is the return type of the method, c is the
--   class, f is the method name, and ps is a list of positional arguments.
classMethodCall
  :: (InternalValueExp r var val typ)
  => VS (r typ) -> VS (r typ) -> Label -> [VS (r val)] -> VS (r val)
classMethodCall t c f ps = classMethodCallMixedArgs' f t c ps []

-- | Calling a class method with named arguments.
classMethodCallNamedArgs
  :: (InternalValueExp r var val typ)
  => VS (r typ) -> VS (r typ) -> Label -> NamedArgs r var val -> VS (r val)
classMethodCallNamedArgs t c f = classMethodCallMixedArgs' f t c []

-- | Calling a class method with a mix of positional and named arguments.
classMethodCallMixedArgs
  :: (InternalValueExp r var val typ)
  => VS (r typ)
  -> VS (r typ)
  -> Label
  -> [VS (r val)]
  -> NamedArgs r var val
  -> VS (r val)
classMethodCallMixedArgs t c f = classMethodCallMixedArgs' f t c

-- | Calling a class method with no parameters.
classMethodCallNoParams
  :: (InternalValueExp r var val typ)
  => VS (r typ) -> VS (r typ) -> Label -> VS (r val)
classMethodCallNoParams t c f = classMethodCall t c f []

class OODeclStatement r stmt var scope val | r -> stmt var scope val where
  objDecDef    :: VS (r var) -> r scope -> VS (r val) -> MS (r stmt)
  -- Parameters: variable to store the object, scope of the variable,
  --             constructor arguments.  Object type is not needed,
  --             as it is inferred from the variable's type.
  objDecNew    :: VS (r var) -> r scope -> [VS (r val)] -> MS (r stmt)
  extObjDecNew :: Library -> VS (r var) -> r scope -> [VS (r val)]
    -> MS (r stmt)

objDecNewNoParams
  :: (OODeclStatement r stmt var scope val)
  => VS (r var) -> r scope
  -> MS (r stmt)
objDecNewNoParams v tp = objDecNew v tp []

extObjDecNewNoParams
  :: (OODeclStatement r stmt var scope val)
  => Library -> VS (r var) -> r scope -> MS (r stmt)
extObjDecNewNoParams l v tp = extObjDecNew l v tp []

class OOFuncAppStatement r stmt var val | r -> stmt var val where
  selfInOutCall :: InOutCall r stmt var val

class ObserverPattern r stmt typ | r -> stmt typ where
  notifyObservers :: VS (r FuncData) -> VS (r typ) -> MS (r stmt)

observerListName :: Label
observerListName = "observerList"

initObserverList
  :: (TypeSym r typ, VariableSym r var typ, DeclStatement r bod stmt var scope val)
  => VS (r typ) -> [VS (r val)] -> r scope -> MS (r stmt)
initObserverList t os scp = listDecDef (var observerListName (listType t)) scp os

addObserver
  ::
    ( TypeSym r typ
    , VariableSym r var typ
    , VariableValue r var val
    , ValueSym r val typ
    , List r val
    , ListStatement r stmt val
    )
  => VS (r val) -> MS (r stmt)
addObserver o = listAdd obsList lastelem o
  where obsList = valueOf $ listOf observerListName (onStateValue valueType o)
        lastelem = listSize obsList

class StrategyPattern r bod block var val | r -> bod block var val where
  runStrategy
    :: Label -> [(Label, MS (r bod))] -> Maybe (VS (r val)) -> Maybe (VS (r var)) -> MS (r block)

class OOFunctionSym r val typ | r -> val typ where
  func :: Label -> VS (r typ) -> [VS (r val)] -> VS (r FuncData)
  objAccess :: VS (r val) -> VS (r FuncData) -> VS (r val)

($.) :: (OOFunctionSym r val typ) => VS (r val) -> VS (r FuncData) -> VS (r val)
infixl 9 $.
($.) = objAccess

selfAccess
  :: (VariableValue r var val, SelfSym r var, OOFunctionSym r val typ)
  => VS (r FuncData) -> VS (r val)
selfAccess = objAccess (valueOf self)

class GetSet r var val | r -> var val where
  get :: VS (r val) -> VS (r var) -> VS (r val)
  set :: VS (r val) -> VS (r var) -> VS (r val) -> VS (r val)

convTypeOO :: (TypeSym r typ, OOTypeSym r typ) => CodeType -> VS (r typ)
convTypeOO (Object n) = obj n
convTypeOO (Reference t) = referenceType (convTypeOO t)
convTypeOO t = convType t
