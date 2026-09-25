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
  Label, Library, SVariable, NamedArgs, MixedCtorCall, PosCall, PosCtorCall,
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
  CommandLineArgs r val, Literal r typ val, MathConstant r val,
  VariableValue r val, VariableSym r typ, TypeSym r typ, OOTypeSym r typ,
  OOVariableSym r typ val, SelfSym r, BooleanExpression r val, Comparison r val,
  NumericExpression r val, ValueSym r typ val, InternalValueExp r typ val,
  ValueExpression r typ val, OOValueExpression r typ val, IndexTranslator r val,
  Array r val, List r val, ListStatement r val stmt, Reference r val, Set r val,
  OOFunctionSym r typ val, ParameterSym r param, ScopeSym r scope,
  BinderSym r typ, InternalList r val block, MethodSym r vis typ param mthd bod,
  OOMethodSym r vis typ param val mthd attch bod, AttachmentSym r attch,
  VisibilitySym r vis, StateVarSym r vis val stvr attch, ClassSym r mthd stvr,
  TypeElim r typ, VariableElim r typ, EmptyStatement r stmt,
  MultiStatement r stmt, ValueStatement r val stmt, CommentStatement r stmt,
  DeclStatement r scope val stmt bod, OODeclStatement r scope val stmt,
  AssignStatement r val stmt, FuncAppStatement r val stmt,
  OOFuncAppStatement r val stmt, ControlStatement r val stmt bod,
  StringStatement r val stmt, PrintConsole r val stmt, ReadConsole r stmt,
  FileHandling r val stmt, PrintFile r val stmt, ReadFile r val stmt,
  ModuleSym r mod mthd, FileSym r file mod, ProgramSym r prg file
  ) => OOProg r vis scope typ param val stmt mthd stvr attch prg file mod bod block

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

type Initializers r val = [(SVariable r, VS (r val))]

class OOMethodSym r vis typ param val mthd attch bod | r -> vis typ param val mthd attch bod where
  method      :: Label -> r vis -> r attch -> VS (r typ) ->
    [MS (r param)] -> MS (r bod) -> MS (r mthd)
  getMethod   :: SVariable r -> MS (r mthd)
  setMethod   :: SVariable r -> MS (r mthd)
  constructor :: [MS (r param)] -> Initializers r val -> MS (r bod) -> MS (r mthd)

  -- inOutMethod and docInOutMethod both need AttachmentSym
  inOutMethod :: Label -> r vis -> r attch -> InOutFunc r mthd bod
  docInOutMethod :: Label -> r vis -> r attch -> DocInOutFunc r mthd bod

privMethod
  ::
    ( OOMethodSym r vis typ param val mthd attch bod
    , AttachmentSym r attch
    , VisibilitySym r vis
    )
  => Label -> VS (r typ) -> [MS (r param)] -> MS (r bod) -> MS (r mthd)
privMethod n = method n private instanceLevel

pubMethod
  ::
    ( OOMethodSym r vis typ param val mthd attch bod
    , AttachmentSym r attch
    , VisibilitySym r vis
    )
  => Label -> VS (r typ) -> [MS (r param)] -> MS (r bod) -> MS (r mthd)
pubMethod n = method n public instanceLevel

initializer
  :: (OOMethodSym r vis typ param val mthd attch bod, BodySym r bod block)
  => [MS (r param)] -> Initializers r val -> MS (r mthd)
initializer ps is = constructor ps is (body [])

nonInitConstructor
  :: (OOMethodSym r vis typ param val mthd attch bod)
  => [MS (r param)] -> MS (r bod) -> MS (r mthd)
nonInitConstructor ps = constructor ps []

type StateVar = Doc
type CSStateVar r stvr = CS (r stvr)

-- | Class for representing class variables, both instance- and class-level.
-- Used when creating a class, to hold extra information about `Attachment`
-- and `Visibility`.
-- Usually 'Doc' is used for the representation.
class StateVarSym r vis val stvr attch | r -> vis val stvr attch where
  -- | Given a visibility, attachment, and variable, represent the declaration
  -- of a state variable with no initial value.
  stateVar :: r vis -> r attch -> SVariable r -> CSStateVar r stvr
  -- | Given a visibility, attachment, variable, and initial value,
  -- represent the declaration of a state variable with the given initial value.
  stateVarDef :: r vis -> r attch -> SVariable r -> VS (r val) -> CSStateVar r stvr
  -- | Given a visibility, variable, and value, represent the declaration of
  -- a state constant with the given value.
  constVar :: r vis ->  SVariable r -> VS (r val) -> CSStateVar r stvr

privDVar
  ::
    ( AttachmentSym r attch
    , VisibilitySym r vis
    , StateVarSym r vis val stvr attch
    )
  => SVariable r -> CSStateVar r stvr
privDVar = stateVar private instanceLevel

pubDVar
  ::
    ( AttachmentSym r attch
    , VisibilitySym r vis
    , StateVarSym r vis val stvr attch
    )
  => SVariable r -> CSStateVar r stvr
pubDVar = stateVar public instanceLevel

pubSVar
  ::
    ( AttachmentSym r attch
    , VisibilitySym r vis
    , StateVarSym r vis val stvr attch
    )
  => SVariable r -> CSStateVar r stvr
pubSVar = stateVar public classLevel

-- | Used to differentiate whether a member is attached to the class or the instance
class AttachmentSym r attch | r -> attch where
  classLevel  :: r attch
  instanceLevel :: r attch

class OOTypeSym r typ | r -> typ where
  obj :: ClassName -> VS (r typ)

class OOVariableSym r typ val | r -> typ val where
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
  instanceVarAccess :: VS (r val) -> SVariable r -> SVariable r

($->) :: (OOVariableSym r typ val) => VS (r val) -> SVariable r -> SVariable r
infixl 9 $->
($->) = instanceVarAccess

class SelfSym r where
  -- | `self` keyword
  self              :: SVariable r

-- | Given a variable `v`, creates `self.v`
instanceVarSelf
  :: (OOVariableSym r typ val, SelfSym r, VariableValue r val)
  => SVariable r -> SVariable r
instanceVarSelf = instanceVarAccess (valueOf self)

-- for values that can include expressions
class OOValueExpression r typ val | r -> typ val where
  newObjMixedArgs         ::            MixedCtorCall r typ val
  extNewObjMixedArgs      :: Library -> MixedCtorCall r typ val
  libNewObjMixedArgs      :: Library -> MixedCtorCall r typ val

selfMethodCall
  :: (InternalValueExp r typ val, VariableValue r val, SelfSym r)
  => PosCall r typ val
selfMethodCall n t = objMethodCall t (valueOf self) n

newObj
  :: (OOValueExpression r typ val) => PosCtorCall r typ val
newObj t vs = newObjMixedArgs t vs []

extNewObj
  :: (OOValueExpression r typ val) => Library -> PosCtorCall r typ val
extNewObj l t vs = extNewObjMixedArgs l t vs []

libNewObj
  :: (OOValueExpression r typ val) => Library -> PosCtorCall r typ val
libNewObj l t vs = libNewObjMixedArgs l t vs []

-- TODO [Brandon Bosman, 07/22/2026]: Give this a better name
-- | A class for representing method calls, both instance- and class-level
class InternalValueExp r typ val | r -> typ val where
  -- TODO [Brandon Bosman, 07/22/2026]: rename this to `instanceMethodCallMixedArgs'`
  -- | Generic function for calling a method.
  --   Takes the function name, the return type, the object, a list of
  --   positional arguments, and a list of named arguments.
  objMethodCallMixedArgs'
    :: Label -> VS (r typ) -> VS (r val) -> [VS (r val)] -> NamedArgs r val -> VS (r val)
  -- | Generic function for calling a class method.
  --   Takes the function name, the return type, the class type,
  --   a list of positional arguments, and a list of named arguments.
  classMethodCallMixedArgs'
    :: Label -> VS (r typ) -> VS (r typ) -> [VS (r val)] -> NamedArgs r val -> VS (r val)

-- | Calling a method. t is the return type of the method, o is the
--   object, f is the method name, and ps is a list of positional arguments.
objMethodCall
  :: (InternalValueExp r typ val)
  => VS (r typ) -> VS (r val) -> Label -> [VS (r val)] -> VS (r val)
objMethodCall t o f ps = objMethodCallMixedArgs' f t o ps []

-- | Calling a method with named arguments.
objMethodCallNamedArgs
  :: (InternalValueExp r typ val)
  => VS (r typ) -> VS (r val) -> Label -> NamedArgs r val -> VS (r val)
objMethodCallNamedArgs t o f = objMethodCallMixedArgs' f t o []

-- | Calling a method with a mix of positional and named arguments.
objMethodCallMixedArgs
  :: (InternalValueExp r typ val)
  => VS (r typ) -> VS (r val) -> Label -> [VS (r val)] -> NamedArgs r val -> VS (r val)
objMethodCallMixedArgs t o f = objMethodCallMixedArgs' f t o

-- | Calling a method with no parameters.
objMethodCallNoParams
  :: (InternalValueExp r typ val)
  => VS (r typ) -> VS (r val) -> Label -> VS (r val)
objMethodCallNoParams t o f = objMethodCall t o f []

-- | Calling a class method. t is the return type of the method, c is the
--   class, f is the method name, and ps is a list of positional arguments.
classMethodCall
  :: (InternalValueExp r typ val)
  => VS (r typ) -> VS (r typ) -> Label -> [VS (r val)] -> VS (r val)
classMethodCall t c f ps = classMethodCallMixedArgs' f t c ps []

-- | Calling a class method with named arguments.
classMethodCallNamedArgs
  :: (InternalValueExp r typ val)
  => VS (r typ) -> VS (r typ) -> Label -> NamedArgs r val -> VS (r val)
classMethodCallNamedArgs t c f = classMethodCallMixedArgs' f t c []

-- | Calling a class method with a mix of positional and named arguments.
classMethodCallMixedArgs
  :: (InternalValueExp r typ val)
  => VS (r typ)
  -> VS (r typ)
  -> Label
  -> [VS (r val)]
  -> NamedArgs r val
  -> VS (r val)
classMethodCallMixedArgs t c f = classMethodCallMixedArgs' f t c

-- | Calling a class method with no parameters.
classMethodCallNoParams
  :: (InternalValueExp r typ val)
  => VS (r typ) -> VS (r typ) -> Label -> VS (r val)
classMethodCallNoParams t c f = classMethodCall t c f []

class OODeclStatement r scope val stmt | r -> scope val stmt where
  objDecDef    :: SVariable r -> r scope -> VS (r val) -> MS (r stmt)
  -- Parameters: variable to store the object, scope of the variable,
  --             constructor arguments.  Object type is not needed,
  --             as it is inferred from the variable's type.
  objDecNew    :: SVariable r -> r scope -> [VS (r val)] -> MS (r stmt)
  extObjDecNew :: Library -> SVariable r -> r scope -> [VS (r val)]
    -> MS (r stmt)

objDecNewNoParams :: (OODeclStatement r scope val stmt) => SVariable r -> r scope
  -> MS (r stmt)
objDecNewNoParams v tp = objDecNew v tp []

extObjDecNewNoParams
  :: (OODeclStatement r scope val stmt)
  => Library -> SVariable r -> r scope -> MS (r stmt)
extObjDecNewNoParams l v tp = extObjDecNew l v tp []

class OOFuncAppStatement r val stmt | r -> val stmt where
  selfInOutCall :: InOutCall r val stmt

class ObserverPattern r typ stmt | r -> typ stmt where
  notifyObservers :: VS (r FuncData) -> VS (r typ) -> MS (r stmt)

observerListName :: Label
observerListName = "observerList"

initObserverList
  :: (TypeSym r typ, VariableSym r typ, DeclStatement r scope val stmt bod)
  => VS (r typ) -> [VS (r val)] -> r scope -> MS (r stmt)
initObserverList t os scp = listDecDef (var observerListName (listType t)) scp os

addObserver
  ::
    ( TypeSym r typ
    , VariableSym r typ
    , VariableValue r val
    , ValueSym r typ val
    , List r val
    , ListStatement r val stmt
    )
  => VS (r val) -> MS (r stmt)
addObserver o = listAdd obsList lastelem o
  where obsList = valueOf $ listOf observerListName (onStateValue valueType o)
        lastelem = listSize obsList

class StrategyPattern r val bod block | r -> val bod block where
  runStrategy
    :: Label -> [(Label, MS (r bod))] -> Maybe (VS (r val)) -> Maybe (SVariable r) -> MS (r block)

class OOFunctionSym r typ val | r -> typ val where
  func :: Label -> VS (r typ) -> [VS (r val)] -> VS (r FuncData)
  objAccess :: VS (r val) -> VS (r FuncData) -> VS (r val)

($.) :: (OOFunctionSym r typ val) => VS (r val) -> VS (r FuncData) -> VS (r val)
infixl 9 $.
($.) = objAccess

selfAccess
  :: (VariableValue r val, SelfSym r, OOFunctionSym r typ val)
  => VS (r FuncData) -> VS (r val)
selfAccess = objAccess (valueOf self)

class GetSet r val | r -> val where
  get :: VS (r val) -> SVariable r -> VS (r val)
  set :: VS (r val) -> SVariable r -> VS (r val) -> VS (r val)

convTypeOO :: (TypeSym r typ, OOTypeSym r typ) => CodeType -> VS (r typ)
convTypeOO (Object n) = obj n
convTypeOO (Reference t) = referenceType (convTypeOO t)
convTypeOO t = convType t
