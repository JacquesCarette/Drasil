{-# LANGUAGE TypeFamilies #-}

module GOOL.Drasil.InterfaceGOOL (
  -- Types
  VSFunction,
  -- Typeclasses
  OOProg, OOTypeSym(..), OOVariableSym(..),
  ($->), OOValueSym, OOVariableValue, OOValueExpression(..), selfFuncApp,
  newObj, extNewObj, libNewObj, OODeclStatement(..), objDecNewNoParams,
  extObjDecNewNoParams, OOFuncAppStatement(..), GetSet(..),
  InternalValueExp(..), objMethodCall, objMethodCallNamedArgs,
  objMethodCallMixedArgs, objMethodCallNoParams, FunctionSym(..), ($.),
  selfAccess, ObserverPattern(..), observerListName, initObserverList,
  addObserver, StrategyPattern(..), convTypeOO
  ) where

import GOOL.Drasil.InterfaceCommon (Label, Library, MSBody, MSBlock, VSType,
  SVariable, SValue, MSStatement, NamedArgs, MixedCall, MixedCtorCall,
  PosCall, PosCtorCall, InOutCall, SharedProg, ProgramSym, BodySym,
  TypeSym(listType), VariableSym(var), ValueSym(valueType),
  VariableValue(valueOf), ValueExpression, List(listSize, listAdd), listOf,
  StatementSym(valStmt), DeclStatement(listDecDef), FuncAppStatement, convType)
import GOOL.Drasil.CodeType (CodeType(..), ClassName)
import GOOL.Drasil.Helpers (onStateValue)
import GOOL.Drasil.State (VS)

class (SharedProg r, ProgramSym r, OOVariableValue r, OODeclStatement r,
  OOFuncAppStatement r, OOValueExpression r, InternalValueExp r, GetSet r,
  ObserverPattern r, StrategyPattern r
  ) => OOProg r

class (TypeSym r) => OOTypeSym r where
  obj :: ClassName -> VSType r

class (ValueSym r, OOTypeSym r) => OOValueSym r

class (VariableSym r, OOTypeSym r) => OOVariableSym r where
  staticVar    :: Label -> VSType r -> SVariable r -- I *think* this is OO-only
  self         :: SVariable r
  classVar     :: VSType r -> SVariable r -> SVariable r
  extClassVar  :: VSType r -> SVariable r -> SVariable r
  objVar       :: SVariable r -> SVariable r -> SVariable r
  objVarSelf   :: SVariable r -> SVariable r

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
  objDecDef    :: SVariable r -> SValue r -> MSStatement r
  objDecNew    :: SVariable r -> [SValue r] -> MSStatement r
  extObjDecNew :: Library -> SVariable r -> [SValue r] -> MSStatement r

objDecNewNoParams :: (OODeclStatement r) => SVariable r -> MSStatement r
objDecNewNoParams v = objDecNew v []

extObjDecNewNoParams :: (OODeclStatement r) => Library -> SVariable r -> 
  MSStatement r
extObjDecNewNoParams l v = extObjDecNew l v []

class (FuncAppStatement r, OOVariableSym r) => OOFuncAppStatement r where
  selfInOutCall :: InOutCall r

class (StatementSym r, FunctionSym r) => ObserverPattern r where
  notifyObservers :: VSFunction r -> VSType r -> MSStatement r

observerListName :: Label
observerListName = "observerList"

initObserverList :: (DeclStatement r) => VSType r -> [SValue r] -> MSStatement r
initObserverList t = listDecDef (var observerListName (listType t))

addObserver :: (StatementSym r, OOVariableValue r, List r) => SValue r -> 
  MSStatement r
addObserver o = valStmt $ listAdd obsList lastelem o
  where obsList = valueOf $ observerListName `listOf` onStateValue valueType o
        lastelem = listSize obsList

class (BodySym r, VariableSym r) => StrategyPattern r where
  runStrategy :: Label -> [(Label, MSBody r)] -> Maybe (SValue r) ->
    Maybe (SVariable r) -> MSBlock r

type VSFunction a = VS (a (Function a))

class (ValueSym r) => FunctionSym r where
  type Function r
  func :: Label -> VSType r -> [SValue r] -> VSFunction r
  objAccess :: SValue r -> VSFunction r -> SValue r

($.) :: (FunctionSym r) => SValue r -> VSFunction r -> SValue r
infixl 9 $.
($.) = objAccess

selfAccess :: (OOVariableValue r, FunctionSym r) => VSFunction r -> SValue r
selfAccess = objAccess (valueOf self)

class (ValueSym r, VariableSym r) => GetSet r where
  get :: SValue r -> SVariable r -> SValue r
  set :: SValue r -> SVariable r -> SValue r -> SValue r

convTypeOO :: (OOTypeSym r) => CodeType -> VSType r
convTypeOO (Object n) = obj n
convTypeOO t = convType t
