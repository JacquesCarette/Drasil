{-# LANGUAGE TypeFamilies #-}

module GOOL.Drasil.ClassInterface (
  -- Types
  Label, Library, GSProgram, SFile, MSBody, MSBlock, VSType, SVariable, SValue, 
  VSFunction, MSStatement, MSParameter, SMethod, CSStateVar, SClass, FSModule,
  NamedArgs, Initializers, MixedCall, MixedCtorCall, PosCall, PosCtorCall,
  -- Typeclasses
  OOProg, ProgramSym(..), FileSym(..), PermanenceSym(..), BodySym(..), 
  bodyStatements, oneLiner, BlockSym(..), TypeSym(..), TypeElim(..), 
  VariableSym(..), VariableElim(..), ($->), listOf, listVar, ValueSym(..), 
  Argument(..), Literal(..), MathConstant(..), VariableValue(..), 
  CommandLineArgs(..), NumericExpression(..), BooleanExpression(..), 
  Comparison(..), ValueExpression(..), funcApp, funcAppNamedArgs, selfFuncApp, 
  extFuncApp, libFuncApp, newObj, extNewObj, libNewObj, exists, 
  InternalValueExp(..), objMethodCall, objMethodCallNamedArgs, 
  objMethodCallMixedArgs, objMethodCallNoParams, FunctionSym(..), ($.),
  selfAccess, GetSet(..), List(..), InternalList(..), listSlice, 
  listIndexExists, at, StatementSym(..), AssignStatement(..), (&=), 
  assignToListIndex, DeclStatement(..), objDecNewNoParams, 
  extObjDecNewNoParams, IOStatement(..), StringStatement(..),
  FuncAppStatement(..), CommentStatement(..), ControlStatement(..), 
  StatePattern(..), initState, changeState, ObserverPattern(..), 
  observerListName, initObserverList, addObserver, StrategyPattern(..), 
  ifNoElse, switchAsIf, ScopeSym(..), ParameterSym(..), MethodSym(..), 
  privMethod, pubMethod, initializer, nonInitConstructor, StateVarSym(..), 
  privDVar, pubDVar, pubSVar, ClassSym(..), ModuleSym(..), convType
) where

import GOOL.Drasil.CodeType (CodeType(..), ClassName)
import GOOL.Drasil.Helpers (onStateValue)
import GOOL.Drasil.State (GS, FS, CS, MS, VS)

import Data.Bifunctor (first)

type Label = String
type Library = String

type GSProgram a = GS (a (Program a))

-- In relation to GOOL, the type variable r can be considered as short for "representation"

-- Functions in GOOL's interface beginning with "ext" are to be used to access items from other modules in the same program/project
-- Functions in GOOL's interface beginning with "lib" are to be used to access items from different libraries/projects

class (ProgramSym r, AssignStatement r, DeclStatement r, IOStatement r, 
  StringStatement r, FuncAppStatement r, CommentStatement r, ControlStatement r,
  InternalList r, Argument r, Literal r, MathConstant r, VariableValue r, 
  CommandLineArgs r, NumericExpression r, BooleanExpression r, Comparison r, 
  ValueExpression r, InternalValueExp r, GetSet r, List r, StatePattern r, 
  ObserverPattern r, StrategyPattern r, TypeElim r, VariableElim r) => OOProg r

class (FileSym r) => ProgramSym r where
  type Program r
  prog :: Label -> [SFile r] -> GSProgram r

type SFile a = FS (a (File a))

class (ModuleSym r) => FileSym r where 
  type File r
  fileDoc :: FSModule r -> SFile r

  -- Module description, list of author names, date as a String, file to comment
  docMod :: String -> [String] -> String -> SFile r -> SFile r

class PermanenceSym r where
  type Permanence r
  static  :: r (Permanence r)
  dynamic :: r (Permanence r)

type MSBody a = MS (a (Body a))

class (BlockSym r) => BodySym r where
  type Body r
  body           :: [MSBlock r] -> MSBody r

  addComments :: Label -> MSBody r -> MSBody r

bodyStatements :: (BodySym r) => [MSStatement r] -> MSBody r
bodyStatements sts = body [block sts]

oneLiner :: (BodySym r) => MSStatement r -> MSBody r
oneLiner s = bodyStatements [s]

type MSBlock a = MS (a (Block a))

class (StatementSym r) => BlockSym r where
  type Block r
  block   :: [MSStatement r] -> MSBlock r

type VSType a = VS (a (Type a))

class TypeSym r where
  type Type r
  bool          :: VSType r
  int           :: VSType r -- This is 32-bit signed ints except in Python, 
                            -- which has unlimited precision ints
  float         :: VSType r
  double        :: VSType r
  char          :: VSType r
  string        :: VSType r
  infile        :: VSType r
  outfile       :: VSType r
  listType      :: VSType r -> VSType r
  arrayType     :: VSType r -> VSType r
  listInnerType :: VSType r -> VSType r
  obj           :: ClassName -> VSType r
  funcType      :: [VSType r] -> VSType r -> VSType r
  void          :: VSType r

class (TypeSym r) => TypeElim r where
  getType :: r (Type r) -> CodeType
  getTypeString :: r (Type r) -> String

type SVariable a = VS (a (Variable a))

class (TypeSym r) => VariableSym r where
  type Variable r
  var          :: Label -> VSType r -> SVariable r
  staticVar    :: Label -> VSType r -> SVariable r
  constant     :: Label -> VSType r -> SVariable r
  extVar       :: Library -> Label -> VSType r -> SVariable r
  self         :: SVariable r
  classVar     :: VSType r -> SVariable r -> SVariable r
  extClassVar  :: VSType r -> SVariable r -> SVariable r
  objVar       :: SVariable r -> SVariable r -> SVariable r
  objVarSelf   :: SVariable r -> SVariable r
  arrayElem    :: Integer -> SVariable r -> SVariable r
  
class (VariableSym r) => VariableElim r where
  variableName :: r (Variable r) -> String
  variableType :: r (Variable r) -> r (Type r)

($->) :: (VariableSym r) => SVariable r -> SVariable r -> SVariable r
infixl 9 $->
($->) = objVar

listVar :: (VariableSym r) => Label -> VSType r -> SVariable r
listVar n t = var n (listType t)

listOf :: (VariableSym r) => Label -> VSType r -> SVariable r
listOf = listVar

type SValue a = VS (a (Value a))

class (TypeSym r) => ValueSym r where
  type Value r
  valueType :: r (Value r) -> r (Type r)

class (ValueSym r) => Argument r where
  pointerArg :: SValue r -> SValue r

class (ValueSym r) => Literal r where
  litTrue   :: SValue r
  litFalse  :: SValue r
  litChar   :: Char -> SValue r
  litDouble :: Double -> SValue r
  litFloat  :: Float -> SValue r
  litInt    :: Integer -> SValue r
  litString :: String -> SValue r
  litArray  :: VSType r -> [SValue r] -> SValue r
  litList   :: VSType r -> [SValue r] -> SValue r

class (ValueSym r) => MathConstant r where
  pi :: SValue r

class (VariableSym r, ValueSym r) => VariableValue r where
  valueOf       :: SVariable r -> SValue r

class (ValueSym r) => CommandLineArgs r where
  arg          :: Integer -> SValue r
  argsList     :: SValue r
  argExists    :: Integer -> SValue r

class (ValueSym r) => NumericExpression r where
  (#~)  :: SValue r -> SValue r
  infixl 8 #~ -- Negation
  (#/^) :: SValue r -> SValue r
  infixl 7 #/^ -- Square root
  (#|)  :: SValue r -> SValue r
  infixl 7 #| -- Absolute value
  (#+)  :: SValue r -> SValue r -> SValue r
  infixl 5 #+
  (#-)  :: SValue r -> SValue r -> SValue r
  infixl 5 #-
  (#*)  :: SValue r -> SValue r -> SValue r
  infixl 6 #*
  (#/)  :: SValue r -> SValue r -> SValue r
  infixl 6 #/
  (#%)  :: SValue r -> SValue r -> SValue r
  infixl 6 #% -- Modulo
  (#^)  :: SValue r -> SValue r -> SValue r
  infixl 7 #^ -- Exponentiation

  log    :: SValue r -> SValue r
  ln     :: SValue r -> SValue r
  exp    :: SValue r -> SValue r
  sin    :: SValue r -> SValue r
  cos    :: SValue r -> SValue r
  tan    :: SValue r -> SValue r
  csc    :: SValue r -> SValue r
  sec    :: SValue r -> SValue r
  cot    :: SValue r -> SValue r
  arcsin :: SValue r -> SValue r
  arccos :: SValue r -> SValue r
  arctan :: SValue r -> SValue r
  floor  :: SValue r -> SValue r
  ceil   :: SValue r -> SValue r

class (ValueSym r) => BooleanExpression r where
  (?!)  :: SValue r -> SValue r
  infixr 6 ?! -- Boolean 'not'
  (?&&) :: SValue r -> SValue r -> SValue r
  infixl 2 ?&&
  (?||) :: SValue r -> SValue r -> SValue r
  infixl 1 ?||

class (ValueSym r) => Comparison r where
  (?<)  :: SValue r -> SValue r -> SValue r
  infixl 4 ?<
  (?<=) :: SValue r -> SValue r -> SValue r
  infixl 4 ?<=
  (?>)  :: SValue r -> SValue r -> SValue r
  infixl 4 ?>
  (?>=) :: SValue r -> SValue r -> SValue r
  infixl 4 ?>=
  (?==) :: SValue r -> SValue r -> SValue r
  infixl 3 ?==
  (?!=) :: SValue r -> SValue r -> SValue r
  infixl 3 ?!=

type NamedArgs r = [(SVariable r, SValue r)]
-- Function call with both positional and named arguments
type MixedCall r = Label -> VSType r -> [SValue r] -> NamedArgs r -> SValue r
-- Constructor call with both positional and named arguments
type MixedCtorCall r = VSType r -> [SValue r] -> NamedArgs r -> SValue r
-- Function call with only positional arguments
type PosCall r = Label -> VSType r -> [SValue r] -> SValue r
-- Constructor call with only positional arguments
type PosCtorCall r = VSType r -> [SValue r] -> SValue r

-- for values that can include expressions
class (VariableSym r, ValueSym r) => ValueExpression r where
  inlineIf     :: SValue r -> SValue r -> SValue r -> SValue r
  
  funcAppMixedArgs     ::            MixedCall r
  selfFuncAppMixedArgs ::            MixedCall r
  extFuncAppMixedArgs  :: Library -> MixedCall r
  libFuncAppMixedArgs  :: Library -> MixedCall r
  newObjMixedArgs      ::            MixedCtorCall r
  extNewObjMixedArgs   :: Library -> MixedCtorCall r
  libNewObjMixedArgs   :: Library -> MixedCtorCall r

  lambda :: [SVariable r] -> SValue r -> SValue r

  notNull :: SValue r -> SValue r

funcApp          :: (ValueExpression r) =>            PosCall r
funcApp n t vs = funcAppMixedArgs n t vs []

funcAppNamedArgs :: (ValueExpression r) =>            Label -> VSType r ->
  NamedArgs r -> SValue r
funcAppNamedArgs n t = funcAppMixedArgs n t []

selfFuncApp      :: (ValueExpression r) =>            PosCall r
selfFuncApp n t vs = selfFuncAppMixedArgs n t vs []

extFuncApp       :: (ValueExpression r) => Library -> PosCall r
extFuncApp l n t vs = extFuncAppMixedArgs l n t vs []

libFuncApp       :: (ValueExpression r) => Library -> PosCall r
libFuncApp l n t vs = libFuncAppMixedArgs l n t vs []

newObj           :: (ValueExpression r) =>            PosCtorCall r
newObj t vs = newObjMixedArgs t vs []

extNewObj        :: (ValueExpression r) => Library -> PosCtorCall r
extNewObj l t vs = extNewObjMixedArgs l t vs []

libNewObj        :: (ValueExpression r) => Library -> PosCtorCall r
libNewObj l t vs = libNewObjMixedArgs l t vs []

exists :: (ValueExpression r) => SValue r -> SValue r
exists = notNull

class (FunctionSym r) => InternalValueExp r where
  objMethodCallMixedArgs' :: Label -> VSType r -> SValue r -> [SValue r] -> 
    NamedArgs r -> SValue r

objMethodCall :: (InternalValueExp r) => VSType r -> SValue r -> Label -> 
  [SValue r] -> SValue r
objMethodCall t o f ps = objMethodCallMixedArgs' f t o ps []

objMethodCallNamedArgs :: (InternalValueExp r) => VSType r -> SValue r -> Label 
  -> NamedArgs r -> SValue r
objMethodCallNamedArgs t o f = objMethodCallMixedArgs' f t o []

objMethodCallMixedArgs :: (InternalValueExp r) => VSType r -> SValue r -> Label 
  -> [SValue r] -> NamedArgs r -> SValue r
objMethodCallMixedArgs t o f = objMethodCallMixedArgs' f t o

objMethodCallNoParams :: (InternalValueExp r) => VSType r -> SValue r -> Label 
  -> SValue r
objMethodCallNoParams t o f = objMethodCall t o f []

type VSFunction a = VS (a (Function a))

class (ValueSym r) => FunctionSym r where
  type Function r
  func :: Label -> VSType r -> [SValue r] -> VSFunction r
  objAccess :: SValue r -> VSFunction r -> SValue r

($.) :: (FunctionSym r) => SValue r -> VSFunction r -> SValue r
infixl 9 $.
($.) = objAccess

selfAccess :: (VariableValue r, FunctionSym r) => VSFunction r -> SValue r
selfAccess = objAccess (valueOf self)

class (ValueSym r, VariableSym r) => GetSet r where
  get :: SValue r -> SVariable r -> SValue r
  set :: SValue r -> SVariable r -> SValue r -> SValue r

class (ValueSym r) => List r where
  listSize   :: SValue r -> SValue r
  listAdd    :: SValue r -> SValue r -> SValue r -> SValue r
  listAppend :: SValue r -> SValue r -> SValue r
  listAccess :: SValue r -> SValue r -> SValue r
  listSet    :: SValue r -> SValue r -> SValue r -> SValue r
  
  indexOf :: SValue r -> SValue r -> SValue r

class (ValueSym r) => InternalList r where
  listSlice'      :: Maybe (SValue r) -> Maybe (SValue r) -> Maybe (SValue r) 
    -> SVariable r -> SValue r -> MSBlock r
  
listSlice :: (InternalList r) => SVariable r -> SValue r -> Maybe (SValue r) -> 
  Maybe (SValue r) -> Maybe (SValue r) -> MSBlock r
listSlice vnew vold b e s = listSlice' b e s vnew vold

listIndexExists :: (List r, Comparison r) => SValue r -> SValue r -> SValue r
listIndexExists lst index = listSize lst ?> index

at :: (List r) => SValue r -> SValue r -> SValue r
at = listAccess

type MSStatement a = MS (a (Statement a))

class (ValueSym r) => StatementSym r where
  type Statement r
  valStmt :: SValue r -> MSStatement r -- converts value to statement
  multi     :: [MSStatement r] -> MSStatement r

class (VariableSym r, StatementSym r) => AssignStatement r where
  (&-=)  :: SVariable r -> SValue r -> MSStatement r
  infixl 1 &-=
  (&+=)  :: SVariable r -> SValue r -> MSStatement r
  infixl 1 &+=
  (&++)  :: SVariable r -> MSStatement r
  infixl 8 &++
  (&--)  :: SVariable r -> MSStatement r
  infixl 8 &--

  assign :: SVariable r -> SValue r -> MSStatement r

(&=) :: (AssignStatement r) => SVariable r -> SValue r -> MSStatement r
infixr 1 &=
(&=) = assign

assignToListIndex :: (StatementSym r, VariableValue r, List r) => SVariable r 
  -> SValue r -> SValue r -> MSStatement r
assignToListIndex lst index v = valStmt $ listSet (valueOf lst) index v

class (VariableSym r, StatementSym r) => DeclStatement r where
  varDec       :: SVariable r -> MSStatement r
  varDecDef    :: SVariable r -> SValue r -> MSStatement r
  listDec      :: Integer -> SVariable r -> MSStatement r
  listDecDef   :: SVariable r -> [SValue r] -> MSStatement r
  arrayDec     :: Integer -> SVariable r -> MSStatement r
  arrayDecDef  :: SVariable r -> [SValue r] -> MSStatement r
  objDecDef    :: SVariable r -> SValue r -> MSStatement r
  objDecNew    :: SVariable r -> [SValue r] -> MSStatement r
  extObjDecNew :: Library -> SVariable r -> [SValue r] -> MSStatement r
  constDecDef  :: SVariable r -> SValue r -> MSStatement r
  funcDecDef   :: SVariable r -> [SVariable r] -> MSBody r -> MSStatement r
  
objDecNewNoParams :: (DeclStatement r) => SVariable r -> MSStatement r
objDecNewNoParams v = objDecNew v []

extObjDecNewNoParams :: (DeclStatement r) => Library -> SVariable r -> 
  MSStatement r
extObjDecNewNoParams l v = extObjDecNew l v []

class (VariableSym r, StatementSym r) => IOStatement r where
  print      :: SValue r -> MSStatement r
  printLn    :: SValue r -> MSStatement r
  printStr   :: String -> MSStatement r
  printStrLn :: String -> MSStatement r

  printFile      :: SValue r -> SValue r -> MSStatement r
  printFileLn    :: SValue r -> SValue r -> MSStatement r
  printFileStr   :: SValue r -> String -> MSStatement r
  printFileStrLn :: SValue r -> String -> MSStatement r

  getInput         :: SVariable r -> MSStatement r
  discardInput     :: MSStatement r
  getFileInput     :: SValue r -> SVariable r -> MSStatement r
  discardFileInput :: SValue r -> MSStatement r

  openFileR :: SVariable r -> SValue r -> MSStatement r
  openFileW :: SVariable r -> SValue r -> MSStatement r
  openFileA :: SVariable r -> SValue r -> MSStatement r
  closeFile :: SValue r -> MSStatement r

  getFileInputLine :: SValue r -> SVariable r -> MSStatement r
  discardFileLine  :: SValue r -> MSStatement r
  getFileInputAll  :: SValue r -> SVariable r -> MSStatement r

class (VariableSym r, StatementSym r) => StringStatement r where
  stringSplit :: Char -> SVariable r -> SValue r -> MSStatement r

  stringListVals  :: [SVariable r] -> SValue r -> MSStatement r
  stringListLists :: [SVariable r] -> SValue r -> MSStatement r

type InOutCall r = Label -> [SValue r] -> [SVariable r] -> [SVariable r] -> 
  MSStatement r

class (VariableSym r, StatementSym r) => FuncAppStatement r where
  -- The three lists are inputs, outputs, and both, respectively
  inOutCall     ::            InOutCall r
  selfInOutCall ::            InOutCall r
  extInOutCall  :: Library -> InOutCall r

type Comment = String  

class (StatementSym r) => CommentStatement r where
  comment :: Comment -> MSStatement r

class (BodySym r, VariableSym r) => ControlStatement r where
  break :: MSStatement r
  continue :: MSStatement r

  returnStmt :: SValue r -> MSStatement r

  throw :: Label -> MSStatement r

  ifCond     :: [(SValue r, MSBody r)] -> MSBody r -> MSStatement r
  switch     :: SValue r -> [(SValue r, MSBody r)] -> MSBody r -> MSStatement r

  ifExists :: SValue r -> MSBody r -> MSBody r -> MSStatement r

  for      :: MSStatement r -> SValue r -> MSStatement r -> MSBody r -> 
    MSStatement r
  forRange :: SVariable r -> SValue r -> SValue r -> SValue r -> MSBody r -> 
    MSStatement r
  forEach  :: SVariable r -> SValue r -> MSBody r -> MSStatement r
  while    :: SValue r -> MSBody r -> MSStatement r 

  tryCatch :: MSBody r -> MSBody r -> MSStatement r

ifNoElse :: (ControlStatement r) => [(SValue r, MSBody r)] -> MSStatement r
ifNoElse bs = ifCond bs $ body []

switchAsIf :: (ControlStatement r, Comparison r) => SValue r -> 
  [(SValue r, MSBody r)] -> MSBody r -> MSStatement r
switchAsIf v = ifCond . map (first (v ?==))

class (BodySym r) => StatePattern r where
  checkState      :: Label -> [(SValue r, MSBody r)] -> MSBody r -> 
    MSStatement r

initState :: (DeclStatement r, Literal r) => Label -> Label -> MSStatement r
initState fsmName initialState = varDecDef (var fsmName string) 
  (litString initialState)

changeState :: (AssignStatement r, Literal r) => Label -> Label -> MSStatement r
changeState fsmName toState = var fsmName string &= litString toState

class (StatementSym r, FunctionSym r) => ObserverPattern r where
  notifyObservers :: VSFunction r -> VSType r -> MSStatement r

observerListName :: Label
observerListName = "observerList"

initObserverList :: (DeclStatement r) => VSType r -> [SValue r] -> MSStatement r
initObserverList t = listDecDef (var observerListName (listType t))

addObserver :: (StatementSym r, VariableValue r, List r) => SValue r -> 
  MSStatement r
addObserver o = valStmt $ listAdd obsList lastelem o
  where obsList = valueOf $ observerListName `listOf` onStateValue valueType o
        lastelem = listSize obsList

class (BodySym r, VariableSym r) => StrategyPattern r where
  runStrategy     :: Label -> [(Label, MSBody r)] -> Maybe (SValue r) -> 
    Maybe (SVariable r) -> MSBlock r

class ScopeSym r where
  type Scope r
  private :: r (Scope r)
  public  :: r (Scope r)

type MSParameter a = MS (a (Parameter a))

class (VariableSym r) => ParameterSym r where
  type Parameter r
  param :: SVariable r -> MSParameter r
  pointerParam :: SVariable r -> MSParameter r

type SMethod a = MS (a (Method a))
type Initializers r = [(SVariable r, SValue r)]

-- The three lists are inputs, outputs, and both, respectively
type InOutFunc r = [SVariable r] -> [SVariable r] -> [SVariable r] -> 
  MSBody r -> SMethod r
-- Parameters are: brief description of function, input descriptions and 
-- variables, output descriptions and variables, descriptions and variables 
-- for parameters that are both input and output, function body
type DocInOutFunc r = String -> [(String, SVariable r)] -> 
  [(String, SVariable r)] -> [(String, SVariable r)] -> MSBody r -> SMethod r

class (BodySym r, ParameterSym r, ScopeSym r, PermanenceSym r) => MethodSym r 
  where
  type Method r
  method      :: Label -> r (Scope r) -> r (Permanence r) -> VSType r -> 
    [MSParameter r] -> MSBody r -> SMethod r
  getMethod   :: SVariable r -> SMethod r
  setMethod   :: SVariable r -> SMethod r 
  constructor :: [MSParameter r] -> Initializers r -> MSBody r -> SMethod r

  docMain :: MSBody r -> SMethod r

  function :: Label -> r (Scope r) -> VSType r -> [MSParameter r] -> 
    MSBody r -> SMethod r
  mainFunction  :: MSBody r -> SMethod r
  -- Parameters are: function description, parameter descriptions, 
  --   return value description if applicable, function
  docFunc :: String -> [String] -> Maybe String -> SMethod r -> SMethod r

  -- inOutMethod and docInOutMethod both need the Permanence parameter
  inOutMethod :: Label -> r (Scope r) -> r (Permanence r) -> InOutFunc r
  docInOutMethod :: Label -> r (Scope r) -> r (Permanence r) -> DocInOutFunc r
  -- inOutFunc and docInOutFunc both do not need the Permanence parameter
  inOutFunc :: Label -> r (Scope r) -> InOutFunc r
  docInOutFunc :: Label -> r (Scope r) -> DocInOutFunc r

privMethod :: (MethodSym r) => Label -> VSType r -> [MSParameter r] -> MSBody r 
  -> SMethod r
privMethod n = method n private dynamic

pubMethod :: (MethodSym r) => Label -> VSType r -> [MSParameter r] -> MSBody r 
  -> SMethod r
pubMethod n = method n public dynamic

initializer :: (MethodSym r) => [MSParameter r] -> Initializers r -> SMethod r
initializer ps is = constructor ps is (body [])

nonInitConstructor :: (MethodSym r) => [MSParameter r] -> MSBody r -> SMethod r
nonInitConstructor ps = constructor ps []

type CSStateVar a = CS (a (StateVar a))

class (ScopeSym r, PermanenceSym r, VariableSym r) => StateVarSym r where
  type StateVar r
  stateVar :: r (Scope r) -> r (Permanence r) -> SVariable r -> CSStateVar r
  stateVarDef :: r (Scope r) -> r (Permanence r) -> SVariable r -> 
    SValue r -> CSStateVar r
  constVar :: r (Scope r) ->  SVariable r -> SValue r -> CSStateVar r

privDVar :: (StateVarSym r) => SVariable r -> CSStateVar r
privDVar = stateVar private dynamic

pubDVar :: (StateVarSym r) => SVariable r -> CSStateVar r
pubDVar = stateVar public dynamic

pubSVar :: (StateVarSym r) => SVariable r -> CSStateVar r
pubSVar = stateVar public static

type SClass a = CS (a (Class a))

class (MethodSym r, StateVarSym r) => ClassSym r where
  type Class r
  buildClass :: Maybe Label -> [CSStateVar r] -> [SMethod r] -> 
    SClass r
  extraClass :: Label -> Maybe Label -> [CSStateVar r] -> [SMethod r] -> 
    SClass r
  implementingClass :: Label -> [Label] -> [CSStateVar r] -> [SMethod r] -> 
    SClass r

  docClass :: String -> SClass r -> SClass r

type FSModule a = FS (a (Module a))

class (ClassSym r) => ModuleSym r where
  type Module r
  buildModule :: Label -> [Label] -> [SMethod r] -> [SClass r] -> FSModule r

-- Utility

convType :: (TypeSym r) => CodeType -> VSType r
convType Boolean = bool
convType Integer = int
convType Float = float
convType Double = double
convType Char = char
convType String = string
convType (List t) = listType (convType t)
convType (Array t) = arrayType (convType t)
convType (Object n) = obj n
convType (Func ps r) = funcType (map convType ps) (convType r)
convType Void = void
convType InFile = infile
convType OutFile = outfile