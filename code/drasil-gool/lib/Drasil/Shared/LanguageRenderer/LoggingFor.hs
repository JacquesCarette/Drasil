{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
-- | MVP renderer for logging statements.
module Drasil.Shared.LanguageRenderer.LoggingFor (LoggingFor(..)) where

import Drasil.Shared.InterfaceCommon
import qualified Drasil.GProc.InterfaceProc as P
import qualified Drasil.GOOL.InterfaceGOOL as G
import Drasil.Shared.State

import Prelude hiding (print, break, pi, log, floor, sin, cos, tan, exp)
import Control.Lens (zoom)
import Data.Kind (Type)
import Control.Monad.State
import Data.Bifunctor (bimap)
import Drasil.Shared.CodeType (CodeType)

newtype LoggingFor (r :: Type -> Type) a = LC {unLC :: r a}
  deriving newtype (Functor, Applicative, Monad)

class LiftLogging u l | l -> u where
  liftLogging :: u -> l
  lowerLogging :: l -> u

instance (LiftLogging u1 l1, LiftLogging u2 l2) => LiftLogging (u1 -> u2) (l1 -> l2) where
  liftLogging k = liftLogging . k . lowerLogging
  lowerLogging k = lowerLogging . k . liftLogging

instance LiftLogging (r a) (LoggingFor r a) where
  lowerLogging = unLC
  liftLogging = LC

instance (LiftLogging u l) => LiftLogging (State s u) (State s l) where
  liftLogging = fmap liftLogging
  lowerLogging = fmap lowerLogging

instance (LiftLogging u l) => LiftLogging (Maybe u) (Maybe l) where
  liftLogging = fmap liftLogging
  lowerLogging = fmap lowerLogging

instance {-# OVERLAPPABLE #-} (LiftLogging u l) => LiftLogging [u] [l] where
  liftLogging = fmap liftLogging
  lowerLogging = fmap lowerLogging

instance LiftLogging String String where
  liftLogging = id
  lowerLogging = id

instance LiftLogging Integer Integer where
  liftLogging = id
  lowerLogging = id

instance LiftLogging Char Char where
  liftLogging = id
  lowerLogging = id

instance LiftLogging Double Double where
  liftLogging = id
  lowerLogging = id

instance LiftLogging Float Float where
  liftLogging = id
  lowerLogging = id

instance LiftLogging CodeType CodeType where
  liftLogging = id
  lowerLogging = id

instance (LiftLogging u1 l1, LiftLogging u2 l2) => LiftLogging (u1, u2) (l1, l2) where
  liftLogging = bimap liftLogging liftLogging
  lowerLogging = bimap lowerLogging lowerLogging

-- TODO [Brandon Bosman, 06/19/2026]: This should be passed down from drasil-code
varLogFile :: (TypeSym r typ, VariableSym r var typ) => VS (r var)
varLogFile = var "outfile" outfile

valLogFile
  :: (TypeSym r typ, VariableSym r var typ, VariableValue r var val)
  => VS (r val)
valLogFile = valueOf varLogFile

-- TODO [Brandon Bosman, 06/19/2026]: This should be passed down from drasil-code
logName :: (Literal r val typ) => VS (r val)
logName = litString "log.txt"

logVarUpdate
  ::
    ( FileHandling r stmt var val
    , PrintFile r stmt val
    , TypeSym r typ
    , VariableSym r var typ
    , VariableValue r var val
    , VariableElim r var typ
    , Literal r val typ
    )
  => VS ((LoggingFor r) var) -> [MS (r stmt)]
logVarUpdate x =
  [ openFileA varLogFile logName
  , do
      x' <- variableName . lowerLogging <$> zoom lensMStoVS x
      printFileStr valLogFile $ "var '" <> x' <> "' assigned "
  , printFile valLogFile $ valueOf (lowerLogging x)
  , do
      modName <- zoom lensMStoFS getModuleName
      printFileStrLn valLogFile $ " in module " <> modName
  , closeFile valLogFile
  ]

instance
  ( MultiStatement r stmt
  , AssignStatement r stmt var val
  , FileHandling r stmt var val
  , PrintFile r stmt val
  , TypeSym r typ
  , VariableSym r var typ
  , VariableValue r var val
  , VariableElim r var typ
  , Literal r val typ
  ) => AssignStatement (LoggingFor r) stmt var val where
  (&-=) = liftLogging (&-=)
  (&+=) = liftLogging (&+=)
  (&++) = liftLogging (&++)
  (&--) = liftLogging (&--)
  assign x e = liftLogging $ multi $
    assign (lowerLogging x) (lowerLogging e)
    : logVarUpdate x

instance (List r val) => List (LoggingFor r) val where
  listSize = liftLogging listSize
  listAccess = liftLogging listAccess
  indexOf = liftLogging indexOf

instance (ListStatement r stmt val) => ListStatement (LoggingFor r) stmt val where
  listAdd = liftLogging listAdd
  listAppend = liftLogging listAppend
  listSet = liftLogging listSet -- TODO [Brandon Bosman, 06/23/2026]: Add logging
                                -- (Can't right now because RC.value isn't exposed)

instance
  ( MultiStatement r stmt
  , DeclStatement r bod stmt var scope val
  , FileHandling r stmt var val
  , PrintFile r stmt val
  , TypeSym r typ
  , VariableSym r var typ
  , VariableValue r var val
  , VariableElim r var typ
  , Literal r val typ
  ) => DeclStatement (LoggingFor r) bod stmt var scope val where
  varDec = liftLogging varDec
  varDecDef vr scp vl = liftLogging $ multi $
    varDecDef (lowerLogging vr) (lowerLogging scp) (lowerLogging vl)
    : logVarUpdate vr
  listDec = liftLogging listDec
  listDecDef vr scp vls = liftLogging $
    listDecDef (lowerLogging vr) (lowerLogging scp) (lowerLogging vls)
  setDec = liftLogging setDec
  setDecDef vr scp vl = liftLogging $
    setDecDef (lowerLogging vr) (lowerLogging scp) (lowerLogging vl)
  arrayDec = liftLogging arrayDec
  arrayDecDef vr scp vls = liftLogging $
    arrayDecDef (lowerLogging vr) (lowerLogging scp) (lowerLogging vls)
  constDecDef cnst scp vl = liftLogging $ multi $
    constDecDef (lowerLogging cnst) (lowerLogging scp) (lowerLogging vl)
    : logVarUpdate cnst
  funcDecDef = liftLogging funcDecDef

instance (PrintConsole r stmt val) => PrintConsole (LoggingFor r) stmt val where
  print = liftLogging print
  printLn = liftLogging printLn
  printStr = liftLogging printStr
  printStrLn = liftLogging printStrLn

instance
  ( MultiStatement r stmt
  , FileHandling r stmt var val
  , PrintFile r stmt val
  , ReadConsole r stmt var
  , TypeSym r typ
  , VariableSym r var typ
  , VariableValue r var val
  , VariableElim r var typ
  , Literal r val typ
  ) => ReadConsole (LoggingFor r) stmt var where
  getInput vr = liftLogging $ multi $
    getInput (lowerLogging vr) : logVarUpdate vr
  discardInput = liftLogging discardInput

instance (FileHandling r stmt var val) => FileHandling (LoggingFor r) stmt var val where
  openFileR = liftLogging openFileR
  openFileW = liftLogging openFileW
  openFileA = liftLogging openFileA
  closeFile = liftLogging closeFile

instance (PrintFile r stmt val) => PrintFile (LoggingFor r) stmt val where
  printFile = liftLogging printFile
  printFileLn = liftLogging printFileLn
  printFileStr = liftLogging printFileStr
  printFileStrLn = liftLogging printFileStrLn

instance
  ( MultiStatement r stmt
  , FileHandling r stmt var val
  , PrintFile r stmt val
  , ReadFile r stmt var val
  , TypeSym r typ
  , VariableSym r var typ
  , VariableValue r var val
  , VariableElim r var typ
  , Literal r val typ
  ) => ReadFile (LoggingFor r) stmt var val where
  getFileInput file vr = liftLogging $ multi $
    getFileInput (lowerLogging file) (lowerLogging vr)
    : logVarUpdate vr
  discardFileInput = liftLogging discardFileInput
  getFileInputLine = liftLogging getFileInputLine
  discardFileLine = liftLogging discardFileLine
  getFileInputAll = liftLogging getFileInputAll

instance
  ( MultiStatement r stmt
  , StringStatement r stmt var val
  , FileHandling r stmt var val
  , PrintFile r stmt val
  , TypeSym r typ
  , VariableSym r var typ
  , VariableValue r var val
  , VariableElim r var typ
  , Literal r val typ
  ) => StringStatement (LoggingFor r) stmt var val where
  stringSplit chr vr str  = liftLogging $
    stringSplit (lowerLogging chr) (lowerLogging vr) (lowerLogging str)
  stringListVals vrs strs  = liftLogging $
    stringListVals (lowerLogging vrs) (lowerLogging strs)
  stringListLists vrs strs = liftLogging $ multi $
    stringListLists (lowerLogging vrs) (lowerLogging strs)
    : concatMap logVarUpdate vrs

-- SharedProg Boilerplate

instance (VariableSym r var typ) => VariableSym (LoggingFor r) var typ where
  var = liftLogging var
  constant = liftLogging constant
  extVar = liftLogging extVar

instance (TypeSym r typ) => TypeSym (LoggingFor r) typ where
  bool = liftLogging bool
  int = liftLogging int
  float = liftLogging float
  double = liftLogging double
  char = liftLogging char
  string = liftLogging string
  infile = liftLogging infile
  outfile = liftLogging outfile
  referenceType = liftLogging referenceType
  listType = liftLogging listType
  setType = liftLogging setType
  arrayType = liftLogging arrayType
  innerType = liftLogging innerType
  funcType = liftLogging funcType
  void = liftLogging void

instance (TypeElim r typ) => TypeElim (LoggingFor r) typ where
  getCodeType = liftLogging getCodeType

instance (ValueSym r val typ) => ValueSym (LoggingFor r) val typ where
  valueType = liftLogging valueType

instance EmptyStatement r stmt => EmptyStatement (LoggingFor r) stmt where
  emptyStmt = liftLogging emptyStmt

instance MultiStatement r stmt => MultiStatement (LoggingFor r) stmt where
  multi = liftLogging multi

instance ValueStatement r stmt val => ValueStatement (LoggingFor r) stmt val where
  valStmt = liftLogging valStmt

instance (Argument r val) => Argument (LoggingFor r) val where
  pointerArg = liftLogging pointerArg

instance (Reference r val) => Reference (LoggingFor r) val where
  makeRef = liftLogging makeRef
  maybeDeref = liftLogging maybeDeref

instance (Array r var val) => Array (LoggingFor r) var val where
  arrayElem = liftLogging arrayElem
  arrayLength = liftLogging arrayLength
  arrayCopy = liftLogging arrayCopy

instance (BinderSym r binder typ) => BinderSym (LoggingFor r) binder typ where
  binder = liftLogging binder

instance (BooleanExpression r val) => BooleanExpression (LoggingFor r) val where
  (?!) = liftLogging (?!)
  (?&&) = liftLogging (?&&)
  (?||) = liftLogging (?||)

instance (CommandLineArgs r val) => CommandLineArgs (LoggingFor r) val where
  arg = liftLogging arg
  argsList = liftLogging argsList
  argExists = liftLogging argExists

instance (CommentStatement r stmt) => CommentStatement (LoggingFor r) stmt where
  comment = liftLogging comment

instance (Comparison r val) => Comparison (LoggingFor r) val where
  (?<) = liftLogging (?<)
  (?<=) = liftLogging (?<=)
  (?>) = liftLogging (?>)
  (?>=) = liftLogging (?>=)
  (?==) = liftLogging (?==)
  (?!=) = liftLogging (?!=)

instance (BlockSym r block stmt) => BlockSym (LoggingFor r) block stmt where
  block = liftLogging block

instance (BodySym r bod block) => BodySym (LoggingFor r) bod block where
  body = liftLogging body
  addComments = liftLogging addComments

instance (ControlStatement r bod stmt var val) => ControlStatement (LoggingFor r) bod stmt var val where
  break = liftLogging break
  continue = liftLogging continue
  returnStmt = liftLogging returnStmt
  throw = liftLogging throw
  ifCond = liftLogging ifCond
  switch = liftLogging switch
  ifExists = liftLogging ifExists
  for = liftLogging for
  forRange = liftLogging forRange
  forEach = liftLogging forEach
  while = liftLogging while
  tryCatch = liftLogging tryCatch
  assert = liftLogging assert

instance (ScopeSym r scope) => ScopeSym (LoggingFor r) scope where
  global = liftLogging global
  mainFn = liftLogging mainFn
  local = liftLogging local

instance (FuncAppStatement r stmt var val) => FuncAppStatement (LoggingFor r) stmt var val where
  inOutCall = liftLogging inOutCall
  extInOutCall = liftLogging extInOutCall

instance (InternalList r block var val) => InternalList (LoggingFor r) block var val where
  listSlice' = liftLogging listSlice'

instance (Literal r val typ) => Literal (LoggingFor r) val typ where
  litTrue = liftLogging litTrue
  litFalse = liftLogging litFalse
  litChar = liftLogging litChar
  litDouble = liftLogging litDouble
  litFloat = liftLogging litFloat
  litInt = liftLogging litInt
  litString = liftLogging litString
  litArray = liftLogging litArray
  litList = liftLogging litList
  litSet = liftLogging litSet

instance (MathConstant r val) => MathConstant (LoggingFor r) val where
  pi = liftLogging pi

instance (ParameterSym r param var) => ParameterSym (LoggingFor r) param var where
  param = liftLogging param
  pointerParam = liftLogging pointerParam

instance (VisibilitySym r vis) => VisibilitySym (LoggingFor r) vis where
  private = liftLogging private
  public = liftLogging public

instance (MethodSym r mthd vis param bod var typ) => MethodSym (LoggingFor r) mthd vis param bod var typ where
  docMain = liftLogging docMain
  function = liftLogging function
  mainFunction = liftLogging mainFunction
  docFunc = liftLogging docFunc
  inOutFunc = liftLogging inOutFunc
  docInOutFunc = liftLogging docInOutFunc

instance (NumericExpression r val) => NumericExpression (LoggingFor r) val where
  (#~) = liftLogging (#~)
  (#/^) = liftLogging (#/^)
  (#|) = liftLogging (#|)
  (#+) = liftLogging (#+)
  (#-) = liftLogging (#-)
  (#*) = liftLogging (#*)
  (#/) = liftLogging (#/)
  (#%) = liftLogging (#%)
  (#^) = liftLogging (#^)
  log = liftLogging log
  ln = liftLogging ln
  exp = liftLogging exp
  sin = liftLogging sin
  cos = liftLogging cos
  tan = liftLogging tan
  csc = liftLogging csc
  sec = liftLogging sec
  cot = liftLogging cot
  arcsin = liftLogging arcsin
  arccos = liftLogging arccos
  arctan = liftLogging arctan
  floor = liftLogging floor
  ceil = liftLogging ceil

instance (Set r val) => Set (LoggingFor r) val where
  contains = liftLogging contains
  setAdd = liftLogging setAdd
  setRemove = liftLogging setRemove
  setUnion = liftLogging setUnion

instance (UnRepr r contents) => UnRepr (LoggingFor r) contents where
  unRepr = unRepr . unLC

instance (ValueExpression r var val binder typ) => ValueExpression (LoggingFor r) var val binder typ where
  inlineIf = liftLogging inlineIf
  funcAppMixedArgs = liftLogging funcAppMixedArgs
  extFuncAppMixedArgs = liftLogging extFuncAppMixedArgs
  libFuncAppMixedArgs = liftLogging libFuncAppMixedArgs
  lambda = liftLogging lambda
  notNull = liftLogging notNull

instance (VariableElim r var typ) => VariableElim (LoggingFor r) var typ where
  variableName = liftLogging variableName
  variableType = liftLogging variableType

instance (VariableValue r var val) => VariableValue (LoggingFor r) var val where
  valueOf = liftLogging valueOf

instance (IndexTranslator r val) => IndexTranslator (LoggingFor r) val where
  intToIndex = liftLogging intToIndex
  indexToInt = liftLogging indexToInt

instance (NativeVector lang val typ) => NativeVector (LoggingFor lang) val typ where
  vecType = liftLogging vecType
  litVec = liftLogging litVec
  vecScale = liftLogging vecScale
  vecAdd = liftLogging vecAdd
  vecIndex = liftLogging vecIndex
  vecDot = liftLogging vecDot
  vecMag = liftLogging vecMag
  vecUnit = liftLogging vecUnit

-- GProc

instance (P.ProcProg r prg file mod mthd vis param bod block stmt var scope val binder typ) => P.ProcProg (LoggingFor r) prg file mod mthd vis param bod block stmt var scope val binder typ

instance (P.ModuleSym r mod mthd) => P.ModuleSym (LoggingFor r) mod mthd where
  buildModule = liftLogging P.buildModule

instance (P.FileSym r file mod) => P.FileSym (LoggingFor r) file mod where
  fileDoc = liftLogging P.fileDoc
  docMod = liftLogging P.docMod

instance (P.ProgramSym r prg file) => P.ProgramSym (LoggingFor r) prg file where
  prog = liftLogging P.prog

-- GOOL

instance (G.OOProg r prg file mod cls stvr mthd attch vis param bod block stmt var scope val binder typ) => G.OOProg (LoggingFor r) prg file mod cls stvr mthd attch vis param bod block stmt var scope val binder typ

instance (G.GetSet r var val) => G.GetSet (LoggingFor r) var val where
  get = liftLogging G.get
  set = liftLogging G.set

instance (G.InternalValueExp r var val typ) => G.InternalValueExp (LoggingFor r) var val typ where
  objMethodCallMixedArgs' = liftLogging G.objMethodCallMixedArgs'
  classMethodCallMixedArgs' = liftLogging G.classMethodCallMixedArgs'

instance (G.OOTypeSym r typ) => G.OOTypeSym (LoggingFor r) typ where
  obj = liftLogging G.obj

instance (G.OOVariableSym r var val typ) => G.OOVariableSym (LoggingFor r) var val typ where
  classVar = liftLogging G.classVar
  classConst = liftLogging G.classConst
  classVarAccess = liftLogging G.classVarAccess
  extClassVarAccess = liftLogging G.extClassVarAccess
  instanceVarAccess = liftLogging G.instanceVarAccess

instance
  ( DeclStatement (LoggingFor r) bod stmt var scope val
  , G.OODeclStatement r stmt var scope val
  ) => G.OODeclStatement (LoggingFor r) stmt var scope val where
  objDecDef = liftLogging G.objDecDef
  objDecNew = liftLogging G.objDecNew
  extObjDecNew = liftLogging G.extObjDecNew

instance (G.OOFuncAppStatement r stmt var val) => G.OOFuncAppStatement (LoggingFor r) stmt var val where
  selfInOutCall = liftLogging G.selfInOutCall

instance (G.OOValueExpression r var val typ) => G.OOValueExpression (LoggingFor r) var val typ where
  newObjMixedArgs = liftLogging G.newObjMixedArgs
  extNewObjMixedArgs = liftLogging G.extNewObjMixedArgs
  libNewObjMixedArgs = liftLogging G.libNewObjMixedArgs

instance (G.SelfSym r var) => G.SelfSym (LoggingFor r) var where
  self = liftLogging G.self

instance (G.OOFunctionSym r val typ) => G.OOFunctionSym (LoggingFor r) val typ where
  func = liftLogging G.func
  objAccess = liftLogging G.objAccess

instance (G.ObserverPattern r stmt typ) => G.ObserverPattern (LoggingFor r) stmt typ where
  notifyObservers = liftLogging G.notifyObservers

instance (G.AttachmentSym r attch) => G.AttachmentSym (LoggingFor r) attch where
  classLevel = liftLogging G.classLevel
  instanceLevel = liftLogging G.instanceLevel

instance (G.OOMethodSym r mthd attch vis param bod var val typ) => G.OOMethodSym (LoggingFor r) mthd attch vis param bod var val typ where
  method = liftLogging G.method
  getMethod = liftLogging G.getMethod
  setMethod = liftLogging G.setMethod
  constructor = liftLogging G.constructor
  inOutMethod = liftLogging G.inOutMethod
  docInOutMethod = liftLogging G.docInOutMethod

instance (G.StateVarSym r stvr attch vis var val) => G.StateVarSym (LoggingFor r) stvr attch vis var val where
  stateVar = liftLogging G.stateVar
  stateVarDef = liftLogging G.stateVarDef
  constVar = liftLogging G.constVar

instance (G.ClassSym r cls stvr mthd) => G.ClassSym (LoggingFor r) cls stvr mthd where
  buildClass = liftLogging G.buildClass
  extraClass = liftLogging G.extraClass
  implementingClass = liftLogging G.implementingClass
  docClass = liftLogging G.docClass

instance (G.ModuleSym r mod cls mthd) => G.ModuleSym (LoggingFor r) mod cls mthd where
  buildModule = liftLogging G.buildModule

instance (G.FileSym r file mod) => G.FileSym (LoggingFor r) file mod where
  fileDoc = liftLogging G.fileDoc
  docMod = liftLogging G.docMod

instance (G.ProgramSym r prg file) => G.ProgramSym (LoggingFor r) prg file where
  prog = liftLogging G.prog

instance (G.StrategyPattern r bod block var val) => G.StrategyPattern (LoggingFor r) bod block var val where
  runStrategy = liftLogging G.runStrategy
