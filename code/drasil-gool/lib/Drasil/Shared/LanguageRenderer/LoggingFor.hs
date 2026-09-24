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
varLogFile :: (TypeSym r typ, VariableSym r typ) => SVariable r
varLogFile = var "outfile" outfile

valLogFile :: (TypeSym r typ, VariableSym r typ, VariableValue r val) => VS (r val)
valLogFile = valueOf varLogFile

-- TODO [Brandon Bosman, 06/19/2026]: This should be passed down from drasil-code
logName :: (Literal r typ val) => VS (r val)
logName = litString "log.txt"

logVarUpdate
  ::
    ( FileHandling r val stmt
    , PrintFile r val stmt
    , TypeSym r typ
    , VariableSym r typ
    , VariableValue r val
    , VariableElim r typ
    , Literal r typ val
    )
  => SVariable (LoggingFor r) -> [MS (r stmt)]
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
  , AssignStatement r val stmt
  , FileHandling r val stmt
  , PrintFile r val stmt
  , TypeSym r typ
  , VariableSym r typ
  , VariableValue r val
  , VariableElim r typ
  , Literal r typ val
  ) => AssignStatement (LoggingFor r) val stmt where
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

instance (ListStatement r val stmt) => ListStatement (LoggingFor r) val stmt where
  listAdd = liftLogging listAdd
  listAppend = liftLogging listAppend
  listSet = liftLogging listSet -- TODO [Brandon Bosman, 06/23/2026]: Add logging
                                -- (Can't right now because RC.value isn't exposed)

instance
  ( MultiStatement r stmt
  , DeclStatement r val stmt bod
  , FileHandling r val stmt
  , PrintFile r val stmt
  , TypeSym r typ
  , VariableSym r typ
  , VariableValue r val
  , VariableElim r typ
  , Literal r typ val
  ) => DeclStatement (LoggingFor r) val stmt bod where
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

instance (PrintConsole r val stmt) => PrintConsole (LoggingFor r) val stmt where
  print = liftLogging print
  printLn = liftLogging printLn
  printStr = liftLogging printStr
  printStrLn = liftLogging printStrLn

instance
  ( MultiStatement r stmt
  , FileHandling r val stmt
  , PrintFile r val stmt
  , ReadConsole r stmt
  , TypeSym r typ
  , VariableSym r typ
  , VariableValue r val
  , VariableElim r typ
  , Literal r typ val
  ) => ReadConsole (LoggingFor r) stmt where
  getInput vr = liftLogging $ multi $
    getInput (lowerLogging vr) : logVarUpdate vr
  discardInput = liftLogging discardInput

instance (FileHandling r val stmt) => FileHandling (LoggingFor r) val stmt where
  openFileR = liftLogging openFileR
  openFileW = liftLogging openFileW
  openFileA = liftLogging openFileA
  closeFile = liftLogging closeFile

instance (PrintFile r val stmt) => PrintFile (LoggingFor r) val stmt where
  printFile = liftLogging printFile
  printFileLn = liftLogging printFileLn
  printFileStr = liftLogging printFileStr
  printFileStrLn = liftLogging printFileStrLn

instance
  ( MultiStatement r stmt
  , FileHandling r val stmt
  , PrintFile r val stmt
  , ReadFile r val stmt
  , TypeSym r typ
  , VariableSym r typ
  , VariableValue r val
  , VariableElim r typ
  , Literal r typ val
  ) => ReadFile (LoggingFor r) val stmt where
  getFileInput file vr = liftLogging $ multi $
    getFileInput (lowerLogging file) (lowerLogging vr)
    : logVarUpdate vr
  discardFileInput = liftLogging discardFileInput
  getFileInputLine = liftLogging getFileInputLine
  discardFileLine = liftLogging discardFileLine
  getFileInputAll = liftLogging getFileInputAll

instance
  ( MultiStatement r stmt
  , StringStatement r val stmt
  , FileHandling r val stmt
  , PrintFile r val stmt
  , TypeSym r typ
  , VariableSym r typ
  , VariableValue r val
  , VariableElim r typ
  , Literal r typ val
  ) => StringStatement (LoggingFor r) val stmt where
  stringSplit chr vr str  = liftLogging $
    stringSplit (lowerLogging chr) (lowerLogging vr) (lowerLogging str)
  stringListVals vrs strs  = liftLogging $
    stringListVals (lowerLogging vrs) (lowerLogging strs)
  stringListLists vrs strs = liftLogging $ multi $
    stringListLists (lowerLogging vrs) (lowerLogging strs)
    : concatMap logVarUpdate vrs

-- SharedProg Boilerplate

instance (VariableSym r typ) => VariableSym (LoggingFor r) typ where
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

instance (ValueSym r typ val) => ValueSym (LoggingFor r) typ val where
  valueType = liftLogging valueType

instance EmptyStatement r stmt => EmptyStatement (LoggingFor r) stmt where
  emptyStmt = liftLogging emptyStmt

instance MultiStatement r stmt => MultiStatement (LoggingFor r) stmt where
  multi = liftLogging multi

instance ValueStatement r val stmt => ValueStatement (LoggingFor r) val stmt where
  valStmt = liftLogging valStmt

instance (Argument r val) => Argument (LoggingFor r) val where
  pointerArg = liftLogging pointerArg

instance (Reference r val) => Reference (LoggingFor r) val where
  makeRef = liftLogging makeRef
  maybeDeref = liftLogging maybeDeref

instance (Array r val) => Array (LoggingFor r) val where
  arrayElem = liftLogging arrayElem
  arrayLength = liftLogging arrayLength
  arrayCopy = liftLogging arrayCopy

instance (BinderSym r typ) => BinderSym (LoggingFor r) typ where
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

instance (ControlStatement r val stmt bod) => ControlStatement (LoggingFor r) val stmt bod where
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

instance (ScopeSym r) => ScopeSym (LoggingFor r) where
  global = liftLogging global
  mainFn = liftLogging mainFn
  local = liftLogging local

instance (FuncAppStatement r val stmt) => FuncAppStatement (LoggingFor r) val stmt where
  inOutCall = liftLogging inOutCall
  extInOutCall = liftLogging extInOutCall

instance (InternalList r val block) => InternalList (LoggingFor r) val block where
  listSlice' = liftLogging listSlice'

instance (Literal r typ val) => Literal (LoggingFor r) typ val where
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

instance (ParameterSym r) => ParameterSym (LoggingFor r) where
  param = liftLogging param
  pointerParam = liftLogging pointerParam

instance (VisibilitySym r vis) => VisibilitySym (LoggingFor r) vis where
  private = liftLogging private
  public = liftLogging public

instance (MethodSym r vis typ mthd bod) => MethodSym (LoggingFor r) vis typ mthd bod where
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

instance (ValueExpression r typ val) => ValueExpression (LoggingFor r) typ val where
  inlineIf = liftLogging inlineIf
  funcAppMixedArgs = liftLogging funcAppMixedArgs
  extFuncAppMixedArgs = liftLogging extFuncAppMixedArgs
  libFuncAppMixedArgs = liftLogging libFuncAppMixedArgs
  lambda = liftLogging lambda
  notNull = liftLogging notNull

instance (VariableElim r typ) => VariableElim (LoggingFor r) typ where
  variableName = liftLogging variableName
  variableType = liftLogging variableType

instance (VariableValue r val) => VariableValue (LoggingFor r) val where
  valueOf = liftLogging valueOf

instance (IndexTranslator r val) => IndexTranslator (LoggingFor r) val where
  intToIndex = liftLogging intToIndex
  indexToInt = liftLogging indexToInt

instance (NativeVector lang typ val) => NativeVector (LoggingFor lang) typ val where
  vecType = liftLogging vecType
  litVec = liftLogging litVec
  vecScale = liftLogging vecScale
  vecAdd = liftLogging vecAdd
  vecIndex = liftLogging vecIndex
  vecDot = liftLogging vecDot
  vecMag = liftLogging vecMag
  vecUnit = liftLogging vecUnit

-- GProc

instance (P.ProcProg r vis typ val stmt mthd prg file mod bod block) => P.ProcProg (LoggingFor r) vis typ val stmt mthd prg file mod bod block

instance (P.ModuleSym r mod mthd) => P.ModuleSym (LoggingFor r) mod mthd where
  buildModule = liftLogging P.buildModule

instance (P.FileSym r file mod) => P.FileSym (LoggingFor r) file mod where
  fileDoc = liftLogging P.fileDoc
  docMod = liftLogging P.docMod

instance (P.ProgramSym r prg file) => P.ProgramSym (LoggingFor r) prg file where
  prog = liftLogging P.prog

-- GOOL

instance (G.OOProg r vis typ val stmt mthd stvr attch prg file mod bod block) => G.OOProg (LoggingFor r) vis typ val stmt mthd stvr attch prg file mod bod block

instance (G.GetSet r val) => G.GetSet (LoggingFor r) val where
  get = liftLogging G.get
  set = liftLogging G.set

instance (G.InternalValueExp r typ val) => G.InternalValueExp (LoggingFor r) typ val where
  objMethodCallMixedArgs' = liftLogging G.objMethodCallMixedArgs'
  classMethodCallMixedArgs' = liftLogging G.classMethodCallMixedArgs'

instance (G.OOTypeSym r typ) => G.OOTypeSym (LoggingFor r) typ where
  obj = liftLogging G.obj

instance (G.OOVariableSym r typ val) => G.OOVariableSym (LoggingFor r) typ val where
  classVar = liftLogging G.classVar
  classConst = liftLogging G.classConst
  classVarAccess = liftLogging G.classVarAccess
  extClassVarAccess = liftLogging G.extClassVarAccess
  instanceVarAccess = liftLogging G.instanceVarAccess

instance (DeclStatement (LoggingFor r) val stmt bod, G.OODeclStatement r val stmt) =>
    G.OODeclStatement (LoggingFor r) val stmt where
  objDecDef = liftLogging G.objDecDef
  objDecNew = liftLogging G.objDecNew
  extObjDecNew = liftLogging G.extObjDecNew

instance (G.OOFuncAppStatement r val stmt) => G.OOFuncAppStatement (LoggingFor r) val stmt where
  selfInOutCall = liftLogging G.selfInOutCall

instance (G.OOValueExpression r typ val) => G.OOValueExpression (LoggingFor r) typ val where
  newObjMixedArgs = liftLogging G.newObjMixedArgs
  extNewObjMixedArgs = liftLogging G.extNewObjMixedArgs
  libNewObjMixedArgs = liftLogging G.libNewObjMixedArgs

instance (G.SelfSym r) => G.SelfSym (LoggingFor r) where
  self = liftLogging G.self

instance (G.OOFunctionSym r typ val) => G.OOFunctionSym (LoggingFor r) typ val where
  func = liftLogging G.func
  objAccess = liftLogging G.objAccess

instance (G.ObserverPattern r typ stmt) => G.ObserverPattern (LoggingFor r) typ stmt where
  notifyObservers = liftLogging G.notifyObservers

instance (G.AttachmentSym r attch) => G.AttachmentSym (LoggingFor r) attch where
  classLevel = liftLogging G.classLevel
  instanceLevel = liftLogging G.instanceLevel

instance (G.OOMethodSym r vis typ val mthd attch bod) => G.OOMethodSym (LoggingFor r) vis typ val mthd attch bod where
  method = liftLogging G.method
  getMethod = liftLogging G.getMethod
  setMethod = liftLogging G.setMethod
  constructor = liftLogging G.constructor
  inOutMethod = liftLogging G.inOutMethod
  docInOutMethod = liftLogging G.docInOutMethod

instance (G.StateVarSym r vis val stvr attch) => G.StateVarSym (LoggingFor r) vis val stvr attch where
  stateVar = liftLogging G.stateVar
  stateVarDef = liftLogging G.stateVarDef
  constVar = liftLogging G.constVar

instance (G.ClassSym r mthd stvr) => G.ClassSym (LoggingFor r) mthd stvr where
  buildClass = liftLogging G.buildClass
  extraClass = liftLogging G.extraClass
  implementingClass = liftLogging G.implementingClass
  docClass = liftLogging G.docClass

instance (G.ModuleSym r mod mthd) => G.ModuleSym (LoggingFor r) mod mthd where
  buildModule = liftLogging G.buildModule

instance (G.FileSym r file mod) => G.FileSym (LoggingFor r) file mod where
  fileDoc = liftLogging G.fileDoc
  docMod = liftLogging G.docMod

instance (G.ProgramSym r prg file) => G.ProgramSym (LoggingFor r) prg file where
  prog = liftLogging G.prog

instance (G.StrategyPattern r val bod block) => G.StrategyPattern (LoggingFor r) val bod block where
  runStrategy = liftLogging G.runStrategy
