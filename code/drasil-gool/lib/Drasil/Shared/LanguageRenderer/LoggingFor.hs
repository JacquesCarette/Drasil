{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}

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
varLogFile :: (VariableSym r) => SVariable r
varLogFile = var "outfile" outfile

valLogFile :: (VariableValue r) => SValue r
valLogFile = valueOf varLogFile

-- TODO [Brandon Bosman, 06/19/2026]: This should be passed down from drasil-code
logName :: (Literal r) => SValue r
logName = litString "log.txt"

logVarUpdate :: (SharedProg r vis smt) => SVariable (LoggingFor r) -> [MS (r smt)]
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

instance (SharedProg r vis smt) => AssignStatement (LoggingFor r) smt where
  (&-=) = liftLogging (&-=)
  (&+=) = liftLogging (&+=)
  (&++) = liftLogging (&++)
  (&--) = liftLogging (&--)
  assign x e = liftLogging $ multi $
    assign (lowerLogging x) (lowerLogging e)
    : logVarUpdate x

instance (List r smt) => List (LoggingFor r) smt where
  listSize = liftLogging listSize
  listAdd = liftLogging listAdd
  listAppend = liftLogging listAppend
  listAccess = liftLogging listAccess
  listSet = liftLogging listSet -- TODO [Brandon Bosman, 06/23/2026]: Add logging
                                -- (Can't right now because RC.value isn't exposed)
  indexOf = liftLogging indexOf

instance (SharedProg r vis smt) => DeclStatement (LoggingFor r) smt where
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

instance (SharedProg r vis smt) => IOStatement (LoggingFor r) smt where
  print = liftLogging print
  printLn = liftLogging printLn
  printStr = liftLogging printStr
  printStrLn = liftLogging printStrLn
  printFile = liftLogging printFile
  printFileLn = liftLogging printFileLn
  printFileStr = liftLogging printFileStr
  printFileStrLn = liftLogging printFileStrLn
  getInput vr = liftLogging $ multi $
    getInput (lowerLogging vr) : logVarUpdate vr
  discardInput = liftLogging discardInput
  getFileInput file vr = liftLogging $ multi $
    getFileInput (lowerLogging file) (lowerLogging vr)
    : logVarUpdate vr
  discardFileInput = liftLogging discardFileInput
  openFileR = liftLogging openFileR
  openFileW = liftLogging openFileW
  openFileA = liftLogging openFileA
  closeFile = liftLogging closeFile
  getFileInputLine = liftLogging getFileInputLine
  discardFileLine = liftLogging discardFileLine
  getFileInputAll = liftLogging getFileInputAll

instance (SharedProg r vis smt) => StringStatement (LoggingFor r) smt where
  stringSplit chr vr str  = liftLogging $
    stringSplit (lowerLogging chr) (lowerLogging vr) (lowerLogging str)
  stringListVals vrs strs  = liftLogging $
    stringListVals (lowerLogging vrs) (lowerLogging strs)
  stringListLists vrs strs = liftLogging $ multi $
    stringListLists (lowerLogging vrs) (lowerLogging strs)
    : concatMap logVarUpdate vrs

-- SharedProg Boilerplate

instance (SharedProg r vis smt) => SharedProg (LoggingFor r) vis smt

instance (VariableSym r) => VariableSym (LoggingFor r) where
  type Variable (LoggingFor r) = Variable r
  var = liftLogging var
  constant = liftLogging constant
  extVar = liftLogging extVar

instance (TypeSym r) => TypeSym (LoggingFor r) where
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

instance (TypeElim r) => TypeElim (LoggingFor r) where
  getCodeType = liftLogging getCodeType

instance (ValueSym r) => ValueSym (LoggingFor r) where
  type Value (LoggingFor r) = Value r
  valueType = liftLogging valueType

instance StatementSym r smt => StatementSym (LoggingFor r) smt where
  valStmt = liftLogging valStmt
  emptyStmt = liftLogging emptyStmt
  multi = liftLogging multi

instance (Argument r) => Argument (LoggingFor r) where
  pointerArg = liftLogging pointerArg

instance (Reference r) => Reference (LoggingFor r) where
  makeRef = liftLogging makeRef
  maybeDeref = liftLogging maybeDeref

instance (Array r) => Array (LoggingFor r) where
  arrayElem = liftLogging arrayElem
  arrayLength = liftLogging arrayLength
  arrayCopy = liftLogging arrayCopy

instance (BinderSym r) => BinderSym (LoggingFor r) where
  binder = liftLogging binder

instance (BooleanExpression r) => BooleanExpression (LoggingFor r) where
  (?!) = liftLogging (?!)
  (?&&) = liftLogging (?&&)
  (?||) = liftLogging (?||)

instance (CommandLineArgs r) => CommandLineArgs (LoggingFor r) where
  arg = liftLogging arg
  argsList = liftLogging argsList
  argExists = liftLogging argExists

instance (CommentStatement r smt) => CommentStatement (LoggingFor r) smt where
  comment = liftLogging comment

instance (Comparison r) => Comparison (LoggingFor r) where
  (?<) = liftLogging (?<)
  (?<=) = liftLogging (?<=)
  (?>) = liftLogging (?>)
  (?>=) = liftLogging (?>=)
  (?==) = liftLogging (?==)
  (?!=) = liftLogging (?!=)

instance (BlockSym r smt) => BlockSym (LoggingFor r) smt where
  type Block (LoggingFor r) = Block r
  block = liftLogging block

instance (BodySym r smt) => BodySym (LoggingFor r) smt where
  type Body (LoggingFor r) = Body r
  body = liftLogging body
  addComments = liftLogging addComments

instance (ControlStatement r smt) => ControlStatement (LoggingFor r) smt where
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

instance (FuncAppStatement r smt) => FuncAppStatement (LoggingFor r) smt where
  inOutCall = liftLogging inOutCall
  extInOutCall = liftLogging extInOutCall

instance (FunctionSym r) => FunctionSym (LoggingFor r) where

instance (InternalList r) => InternalList (LoggingFor r) where
  listSlice' = liftLogging listSlice'

instance (Literal r) => Literal (LoggingFor r) where
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

instance (MathConstant r) => MathConstant (LoggingFor r) where
  pi = liftLogging pi

instance (ParameterSym r) => ParameterSym (LoggingFor r) where
  param = liftLogging param
  pointerParam = liftLogging pointerParam

instance (VisibilitySym r vis) => VisibilitySym (LoggingFor r) vis where
  private = liftLogging private
  public = liftLogging public

instance (MethodSym r vis smt) => MethodSym (LoggingFor r) vis smt where
  type Method (LoggingFor r) = Method r
  docMain = liftLogging docMain
  function = liftLogging function
  mainFunction = liftLogging mainFunction
  docFunc = liftLogging docFunc
  inOutFunc = liftLogging inOutFunc
  docInOutFunc = liftLogging docInOutFunc

instance (NumericExpression r) => NumericExpression (LoggingFor r) where
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

instance (Set r) => Set (LoggingFor r) where
  contains = liftLogging contains
  setAdd = liftLogging setAdd
  setRemove = liftLogging setRemove
  setUnion = liftLogging setUnion

instance (UnRepr r contents) => UnRepr (LoggingFor r) contents where
  unRepr = unRepr . unLC

instance (ValueExpression r) => ValueExpression (LoggingFor r) where
  inlineIf = liftLogging inlineIf
  funcAppMixedArgs = liftLogging funcAppMixedArgs
  extFuncAppMixedArgs = liftLogging extFuncAppMixedArgs
  libFuncAppMixedArgs = liftLogging libFuncAppMixedArgs
  lambda = liftLogging lambda
  notNull = liftLogging notNull

instance (VariableElim r) => VariableElim (LoggingFor r) where
  variableName = liftLogging variableName
  variableType = liftLogging variableType

instance (VariableValue r) => VariableValue (LoggingFor r) where
  valueOf = liftLogging valueOf

instance (IndexTranslator r) => IndexTranslator (LoggingFor r) where
  intToIndex = liftLogging intToIndex
  indexToInt = liftLogging indexToInt

instance (NativeVector lang) => NativeVector (LoggingFor lang) where
  vecScale = liftLogging vecScale
  vecAdd = liftLogging vecAdd
  vecIndex = liftLogging vecIndex
  vecDot = liftLogging vecDot
  vecMag = liftLogging vecMag
  vecUnit = liftLogging vecUnit

-- GProc

instance (P.ProcProg r vis smt) => P.ProcProg (LoggingFor r) vis smt

instance (P.ModuleSym r vis smt) => P.ModuleSym (LoggingFor r) vis smt where
  type Module (LoggingFor r) = P.Module r
  buildModule = liftLogging P.buildModule

instance (P.FileSym r vis smt) => P.FileSym (LoggingFor r) vis smt where
  type File (LoggingFor r) = P.File r
  fileDoc = liftLogging P.fileDoc
  docMod = liftLogging P.docMod

instance (P.ProgramSym r vis smt) => P.ProgramSym (LoggingFor r) vis smt where
  type Program (LoggingFor r) = P.Program r
  prog = liftLogging P.prog

-- GOOL

instance (G.OOProg r vis smt) => G.OOProg (LoggingFor r) vis smt

instance (G.GetSet r) => G.GetSet (LoggingFor r) where
  get = liftLogging G.get
  set = liftLogging G.set

instance (G.InternalValueExp r) => G.InternalValueExp (LoggingFor r) where
  objMethodCallMixedArgs' = liftLogging G.objMethodCallMixedArgs'
  classMethodCallMixedArgs' = liftLogging G.classMethodCallMixedArgs'

instance (G.OOTypeSym r) => G.OOTypeSym (LoggingFor r) where
  obj = liftLogging G.obj

instance (G.OOVariableSym r) => G.OOVariableSym (LoggingFor r) where
  classVar = liftLogging G.classVar
  classConst = liftLogging G.classConst
  classVarAccess = liftLogging G.classVarAccess
  extClassVarAccess = liftLogging G.extClassVarAccess
  instanceVarAccess = liftLogging G.instanceVarAccess

instance (DeclStatement (LoggingFor r) smt, G.OODeclStatement r smt) =>
    G.OODeclStatement (LoggingFor r) smt where
  objDecDef = liftLogging G.objDecDef
  objDecNew = liftLogging G.objDecNew
  extObjDecNew = liftLogging G.extObjDecNew

instance (G.OOFuncAppStatement r smt) => G.OOFuncAppStatement (LoggingFor r) smt where
  selfInOutCall = liftLogging G.selfInOutCall

instance (G.OOValueSym r) => G.OOValueSym (LoggingFor r) where

instance (G.OOValueExpression r) => G.OOValueExpression (LoggingFor r) where
  newObjMixedArgs = liftLogging G.newObjMixedArgs
  extNewObjMixedArgs = liftLogging G.extNewObjMixedArgs
  libNewObjMixedArgs = liftLogging G.libNewObjMixedArgs

instance (G.SelfSym r) => G.SelfSym (LoggingFor r) where
  self = liftLogging G.self

instance (G.OOVariableValue r) => G.OOVariableValue (LoggingFor r)

instance (G.OOFunctionSym r) => G.OOFunctionSym (LoggingFor r) where
  func = liftLogging G.func
  objAccess = liftLogging G.objAccess

instance (G.ObserverPattern r smt) => G.ObserverPattern (LoggingFor r) smt where
  notifyObservers = liftLogging G.notifyObservers

instance (G.AttachmentSym r) => G.AttachmentSym (LoggingFor r) where
  type Attachment (LoggingFor r) = G.Attachment r
  classLevel = liftLogging G.classLevel
  instanceLevel = liftLogging G.instanceLevel

instance (G.OOMethodSym r vis smt) => G.OOMethodSym (LoggingFor r) vis smt where
  method = liftLogging G.method
  getMethod = liftLogging G.getMethod
  setMethod = liftLogging G.setMethod
  constructor = liftLogging G.constructor
  inOutMethod = liftLogging G.inOutMethod
  docInOutMethod = liftLogging G.docInOutMethod

instance (G.StateVarSym r vis) => G.StateVarSym (LoggingFor r) vis where
  type StateVar (LoggingFor r) = G.StateVar r
  stateVar = liftLogging G.stateVar
  stateVarDef = liftLogging G.stateVarDef
  constVar = liftLogging G.constVar

instance (G.ClassSym r vis smt) => G.ClassSym (LoggingFor r) vis smt where
  type Class (LoggingFor r) = G.Class r
  buildClass = liftLogging G.buildClass
  extraClass = liftLogging G.extraClass
  implementingClass = liftLogging G.implementingClass
  docClass = liftLogging G.docClass

instance (G.ModuleSym r vis smt) => G.ModuleSym (LoggingFor r) vis smt where
  type Module (LoggingFor r) = G.Module r
  buildModule = liftLogging G.buildModule

instance (G.FileSym r vis smt) => G.FileSym (LoggingFor r) vis smt where
  type File (LoggingFor r) = G.File r
  fileDoc = liftLogging G.fileDoc
  docMod = liftLogging G.docMod

instance (G.ProgramSym r vis smt) => G.ProgramSym (LoggingFor r) vis smt where
  type Program (LoggingFor r) = G.Program r
  prog = liftLogging G.prog

instance (G.StrategyPattern r smt) => G.StrategyPattern (LoggingFor r) smt where
  runStrategy = liftLogging G.runStrategy
