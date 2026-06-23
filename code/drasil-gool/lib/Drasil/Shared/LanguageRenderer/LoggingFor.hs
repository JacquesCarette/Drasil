{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

newtype LoggingFor (lang :: Type -> Type) a = LC {unLC :: lang a}
  deriving newtype (Functor, Applicative, Monad)

class LiftLogging u l | l -> u where
  liftLogging :: u -> l
  lowerLogging :: l -> u

instance (LiftLogging u1 l1, LiftLogging u2 l2) => LiftLogging (u1 -> u2) (l1 -> l2) where
  liftLogging k = liftLogging . k . lowerLogging
  lowerLogging k = lowerLogging . k . liftLogging

instance LiftLogging (lang a) (LoggingFor lang a) where
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

logVarUpdate :: (SharedProg lang) => SVariable (LoggingFor lang) -> [MSStatement lang]
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

instance (SharedProg lang) => AssignStatement (LoggingFor lang) where
  (&-=) = liftLogging (&-=)
  (&+=) = liftLogging (&+=)
  (&++) = liftLogging (&++)
  (&--) = liftLogging (&--)
  assign x e = liftLogging $ multi $ assign (lowerLogging <$> x) (lowerLogging <$> e) : logVarUpdate x

instance (List lang) => List (LoggingFor lang) where
  listSize = liftLogging listSize
  listAdd = liftLogging listAdd
  listAppend = liftLogging listAppend
  listAccess = liftLogging listAccess
  listSet = liftLogging listSet -- TODO [Brandon Bosman, 06/23/2026]: Add logging
  indexOf = liftLogging indexOf

instance (DeclStatement lang) => DeclStatement (LoggingFor lang) where
  varDec = liftLogging varDec
  varDecDef = liftLogging varDecDef -- TODO [Brandon Bosman, 06/23/2026]: Add logging
  listDec = liftLogging listDec
  listDecDef = liftLogging listDecDef -- TODO [Brandon Bosman, 06/23/2026]: Add logging
  setDec = liftLogging setDec
  setDecDef = liftLogging setDecDef -- TODO [Brandon Bosman, 06/23/2026]: Add logging
  arrayDec = liftLogging arrayDec
  arrayDecDef = liftLogging arrayDecDef -- TODO [Brandon Bosman, 06/23/2026]: Add logging
  constDecDef = liftLogging constDecDef -- TODO [Brandon Bosman, 06/23/2026]: Add logging
  funcDecDef = liftLogging funcDecDef

instance (IOStatement lang) => IOStatement (LoggingFor lang) where
  print = liftLogging print
  printLn = liftLogging printLn
  printStr = liftLogging printStr
  printStrLn = liftLogging printStrLn
  printFile = liftLogging printFile
  printFileLn = liftLogging printFileLn
  printFileStr = liftLogging printFileStr
  printFileStrLn = liftLogging printFileStrLn
  getInput = liftLogging getInput -- TODO [Brandon Bosman, 06/23/2026]: Add logging
  discardInput = liftLogging discardInput
  getFileInput = liftLogging getFileInput -- TODO [Brandon Bosman, 06/23/2026]: Add logging
  discardFileInput = liftLogging discardFileInput
  openFileR = liftLogging openFileR
  openFileW = liftLogging openFileW
  openFileA = liftLogging openFileA
  closeFile = liftLogging closeFile
  getFileInputLine = liftLogging getFileInputLine -- TODO [Brandon Bosman, 06/23/2026]: Add logging
  discardFileLine = liftLogging discardFileLine
  getFileInputAll = liftLogging getFileInputAll -- TODO [Brandon Bosman, 06/23/2026]: Add logging

-- SharedProg Boilerplate

instance (SharedProg lang) => SharedProg (LoggingFor lang)

instance (VariableSym lang) => VariableSym (LoggingFor lang) where
  type Variable (LoggingFor lang) = Variable lang
  var = liftLogging var
  constant = liftLogging constant
  extVar = liftLogging extVar

instance (TypeSym lang) => TypeSym (LoggingFor lang) where
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

instance (ValueSym lang) => ValueSym (LoggingFor lang) where
  type Value (LoggingFor lang) = Value lang
  valueType = liftLogging valueType

instance StatementSym lang => StatementSym (LoggingFor lang) where
  type Statement (LoggingFor lang) = Statement lang
  valStmt = liftLogging valStmt
  emptyStmt = liftLogging emptyStmt
  multi = liftLogging multi

instance (Argument lang) => Argument (LoggingFor lang) where
  pointerArg = liftLogging pointerArg

instance (Array lang) => Array (LoggingFor lang) where
  arrayElem = liftLogging arrayElem
  arrayLength = liftLogging arrayLength
  arrayCopy = liftLogging arrayCopy

instance (BinderSym lang) => BinderSym (LoggingFor lang) where
  binder = liftLogging binder

instance (BooleanExpression lang) => BooleanExpression (LoggingFor lang) where
  (?!) = liftLogging (?!)
  (?&&) = liftLogging (?&&)
  (?||) = liftLogging (?||)

instance (CommandLineArgs lang) => CommandLineArgs (LoggingFor lang) where
  arg = liftLogging arg
  argsList = liftLogging argsList
  argExists = liftLogging argExists

instance (CommentStatement lang) => CommentStatement (LoggingFor lang) where
  comment = liftLogging comment

instance (Comparison lang) => Comparison (LoggingFor lang) where
  (?<) = liftLogging (?<)
  (?<=) = liftLogging (?<=)
  (?>) = liftLogging (?>)
  (?>=) = liftLogging (?>=)
  (?==) = liftLogging (?==)
  (?!=) = liftLogging (?!=)

instance (BlockSym lang) => BlockSym (LoggingFor lang) where
  type Block (LoggingFor lang) = Block lang
  block = liftLogging block

instance (BodySym lang) => BodySym (LoggingFor lang) where
  type Body (LoggingFor lang) = Body lang
  body = liftLogging body
  addComments = liftLogging addComments

instance (ControlStatement lang) => ControlStatement (LoggingFor lang) where
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

instance (ScopeSym lang) => ScopeSym (LoggingFor lang) where
  global = liftLogging global
  mainFn = liftLogging mainFn
  local = liftLogging local

instance (FuncAppStatement lang) => FuncAppStatement (LoggingFor lang) where
  inOutCall = liftLogging inOutCall
  extInOutCall = liftLogging extInOutCall

instance (FunctionSym lang) => FunctionSym (LoggingFor lang) where
  type Function (LoggingFor lang) = Function lang

instance (InternalList lang) => InternalList (LoggingFor lang) where
  listSlice' = liftLogging listSlice'

instance (Literal lang) => Literal (LoggingFor lang) where
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

instance (StringStatement lang) => StringStatement (LoggingFor lang) where
  stringSplit = liftLogging stringSplit
  stringListVals = liftLogging stringListVals
  stringListLists = liftLogging stringListLists

instance (MathConstant lang) => MathConstant (LoggingFor lang) where
  pi = liftLogging pi

instance (ParameterSym lang) => ParameterSym (LoggingFor lang) where
  type Parameter (LoggingFor lang) = Parameter lang
  param = liftLogging param
  pointerParam = liftLogging pointerParam

instance (VisibilitySym lang) => VisibilitySym (LoggingFor lang) where
  type Visibility (LoggingFor lang) = Visibility lang
  private = liftLogging private
  public = liftLogging public

instance (MethodSym lang) => MethodSym (LoggingFor lang) where
  type Method (LoggingFor lang) = Method lang
  docMain = liftLogging docMain
  function = liftLogging function
  mainFunction = liftLogging mainFunction
  docFunc = liftLogging docFunc
  inOutFunc = liftLogging inOutFunc
  docInOutFunc = liftLogging docInOutFunc

instance (NumericExpression lang) => NumericExpression (LoggingFor lang) where
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

instance (Set lang) => Set (LoggingFor lang) where
  contains = liftLogging contains
  setAdd = liftLogging setAdd
  setRemove = liftLogging setRemove
  setUnion = liftLogging setUnion

instance (UnRepr lang contents) => UnRepr (LoggingFor lang) contents where
  unRepr = unRepr . unLC

instance (ValueExpression lang) => ValueExpression (LoggingFor lang) where
  inlineIf = liftLogging inlineIf
  funcAppMixedArgs = liftLogging funcAppMixedArgs
  extFuncAppMixedArgs = liftLogging extFuncAppMixedArgs
  libFuncAppMixedArgs = liftLogging libFuncAppMixedArgs
  lambda = liftLogging lambda
  notNull = liftLogging notNull

instance (VariableElim lang) => VariableElim (LoggingFor lang) where
  variableName = liftLogging variableName
  variableType = liftLogging variableType

instance (VariableValue lang) => VariableValue (LoggingFor lang) where
  valueOf = liftLogging valueOf

instance (IndexTranslator lang) => IndexTranslator (LoggingFor lang) where
  intToIndex = liftLogging intToIndex
  indexToInt = liftLogging indexToInt

-- GProc

instance (P.ProcProg lang) => P.ProcProg (LoggingFor lang)

instance (P.ModuleSym lang) => P.ModuleSym (LoggingFor lang) where
  type Module (LoggingFor lang) = P.Module lang
  buildModule = liftLogging P.buildModule

instance (P.FileSym lang) => P.FileSym (LoggingFor lang) where
  type File (LoggingFor lang) = P.File lang
  fileDoc = liftLogging P.fileDoc
  docMod = liftLogging P.docMod

instance (P.ProgramSym lang) => P.ProgramSym (LoggingFor lang) where
  type Program (LoggingFor lang) = P.Program lang
  prog = liftLogging P.prog

-- GOOL

instance (G.OOProg lang) => G.OOProg (LoggingFor lang)

instance (G.GetSet lang) => G.GetSet (LoggingFor lang) where
  get = liftLogging G.get
  set = liftLogging G.set

instance (G.InternalValueExp lang) => G.InternalValueExp (LoggingFor lang) where
  objMethodCallMixedArgs' = liftLogging G.objMethodCallMixedArgs'
  classMethodCallMixedArgs' = liftLogging G.classMethodCallMixedArgs'

instance (G.OOTypeSym lang) => G.OOTypeSym (LoggingFor lang) where
  obj = liftLogging G.obj

instance (G.OOVariableSym lang) => G.OOVariableSym (LoggingFor lang) where
  classVar = liftLogging G.classVar
  classConst = liftLogging G.classConst
  classVarAccess = liftLogging G.classVarAccess
  extClassVarAccess = liftLogging G.extClassVarAccess
  instanceVarAccess = liftLogging G.instanceVarAccess

instance (G.OODeclStatement lang) => G.OODeclStatement (LoggingFor lang) where
  objDecDef = liftLogging G.objDecDef
  objDecNew = liftLogging G.objDecNew
  extObjDecNew = liftLogging G.extObjDecNew

instance (G.OOFuncAppStatement lang) => G.OOFuncAppStatement (LoggingFor lang) where
  selfInOutCall = liftLogging G.selfInOutCall

instance (G.OOValueSym lang) => G.OOValueSym (LoggingFor lang) where

instance (G.OOValueExpression lang) => G.OOValueExpression (LoggingFor lang) where
  newObjMixedArgs = liftLogging G.newObjMixedArgs
  extNewObjMixedArgs = liftLogging G.extNewObjMixedArgs
  libNewObjMixedArgs = liftLogging G.libNewObjMixedArgs

instance (G.InstanceVarSelfSym lang) => G.InstanceVarSelfSym (LoggingFor lang) where
  instanceVarSelf = liftLogging G.instanceVarSelf

instance (G.SelfSym lang) => G.SelfSym (LoggingFor lang) where
  self = liftLogging G.self

instance (G.OOVariableValue lang) => G.OOVariableValue (LoggingFor lang)

instance (G.OOFunctionSym lang) => G.OOFunctionSym (LoggingFor lang) where
  func = liftLogging G.func
  objAccess = liftLogging G.objAccess

instance (G.ObserverPattern lang) => G.ObserverPattern (LoggingFor lang) where
  notifyObservers = liftLogging G.notifyObservers

instance (G.AttachmentSym lang) => G.AttachmentSym (LoggingFor lang) where
  type Attachment (LoggingFor lang) = G.Attachment lang
  classLevel = liftLogging G.classLevel
  instanceLevel = liftLogging G.instanceLevel

instance (G.OOMethodSym lang) => G.OOMethodSym (LoggingFor lang) where
  method = liftLogging G.method
  getMethod = liftLogging G.getMethod
  setMethod = liftLogging G.setMethod
  constructor = liftLogging G.constructor
  inOutMethod = liftLogging G.inOutMethod
  docInOutMethod = liftLogging G.docInOutMethod

instance (G.StateVarSym lang) => G.StateVarSym (LoggingFor lang) where
  type StateVar (LoggingFor lang) = G.StateVar lang
  stateVar = liftLogging G.stateVar
  stateVarDef = liftLogging G.stateVarDef
  constVar = liftLogging G.constVar

instance (G.ClassSym lang) => G.ClassSym (LoggingFor lang) where
  type Class (LoggingFor lang) = G.Class lang
  buildClass = liftLogging G.buildClass
  extraClass = liftLogging G.extraClass
  implementingClass = liftLogging G.implementingClass
  docClass = liftLogging G.docClass

instance (G.ModuleSym lang) => G.ModuleSym (LoggingFor lang) where
  type Module (LoggingFor lang) = G.Module lang
  buildModule = liftLogging G.buildModule

instance (G.FileSym lang) => G.FileSym (LoggingFor lang) where
  type File (LoggingFor lang) = G.File lang
  fileDoc = liftLogging G.fileDoc
  docMod = liftLogging G.docMod

instance (G.ProgramSym lang) => G.ProgramSym (LoggingFor lang) where
  type Program (LoggingFor lang) = G.Program lang
  prog = liftLogging G.prog

instance (G.StrategyPattern lang) => G.StrategyPattern (LoggingFor lang) where
  runStrategy = liftLogging G.runStrategy
