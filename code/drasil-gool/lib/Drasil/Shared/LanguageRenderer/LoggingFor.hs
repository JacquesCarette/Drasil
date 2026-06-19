{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | MVP renderer for logging statements.

module Drasil.Shared.LanguageRenderer.LoggingFor (LoggingFor(..)) where

import Drasil.Shared.InterfaceCommon
import qualified Drasil.GProc.InterfaceProc as P
import qualified Drasil.GOOL.InterfaceGOOL as G
import Drasil.Shared.State

import Prelude hiding (print, break, pi, log, floor, sin, cos, tan, exp)
import Control.Lens (zoom)
import Data.Kind (Type)

newtype LoggingFor (lang :: Type -> Type) a = LC {unLC :: lang a}
  deriving newtype (Functor, Applicative, Monad)

-- TODO [Brandon Bosman, 06/19/2026]: This should be passed down from drasil-code
varLogFile :: (VariableSym r) => SVariable r
varLogFile = var "outfile" outfile

valLogFile :: (VariableValue r) => SValue r
valLogFile = valueOf varLogFile

-- TODO [Brandon Bosman, 06/19/2026]: This should be passed down from drasil-code
logName :: (Literal r) => SValue r
logName = litString "log.txt"

instance (SharedProg lang) => AssignStatement (LoggingFor lang) where
  (&-=) = (&-=)
  (&+=) = (&+=)
  (&++) = (&++)
  (&--) = (&--)
  assign x e = do
    modName <- zoom lensMStoFS getModuleName
    LC <$> multi
      [ openFileA (unLC <$> varLogFile) logName
      , assign (unLC <$> x) (unLC <$> e)
      , do
          x' <- variableName . unLC <$> zoom lensMStoVS x
          printFileStr valLogFile $ "var '" <> x' <> "' assigned"
      , printFile valLogFile $ valueOf (unLC <$> x)
      , printLn $ litString $ " in module " <> modName
      ]

-- SharedProg Boilerplate

instance (SharedProg lang) => SharedProg (LoggingFor lang)

instance (VariableSym lang) => VariableSym (LoggingFor lang) where
  type Variable (LoggingFor lang) = Variable lang
  var = var
  constant = constant
  extVar = extVar

instance (TypeSym lang) => TypeSym (LoggingFor lang) where
  bool = bool
  int = int
  float = float
  double = double
  char = char
  string = string
  infile = infile
  outfile = outfile
  referenceType = referenceType
  listType = listType
  setType = setType
  arrayType = arrayType
  innerType = innerType
  funcType = funcType
  void = void

instance (ValueSym lang) => ValueSym (LoggingFor lang) where
  type Value (LoggingFor lang) = Value lang
  valueType = valueType

instance StatementSym lang => StatementSym (LoggingFor lang) where
  type Statement (LoggingFor lang) = Statement lang
  valStmt = valStmt
  emptyStmt = emptyStmt
  multi = multi

instance (Argument lang) => Argument (LoggingFor lang) where
  pointerArg = pointerArg

instance (Array lang) => Array (LoggingFor lang) where
  arrayElem = arrayElem
  arrayLength = arrayLength
  arrayCopy = arrayCopy

instance (BinderSym lang) => BinderSym (LoggingFor lang) where
  binder = binder

instance (BooleanExpression lang) => BooleanExpression (LoggingFor lang) where
  (?!) = (?!)
  (?&&) = (?&&)
  (?||) = (?||)

instance (CommandLineArgs lang) => CommandLineArgs (LoggingFor lang) where
  arg = arg
  argsList = argsList
  argExists = argExists

instance (CommentStatement lang) => CommentStatement (LoggingFor lang) where
  comment = comment

instance (Comparison lang) => Comparison (LoggingFor lang) where
  (?<) = (?<)
  (?<=) = (?<=)
  (?>) = (?>)
  (?>=) = (?>=)
  (?==) = (?==)
  (?!=) = (?!=)

instance (BlockSym lang) => BlockSym (LoggingFor lang) where
  type Block (LoggingFor lang) = Block lang
  block = block

instance (BodySym lang) => BodySym (LoggingFor lang) where
  type Body (LoggingFor lang) = Body lang
  body = body
  addComments = addComments

instance (ControlStatement lang) => ControlStatement (LoggingFor lang) where
  break = break
  continue = continue
  returnStmt = returnStmt
  throw = throw
  ifCond = ifCond
  switch = switch
  ifExists = ifExists
  for = for
  forRange = forRange
  forEach = forEach
  while = while
  tryCatch = tryCatch
  assert = assert

instance (ScopeSym lang) => ScopeSym (LoggingFor lang) where
  global = global
  mainFn = mainFn
  local = local

instance (DeclStatement lang) => DeclStatement (LoggingFor lang) where
  varDec = varDec
  varDecDef = varDecDef
  listDec = listDec
  listDecDef = listDecDef
  setDec = setDec
  setDecDef = setDecDef
  arrayDec = arrayDec
  arrayDecDef = arrayDecDef
  constDecDef = constDecDef
  funcDecDef = funcDecDef

instance (FuncAppStatement lang) => FuncAppStatement (LoggingFor lang) where
  inOutCall = inOutCall
  extInOutCall = extInOutCall

instance (FunctionSym lang) => FunctionSym (LoggingFor lang) where
  type Function (LoggingFor lang) = Function lang

instance (IOStatement lang) => IOStatement (LoggingFor lang) where
  print = print
  printLn = printLn
  printStr = printStr
  printStrLn = printStrLn
  printFile = printFile
  printFileLn = printFileLn
  printFileStr = printFileStr
  printFileStrLn = printFileStrLn
  getInput = getInput
  discardInput = discardInput
  getFileInput = getFileInput
  discardFileInput = discardFileInput
  openFileR = openFileR
  openFileW = openFileW
  openFileA = openFileA
  closeFile = closeFile
  getFileInputLine = getFileInputLine
  discardFileLine = discardFileLine
  getFileInputAll = getFileInputAll

instance (InternalList lang) => InternalList (LoggingFor lang) where
  listSlice' = listSlice'

instance (List lang) => List (LoggingFor lang) where
  listSize = listSize
  listAdd = listAdd
  listAppend = listAppend
  listAccess = listAccess
  listSet = listSet
  indexOf = indexOf

instance (Literal lang) => Literal (LoggingFor lang) where
  litTrue = litTrue
  litFalse = litFalse
  litChar = litChar
  litDouble = litDouble
  litFloat = litFloat
  litInt = litInt
  litString = litString
  litArray = litArray
  litList = litList
  litSet = litSet

instance (StringStatement lang) => StringStatement (LoggingFor lang) where
  stringSplit = stringSplit
  stringListVals = stringListVals
  stringListLists = stringListLists

instance (MathConstant lang) => MathConstant (LoggingFor lang) where
  pi = pi

instance (ParameterSym lang) => ParameterSym (LoggingFor lang) where
  type Parameter (LoggingFor lang) = Parameter lang
  param = param
  pointerParam = pointerParam

instance (VisibilitySym lang) => VisibilitySym (LoggingFor lang) where
  type Visibility (LoggingFor lang) = Visibility lang
  private = private
  public = public

instance (MethodSym lang) => MethodSym (LoggingFor lang) where
  type Method (LoggingFor lang) = Method lang
  docMain = docMain
  function = function
  mainFunction = mainFunction
  docFunc = docFunc
  inOutFunc = inOutFunc
  docInOutFunc = docInOutFunc

instance (NumericExpression lang) => NumericExpression (LoggingFor lang) where
  (#~) = (#~)
  (#/^) = (#/^)
  (#|) = (#|)
  (#+) = (#+)
  (#-) = (#-)
  (#*) = (#*)
  (#/) = (#/)
  (#%) = (#%)
  (#^) = (#^)
  log = log
  ln = ln
  exp = exp
  sin = sin
  cos = cos
  tan = tan
  csc = csc
  sec = sec
  cot = cot
  arcsin = arcsin
  arccos = arccos
  arctan = arctan
  floor = floor
  ceil = ceil

instance (Set lang) => Set (LoggingFor lang) where
  contains = contains
  setAdd = setAdd
  setRemove = setRemove
  setUnion = setUnion

instance (UnRepr lang contents) => UnRepr (LoggingFor lang) contents where
  unRepr = unRepr

instance (ValueExpression lang) => ValueExpression (LoggingFor lang) where
  inlineIf = inlineIf
  funcAppMixedArgs = funcAppMixedArgs
  extFuncAppMixedArgs = extFuncAppMixedArgs
  libFuncAppMixedArgs = libFuncAppMixedArgs
  lambda = lambda
  notNull = notNull

instance (VariableElim lang) => VariableElim (LoggingFor lang) where
  variableName = variableName
  variableType = variableType

instance (VariableValue lang) => VariableValue (LoggingFor lang) where
  valueOf = valueOf

instance (IndexTranslator lang) => IndexTranslator (LoggingFor lang) where
  intToIndex = intToIndex
  indexToInt = indexToInt

-- GProc

instance (P.ProcProg lang) => P.ProcProg (LoggingFor lang)

instance (P.ModuleSym lang) => P.ModuleSym (LoggingFor lang) where
  type Module (LoggingFor lang) = P.Module lang
  buildModule = P.buildModule

instance (P.FileSym lang) => P.FileSym (LoggingFor lang) where
  type File (LoggingFor lang) = P.File lang
  fileDoc = P.fileDoc
  docMod = P.docMod

instance (P.ProgramSym lang) => P.ProgramSym (LoggingFor lang) where
  type Program (LoggingFor lang) = P.Program lang
  prog = P.prog

-- GOOL

instance (G.OOProg lang) => G.OOProg (LoggingFor lang)

instance (G.GetSet lang) => G.GetSet (LoggingFor lang) where
  get = G.get
  set = G.set

instance (G.InternalValueExp lang) => G.InternalValueExp (LoggingFor lang) where
  objMethodCallMixedArgs' = G.objMethodCallMixedArgs'
  classMethodCallMixedArgs' = G.classMethodCallMixedArgs'

instance (G.OOTypeSym lang) => G.OOTypeSym (LoggingFor lang) where
  obj = G.obj

instance (G.OOVariableSym lang) => G.OOVariableSym (LoggingFor lang) where
  classVar = G.classVar
  classConst = G.classConst
  classVarAccess = G.classVarAccess
  extClassVarAccess = G.extClassVarAccess
  instanceVarAccess = G.instanceVarAccess

instance (G.OODeclStatement lang) => G.OODeclStatement (LoggingFor lang) where
  objDecDef = G.objDecDef
  objDecNew = G.objDecNew
  extObjDecNew = G.extObjDecNew

instance (G.OOFuncAppStatement lang) => G.OOFuncAppStatement (LoggingFor lang) where
  selfInOutCall = G.selfInOutCall

instance (G.OOValueSym lang) => G.OOValueSym (LoggingFor lang) where

instance (G.OOValueExpression lang) => G.OOValueExpression (LoggingFor lang) where
  newObjMixedArgs = G.newObjMixedArgs
  extNewObjMixedArgs = G.extNewObjMixedArgs
  libNewObjMixedArgs = G.libNewObjMixedArgs

instance (G.InstanceVarSelfSym lang) => G.InstanceVarSelfSym (LoggingFor lang) where
  instanceVarSelf = G.instanceVarSelf

instance (G.SelfSym lang) => G.SelfSym (LoggingFor lang) where
  self = G.self

instance (G.OOVariableValue lang) => G.OOVariableValue (LoggingFor lang)

instance (G.OOFunctionSym lang) => G.OOFunctionSym (LoggingFor lang) where
  func = G.func
  objAccess = G.objAccess

instance (G.ObserverPattern lang) => G.ObserverPattern (LoggingFor lang) where
  notifyObservers = G.notifyObservers

instance (G.AttachmentSym lang) => G.AttachmentSym (LoggingFor lang) where
  type Attachment (LoggingFor lang) = G.Attachment lang
  classLevel = G.classLevel
  instanceLevel = G.instanceLevel

instance (G.OOMethodSym lang) => G.OOMethodSym (LoggingFor lang) where
  method = G.method
  getMethod = G.getMethod
  setMethod = G.setMethod
  constructor = G.constructor
  inOutMethod = G.inOutMethod
  docInOutMethod = G.docInOutMethod

instance (G.StateVarSym lang) => G.StateVarSym (LoggingFor lang) where
  type StateVar (LoggingFor lang) = G.StateVar lang
  stateVar = G.stateVar
  stateVarDef = G.stateVarDef
  constVar = G.constVar

instance (G.ClassSym lang) => G.ClassSym (LoggingFor lang) where
  type Class (LoggingFor lang) = G.Class lang
  buildClass = G.buildClass
  extraClass = G.extraClass
  implementingClass = G.implementingClass
  docClass = G.docClass

instance (G.ModuleSym lang) => G.ModuleSym (LoggingFor lang) where
  type Module (LoggingFor lang) = G.Module lang
  buildModule = G.buildModule

instance (G.FileSym lang) => G.FileSym (LoggingFor lang) where
  type File (LoggingFor lang) = G.File lang
  fileDoc = G.fileDoc
  docMod = G.docMod

instance (G.ProgramSym lang) => G.ProgramSym (LoggingFor lang) where
  type Program (LoggingFor lang) = G.Program lang
  prog = G.prog

instance (G.StrategyPattern lang) => G.StrategyPattern (LoggingFor lang) where
  runStrategy = G.runStrategy
