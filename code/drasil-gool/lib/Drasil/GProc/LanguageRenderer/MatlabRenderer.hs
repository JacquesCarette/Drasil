{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The logic to render MATLAB code is contained in this module.
module Drasil.GProc.LanguageRenderer.MatlabRenderer (
  -- * MATLAB Code Configuration -- defines syntax of all MATLAB code
  MatlabCode(..), mlName, mlVersion
) where

import Drasil.Shared.InterfaceCommon (Label, Value, SValue, Variable, SVariable,
  getCodeType, UnRepr(..), Body, BodySym(..), BlockSym(..), TypeSym(..),
  TypeElim(..), VariableSym(..), VariableElim(..), ValueSym(..), Argument(..),
  Literal(..), MathConstant(..), VariableValue(..), CommandLineArgs(..),
  NumericExpression(..), BooleanExpression(..), Comparison(..),
  ValueExpression(..), IndexTranslator(..), Reference(..), Array(..), List(..),
  Set(..), NativeVector(..), InternalList(..), StatementSym(..),
  AssignStatement(..), DeclStatement(..), PrintConsole(..), ReadConsole(..),
  FileHandling(..), PrintFile(..), ReadFile(..), StringStatement(..),
  FunctionSym, FuncAppStatement(..), CommentStatement(..), ControlStatement(..),
  switchAsIf, VisibilitySym(..), ScopeSym(..), ParameterSym(..), BinderSym(..),
  BinderElim(..), MethodSym(..), funcApp, (&=))
import Drasil.GProc.InterfaceProc (ProcProg, ProgramSym(..),
  FileSym(..), ModuleSym(..))

import Drasil.Shared.RendererClassesCommon (CommonRenderSym, ImportSym(..),
  RenderBody(..), BodyElim, RenderBlock(..), BlockElim, RenderType(..),
  UnaryOpSym(..), BinaryOpSym(..), OpElim(uOpPrec, bOpPrec), RenderVariable(..),
  InternalVarElim(variableBind), RenderValue(..), ValueElim(..),
  InternalListFunc(..), RenderFunction(..), FunctionElim(functionType),
  InternalAssignStmt(..), InternalIOStmt(..), InternalControlStmt(..),
  RenderStatement(..), StatementElim(statementTerm), RenderVisibility(..),
  VisibilityElim, MethodTypeSym(..), RenderParam(..),
  ParamElim(parameterName, parameterType), RenderMethod(..), MethodElim,
  BlockCommentSym(..), BlockCommentElim, ScopeElim(..), InternalBinderElim(..))
import qualified Drasil.Shared.RendererClassesCommon as RC (body, block, uOp,
  bOp, variable, value, function, statement, visibility, parameter, method,
  blockComment')
import Drasil.GProc.RendererClassesProc (ProcRenderSym, RenderFile(..),
  RenderMod(..), ModuleElim, ProcRenderMethod(..))
import qualified Drasil.GProc.LanguageRenderer.AbstractProc as A (fileDoc,
  docMod, fileFromData, buildModule, modFromData, function, funcDecDef,
  innerType)
import qualified Drasil.Shared.LanguageRenderer as R (commentedMod,
  commentedItem, parameterList, body, addComments, multiStmt, sqrt, abs, log10,
  log, exp, sin, cos, tan, asin, acos, atan, floor, ceil, break, continue,
  elseIfLabel)
import qualified Drasil.GProc.RendererClassesProc as RC (module')
import qualified Drasil.Shared.LanguageRenderer.LanguagePolymorphic as G (
  comment, param, docFunc, var, multiBody, block, multiBlock, stmt, loopStmt,
  negateOp, plusOp, minusOp, multOp, divideOp, equalOp, greaterOp,
  greaterEqualOp, lessOp, lessEqualOp, csc, sec, cot, valueOf, litDouble,
  litInt, litString, valStmt, emptyStmt, assign, funcAppMixedArgs, call, print,
  ifCond, tryCatch, listAccess)
import qualified Drasil.Shared.LanguageRenderer.CommonPseudoOO as CP (mainBody,
  functionDoc, docInOutFunc', inOutCall, multiAssign, intToIndex', indexToInt',
  listDecDef, litSet)
import qualified Drasil.Shared.LanguageRenderer.CLike as C (andOp, orOp, litTrue,
  litFalse, while)
import qualified Drasil.Shared.LanguageRenderer.Common as CS (varDecDef,
  extFuncAppMixedArgs, funcType, listSize, forEach')
import qualified Drasil.Shared.LanguageRenderer.Macros as M (ifExists,
  increment1, decrement1, stringListVals, stringListLists)
import Drasil.Shared.AST (Terminator(..), FileType(Combined), fileD, md,
  updateMod, MethodData, mthd, updateMthd, ParamData, paramVar, paramDoc, pd,
  ProgData, TypeData, cType, vd, val, valPrec, valInt, valType, opDoc, opPrec,
  varName, varType, varBind, varDoc, vard, progD, mthdDoc, modDoc,
  FuncData(fType, funcDoc), fd)
import Drasil.Shared.CodeType (CodeType(..))
import Drasil.Shared.LanguageRenderer.Constructors (typeFromData, unOpPrec,
  powerPrec, unExpr, unExpr', binExpr, mkStateVal, mkVal, mkStateVar, mkStmtNoEnd,
  compEqualPrec, typeUnExpr, typeBinExpr)
import Drasil.Shared.LanguageRenderer (listSep', valueList, intValue)
import Drasil.Shared.LanguageRenderer.LanguagePolymorphic (OptionalSpace(..))
import Drasil.Shared.Helpers (toCode, toState, onCodeValue, onStateValue,
  onCodeList, onStateList, on2CodeValues, on2StateValues, emptyIfEmpty)
import Drasil.Shared.State (MS, VS, FS, lensGStoFS, lensMStoVS, revFiles,
  setFileType, getMainDoc)

import Control.Lens.Zoom (zoom)
import Control.Monad.State (modify)

import Drasil.FileHandling.Legacy (indent)
import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import Text.PrettyPrint.HughesPJ (Doc, empty, text, (<>), (<+>), ($$), vcat,
  hcat, parens, brackets, braces, equals, punctuate, render)

newtype MatlabCode a = MLC {unMLC :: a} deriving Functor

instance Applicative MatlabCode where
  pure = MLC
  (MLC f) <*> (MLC x) = MLC (f x)

instance Monad MatlabCode where
  MLC x >>= f = f x

instance ProcProg MatlabCode Doc (Doc, Terminator) MethodData ProgData

instance ProgramSym MatlabCode Doc (Doc, Terminator) MethodData ProgData where
  prog n st files = do
    fs <- mapM (zoom lensGStoFS) files
    modify revFiles
    pure $ onCodeList (progD n st) fs

instance CommonRenderSym MatlabCode Doc (Doc, Terminator) MethodData
instance ProcRenderSym MatlabCode Doc (Doc, Terminator) MethodData

instance UnRepr MatlabCode inner where
  unRepr = unMLC

instance FileSym MatlabCode Doc (Doc, Terminator) MethodData where
  fileDoc m = do
    modify (setFileType Combined)
    A.fileDoc mlExt m
  docMod = A.docMod mlExt

instance RenderFile MatlabCode where
  top _ = toCode empty
  bottom = toCode empty
  commentedMod = on2StateValues (on2CodeValues R.commentedMod)
  fileFromData = A.fileFromData (onCodeValue . fileD)

instance ImportSym MatlabCode where
  langImport = undefined
  modImport = undefined

instance BodySym MatlabCode (Doc, Terminator) where
  body = onStateList (onCodeList R.body)
  addComments s = onStateValue (onCodeValue (R.addComments s mlCmtStart))

instance RenderBody MatlabCode where
  multiBody = G.multiBody

instance BodyElim MatlabCode where
  body = unMLC

instance BlockSym MatlabCode (Doc, Terminator) where
  block = G.block

instance RenderBlock MatlabCode where
  multiBlock = G.multiBlock

instance BlockElim MatlabCode where
  block = unMLC

instance TypeSym MatlabCode where
  bool = mlTy Boolean "logical"
  int = mlTy Integer "int"
  float = mlTy Float "double"
  double = mlTy Double "double"
  char = mlTy Char "char"
  string = mlTy String "string"
  infile = mlTy InFile "file"
  outfile = mlTy OutFile "file"
  referenceType = id -- Ignore reference types in "high-level" langauges for now; later on think about using boxed/unboxed types
  listType = mlListType
  setType = listType
  arrayType = listType -- Treat arrays and lists the same, as in Julia/Python
  innerType = A.innerType
  funcType = CS.funcType
  void = mlTy Void "void"

instance TypeElim MatlabCode where
  getCodeType = cType . unMLC

instance RenderType MatlabCode where
  multiType = undefined

instance UnaryOpSym MatlabCode where
  notOp = unOpPrec "~"
  negateOp = G.negateOp
  sqrtOp = unOpPrec R.sqrt
  absOp = unOpPrec R.abs
  logOp = unOpPrec R.log10
  lnOp = unOpPrec R.log
  expOp = unOpPrec R.exp
  sinOp = unOpPrec R.sin
  cosOp = unOpPrec R.cos
  tanOp = unOpPrec R.tan
  asinOp = unOpPrec R.asin
  acosOp = unOpPrec R.acos
  atanOp = unOpPrec R.atan
  floorOp = unOpPrec R.floor
  ceilOp = unOpPrec R.ceil

instance BinaryOpSym MatlabCode where
  equalOp = G.equalOp
  notEqualOp = compEqualPrec "~="
  greaterOp = G.greaterOp
  greaterEqualOp = G.greaterEqualOp
  lessOp = G.lessOp
  lessEqualOp = G.lessEqualOp
  plusOp = G.plusOp
  minusOp = G.minusOp
  multOp = G.multOp
  divideOp = G.divideOp
  powerOp = powerPrec "^"
  moduloOp = undefined
  andOp = C.andOp
  orOp = C.orOp

instance OpElim MatlabCode where
  uOp = opDoc . unMLC
  bOp = opDoc . unMLC
  uOpPrec = opPrec . unMLC
  bOpPrec = opPrec . unMLC

instance ScopeSym MatlabCode where
  global = undefined
  mainFn = undefined
  local = undefined

instance ScopeElim MatlabCode where
  scopeData = unMLC

instance VariableSym MatlabCode where
  var = G.var
  constant = var
  extVar = undefined

instance VariableElim MatlabCode where
  variableName = varName . unMLC
  variableType = onCodeValue varType

instance InternalVarElim MatlabCode where
  variableBind = varBind . unMLC
  variable = varDoc . unMLC

instance RenderVariable MatlabCode where
  varFromData b n t' d = do
    t <- t'
    toState $ on2CodeValues (vard b n) t (toCode d)

instance ValueSym MatlabCode where
  valueType v = valType <$> v

instance Argument MatlabCode where
  pointerArg = id

instance Literal MatlabCode where
  litTrue = C.litTrue
  litFalse = C.litFalse
  litChar = undefined
  litDouble = G.litDouble
  litFloat = undefined
  litInt = G.litInt
  litString = G.litString
  litArray = litList
  litList = mlLitList
  litSet = CP.litSet id brackets

instance MathConstant MatlabCode where
  pi = mkStateVal double (text "pi")

instance VariableValue MatlabCode where
  valueOf = G.valueOf

instance CommandLineArgs MatlabCode where
  -- Args come in through the entry function's varargin (1-based, cell-indexed).
  arg n = mlArg (litInt (n + 1))
  argsList = mkStateVal (arrayType string) (text "varargin")
  argExists = undefined

instance NumericExpression MatlabCode where
  (#~) = unExpr' negateOp
  (#/^) = unExpr sqrtOp
  (#|) = unExpr absOp
  (#+) = binExpr plusOp
  (#-) = binExpr minusOp
  (#*) = binExpr multOp
  (#/) = binExpr divideOp
  (#%) = undefined
  (#^) = binExpr powerOp
  log = unExpr logOp
  ln = unExpr lnOp
  exp = unExpr expOp
  sin = unExpr sinOp
  cos = unExpr cosOp
  tan = unExpr tanOp
  csc = G.csc
  sec = G.sec
  cot = G.cot
  arcsin = unExpr asinOp
  arccos = unExpr acosOp
  arctan = unExpr atanOp
  floor = unExpr floorOp
  ceil = unExpr ceilOp

instance BooleanExpression MatlabCode where
  (?!) = typeUnExpr notOp bool
  (?&&) = typeBinExpr andOp bool
  (?||) = typeBinExpr orOp bool

instance Comparison MatlabCode where
  (?<) = typeBinExpr lessOp bool
  (?<=) = typeBinExpr lessEqualOp bool
  (?>) = typeBinExpr greaterOp bool
  (?>=) = typeBinExpr greaterEqualOp bool
  (?==) = typeBinExpr equalOp bool
  (?!=) = typeBinExpr notEqualOp bool

instance ValueExpression MatlabCode where
  inlineIf = mlInlineIf
  funcAppMixedArgs = G.funcAppMixedArgs
  extFuncAppMixedArgs = CS.extFuncAppMixedArgs
  libFuncAppMixedArgs = CS.extFuncAppMixedArgs
  lambda = undefined
  notNull = undefined

instance RenderValue MatlabCode where
  inputFunc = undefined
  printFunc = mlPrintFunc
  printLnFunc = mlPrintFunc
  printFileFunc _ = mlPrintFunc
  printFileLnFunc _ = mlPrintFunc
  cast = mlCast
  call = G.call equals
  valFromData p i t' d = do
    t <- t'
    toState $ on2CodeValues (vd p i) t (toCode d)

instance ValueElim MatlabCode where
  valuePrec = valPrec . unMLC
  valueInt = valInt . unMLC
  value = val . unMLC

instance IndexTranslator MatlabCode where
  intToIndex = CP.intToIndex'
  indexToInt = CP.indexToInt'

instance Reference MatlabCode where
  makeRef = id
  maybeDeref = id

instance Array MatlabCode where
  arrayElem = mlArrayElem
  arrayLength = listSize
  arrayCopy arr = let arrTp = onStateValue valueType arr
    in funcApp "copy" arrTp [arr]

instance List MatlabCode (Doc, Terminator) where
  listSize = CS.listSize "length"   -- length(v)
  listAdd = undefined
  listAppend lst = listSet lst (listSize lst)
  listAccess = G.listAccess
  listSet = mlListSet
  indexOf lst v = funcApp "find" int [lst ?== v, litInt 1] #- litInt 1

instance Set MatlabCode where
  contains s e = funcApp "ismember" bool [e, s]
  setAdd = undefined
  setRemove = undefined
  setUnion = undefined

instance NativeVector MatlabCode where
  vecScale = binExpr multOp           -- s * v
  vecAdd   = binExpr plusOp           -- a + b
  vecIndex = mlVecIndex               -- a(1) / a(i + 1)
  vecDot a b = funcApp "dot" double [a, b]   -- dot(a, b)
  vecMag a = funcApp "norm" double [a]       -- norm(a)
  vecUnit a = a #/ vecMag a                  -- a / norm(a)

instance InternalList MatlabCode where
  listSlice' = undefined

instance InternalListFunc MatlabCode where
  listAccessFunc t v = intValue v >>= ((`funcFromData` t) . mlListAccessFunc)

mlListAccessFunc :: (CommonRenderSym r vis stmt mthd) => r Value -> Doc
mlListAccessFunc v = parens $ RC.value v

instance BinderSym MatlabCode where
  binder = undefined

instance BinderElim MatlabCode where
  binderName = undefined
  binderType = undefined

instance InternalBinderElim MatlabCode where
  binderElim = undefined

instance RenderFunction MatlabCode where
  funcFromData d = onStateValue $ onCodeValue (`fd` d)

instance FunctionElim MatlabCode where
  functionType = onCodeValue fType
  function = funcDoc . unMLC

instance InternalAssignStmt MatlabCode (Doc, Terminator) where
  multiAssign = CP.multiAssign brackets

instance InternalIOStmt MatlabCode (Doc, Terminator) where
  printSt = mlPrint

instance InternalControlStmt MatlabCode (Doc, Terminator) where
  multiReturn = undefined

instance RenderStatement MatlabCode (Doc, Terminator) where
  stmt = G.stmt
  loopStmt = G.loopStmt
  stmtFromData d t = toState $ toCode (d, t)

instance StatementElim MatlabCode (Doc, Terminator) where
  statement = fst . unMLC
  statementTerm = snd . unMLC

instance StatementSym MatlabCode (Doc, Terminator) where
  valStmt = G.valStmt Semi
  emptyStmt = G.emptyStmt
  multi = onStateList (onCodeList R.multiStmt)

instance AssignStatement MatlabCode (Doc, Terminator) where
  assign = G.assign Semi
  (&-=) vr v = vr &= (valueOf vr #- v)
  (&+=) vr v = vr &= (valueOf vr #+ v)
  (&++) = M.increment1
  (&--) = M.decrement1

instance DeclStatement MatlabCode (Doc, Terminator) where
  varDec v scp = CS.varDecDef v scp Nothing
  varDecDef v scp e = CS.varDecDef v scp (Just e)
  setDec = varDec
  setDecDef = varDecDef
  listDec _ = varDec
  listDecDef = CP.listDecDef
  arrayDec _ _ = varDec
  arrayDecDef = listDecDef
  constDecDef = varDecDef
  funcDecDef = A.funcDecDef

instance PrintConsole MatlabCode (Doc, Terminator) where
  print = G.print False Nothing printFunc
  printLn = G.print True Nothing printLnFunc
  printStr = G.print False Nothing printFunc . litString
  printStrLn = G.print True Nothing printLnFunc . litString

instance ReadConsole MatlabCode (Doc, Terminator) where
  getInput = undefined
  discardInput = undefined

instance FileHandling MatlabCode (Doc, Terminator) where
  openFileR f n = f &= funcApp "fopen" infile [n, litString "r"]
  openFileW f n = f &= funcApp "fopen" outfile [n, litString "w"]
  openFileA f n = f &= funcApp "fopen" outfile [n, litString "a"]
  closeFile f = valStmt $ funcApp "fclose" void [f]

instance PrintFile MatlabCode (Doc, Terminator) where
  printFile f = G.print False (Just f) printFunc
  printFileLn f = G.print True (Just f) printLnFunc
  printFileStr f = printFile f . litString
  printFileStrLn f = printFileLn f . litString

instance ReadFile MatlabCode (Doc, Terminator) where
  getFileInput f = mlInput (mlReadLine f)
  discardFileInput f = valStmt (mlReadLine f)
  getFileInputLine = getFileInput
  discardFileLine = discardFileInput
  getFileInputAll f v = v &= funcApp "readlines" (listType string) [f]

instance StringStatement MatlabCode (Doc, Terminator) where
  stringSplit d vnew s = vnew &= funcApp "strsplit" (listType string) [s, litString [d]]
  stringListVals = M.stringListVals
  stringListLists = M.stringListLists

instance FunctionSym MatlabCode where

instance FuncAppStatement MatlabCode (Doc, Terminator) where
  inOutCall = CP.inOutCall funcApp
  extInOutCall = undefined

instance CommentStatement MatlabCode (Doc, Terminator) where
  comment = G.comment mlCmtStart

instance ControlStatement MatlabCode (Doc, Terminator) where
  break = mkStmtNoEnd R.break
  continue = mkStmtNoEnd R.continue
  -- MATLAB has no `return <expr>`: a function returns by assigning its named
  -- output, so a return becomes `result = <value>;`.
  returnStmt v' = do
    v <- zoom lensMStoVS v'
    var mlRet (toState (valueType v)) &= v'
  throw errMsg = valStmt $ funcApp "error" void [litString errMsg]
  ifCond = G.ifCond id empty (OSpace empty) R.elseIfLabel empty mlEnd
  switch = switchAsIf
  ifExists = M.ifExists
  for initS cond updateS b = do
    i <- initS
    c <- zoom lensMStoVS cond
    u <- updateS
    bd <- b
    mkStmtNoEnd (vcat [
      RC.statement i <> text ";",
      text "while" <+> RC.value c,
      indent (RC.body bd),
      indent (RC.statement u <> text ";"),
      mlEnd])
  forRange i initv finalv stepv = forEach i (mlRange initv finalv stepv)
  forEach = CS.forEach' mlForEach
  while = C.while id empty mlEnd
  tryCatch = G.tryCatch mlTryCatch
  assert cond errMsg = valStmt $ funcApp "assert" void [cond, errMsg]

instance VisibilitySym MatlabCode Doc where
  private = toCode empty
  public = toCode empty

instance RenderVisibility MatlabCode Doc where
  visibilityFromData = undefined

instance VisibilityElim MatlabCode Doc where
  visibility = unMLC

instance MethodTypeSym MatlabCode where
  mType = zoom lensMStoVS

instance ParameterSym MatlabCode where
  -- A MATLAB parameter is just the variable name.
  param = G.param mlParam
  pointerParam = param

instance RenderParam MatlabCode where
  paramFromData v' d = do
    v <- zoom lensMStoVS v'
    toState $ on2CodeValues pd v (toCode d)

instance ParamElim MatlabCode where
  parameterName = variableName . onCodeValue paramVar
  parameterType = variableType . onCodeValue paramVar
  parameter = paramDoc . unMLC

instance MethodSym MatlabCode Doc (Doc, Terminator) MethodData where
  docMain = mainFunction
  function = A.function
  mainFunction = CP.mainBody
  docFunc = G.docFunc CP.functionDoc

  -- MATLAB returns values through the output list in the header
  -- (function [outs] = name(ins)), not through a return statement, so we build
  -- the method directly instead of reusing the shared inOutFunc machinery.
  inOutFunc n _ ins outs both b = do
    pms  <- mapM param (both ++ ins)
    rets <- mapM (zoom lensMStoVS) (both ++ outs)
    bod  <- b
    pure $ toCode $ mthd $ mlFuncDoc n (map RC.variable rets) pms (RC.body bod)
  docInOutFunc n s = CP.docInOutFunc' CP.functionDoc (inOutFunc n s)

instance RenderMethod MatlabCode MethodData where
  commentedFunc cmt m = on2StateValues (on2CodeValues updateMthd) m
    (onStateValue (onCodeValue R.commentedItem) cmt)
  mthdFromData _ d = toState $ toCode $ mthd d

instance ProcRenderMethod MatlabCode Doc MethodData where
  intFunc _ n _ t ps b = do
    pms <- sequence ps
    tp  <- t
    bod <- b
    -- A function with a non-void return type declares the output variable
    -- 'result' in its header (function result = name(...)); returnStmt then
    -- assigns to it. Void functions declare no output.
    let outs = [text mlRet | cType (unMLC tp) /= Void]
    pure $ toCode $ mthd $ mlFuncDoc n outs pms (RC.body bod)

instance MethodElim MatlabCode MethodData where
  method = mthdDoc . unMLC

instance ModuleSym MatlabCode Doc (Doc, Terminator) MethodData where
  -- Function-file layout (runs in both MATLAB and Octave): the main code
  -- becomes the entry function `function <name>(varargin) ... end` and comes
  -- first, then the local functions. Command-line args map to varargin.
  buildModule n _ = A.buildModule n (mlMainFunc n) (pure empty)

instance RenderMod MatlabCode where
  modFromData n = A.modFromData n (toCode . md n)
  updateModuleDoc f = onCodeValue (updateMod f)

instance ModuleElim MatlabCode where
  module' = modDoc . unMLC

instance BlockCommentSym MatlabCode where
  blockComment lns = toCode $ mlLineCmt lns
  docComment = onStateValue (toCode . mlLineCmt)

instance BlockCommentElim MatlabCode where
  blockComment' = unMLC

-- convenience
mlName, mlVersion :: String
mlName = "MATLAB"
mlVersion = "R2024b"

-- | MATLAB source file extension.
mlExt :: String
mlExt = "m"

-- | Name of the implicit output variable a value-returning function assigns to.
mlRet :: String
mlRet = "result"

-- | Makes a MATLAB type. Only the 'CodeType' tag is used (internally); the
--   name is never written out, since MATLAB code has no type annotations.
mlTy :: CodeType -> String -> VS (MatlabCode TypeData)
mlTy c s = typeFromData c s (text s)

-- | A MATLAB parameter renders as just the variable name.
mlParam :: MatlabCode Variable -> Doc
mlParam = RC.variable

-- | Renders a MATLAB function: @function [outs] = name(ins) ... end@.
--   With no outputs the @[outs] =@ part is dropped; with a single output the
--   brackets are dropped (@function out = name(ins)@).
mlFuncDoc :: (ParamElim r) => Label -> [Doc] ->
  [r ParamData] -> Doc -> Doc
mlFuncDoc n outs pms bod =
  vcat [text "function" <+> (retDoc <> text n) <> parens (R.parameterList pms),
        indent bod,
        text "end"]
  where retDoc = case outs of
          []  -> empty
          [o] -> o <+> equals <> text " "
          os  -> brackets (hcat (punctuate (text ", ") os)) <+> equals <> text " "

-- Comments
mlCmtStart :: Doc
mlCmtStart = text "%"

-- | Makes a MATLAB comment. Every line starts with %.
--   (We avoid %{ %} blocks: those need the markers alone on a line.)
mlLineCmt :: [String] -> Doc
mlLineCmt = vcat . map ((mlCmtStart <+>) . text)

-- | A stand-in print function. mlPrint never uses it, but it must be a real
--   value so the print methods type-check.
mlPrintFunc :: SValue MatlabCode
mlPrintFunc = mkStateVal void (text "fprintf")

-- | Gets a command-line argument: argv(){n}.
--   (Octave keeps the args in a cell, so we index with {}.)
-- | Wraps the main body as the entry function: @function <name>(varargin) ...
--   end@. Empty if there is no main body.
mlMainFunc :: Label -> FS Doc
mlMainFunc n = do
  b <- getMainDoc
  pure $ emptyIfEmpty b $ vcat
    [text "function" <+> text n <> parens (text "varargin"),
     indent b,
     text "end"]

mlArg :: SValue MatlabCode -> SValue MatlabCode
mlArg n' = do
  n <- n'
  s <- string
  mkVal s (text "varargin" <> braces (RC.value n))

-- | Indexes into a vector. MATLAB is 1-indexed while GOOL is 0-indexed, so the
--   index is translated with 'intToIndex' (which folds constants, e.g. @a(1)@
--   for index 0, and yields @a(i + 1)@ for a variable @i@).
mlVecIndex :: SValue MatlabCode -> SValue MatlabCode -> SValue MatlabCode
mlVecIndex v' i' = do
  v <- v'
  i <- intToIndex i'
  d <- double
  mkVal d (RC.value v <> parens (RC.value i))

-- | A list/vector type. MATLAB never writes types, so only the underlying
--   'CodeType' matters; arrays and lists share this native vector type.
mlListType :: VS (MatlabCode TypeData) -> VS (MatlabCode TypeData)
mlListType t' = do
  t <- t'
  mlTy (List $ getCodeType t) "vector"

-- | A vector literal, rendered as a MATLAB row vector.
mlLitList :: VS (MatlabCode TypeData) -> [SValue MatlabCode] -> SValue MatlabCode
mlLitList t es = do
  elems <- sequence es
  mkStateVal (listType t) (brackets (valueList elems))

-- | Reads one line from a file as text: fgetl(f).
mlReadLine :: SValue MatlabCode -> SValue MatlabCode
mlReadLine f = funcApp "fgetl" string [f]

-- | Reads a value into v. Numbers go through str2double; text is kept as-is.
--   The type of v says which one to use.
mlInput :: SValue MatlabCode -> SVariable MatlabCode -> MS (MatlabCode (Doc, Terminator))
mlInput inSrc v = v &= (v >>= mlInput' . getCodeType . variableType)
  where mlInput' Integer = funcApp "str2double" int [inSrc]
        mlInput' Float   = funcApp "str2double" float [inSrc]
        mlInput' Double  = funcApp "str2double" double [inSrc]
        mlInput' String  = inSrc
        mlInput' _ = error "Attempt to read a value of unreadable type"

-- | Prints a value: fprintf([fid, ]'fmt', value). The format is %s for text
--   and %g for numbers. A line-print adds \n. A file handle, if given, comes
--   first. (We always use fprintf, so the print-function argument is ignored.)
mlPrint :: Bool -> Maybe (SValue MatlabCode) -> SValue MatlabCode
  -> SValue MatlabCode -> MS (MatlabCode (Doc, Terminator))
mlPrint newLn f' _ v' = do
  v  <- zoom lensMStoVS v'
  mf <- traverse (zoom lensMStoVS) f'
  let fmt = case cType (valType (unMLC v)) of
              String -> "%s"
              _      -> "%g"
      nl = if newLn then "\\n" else ""
      fileArg = maybe empty (\fv -> RC.value fv <> listSep') mf
  stmtFromData (text "fprintf" <>
    parens (fileArg <> text ("'" ++ fmt ++ nl ++ "'") <> listSep' <> RC.value v))
    Semi

mlInlineIf :: SValue MatlabCode -> SValue MatlabCode -> SValue MatlabCode
  -> SValue MatlabCode
mlInlineIf c' v1' v2' = do
  c <- c'
  v1 <- v1'
  v2 <- v2'
  mkStateVal (toState $ valueType v1)
    (parens (parens (RC.value c) <+> text ".*" <+> parens (RC.value v1)
    <+> text "+ ~" <> parens (RC.value c) <+> text ".*" <+> parens (RC.value v2)))

mlCast :: VS (MatlabCode TypeData) -> SValue MatlabCode -> SValue MatlabCode
mlCast t' v' = do
  t <- t'
  v <- v'
  let vTp = getCodeType $ valueType v
      tTp = getCodeType t
      vDoc = RC.value v
      mlCast' String Integer = text "str2double" <> parens vDoc
      mlCast' String Float   = text "str2double" <> parens vDoc
      mlCast' String Double  = text "str2double" <> parens vDoc
      mlCast' String Boolean = text "logical" <> parens (text "str2double" <> parens vDoc)
      mlCast' _      String  = text "num2str" <> parens vDoc
      mlCast' _      Char    = text "char" <> parens vDoc
      mlCast' _      _       = vDoc
  mkVal t (mlCast' vTp tTp)

mlEnd :: Doc
mlEnd = text "end"

mlForEach :: (CommonRenderSym r vis stmt mthd) => r Variable ->
  r Value -> r Body -> Doc
mlForEach i lstVar b = vcat [
  text "for" <+> RC.variable i <+> equals <+> RC.value lstVar,
  indent $ RC.body b,
  mlEnd]

mlRange :: (CommonRenderSym r vis stmt mthd) => SValue r -> SValue r ->
  SValue r -> SValue r
mlRange initv finalv stepv = do
  ini <- initv
  fin <- finalv
  stp <- stepv
  d <- double
  mkVal d (RC.value ini <> text ":" <> RC.value stp <> text ":" <> RC.value fin)

mlTryCatch :: (CommonRenderSym r vis stmt mthd) => r Body -> r Body -> Doc
mlTryCatch tryB catchB = vcat [
  text "try",
  indent $ RC.body tryB,
  text "catch" <+> text "e",
  indent $ RC.body catchB,
  mlEnd]

mlArrayElem :: SValue MatlabCode -> SValue MatlabCode -> SVariable MatlabCode
mlArrayElem arr' i' = do
  i <- intToIndex i'
  arr <- arr'
  mkStateVar (render $ RC.value arr) (A.innerType $ return $ valueType arr)
    (RC.value arr <> parens (RC.value i))

mlListSet :: SValue MatlabCode -> SValue MatlabCode -> SValue MatlabCode
  -> MS (MatlabCode (Doc, Terminator))
mlListSet lst' idx' val' = do
  lst <- zoom lensMStoVS lst'
  idx <- zoom lensMStoVS (intToIndex idx')
  let lvar = mkStateVar (render $ RC.value lst)
               (A.innerType $ return $ valueType lst)
               (RC.value lst <> parens (RC.value idx))
  lvar &= val'
