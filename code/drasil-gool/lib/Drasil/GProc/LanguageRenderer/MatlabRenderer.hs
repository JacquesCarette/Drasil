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

import Drasil.Shared.InterfaceCommon (Label, SValue, SVariable, getCodeType,
  UnRepr(..), SharedProg, BodySym(..), BlockSym(..), TypeSym(..), TypeElim(..),
  VariableSym(..), VariableElim(..), ValueSym(..), Argument(..), Literal(..),
  MathConstant(..), VariableValue(..), CommandLineArgs(..),
  NumericExpression(..), BooleanExpression(..), Comparison(..),
  ValueExpression(..), IndexTranslator(..), Reference(..), Array(..), List(..),
  Set(..), InternalList(..), StatementSym(..), AssignStatement(..),
  DeclStatement(..), IOStatement(..), StringStatement(..), FunctionSym(..),
  FuncAppStatement(..), CommentStatement(..), ControlStatement(..),
  VisibilitySym(..), ScopeSym(..), ParameterSym(..), BinderSym(..),
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
  docMod, fileFromData, buildModule, modFromData, function)
import qualified Drasil.Shared.LanguageRenderer as R (commentedMod,
  commentedItem, parameterList, body, addComments, multiStmt, sqrt, abs, log10,
  log, exp, sin, cos, tan, asin, acos, atan, floor, ceil, elseIfLabel)
import qualified Drasil.GProc.RendererClassesProc as RC (module')
import qualified Drasil.Shared.LanguageRenderer.LanguagePolymorphic as G (
  comment, param, docFunc, var, multiBody, block, multiBlock, stmt, loopStmt,
  negateOp, plusOp, minusOp, multOp, divideOp, equalOp, greaterOp,
  greaterEqualOp, lessOp, lessEqualOp, csc, sec, cot, valueOf, litDouble,
  litInt, litString, valStmt, emptyStmt, assign, funcAppMixedArgs, call, print,
  ifCond)
import qualified Drasil.Shared.LanguageRenderer.CommonPseudoOO as CP (mainBody,
  functionDoc, docInOutFunc', inOutCall, multiAssign)
import qualified Drasil.Shared.LanguageRenderer.CLike as C (andOp, orOp, litTrue,
  litFalse)
import qualified Drasil.Shared.LanguageRenderer.Common as CS (varDecDef,
  extFuncAppMixedArgs)
import Drasil.Shared.AST (Terminator(..), FileType(Combined), FileData, fileD,
  FuncData, ModData, md, updateMod, MethodData, mthd, updateMthd, ParamData,
  paramVar, paramDoc, pd, ProgData, TypeData, cType, ValData, vd, val, valPrec,
  valInt, valType, opDoc, opPrec, VarData, varName, varType, varBind, varDoc,
  vard, progD, mthdDoc, modDoc)
import Drasil.Shared.CodeType (CodeType(..))
import Drasil.Shared.LanguageRenderer.Constructors (typeFromData, unOpPrec,
  powerPrec, unExpr, unExpr', binExpr, mkStateVal, mkVal, compEqualPrec,
  typeUnExpr, typeBinExpr)
import Drasil.Shared.LanguageRenderer (listSep')
import Drasil.Shared.LanguageRenderer.LanguagePolymorphic (OptionalSpace(..))
import Drasil.Shared.Helpers (toCode, toState, onCodeValue, onStateValue,
  onCodeList, onStateList, on2CodeValues, on2StateValues, emptyIfEmpty)
import Drasil.Shared.State (MS, VS, FS, lensGStoFS, lensMStoVS, revFiles,
  setFileType, getMainDoc)

import Control.Lens.Zoom (zoom)
import Control.Monad.State (modify)

import Drasil.FileHandling.Legacy (indent)
import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import Text.PrettyPrint.HughesPJ (Doc, empty, text, (<>), (<+>), vcat, hcat,
  parens, brackets, braces, equals, punctuate)

newtype MatlabCode a = MLC {unMLC :: a} deriving Functor

instance Applicative MatlabCode where
  pure = MLC
  (MLC f) <*> (MLC x) = MLC (f x)

instance Monad MatlabCode where
  MLC x >>= f = f x

instance SharedProg MatlabCode TypeData Doc (Doc, Terminator)
instance ProcProg MatlabCode TypeData Doc (Doc, Terminator)

instance ProgramSym MatlabCode TypeData Doc (Doc, Terminator) where
  type Program MatlabCode = ProgData
  prog n st files = do
    fs <- mapM (zoom lensGStoFS) files
    modify revFiles
    pure $ onCodeList (progD n st) fs

instance CommonRenderSym MatlabCode TypeData Doc (Doc, Terminator)
instance ProcRenderSym MatlabCode TypeData Doc (Doc, Terminator)

instance UnRepr MatlabCode inner where
  unRepr = unMLC

instance FileSym MatlabCode TypeData Doc (Doc, Terminator) where
  type File MatlabCode = FileData
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

instance BodySym MatlabCode TypeData (Doc, Terminator) where
  type Body MatlabCode = Doc
  body = onStateList (onCodeList R.body)
  addComments s = onStateValue (onCodeValue (R.addComments s mlCmtStart))

instance RenderBody MatlabCode where
  multiBody = G.multiBody

instance BodyElim MatlabCode where
  body = unMLC

instance BlockSym MatlabCode TypeData (Doc, Terminator) where
  type Block MatlabCode = Doc
  block = G.block

instance RenderBlock MatlabCode where
  multiBlock = G.multiBlock

instance BlockElim MatlabCode where
  block = unMLC

instance TypeSym MatlabCode TypeData where
  bool = mlTy Boolean "logical"
  int = mlTy Integer "int"
  float = mlTy Float "double"
  double = mlTy Double "double"
  char = mlTy Char "char"
  string = mlTy String "string"
  infile = mlTy InFile "file"
  outfile = mlTy OutFile "file"
  referenceType = id -- Ignore reference types in "high-level" langauges for now; later on think about using boxed/unboxed types
  listType = undefined
  setType = undefined
  arrayType = undefined
  innerType = undefined
  funcType = undefined
  void = mlTy Void "void"

instance TypeElim MatlabCode TypeData where
  getCodeType = cType . unMLC

instance RenderType MatlabCode TypeData where
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

instance VariableSym MatlabCode TypeData where
  type Variable MatlabCode = VarData
  var = G.var
  constant = var
  extVar = undefined

instance VariableElim MatlabCode TypeData where
  variableName = varName . unMLC
  variableType = onCodeValue varType

instance InternalVarElim MatlabCode where
  variableBind = varBind . unMLC
  variable = varDoc . unMLC

instance RenderVariable MatlabCode TypeData where
  varFromData b n t' d = do
    t <- t'
    toState $ on2CodeValues (vard b n) t (toCode d)

instance ValueSym MatlabCode TypeData where
  type Value MatlabCode = ValData
  valueType v = valType <$> v

instance Argument MatlabCode TypeData where
  pointerArg = undefined

instance Literal MatlabCode TypeData where
  litTrue = C.litTrue
  litFalse = C.litFalse
  litChar = undefined
  litDouble = G.litDouble
  litFloat = undefined
  litInt = G.litInt
  litString = G.litString
  litArray = undefined
  litList = undefined
  litSet = undefined

instance MathConstant MatlabCode TypeData where
  pi = mkStateVal double (text "pi")

instance VariableValue MatlabCode TypeData where
  valueOf = G.valueOf

instance CommandLineArgs MatlabCode TypeData where
  -- Args come in through the entry function's varargin (1-based, cell-indexed).
  arg n = mlArg (litInt (n + 1))
  argsList = mkStateVal (arrayType string) (text "varargin")
  argExists = undefined

instance NumericExpression MatlabCode TypeData where
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

instance BooleanExpression MatlabCode TypeData where
  (?!) = typeUnExpr notOp bool
  (?&&) = typeBinExpr andOp bool
  (?||) = typeBinExpr orOp bool

instance Comparison MatlabCode TypeData where
  (?<) = typeBinExpr lessOp bool
  (?<=) = typeBinExpr lessEqualOp bool
  (?>) = typeBinExpr greaterOp bool
  (?>=) = typeBinExpr greaterEqualOp bool
  (?==) = typeBinExpr equalOp bool
  (?!=) = typeBinExpr notEqualOp bool

instance ValueExpression MatlabCode TypeData where
  inlineIf = undefined
  funcAppMixedArgs = G.funcAppMixedArgs
  extFuncAppMixedArgs = CS.extFuncAppMixedArgs
  libFuncAppMixedArgs = CS.extFuncAppMixedArgs
  lambda = undefined
  notNull = undefined

instance RenderValue MatlabCode TypeData where
  inputFunc = undefined
  printFunc = mlPrintFunc
  printLnFunc = mlPrintFunc
  printFileFunc _ = mlPrintFunc
  printFileLnFunc _ = mlPrintFunc
  cast = undefined
  call = G.call equals
  valFromData p i t' d = do
    t <- t'
    toState $ on2CodeValues (vd p i) t (toCode d)

instance ValueElim MatlabCode where
  valuePrec = valPrec . unMLC
  valueInt = valInt . unMLC
  value = val . unMLC

instance IndexTranslator MatlabCode TypeData where
  intToIndex = undefined
  indexToInt = undefined

instance Reference MatlabCode TypeData where
  makeRef = id
  maybeDeref = id

instance Array MatlabCode TypeData where
  arrayElem = undefined
  arrayLength = undefined
  arrayCopy = undefined

instance List MatlabCode TypeData (Doc, Terminator) where
  listSize = undefined
  listAdd = undefined
  listAppend = undefined
  listAccess = undefined
  listSet = undefined
  indexOf = undefined

instance Set MatlabCode TypeData where
  contains = undefined
  setAdd = undefined
  setRemove = undefined
  setUnion = undefined

instance InternalList MatlabCode TypeData where
  listSlice' = undefined

instance InternalListFunc MatlabCode TypeData where
  listAccessFunc = undefined

instance BinderSym MatlabCode TypeData where
  binder = undefined

instance BinderElim MatlabCode TypeData where
  binderName = undefined
  binderType = undefined

instance InternalBinderElim MatlabCode where
  binderElim = undefined

instance RenderFunction MatlabCode TypeData where
  funcFromData = undefined

instance FunctionElim MatlabCode TypeData where
  functionType = undefined
  function = undefined

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

instance StatementSym MatlabCode TypeData (Doc, Terminator) where
  valStmt = G.valStmt Semi
  emptyStmt = G.emptyStmt
  multi = onStateList (onCodeList R.multiStmt)

instance AssignStatement MatlabCode TypeData (Doc, Terminator) where
  assign = G.assign Semi
  (&-=) = undefined
  (&+=) = undefined
  (&++) = undefined
  (&--) = undefined

instance DeclStatement MatlabCode TypeData (Doc, Terminator) where
  varDec v scp = CS.varDecDef v scp Nothing
  varDecDef v scp e = CS.varDecDef v scp (Just e)
  setDec = undefined
  setDecDef = undefined
  listDec = undefined
  listDecDef = undefined
  arrayDec = undefined
  arrayDecDef = undefined
  constDecDef = undefined
  funcDecDef = undefined

instance IOStatement MatlabCode TypeData (Doc, Terminator) where
  print = G.print False Nothing printFunc
  printLn = G.print True Nothing printLnFunc
  printStr = G.print False Nothing printFunc . litString
  printStrLn = G.print True Nothing printLnFunc . litString
  printFile f = G.print False (Just f) printFunc
  printFileLn f = G.print True (Just f) printLnFunc
  printFileStr f = printFile f . litString
  printFileStrLn f = printFileLn f . litString
  getInput = undefined
  discardInput = undefined
  getFileInput f = mlInput (mlReadLine f)
  discardFileInput f = valStmt (mlReadLine f)
  openFileR f n = f &= funcApp "fopen" infile [n, litString "r"]
  openFileW f n = f &= funcApp "fopen" outfile [n, litString "w"]
  openFileA f n = f &= funcApp "fopen" outfile [n, litString "a"]
  closeFile f = valStmt $ funcApp "fclose" void [f]
  getFileInputLine = getFileInput
  discardFileLine = discardFileInput
  getFileInputAll = undefined

instance StringStatement MatlabCode TypeData (Doc, Terminator) where
  stringSplit = undefined
  stringListVals = undefined
  stringListLists = undefined

instance FunctionSym MatlabCode TypeData where
  type Function MatlabCode = FuncData

instance FuncAppStatement MatlabCode TypeData (Doc, Terminator) where
  inOutCall = CP.inOutCall funcApp
  extInOutCall = undefined

instance CommentStatement MatlabCode TypeData (Doc, Terminator) where
  comment = G.comment mlCmtStart

instance ControlStatement MatlabCode TypeData (Doc, Terminator) where
  break = undefined
  continue = undefined
  -- MATLAB has no `return <expr>`: a function returns by assigning its named
  -- output, so a return becomes `result = <value>;`.
  returnStmt v' = do
    v <- zoom lensMStoVS v'
    var mlRet (toState (valueType v)) &= v'
  throw = undefined
  ifCond = G.ifCond id empty (OSpace empty) R.elseIfLabel empty (text "end")
  switch = undefined
  ifExists = undefined
  for = undefined
  forRange = undefined
  forEach = undefined
  while = undefined
  tryCatch = undefined
  assert = undefined

instance VisibilitySym MatlabCode Doc where
  private = toCode empty
  public = toCode empty

instance RenderVisibility MatlabCode Doc where
  visibilityFromData = undefined

instance VisibilityElim MatlabCode Doc where
  visibility = unMLC

instance MethodTypeSym MatlabCode TypeData where
  type MethodType MatlabCode = TypeData
  mType = zoom lensMStoVS

instance ParameterSym MatlabCode TypeData where
  type Parameter MatlabCode = ParamData
  -- A MATLAB parameter is just the variable name.
  param = G.param mlParam
  pointerParam = param

instance RenderParam MatlabCode where
  paramFromData v' d = do
    v <- zoom lensMStoVS v'
    toState $ on2CodeValues pd v (toCode d)

instance ParamElim MatlabCode TypeData where
  parameterName = variableName . onCodeValue paramVar
  parameterType = variableType . onCodeValue paramVar
  parameter = paramDoc . unMLC

instance MethodSym MatlabCode TypeData Doc (Doc, Terminator) where
  type Method MatlabCode = MethodData
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

instance RenderMethod MatlabCode TypeData where
  commentedFunc cmt m = on2StateValues (on2CodeValues updateMthd) m
    (onStateValue (onCodeValue R.commentedItem) cmt)
  mthdFromData _ d = toState $ toCode $ mthd d

instance ProcRenderMethod MatlabCode TypeData Doc where
  intFunc _ n _ t ps b = do
    pms <- sequence ps
    tp  <- t
    bod <- b
    -- A function with a non-void return type declares the output variable
    -- 'result' in its header (function result = name(...)); returnStmt then
    -- assigns to it. Void functions declare no output.
    let outs = [text mlRet | cType (unMLC tp) /= Void]
    pure $ toCode $ mthd $ mlFuncDoc n outs pms (RC.body bod)

instance MethodElim MatlabCode where
  method = mthdDoc . unMLC

instance ModuleSym MatlabCode TypeData Doc (Doc, Terminator) where
  type Module MatlabCode = ModData
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
mlParam :: MatlabCode (Variable MatlabCode) -> Doc
mlParam = RC.variable

-- | Renders a MATLAB function: @function [outs] = name(ins) ... end@.
--   With no outputs the @[outs] =@ part is dropped; with a single output the
--   brackets are dropped (@function out = name(ins)@).
mlFuncDoc :: (CommonRenderSym r TypeData vis smt) => Label -> [Doc] ->
  [r (Parameter r)] -> Doc -> Doc
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
