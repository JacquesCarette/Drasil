{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render C# code is contained in this module
module Language.Drasil.Code.Imperative.LanguageRenderer.CSharpRenderer (
  -- * C# Code Configuration -- defines syntax of all C# code
  CSharpCode(..)
) where

import Utils.Drasil (indent)

import Language.Drasil.Code.Code (CodeType(..))
import Language.Drasil.Code.Imperative.Symantics (Label, PackageSym(..), 
  ProgramSym(..), RenderSym(..), InternalFile(..), AuxiliarySym(..), 
  KeywordSym(..), PermanenceSym(..), BodySym(..), BlockSym(..), 
  ControlBlockSym(..), StateTypeSym(..), UnaryOpSym(..), BinaryOpSym(..), 
  VariableSym(..), ValueSym(..), NumericExpression(..), BooleanExpression(..), 
  ValueExpression(..), InternalValue(..), Selector(..), FunctionSym(..), 
  SelectorFunction(..), InternalFunction(..), InternalStatement(..), 
  StatementSym(..), ControlStatementSym(..), ScopeSym(..), InternalScope(..), 
  MethodTypeSym(..), ParameterSym(..), MethodSym(..), StateVarSym(..), 
  ClassSym(..), ModuleSym(..), BlockCommentSym(..))
import Language.Drasil.Code.Imperative.LanguageRenderer (addExt,
  fileDoc', moduleDocD, classDocD, enumDocD,
  enumElementsDocD, multiStateDocD, blockDocD, bodyDocD, printDoc, outDoc,
  printFileDocD, boolTypeDocD, intTypeDocD, charTypeDocD, stringTypeDocD, 
  typeDocD, enumTypeDocD, listTypeDocD, voidDocD, constructDocD, stateParamDocD,
  paramListDocD, mkParam, methodDocD, methodListDocD, 
  stateVarDocD, stateVarListDocD, ifCondDocD, switchDocD, forDocD, 
  forEachDocD, whileDocD, stratDocD, assignDocD, plusEqualsDocD, plusPlusDocD,
  varDecDocD, varDecDefDocD, listDecDocD, listDecDefDocD, objDecDefDocD, 
  constDecDefDocD, statementDocD, returnDocD, mkSt, mkStNoEnd, stringListVals',
  stringListLists', commentDocD, unOpPrec, notOpDocD, negateOpDocD, unExpr, 
  unExpr', typeUnExpr, powerPrec, equalOpDocD, notEqualOpDocD, greaterOpDocD, 
  greaterEqualOpDocD, lessOpDocD, lessEqualOpDocD, plusOpDocD, minusOpDocD, 
  multOpDocD, divideOpDocD, moduloOpDocD, andOpDocD, orOpDocD, binExpr, 
  binExpr', typeBinExpr, mkVal, litTrueD, litFalseD, litCharD, litFloatD, 
  litIntD, litStringD, varDocD, extVarDocD, selfDocD, argDocD, enumElemDocD, 
  objVarDocD, inlineIfD, funcAppDocD, extFuncAppDocD, stateObjDocD, 
  listStateObjDocD, notNullDocD, funcDocD, castDocD, listSetFuncDocD, 
  listAccessFuncDocD, objAccessDocD, castObjDocD, breakDocD, continueDocD, 
  staticDocD, dynamicDocD, privateDocD, publicDocD, dot, new, blockCmtStart, 
  blockCmtEnd, docCmtStart, observerListName, doxConfigName, makefileName, 
  doubleSlash, blockCmtDoc, docCmtDoc, commentedItem, addCommentsDocD, 
  functionDoc, classDoc, moduleDoc, docFuncRepr, valList, appendToBody, 
  surroundBody, getterName, setterName, setMain, setMainMethod, setEmpty, 
  intValue)
import Language.Drasil.Code.Imperative.Data (Terminator(..), AuxData(..), ad, 
  FileData(..), file, updateFileMod, FuncData(..), fd, ModData(..), md, 
  updateModDoc, MethodData(..), mthd, OpData(..), PackData(..), packD, 
  ParamData(..), pd, updateParamDoc, ProgData(..), progD, TypeData(..), td, 
  ValData(..), updateValDoc, VarData(..), vard)
import Language.Drasil.Code.Imperative.Doxygen.Import (makeDoxConfig)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig, Runnable, 
  asFragment, buildAll, nativeBinary, osClassDefault)
import Language.Drasil.Code.Imperative.Build.Import (makeBuild)
import Language.Drasil.Code.Imperative.Helpers (emptyIfEmpty, liftA4, liftA5, 
  liftA6, liftA7, liftList, lift1List, lift3Pair, lift4Pair,
  liftPair, liftPairFst, getInnerType, convType, checkParams)
import Prelude hiding (break,print,(<>),sin,cos,tan,floor)
import qualified Prelude as P ((<>))
import qualified Data.Map as Map (fromList,lookup)
import Data.Maybe (fromMaybe)
import Control.Applicative (Applicative, liftA2, liftA3)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), parens, comma, empty,
  semi, vcat, lbrace, rbrace, colon)

csExt :: String
csExt = "cs"

newtype CSharpCode a = CSC {unCSC :: a} deriving Eq

instance Functor CSharpCode where
  fmap f (CSC x) = CSC (f x)

instance Applicative CSharpCode where
  pure = CSC
  (CSC f) <*> (CSC x) = CSC (f x)

instance Monad CSharpCode where
  return = CSC
  CSC x >>= f = f x

instance PackageSym CSharpCode where
  type Package CSharpCode = PackData
  package = lift1List packD

instance ProgramSym CSharpCode where
  type Program CSharpCode = ProgData
  prog n = liftList (progD n)

instance RenderSym CSharpCode where
  type RenderFile CSharpCode = FileData
  fileDoc code = liftA2 file (fmap (addExt csExt . name) code) (liftA2 
    updateModDoc (liftA2 emptyIfEmpty (fmap modDoc code) $ liftA3 fileDoc' 
    (top code) (fmap modDoc code) bottom) code)

  docMod d a dt m = commentedMod (docComment $ moduleDoc d a dt $ filePath 
    (unCSC m)) m

  commentedMod cmt m = liftA2 updateFileMod (liftA2 updateModDoc
    (liftA2 commentedItem cmt (fmap (modDoc . fileMod) m)) (fmap fileMod m)) m

instance InternalFile CSharpCode where
  top _ = liftA2 cstop endStatement (include "")
  bottom = return empty

instance AuxiliarySym CSharpCode where
  type Auxiliary CSharpCode = AuxData
  doxConfig pName p = fmap (ad doxConfigName) (liftA2 (makeDoxConfig pName)
    optimizeDox p)

  optimizeDox = return $ text "NO"

  makefile cms = fmap (ad makefileName . makeBuild cms csBuildConfig csRunnable)

instance KeywordSym CSharpCode where
  type Keyword CSharpCode = Doc
  endStatement = return semi
  endStatementLoop = return empty

  include _ = return $ text "using"
  inherit = return colon

  list _ = return $ text "List"
  listObj = return new

  blockStart = return lbrace
  blockEnd = return rbrace

  ifBodyStart = blockStart
  elseIf = return $ text "else if"
  
  iterForEachLabel = return $ text "foreach"
  iterInLabel = return $ text "in"

  commentStart = return doubleSlash
  blockCommentStart = return blockCmtStart
  blockCommentEnd = return blockCmtEnd
  docCommentStart = return docCmtStart
  docCommentEnd = blockCommentEnd

instance PermanenceSym CSharpCode where
  type Permanence CSharpCode = Doc
  static_ = return staticDocD
  dynamic_ = return dynamicDocD

instance BodySym CSharpCode where
  type Body CSharpCode = Doc
  body = liftList bodyDocD
  bodyStatements = block
  oneLiner s = bodyStatements [s]

  addComments s = liftA2 (addCommentsDocD s) commentStart

instance BlockSym CSharpCode where
  type Block CSharpCode = Doc
  block sts = lift1List blockDocD endStatement (map (fmap fst . state) sts)

instance StateTypeSym CSharpCode where
  type StateType CSharpCode = TypeData
  bool = return boolTypeDocD
  int = return intTypeDocD
  float = return csFloatTypeDoc
  char = return charTypeDocD
  string = return stringTypeDocD
  infile = return csInfileTypeDoc
  outfile = return csOutfileTypeDoc
  listType p st = liftA2 listTypeDocD st (list p)
  listInnerType t = fmap (getInnerType . cType) t >>= convType
  obj t = return $ typeDocD t
  enumType t = return $ enumTypeDocD t
  iterator _ = error "Iterator-type variables do not exist in C#"
  void = return voidDocD

  getType = cType . unCSC

instance ControlBlockSym CSharpCode where
  runStrategy l strats rv av = maybe
    (strError l "RunStrategy called on non-existent strategy") 
    (liftA2 (flip stratDocD) (state resultState)) 
    (Map.lookup l (Map.fromList strats))
    where resultState = maybe (return (mkStNoEnd empty)) asgState av
          asgState v = maybe (strError l 
            "Attempt to assign null return to a Value") (assign v) rv
          strError n s = error $ "Strategy '" ++ n ++ "': " ++ s ++ "."

  listSlice vnew vold b e s = 
    let l_temp = "temp"
        var_temp = var l_temp (variableType vnew)
        v_temp = valueOf var_temp
        l_i = "i_temp"
        var_i = var l_i int
        v_i = valueOf var_i
    in
      block [
        listDec 0 var_temp,
        for (varDecDef var_i (fromMaybe (litInt 0) b)) 
          (v_i ?< fromMaybe (listSize vold) e) (maybe (var_i &++) (var_i &+=) s)
          (oneLiner $ valState $ listAppend v_temp (listAccess vold v_i)),
        vnew &= v_temp]

instance UnaryOpSym CSharpCode where
  type UnaryOp CSharpCode = OpData
  notOp = return notOpDocD
  negateOp = return negateOpDocD
  sqrtOp = return $ unOpPrec "Math.Sqrt"
  absOp = return $ unOpPrec "Math.Abs"
  logOp = return $ unOpPrec "Math.Log10"
  lnOp = return $ unOpPrec "Math.Log"
  expOp = return $ unOpPrec "Math.Exp"
  sinOp = return $ unOpPrec "Math.Sin"
  cosOp = return $ unOpPrec "Math.Cos"
  tanOp = return $ unOpPrec "Math.Tan"
  asinOp = return $ unOpPrec "Math.Asin"
  acosOp = return $ unOpPrec "Math.Acos"
  atanOp = return $ unOpPrec "Math.Atan"
  floorOp = return $ unOpPrec "Math.Floor"
  ceilOp = return $ unOpPrec "Math.Ceiling"

instance BinaryOpSym CSharpCode where
  type BinaryOp CSharpCode = OpData
  equalOp = return equalOpDocD
  notEqualOp = return notEqualOpDocD
  greaterOp = return greaterOpDocD
  greaterEqualOp = return greaterEqualOpDocD
  lessOp = return lessOpDocD
  lessEqualOp = return lessEqualOpDocD
  plusOp = return plusOpDocD
  minusOp = return minusOpDocD
  multOp = return multOpDocD
  divideOp = return divideOpDocD
  powerOp = return $ powerPrec "Math.Pow"
  moduloOp = return moduloOpDocD
  andOp = return andOpDocD
  orOp = return orOpDocD

instance VariableSym CSharpCode where
  type Variable CSharpCode = VarData
  var n t = liftA2 (vard n) t (return $ varDocD n) 
  const = var
  extVar l n t = liftA2 (vard $ l ++ "." ++ n) t (return $ extVarDocD l n)
  self l = liftA2 (vard "this") (obj l) (return selfDocD)
  enumVar e en = var e (enumType en)
  objVar o v = liftA2 (vard $ variableName o ++ "." ++ variableName v)
    (variableType v) (liftA2 objVarDocD o v)
  objVarSelf l n t = liftA2 (vard $ "this." ++ n) t (liftA2 objVarDocD (self l) 
    (var n t))
  listVar n p t = var n (listType p t)
  n `listOf` t = listVar n static_ t
  iterVar n t = var n (iterator t)

  ($->) = objVar

  variableName = varName . unCSC
  variableType = fmap varType
  variableDoc = varDoc . unCSC

instance ValueSym CSharpCode where
  type Value CSharpCode = ValData
  litTrue = liftA2 mkVal bool (return litTrueD)
  litFalse = liftA2 mkVal bool (return litFalseD)
  litChar c = liftA2 mkVal char (return $ litCharD c)
  litFloat v = liftA2 mkVal float (return $ litFloatD v)
  litInt v = liftA2 mkVal int (return $ litIntD v)
  litString s = liftA2 mkVal string (return $ litStringD s)

  ($:) = enumElement

  valueOf v = liftA2 mkVal (variableType v) (return $ variableDoc v) 
  arg n = liftA2 mkVal string (liftA2 argDocD (litInt n) argsList)
  enumElement en e = liftA2 mkVal (enumType en) (return $ enumElemDocD en e)
  
  argsList = liftA2 mkVal (listType static_ string) (return $ text "args")

  valueType = fmap valType
  valueDoc = valDoc . unCSC

instance NumericExpression CSharpCode where
  (#~) = liftA2 unExpr' negateOp
  (#/^) = liftA2 unExpr sqrtOp
  (#|) = liftA2 unExpr absOp
  (#+) = liftA3 binExpr plusOp
  (#-) = liftA3 binExpr minusOp
  (#*) = liftA3 binExpr multOp
  (#/) = liftA3 binExpr divideOp
  (#%) = liftA3 binExpr moduloOp
  (#^) = liftA3 binExpr' powerOp

  log = liftA2 unExpr logOp
  ln = liftA2 unExpr lnOp
  exp = liftA2 unExpr expOp
  sin = liftA2 unExpr sinOp
  cos = liftA2 unExpr cosOp
  tan = liftA2 unExpr tanOp
  csc v = litFloat 1.0 #/ sin v
  sec v = litFloat 1.0 #/ cos v
  cot v = litFloat 1.0 #/ tan v
  arcsin = liftA2 unExpr asinOp
  arccos = liftA2 unExpr acosOp
  arctan = liftA2 unExpr atanOp
  floor = liftA2 unExpr floorOp
  ceil = liftA2 unExpr ceilOp

instance BooleanExpression CSharpCode where
  (?!) = liftA3 typeUnExpr notOp bool
  (?&&) = liftA4 typeBinExpr andOp bool
  (?||) = liftA4 typeBinExpr orOp bool

  (?<) = liftA4 typeBinExpr lessOp bool
  (?<=) = liftA4 typeBinExpr lessEqualOp bool
  (?>) = liftA4 typeBinExpr greaterOp bool
  (?>=) = liftA4 typeBinExpr greaterEqualOp bool
  (?==) = liftA4 typeBinExpr equalOp bool
  (?!=) = liftA4 typeBinExpr notEqualOp bool
  
instance ValueExpression CSharpCode where
  inlineIf = liftA3 inlineIfD
  funcApp n t vs = liftA2 mkVal t (liftList (funcAppDocD n) vs)
  selfFuncApp = funcApp
  extFuncApp l n t vs = liftA2 mkVal t (liftList (extFuncAppDocD l n) vs)
  stateObj t vs = liftA2 mkVal t (liftA2 stateObjDocD t (liftList valList vs))
  extStateObj _ = stateObj
  listStateObj t vs = liftA2 mkVal t (liftA3 listStateObjDocD listObj t 
    (liftList valList vs))

  exists = notNull
  notNull v = liftA2 mkVal bool (liftA3 notNullDocD notEqualOp v (valueOf (var
    "null" (fmap valType v))))

instance InternalValue CSharpCode where
  inputFunc = liftA2 mkVal string (return $ text "Console.ReadLine()")
  printFunc = liftA2 mkVal void (return $ text "Console.Write")
  printLnFunc = liftA2 mkVal void (return $ text "Console.WriteLine")
  printFileFunc f = liftA2 mkVal void (fmap (printFileDocD "Write") f)
  printFileLnFunc f = liftA2 mkVal void (fmap (printFileDocD "WriteLine") f)
  
  cast = csCast

instance Selector CSharpCode where
  objAccess v f = liftA2 mkVal (fmap funcType f) (liftA2 objAccessDocD v f)
  ($.) = objAccess

  objMethodCall t o f ps = objAccess o (func f t ps)
  objMethodCallNoParams t o f = objMethodCall t o f []

  selfAccess l = objAccess (valueOf $ self l)

  listIndexExists l i = listSize l ?> i
  argExists i = listAccess argsList (litInt $ fromIntegral i)

  indexOf l v = objAccess l (func "IndexOf" int [v])

instance FunctionSym CSharpCode where
  type Function CSharpCode = FuncData
  func l t vs = liftA2 fd t (fmap funcDocD (funcApp l t vs))

  get v vToGet = v $. getFunc vToGet
  set v vToSet toVal = v $. setFunc (valueType v) vToSet toVal

  listSize v = v $. listSizeFunc
  listAdd v i vToAdd = v $. listAddFunc v i vToAdd
  listAppend v vToApp = v $. listAppendFunc vToApp
  
  iterBegin v = v $. iterBeginFunc (valueType v)
  iterEnd v = v $. iterEndFunc (valueType v)

instance SelectorFunction CSharpCode where
  listAccess v i = v $. listAccessFunc (listInnerType $ valueType v) i
  listSet v i toVal = v $. listSetFunc v i toVal
  at v l = listAccess v (valueOf $ var l int)

instance InternalFunction CSharpCode where
  getFunc v = func (getterName $ variableName v) (variableType v) []
  setFunc t v toVal = func (setterName $ variableName v) t [toVal]

  listSizeFunc = liftA2 fd int (fmap funcDocD (valueOf (var "Count" int)))
  listAddFunc _ i v = func "Insert" (fmap valType v) [i, v]
  listAppendFunc v = func "Add" (fmap valType v) [v]

  iterBeginFunc _ = error "Attempt to use iterBeginFunc in C#, but C# has no iterators"
  iterEndFunc _ = error "Attempt to use iterEndFunc in C#, but C# has no iterators"

  listAccessFunc t v = liftA2 fd t (listAccessFuncDocD <$> intValue v)
  listSetFunc v i toVal = liftA2 fd (valueType v) 
    (liftA2 listSetFuncDocD (intValue i) toVal)

  atFunc t l = listAccessFunc t (valueOf $ var l int)

instance InternalStatement CSharpCode where
  printSt _ p v _ = mkSt <$> liftA2 printDoc p v
  
  state = fmap statementDocD
  loopState = fmap (statementDocD . setEmpty)

instance StatementSym CSharpCode where
  type Statement CSharpCode = (Doc, Terminator)
  assign vr vl = mkSt <$> liftA2 assignDocD vr vl
  assignToListIndex lst index v = valState $ listSet (valueOf lst) index v
  multiAssign _ _ = error "No multiple assignment statements in C#"
  (&=) = assign
  (&-=) vr vl = vr &= (valueOf vr #- vl)
  (&+=) vr vl = mkSt <$> liftA2 plusEqualsDocD vr vl
  (&++) v = mkSt <$> fmap plusPlusDocD v
  (&~-) v = v &= (valueOf v #- litInt 1)

  varDec v = mkSt <$> fmap varDecDocD v
  varDecDef v def = mkSt <$> liftA2 varDecDefDocD v def
  listDec n v = mkSt <$> liftA2 listDecDocD v (litInt n)
  listDecDef v vs = mkSt <$> lift1List listDecDefDocD v vs
  objDecDef v def = mkSt <$> liftA2 objDecDefDocD v def
  objDecNew v vs = mkSt <$> liftA2 objDecDefDocD v (stateObj (variableType v) 
    vs)
  extObjDecNew _ = objDecNew
  objDecNewVoid v = mkSt <$> liftA2 objDecDefDocD v (stateObj (variableType v) 
    [])
  extObjDecNewVoid _ = objDecNewVoid
  constDecDef v def = mkSt <$> liftA2 constDecDefDocD v def

  print v = outDoc False printFunc v Nothing
  printLn v = outDoc True printLnFunc v Nothing
  printStr s = outDoc False printFunc (litString s) Nothing
  printStrLn s = outDoc True printLnFunc (litString s) Nothing

  printFile f v = outDoc False (printFileFunc f) v (Just f)
  printFileLn f v = outDoc True (printFileLnFunc f) v (Just f)
  printFileStr f s = outDoc False (printFileFunc f) (litString s) (Just f)
  printFileStrLn f s = outDoc True (printFileLnFunc f) (litString s) (Just f)

  getInput v = v &= liftA2 csInput (variableType v) inputFunc
  discardInput = mkSt <$> fmap csDiscardInput inputFunc
  getFileInput f v = v &= liftA2 csInput (variableType v) (fmap csFileInput f)
  discardFileInput f = valState $ fmap csFileInput f

  openFileR f n = f &= liftA2 csOpenFileR n infile
  openFileW f n = f &= liftA3 csOpenFileWorA n outfile litFalse
  openFileA f n = f &= liftA3 csOpenFileWorA n outfile litTrue
  closeFile f = valState $ objMethodCall void f "Close" []

  getFileInputLine = getFileInput
  discardFileLine f = valState $ fmap csFileInput f
  stringSplit d vnew s = assign vnew $ listStateObj (listType dynamic_ string) 
    [s $. func "Split" (listType static_ string) [litChar d]]

  stringListVals = stringListVals'
  stringListLists = stringListLists'

  break = return (mkSt breakDocD)
  continue = return (mkSt continueDocD)

  returnState v = mkSt <$> liftList returnDocD [v]
  multiReturn _ = error "Cannot return multiple values in C#"

  valState v = mkSt <$> fmap valDoc v

  comment cmt = mkStNoEnd <$> fmap (commentDocD cmt) commentStart

  free _ = error "Cannot free variables in C#" -- could set variable to null? Might be misleading.

  throw errMsg = mkSt <$> fmap csThrowDoc (litString errMsg)

  initState fsmName initialState = varDecDef (var fsmName string) (litString initialState)
  changeState fsmName toState = var fsmName string &= litString toState

  initObserverList t = listDecDef (var observerListName t)
  addObserver o = valState $ listAdd obsList lastelem o
    where obsList = valueOf $ observerListName `listOf` valueType o
          lastelem = listSize obsList

  inOutCall = csInOutCall funcApp
  extInOutCall m = csInOutCall (extFuncApp m)

  multi = lift1List multiStateDocD endStatement

instance ControlStatementSym CSharpCode where
  ifCond bs b = mkStNoEnd <$> lift4Pair ifCondDocD ifBodyStart elseIf blockEnd b
    bs
  ifNoElse bs = ifCond bs $ body []
  switch v cs c = mkStNoEnd <$> lift3Pair switchDocD (state break) v c cs
  switchAsIf v cs = ifCond cases
    where cases = map (\(l, b) -> (v ?== l, b)) cs

  ifExists v ifBody = ifCond [(notNull v, ifBody)]

  for sInit vGuard sUpdate b = mkStNoEnd <$> liftA6 forDocD blockStart blockEnd 
    (loopState sInit) vGuard (loopState sUpdate) b
  forRange i initv finalv stepv = for (varDecDef (var i int) initv) 
    (valueOf (var i int) ?< finalv) (var i int &+= stepv)
  forEach l v b = mkStNoEnd <$> liftA7 (forEachDocD l) blockStart blockEnd 
    iterForEachLabel iterInLabel (listInnerType $ valueType v) v b
  while v b = mkStNoEnd <$> liftA4 whileDocD blockStart blockEnd v b

  tryCatch tb cb = mkStNoEnd <$> liftA2 csTryCatch tb cb

  checkState l = switch (valueOf $ var l string)
  notifyObservers f t = for initv (v_index ?< listSize obsList) 
    (var_index &++) notify
    where obsList = valueOf $ observerListName `listOf` t
          index = "observerIndex"
          var_index = var index int
          v_index = valueOf var_index
          initv = varDecDef var_index $ litInt 0
          notify = oneLiner $ valState $ at obsList index $. f

  getFileInputAll f v = while ((f $. liftA2 fd bool (return $ text 
    ".EndOfStream")) ?!) (oneLiner $ valState $ listAppend (valueOf v) (fmap 
    csFileInput f))

instance ScopeSym CSharpCode where
  type Scope CSharpCode = Doc
  private = return privateDocD
  public = return publicDocD

instance InternalScope CSharpCode where
  includeScope s = s

instance MethodTypeSym CSharpCode where
  type MethodType CSharpCode = TypeData
  mState t = t
  construct n = return $ td (Object n) (constructDocD n)

instance ParameterSym CSharpCode where
  type Parameter CSharpCode = ParamData
  stateParam = fmap (mkParam stateParamDocD)
  pointerParam = stateParam

  parameterName = paramName . unCSC
  parameterType = fmap paramType

instance MethodSym CSharpCode where
  type Method CSharpCode = MethodData
  method n _ s p t ps b = liftA2 (mthd False) (checkParams n <$> sequence ps) 
    (liftA5 (methodDocD n) s p t (liftList paramListDocD ps) b)
  getMethod c v = method (getterName $ variableName v) c public dynamic_ 
    (mState $ variableType v) [] getBody
    where getBody = oneLiner $ returnState (valueOf $ self c $-> v)
  setMethod c v = method (setterName $ variableName v) c public dynamic_ 
    (mState void) [stateParam v] setBody
    where setBody = oneLiner $ (self c $-> v) &= valueOf v
  mainMethod c b = setMainMethod <$> method "Main" c public static_ 
    (mState void) [liftA2 (pd "args") (listType static_ string) 
    (return $ text "string[] args")] b
  privMethod n c = method n c private dynamic_
  pubMethod n c = method n c public dynamic_
  constructor n = method n n public dynamic_ (construct n)
  destructor _ _ = error "Destructors not allowed in C#"

  docMain c b = commentedFunc (docComment $ functionDoc 
    "Controls the flow of the program" 
    [("args", "List of command-line arguments")]) (mainMethod c b)

  function n = method n ""

  docFunc = docFuncRepr

  inOutFunc n s p ins [v] [] b = function n s p (mState $ variableType v) 
    (map stateParam ins) (liftA3 surroundBody (varDec v) b (returnState $ 
    valueOf v))
  inOutFunc n s p ins [] [v] b = function n s p (mState $ variableType v) 
    (map stateParam $ v : ins) (liftA2 appendToBody b (returnState $ valueOf v))
  inOutFunc n s p ins outs both b = function n s p (mState void) (map (fmap 
    (updateParamDoc csRef) . stateParam) both ++ map stateParam ins ++ map (fmap (updateParamDoc csOut) . stateParam) outs) b

  docInOutFunc desc iComms oComms bComms = docFuncRepr desc 
    (bComms ++ iComms ++ oComms)

  commentedFunc cmt fn = liftA3 mthd (fmap isMainMthd fn) (fmap mthdParams fn)
    (liftA2 commentedItem cmt (fmap mthdDoc fn))
  
  parameters m = map return $ (mthdParams . unCSC) m

instance StateVarSym CSharpCode where
  type StateVar CSharpCode = Doc
  stateVar _ s p v = liftA4 stateVarDocD (includeScope s) p v endStatement
  privMVar del = stateVar del private dynamic_
  pubMVar del = stateVar del public dynamic_
  pubGVar del = stateVar del public static_

instance ClassSym CSharpCode where
  -- Bool is True if the method is a main method, False otherwise
  type Class CSharpCode = (Doc, Bool)
  buildClass n p s vs fs = liftPairFst (liftA4 (classDocD n p) inherit s 
    (liftList stateVarListDocD vs) (liftList methodListDocD (map (fmap mthdDoc) 
    fs)), any (isMainMthd . unCSC) fs)
  enum n es s = liftPairFst (liftA2 (enumDocD n) (return $ 
    enumElementsDocD es False) s, False)
  mainClass n vs fs = setMain <$> buildClass n Nothing public vs fs
  privClass n p = buildClass n p private
  pubClass n p = buildClass n p public

  docClass d = commentedClass (docComment $ classDoc d)

  commentedClass cmt cs = liftPair (liftA2 commentedItem cmt (fmap fst cs), 
    fmap snd cs)

instance ModuleSym CSharpCode where
  type Module CSharpCode = ModData
  buildModule n _ ms cs = fmap (md n (any (isMainMthd . unCSC) ms || 
    any (snd . unCSC) cs)) (liftList moduleDocD (if null ms then cs 
    else pubClass n Nothing [] ms : cs))
    
  moduleName m = name (unCSC m)

instance BlockCommentSym CSharpCode where
  type BlockComment CSharpCode = Doc
  blockComment lns = liftA2 (blockCmtDoc lns) blockCommentStart blockCommentEnd
  docComment lns = liftA2 (docCmtDoc lns) docCommentStart docCommentEnd

cstop :: Doc -> Doc -> Doc
cstop end inc = vcat [
  inc <+> text "System" <> end,
  inc <+> text "System.IO" <> end,
  inc <+> text "System.Collections" <> end,
  inc <+> text "System.Collections.Generic" <> end]

csFloatTypeDoc :: TypeData
csFloatTypeDoc = td Float (text "double") -- Same as Java, maybe make a common function

csInfileTypeDoc :: TypeData
csInfileTypeDoc = td File (text "StreamReader")

csOutfileTypeDoc :: TypeData
csOutfileTypeDoc = td File (text "StreamWriter")

csCast :: CSharpCode (StateType CSharpCode) -> CSharpCode (Value CSharpCode) -> 
  CSharpCode (Value CSharpCode)
csCast t v = csCast' (getType t) (getType $ valueType v)
  where csCast' Float String = funcApp "Double.Parse" float [v]
        csCast' _ _ = liftA2 mkVal t $ liftA2 castObjDocD (fmap castDocD t) v

csThrowDoc :: ValData -> Doc
csThrowDoc errMsg = text "throw new" <+> text "Exception" <> 
  parens (valDoc errMsg)

csTryCatch :: Doc -> Doc -> Doc
csTryCatch tb cb= vcat [
  text "try" <+> lbrace,
  indent tb,
  rbrace <+> text "catch" <+> parens (text "Exception" <+> text "exc") <+> 
    lbrace,
  indent cb,
  rbrace]

csDiscardInput :: ValData -> Doc
csDiscardInput = valDoc

csInput :: TypeData -> ValData -> ValData
csInput t inFn = mkVal t $ text (csInput' (cType t)) <> 
  parens (valDoc inFn)
  where csInput' Integer = "Int32.Parse"
        csInput' Float = "Double.Parse"
        csInput' Boolean = "Boolean.Parse"
        csInput' String = ""
        csInput' Char = "Char.Parse"
        csInput' _ = error "Attempt to read value of unreadable type"

csFileInput :: ValData -> ValData
csFileInput f = mkVal (valType f) (valDoc f <> dot <> text "ReadLine()")

csOpenFileR :: ValData -> TypeData -> ValData
csOpenFileR n r = mkVal r $ new <+> typeDoc r <> 
  parens (valDoc n)

csOpenFileWorA :: ValData -> TypeData -> ValData -> ValData
csOpenFileWorA n w a = mkVal w $ new <+> typeDoc w <> 
  parens (valDoc n <> comma <+> valDoc a)

csRef :: Doc -> Doc
csRef p = text "ref" <+> p

csOut :: Doc -> Doc
csOut p = text "out" <+> p

csInOutCall :: (Label -> CSharpCode (StateType CSharpCode) -> 
  [CSharpCode (Value CSharpCode)] -> CSharpCode (Value CSharpCode)) -> Label -> 
  [CSharpCode (Value CSharpCode)] -> [CSharpCode (Variable CSharpCode)] -> 
  [CSharpCode (Variable CSharpCode)] -> CSharpCode (Statement CSharpCode)
csInOutCall f n ins [out] [] = assign out $ f n (variableType out) ins
csInOutCall f n ins [] [out] = assign out $ f n (variableType out) (valueOf out
  : ins)
csInOutCall f n ins outs both = valState $ f n void (map (fmap (updateValDoc 
  csRef) . valueOf) both ++ ins ++ map (fmap (updateValDoc csOut) . valueOf) 
  outs)

csBuildConfig :: Maybe BuildConfig
csBuildConfig = buildAll $ \i o -> [osClassDefault "CSC" "csc" "mcs", 
  asFragment "-out:" P.<> o] ++ i

csRunnable :: Runnable
csRunnable = nativeBinary