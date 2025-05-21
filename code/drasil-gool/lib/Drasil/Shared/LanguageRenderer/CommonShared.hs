module Drasil.Shared.LanguageRenderer.CommonShared (
  bool, boolRender, extVar, funcType, 
  listDec, listDecDef, listAccessFunc, listSetFunc, listSize, listAdd, listAppend, litSet,
  intToIndex', indexToInt',
  extFuncAppMixedArgs, functionDoc, inOutFunc, docInOutFunc', inOutCall, multiReturn, multiAssign,
  openFileR', openFileW', openFileA', openFileR, openFileW, argExists,
  notNull, forLoopError, varDecDef, mainBody, forEach', litArray, stateVar, setDecDef, setDec, funcDecDef,
  fileR, fileOpen, fileW, fileA
) where

import Utils.Drasil (indent, stringList)

import Drasil.Shared.CodeType (CodeType(..))

import Drasil.Shared.InterfaceCommon (Label, Library, Body, MSBody, VSFunction,
  VSType, Variable, SVariable, Value, SValue, MSStatement, MSParameter, SMethod,
  MixedCall, Visibility, bodyStatements, oneLiner, TypeSym(infile, outfile, listInnerType),
  TypeElim(getType, getTypeString), VariableElim(variableName, variableType),
  ValueSym(valueType), Comparison(..), (&=), ControlStatement(returnStmt),
  VisibilitySym(..), MethodSym(function), funcApp, ScopeSym(Scope))

import qualified Drasil.Shared.InterfaceCommon as IC (argsList,
  TypeSym(int, bool, double, string, listType, arrayType, void), VariableSym(var),
  Literal(litTrue, litFalse, litList, litSet, litInt, litString),
  VariableValue(valueOf), StatementSym(valStmt, emptyStmt), DeclStatement(varDec,
  varDecDef, constDecDef), List(intToIndex, indexToInt), ParameterSym(param,
  pointerParam), MethodSym(mainFunction), AssignStatement(assign), ScopeSym(..))
import Drasil.GOOL.InterfaceGOOL (SFile, FSModule, SClass, CSStateVar,
  OOTypeSym(obj), PermanenceSym(..), Initializers, objMethodCallNoParams, objMethodCall)
import qualified Drasil.GOOL.InterfaceGOOL as IG (ClassSym(buildClass),
  OOVariableSym(self, objVar), OOFunctionSym(..))
import Drasil.Shared.RendererClassesCommon (CommonRenderSym, ImportSym(..),
  RenderBody(..), RenderType(..), RenderVariable(varFromData),
  InternalVarElim(variableBind), RenderFunction(funcFromData),
  MethodTypeSym(mType), RenderMethod(commentedFunc, mthdFromData),
  BlockCommentSym(..), ScopeElim(scopeData))
import qualified Drasil.Shared.RendererClassesCommon as S (RenderBody(multiBody),
  RenderValue(call), RenderStatement(stmt),
  InternalAssignStmt(multiAssign), InternalControlStmt(multiReturn),
  InternalListFunc(listSizeFunc, listAddFunc, listAppendFunc))
import qualified Drasil.Shared.RendererClassesCommon as RC (ImportElim(..),
  BodyElim(..), InternalTypeElim(..), InternalVarElim(variable), ValueElim(..),
  StatementElim(statement), VisibilityElim(..), MethodElim(..), FunctionElim(..))
import Drasil.Shared.Helpers (vibcat, toCode, toState, onCodeValue, onStateValue,
  on2StateValues, onStateList)
import Drasil.GOOL.RendererClassesOO (OORenderSym, OORenderMethod(intMethod),
  ParentSpec)
import qualified Drasil.GOOL.RendererClassesOO as S (OOMethodTypeSym(construct),
  OORenderMethod(intFunc), RenderClass(intClass, inherit),
  RenderMod(modFromData))
import qualified Drasil.GOOL.RendererClassesOO as RC (PermElim(..),
  StateVarElim(..), ClassElim(..))
import Drasil.Shared.LanguageRenderer (array', new', args, array, listSep, access,
  mathFunc, ModuleDocRenderer, FuncDocRenderer, functionDox, classDox,
  moduleDox, variableList, valueList, intValue)
import qualified Drasil.Shared.LanguageRenderer as R (self, self', module',
  print, stateVar, stateVarList, constDecDef, extVar, listAccessFunc)
import Drasil.Shared.LanguageRenderer.Constructors (mkStmt, mkStmtNoEnd,
  mkStateVal, mkStateVar, mkVal, mkVal)
import Drasil.Shared.LanguageRenderer.LanguagePolymorphic (classVarCheckStatic,
  call, initStmts, docFunc, docFuncRepr, docClass, docMod, smartAdd, smartSub)
import Drasil.Shared.AST (VisibilityTag(..), ScopeTag(Global), ScopeData, sd)
import Drasil.Shared.State (FS, CS, lensFStoCS, lensFStoMS, lensCStoMS,
  lensMStoVS, lensVStoMS, currParameters, getClassName, getLangImports,
  getLibImports, getModuleImports, setClassName, setCurrMain, setMainDoc,
  useVarName, setVarScope)
import Drasil.Shared.LanguageRenderer.CommonPseudoOO as CP


import Prelude hiding (print,pi,(<>))
import Data.List (sort, intercalate)
import Control.Monad (join)
import Control.Monad.State (get, modify)
import Control.Lens ((^.))
import qualified Control.Lens as L (set)
import Control.Lens.Zoom (zoom)
import Text.PrettyPrint.HughesPJ (Doc, text, empty, render, (<>), (<+>), parens,
  brackets, braces, colon, vcat, equals)
import Metadata.Drasil.DrasilMetaCall (watermark)

-- Python, Java, C#, and Julia --

extVar :: (CommonRenderSym r) => Label -> Label -> VSType r -> SVariable r
extVar l n t = mkStateVar (l `access` n) t (R.extVar l n)

-- Python, Java, C++, and Julia --

funcType :: (CommonRenderSym r) => [VSType r] -> VSType r -> VSType r
funcType ps' r' =  do
  ps <- sequence ps'
  r <- r'
  typeFromData (Func (map getType ps) (getType r)) "" empty


docInOutFunc :: (CommonRenderSym r) => ([SVariable r] -> [SVariable r] ->
    [SVariable r] -> MSBody r -> SMethod r) ->
  String -> [(String, SVariable r)] -> [(String, SVariable r)] ->
  [(String, SVariable r)] -> MSBody r -> SMethod r
docInOutFunc f desc is [o] [] b = docFuncRepr functionDox desc (map fst is)
  [fst o] (f (map snd is) [snd o] [] b)
docInOutFunc f desc is [] [both] b = docFuncRepr functionDox desc (map fst $
  both : is) [fst both] (f (map snd is) [] [snd both] b)
docInOutFunc f desc is os bs b = docFuncRepr functionDox desc (map fst $ bs ++
  is ++ os) [] (f (map snd is) (map snd os) (map snd bs) b)


-- Python and Julia --

-- | For declaring and optionally defining a variable in a language where
--   declaring a variable before defining it is not required.
--   v is the variable to declare, and e is Nothing if we are not defining it,
--   and (Just d) if d is the value we are defining it as.
varDecDef :: (CommonRenderSym r) => SVariable r -> r (Scope r) -> Maybe (SValue r)
  -> MSStatement r
varDecDef v scp e = do
  v' <- zoom lensMStoVS v
  modify $ useVarName (variableName v')
  modify $ setVarScope (variableName v') (scopeData scp)
  def e
  where
    def Nothing = IC.emptyStmt
    def (Just d) = IC.assign v d


-- Python, Java, C#, C++, Swift and Julia --
-- | Convert an integer to an index in a 0-indexed language
--   Since GOOL is 0-indexed, no adjustments need be made
intToIndex :: SValue r -> SValue r
intToIndex = id

-- | Convert an index to an integer in a 0-indexed language
--   Since GOOL is 0-indexed, no adjustments need be made
indexToInt :: SValue r -> SValue r
indexToInt = id

-- Function Helpers

-- Python, Java, C#, Swift, and Julia --
extFuncAppMixedArgs :: (CommonRenderSym r) => Library -> MixedCall r
extFuncAppMixedArgs l = S.call (Just l) Nothing

notNull :: (CommonRenderSym r) => String -> SValue r -> SValue r
notNull nil v = v ?!= IC.valueOf (IC.var nil $ onStateValue valueType v)

listDecDef :: (CommonRenderSym r) => SVariable r -> r (Scope r) ->
  [SValue r] -> MSStatement r
listDecDef v scp vals = do
  vr <- zoom lensMStoVS v 
  let lst = IC.litList (listInnerType $ return $ variableType vr) vals
  IC.varDecDef (return vr) scp lst

setDecDef :: (CommonRenderSym r) => SVariable r -> r (Scope r) -> [SValue r] -> MSStatement r
setDecDef v scp vals = do
  vr <- zoom lensMStoVS v 
  let st = IC.litSet (listInnerType $ return $ variableType vr) vals
  IC.varDecDef (return vr) scp st

setDec :: (OORenderSym r) => (r (Value r) -> Doc) -> SValue r -> SVariable r -> r (Scope r) -> MSStatement r
setDec f vl v scp = do 
  sz <- zoom lensMStoVS vl
  vd <- IC.varDec v scp
  mkStmt (RC.statement vd <> f sz)

-- Python, Java, C++, Swift and Julia --

litArray :: (CommonRenderSym r) => (Doc -> Doc) -> VSType r -> [SValue r] -> SValue r
litArray f t es = sequence es >>= (\elems -> mkStateVal (IC.arrayType t) 
  (f $ valueList elems))

litSet :: (CommonRenderSym r) => (Doc -> Doc) -> (Doc -> Doc) -> VSType r -> [SValue r] -> SValue r
litSet f1 f2 t es = sequence es >>= (\elems -> mkStateVal (IC.arrayType t) 
  (f1 $ f2 $ valueList elems))

-- Python, C#, Swift, and Julia --

listAccessFunc :: (CommonRenderSym r) => VSType r -> SValue r -> VSFunction r
listAccessFunc t v = intValue v >>= ((`funcFromData` t) . R.listAccessFunc)

listSetFunc :: (CommonRenderSym r) => (Doc -> Doc -> Doc) -> SValue r -> SValue r ->
  SValue r -> VSFunction r
listSetFunc f v idx setVal = join $ on2StateValues (\i toVal -> funcFromData
  (f (RC.value i) (RC.value toVal)) (onStateValue valueType v)) (intValue idx)
  setVal

-- Java, C#, Swift and Julia --

openFileR :: (CommonRenderSym r) => (SValue r -> VSType r -> SValue r) -> SVariable r
  -> SValue r -> MSStatement r
openFileR f vr vl = vr &= f vl infile

openFileW :: (CommonRenderSym r) => (SValue r -> VSType r -> SValue r -> SValue r) ->
  SVariable r -> SValue r -> MSStatement r
openFileW f vr vl = vr &= f vl outfile IC.litFalse

stateVar :: (OORenderSym r, Monad r) => r (Visibility  r) -> r (Permanence r) ->
  SVariable r -> CS (r Doc)
stateVar s p v = zoom lensCStoMS $ onStateValue (toCode . R.stateVar
  (RC.visibility s) (RC.perm p) . RC.statement) (S.stmt $ IC.varDec v IC.local)


-- Python, Swift and Julia --

multiAssign :: (CommonRenderSym r) => (Doc -> Doc) -> [SVariable r] -> [SValue r] ->
  MSStatement r
multiAssign _ [] _ = error "Attempt to write assign statement for no variables."
multiAssign _ _ [] = error "Attempt to write assign statement with no values."
multiAssign f vars vals = if length vals /= 1 && length vars /= length vals
  then error $ "Attempted multiple assign statement with different number " ++
    "of variables than values"
  else do
  vrs <- mapM (zoom lensMStoVS) vars
  vls <- mapM (zoom lensMStoVS) vals
  let wrapIfMult :: [a] -> Doc -> Doc
      wrapIfMult l = if length l > 1 then f else id
  mkStateVar "" IC.void (wrapIfMult vrs (variableList vrs)) &= 
    mkStateVal IC.void (wrapIfMult vls (valueList vls))

multiReturn :: (CommonRenderSym r) => (Doc -> Doc) -> [SValue r] -> MSStatement r
multiReturn _ [] = error "Attempt to write return statement with no values."
multiReturn _ [v] = returnStmt v
multiReturn f vs = do
  vs' <- mapM (zoom lensMStoVS) vs
  returnStmt $ mkStateVal IC.void $ f $ valueList vs'

listDec :: (CommonRenderSym r) => SVariable r -> r (Scope r) -> MSStatement r
listDec v scp = listDecDef v scp []


funcDecDef :: (OORenderSym r) => SVariable r -> r (Scope r) -> [SVariable r] ->
  MSBody r -> MSStatement r
funcDecDef v scp ps b = do
  vr <- zoom lensMStoVS v
  modify $ useVarName $ variableName vr
  modify $ setVarScope (variableName vr) (scopeData scp)
  s <- get
  f <- function (variableName vr) private (return $ variableType vr) 
    (map IC.param ps) b
  modify (L.set currParameters (s ^. currParameters))
  mkStmtNoEnd $ RC.method f

inOutCall :: (CommonRenderSym r) => (Label -> VSType r -> [SValue r] -> SValue r) ->
  Label -> [SValue r] -> [SVariable r] -> [SVariable r] -> MSStatement r
inOutCall f n ins [] [] = IC.valStmt $ f n IC.void ins
inOutCall f n ins outs both = S.multiAssign rets [f n IC.void (map IC.valueOf 
  both ++ ins)]
  where rets = both ++ outs

forLoopError :: String -> String
forLoopError l = "Classic for loops not available in " ++ l ++ ", use " ++
  "forRange, forEach, or while instead"

mainBody :: (CommonRenderSym r) => MSBody r -> SMethod r
mainBody b = do
  modify setCurrMain
  bod <- b
  modify (setMainDoc $ RC.body bod)
  mthdFromData Pub empty

inOutFunc :: (CommonRenderSym r) => (VSType r -> [MSParameter r] -> MSBody r ->
  SMethod r) -> [SVariable r] -> [SVariable r] -> [SVariable r] -> MSBody r ->
  SMethod r
inOutFunc f ins [] [] b = f IC.void (map IC.param ins) b
inOutFunc f ins outs both b = f 
  (multiType $ map (onStateValue variableType) rets)  
  (map IC.pointerParam both ++ map IC.param ins) 
  (multiBody [bodyStatements $ map (`IC.varDec` IC.local) outs, b,
    oneLiner $ S.multiReturn $ map IC.valueOf rets])
  where rets = both ++ outs

docInOutFunc' :: (CommonRenderSym r) => FuncDocRenderer -> ([SVariable r] ->
    [SVariable r] -> [SVariable r] -> MSBody r -> SMethod r) ->
  String -> [(String, SVariable r)] -> [(String, SVariable r)] ->
  [(String, SVariable r)] -> MSBody r -> SMethod r
docInOutFunc' dfr f desc is os bs b = docFuncRepr dfr desc (map fst $ bs ++ is)
  (map fst $ bs ++ os) (f (map snd is) (map snd os) (map snd bs) b)

-- Python, Swift, and Julia --

forEach' :: (CommonRenderSym r) => (r (Variable r) -> r (Value r) -> r (Body r) -> Doc)
  -> SVariable r -> SValue r -> MSBody r -> MSStatement r
forEach' f i' v' b' = do
  i <- zoom lensMStoVS i'
  v <- zoom lensMStoVS v'
  b <- b'
  mkStmtNoEnd (f i v b)

-- Swift and Julia --
boolRender :: String
boolRender = "Bool"

bool :: (CommonRenderSym r) => VSType r
bool = typeFromData Boolean boolRender (text boolRender)

-- | Generates Markdown/DocC style function doc comment.
functionDoc :: FuncDocRenderer
functionDoc desc params returns = [desc | not (null desc)]
  ++ map (\(v, vDesc) -> CP.docCommandInit ++ CP.paramDoc ++ " " ++
    v ++ CP.docCommandSep ++ vDesc) params
  ++ map ((CP.docCommandInit ++ CP.returnDoc ++ CP.docCommandSep) ++) returns

openFileR', openFileW', openFileA' :: (CommonRenderSym r) => SValue r -> SValue r
openFileR' n = funcApp CP.fileOpen infile [n, IC.litString fileR]
openFileW' n = funcApp CP.fileOpen infile [n, IC.litString fileW]
openFileA' n = funcApp CP.fileOpen infile [n, IC.litString fileA]

argExists :: (CommonRenderSym r) => Integer -> SValue r
argExists i = listSize IC.argsList ?> IC.litInt (fromIntegral $ i+1)

-- Python, Julia, and MATLAB --

-- | Call to get the size of a list in a language where this is not a method.
listSize :: (CommonRenderSym r) => SValue r -> SValue r
listSize l = do
  f <- S.listSizeFunc l
  mkVal (RC.functionType f) (RC.function f)

-- Julia and MATLAB --

-- | Call to insert a value into a list in a language where this is not a method.
listAdd :: (CommonRenderSym r) => SValue r -> SValue r -> SValue r -> SValue r
listAdd l i v = do
  f <- S.listAddFunc l (IC.intToIndex i) v
  mkVal (RC.functionType f) (RC.function f)


-- | Call to append a value to a list in a language where this is not a method.
listAppend :: (CommonRenderSym r) => SValue r -> SValue r -> SValue r
listAppend l v = do
  f <- S.listAppendFunc l v
  mkVal (RC.functionType f) (RC.function f)

-- | Convert an integer to an index in a 1-indexed language
--   Since GOOL is 0-indexed, we need to add 1
intToIndex' :: (CommonRenderSym r) => SValue r -> SValue r
intToIndex' v = v `smartAdd` IC.litInt 1

-- | Convert an index to an integer in a 1-indexed language
--   Since GOOL is 0-indexed, we need to subtract 1
indexToInt' :: (CommonRenderSym r) => SValue r -> SValue r
indexToInt' v = v `smartSub` IC.litInt 1