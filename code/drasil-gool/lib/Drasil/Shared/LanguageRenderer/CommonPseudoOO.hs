{-# LANGUAGE FlexibleContexts #-}

-- | Implementations defined here are valid in some, but not all, language renderers
module Drasil.Shared.LanguageRenderer.CommonPseudoOO (
  int, constructor, doxFunc, doxClass, doxMod, modDoc', functionDoc, extVar,
  classVarAccess, indexOf, contains, containsInt, discardFileLine, intClass,
  funcType, buildModule, arrayType, pi, printSt, arrayDec, arrayDecDef,
  openFileA, forEach, docMain, mainFunction, buildModule', call', listSizeFunc,
  listAccessFunc', string, docInOutFunc, bindingError, extFuncAppMixedArgs,
  notNull, listDecDef, destructorError, stateVarDef, constVar, litArray, litSet,
  litSetFunc, extraClass, listAccessFunc, doubleRender, double, openFileR,
  openFileW, stateVar, self, multiAssign, multiReturn, listDec, funcDecDef,
  inOutCall, forLoopError, mainBody, inOutFunc, docInOutFunc', bool, floatRender,
  float, stringRender', string', inherit, implements, listSize, listSet,
  setDecDef, setDec, intToIndex, indexToInt, intToIndex', indexToInt', varDecDef,
  openFileR', openFileW', openFileA', argExists, global, setMethodCall
) where

import Utils.Drasil (stringList)
import Drasil.FileHandling.Legacy (indent)

import Drasil.Shared.CodeType (CodeType(..))

import Drasil.Shared.InterfaceCommon (UnRepr(..), varDecDef, bool,
  extFuncAppMixedArgs,funcType, extVar, Label, Library, MSBody, SVariable, Value,
  SValue, MixedCall, bodyStatements, oneLiner,
  TypeSym(infile, outfile, innerType), TypeElim(..), getCodeType, getTypeString,
  VariableElim(variableName, variableType), ValueSym(valueType), Comparison(..),
  (&=), ControlStatement(returnStmt), VisibilitySym(..),
  MethodSym(function), funcApp, listSize)
import qualified Drasil.Shared.InterfaceCommon as IC
import Drasil.GOOL.InterfaceGOOL (File, Module, SClass, CSStateVar,
  OOTypeSym(obj), AttachmentSym(..), Initializers, objMethodCallNoParams,
  objMethodCall, OOStatement)
import qualified Drasil.GOOL.InterfaceGOOL as IG
import Drasil.Shared.RendererClassesCommon (CommonRenderSym, RenderBody(..),
  RenderType(..), RenderVariable(varFromData), InternalVarElim(variableBind),
  MethodTypeSym(mType), RenderMethod(commentedFunc, mthdFromData),
  BlockCommentSym(..), ScopeElim(scopeData))
import qualified Drasil.Shared.RendererClassesCommon as RC
import Drasil.Shared.Helpers (vibcat, toCode, toState, onCodeValue, onStateValue,
  onStateList)
import Drasil.GOOL.RendererClassesOO (OORenderSym, OORenderMethod(intMethod),
  ParentSpec)
import qualified Drasil.GOOL.RendererClassesOO as RG
import Drasil.Shared.LanguageRenderer (listAccessFunc, array', new', args, array,
  listSep, access, mathFunc, ModuleDocRenderer, FuncDocRenderer, functionDox,
  classDox, moduleDox, variableList, valueList, intValue)
import Drasil.GOOL.Renderers (renderType)
import qualified Drasil.Shared.LanguageRenderer as R
import Drasil.Shared.LanguageRenderer.Constructors (mkStmt, mkStmtNoEnd,
  mkStateVal, mkStateVar, typeFromData, mkVar)
import Drasil.Shared.LanguageRenderer.LanguagePolymorphic (
  classVarAccessCheck, call, initStmts, docFunc, docFuncRepr, docClass,
  docMod, smartAdd, smartSub)
import Drasil.Shared.AST (VisibilityTag(..), ScopeTag(Global), ScopeData, sd,
  TypeData, ParamData, FuncData)
import Drasil.Shared.State (MS, VS, FS, CS, lensFStoCS, lensFStoMS, lensCStoMS,
  lensMStoVS, lensVStoMS, currParameters, getClassName, getLangImports,
  getLibImports, getModuleImports, setClassName, setCurrMain, setMainDoc,
  useVarName, setVarScope)

import Prelude hiding (print,pi,(<>))
import Data.List (sort, intercalate)
import Control.Monad.State (get, modify)
import Control.Lens ((^.))
import qualified Control.Lens as L
import Control.Lens.Zoom (zoom)
import Text.PrettyPrint.HughesPJ (Doc, text, empty, render, (<>), (<+>), parens,
  brackets, braces, colon, vcat, equals)

-- Python, Java, C#, C++, and Swift --
-- | Convert an integer to an index in a 0-indexed language
--   Since GOOL is 0-indexed, no adjustments need be made
intToIndex :: SValue r -> SValue r
intToIndex = id

-- | Convert an index to an integer in a 0-indexed language
--   Since GOOL is 0-indexed, no adjustments need be made
indexToInt :: SValue r -> SValue r
indexToInt = id

-- Global for langauges that don't use declarations for them
global :: (Monad r) => r ScopeData
global = toCode $ sd Global

-- Python, Java, C#, and C++ --

intRender :: String
intRender = "int"

int :: (Monad r) => VS (r TypeData)
int = typeFromData Integer intRender (text intRender)

constructor
  :: (OORenderSym r vis smt md svr att, OOStatement r smt)
  => Label -> [MS (r ParamData)] -> Initializers r -> MSBody r -> MS (r md)
constructor fName ps is b = getClassName >>= (\c -> intMethod False fName
  public instanceLevel (RG.construct c) ps (RC.multiBody [initStmts is, b]))

doxFunc :: (RenderMethod r md) => String -> [String] -> Maybe String ->
  MS (r md) -> MS (r md)
doxFunc = docFunc functionDox

doxClass :: (RG.RenderClass r vis md svr) => String -> SClass r -> SClass r
doxClass = docClass classDox

doxMod :: (RG.RenderFile r) => String -> String -> String -> [String] ->
  String -> FS (r File) -> FS (r File)
doxMod = docMod moduleDox

-- Python, Java, and C# --

classVarAccess
  :: (InternalVarElim r, RenderVariable r, UnRepr r TypeData, VariableElim r)
  => (Doc -> Doc -> Doc) -> VS (r TypeData) -> SVariable r -> SVariable r
classVarAccess f c' v'= do
  c <- c'
  v <- v'
  vr <- varFromData
    (variableBind v) (getTypeString c `access` variableName v)
    (toState $ variableType v) (f (renderType c) (RC.variable v))
  toState $ classVarAccessCheck vr

indexOf
  :: (IC.IndexTranslator r, IG.OOFunctionSym r)
  => Label -> SValue r -> SValue r -> SValue r
indexOf f l v = IC.indexToInt $ IG.objAccess l (IG.func f IC.int [v])

contains :: (IG.OOFunctionSym r) => Label -> SValue r -> SValue r -> SValue r
contains f s v = IG.objAccess s (IG.func f IC.bool [v])

containsInt
  :: (Comparison r, IG.OOFunctionSym r)
  => Label -> Label -> SValue r -> SValue r -> SValue r
containsInt f fn s v = contains f s v ?!= IG.objAccess s (IG.func fn IC.bool [])

discardFileLine
  :: (IG.InternalValueExp r, IC.StatementSym r smt)
  => Label -> SValue r -> MS (r smt)
discardFileLine n f = IC.valStmt $ objMethodCallNoParams IC.string f n

-- | An internal function for creating a class.
--   Parameters: render function, class name, scope, parent, class variables,
--               constructor(s), methods
intClass
  :: (RC.MethodElim r md, Monad r, RG.StateVarElim r svr, RC.VisibilityElim r vis)
  => (Label -> Doc -> Doc -> Doc -> Doc -> Doc)
  -> Label
  -> r vis
  -> r ParentSpec
  -> [CSStateVar r svr]
  -> [MS (r md)]
  -> [MS (r md)]
  -> CS (r Doc)
intClass f n s i svrs cstrs mths = do
  modify (setClassName n)
  svs <- onStateList (R.stateVarList . map RG.stateVar) svrs
  ms <- onStateList (vibcat . map RC.method) (map (zoom lensCStoMS) (cstrs ++ mths))
  return $ onCodeValue (\p -> f n p (RC.visibility s) svs ms) i

-- Python and C++ --

-- Parameters: Module name, Doc for imports, Doc to put at top of module (but
-- after imports), Doc to put at bottom of module, methods, classes
-- Renamed top to topDoc to fix shadowing error with RendererClassesOO top
buildModule
  :: (RG.ClassElim r, RC.MethodElim r md, RG.RenderMod r)
  => Label -> FS Doc -> FS Doc -> FS Doc -> [MS (r md)] -> [SClass r] -> FS (r Module)
buildModule n imps topDoc bot fs cs = RG.modFromData n (do
  cls <- mapM (zoom lensFStoCS) cs
  fns <- mapM (zoom lensFStoMS) fs
  is <- imps
  tp <- topDoc
  bt <- bot
  return $ R.module' is (vibcat (tp : map RG.class' cls))
    (vibcat (map RC.method fns ++ [bt])))

-- Java and C# --

arrayType :: (TypeElim r, Monad r, UnRepr r TypeData) =>
  VS (r TypeData) -> VS (r TypeData)
arrayType t' = do
  t <- t'
  typeFromData (Array (getCodeType t))
    (getTypeString t ++ array) (renderType t <> brackets empty)

pi :: (RC.RenderValue r, TypeSym r) => SValue r
pi = mkStateVal IC.double (text $ mathFunc "PI")

printSt
  :: (RC.RenderStatement r smt, RC.ValueElim r)
  => SValue r -> SValue r -> MS (r smt)
printSt va' vb' = do
  va <- zoom lensMStoVS va'
  vb <- zoom lensMStoVS vb'
  mkStmt (R.print va vb)

arrayDec
  :: ( ScopeElim r
     , UnRepr r TypeData
     , InternalVarElim r
     , RC.RenderStatement r smt
     , RC.ValueElim r
     , VariableElim r
     )
  => SValue r -> SVariable r -> r ScopeData -> MS (r smt)
arrayDec n vr scp = do
  sz <- zoom lensMStoVS n
  v <- zoom lensMStoVS vr
  modify $ useVarName $ variableName v
  modify $ setVarScope (variableName v) (scopeData scp)
  let tp = variableType v
  innerTp <- zoom lensMStoVS $ innerType $ return tp
  mkStmt $ renderType tp <+> RC.variable v <+> equals <+> new' <+>
    renderType innerTp <> brackets (RC.value sz)

arrayDecDef
  :: ( IC.DeclStatement r smt
     , RC.RenderStatement r smt
     , RC.StatementElim r smt
     , RC.ValueElim r
     )
  => SVariable r -> r ScopeData -> [SValue r] -> MS (r smt)
arrayDecDef v' scp vals' = do
  vs <- mapM (zoom lensMStoVS) vals'
  vd <- IC.varDec v' scp
  mkStmt (RC.statement vd <+> equals <+> braces (valueList vs))

openFileA
  :: (IC.AssignStatement r smt, IC.Literal r)
  => (SValue r -> VS (r TypeData) -> SValue r -> SValue r)
  -> SVariable r
  -> SValue r
  -> MS (r smt)
openFileA f vr vl = vr &= f vl outfile IC.litTrue

forEach
  :: ( RC.BodyElim r
     , InternalVarElim r
     , RC.RenderStatement r smt
     , UnRepr r TypeData
     , RC.ValueElim r
     , VariableElim r
     )
  => Doc -> Doc -> Doc -> Doc -> SVariable r -> SValue r -> MSBody r -> MS (r smt)
forEach bStart bEnd forEachLabel inLbl e' v' b' = do
  e <- zoom lensMStoVS e'
  v <- zoom lensMStoVS v'
  b <- b'
  mkStmtNoEnd $ vcat [
    forEachLabel <+> parens (renderType (variableType e) <+> RC.variable e <+>
      inLbl <+> RC.value v) <+> bStart,
    indent $ RC.body b,
    bEnd]

mainDesc, argsDesc :: String
mainDesc = "Controls the flow of the program"
argsDesc = "List of command-line arguments"

docMain :: (OORenderSym r vis smt md svr att) => MSBody r -> MS (r md)
docMain b = commentedFunc (docComment $ toState $ functionDox
  mainDesc [(args, argsDesc)] []) (IC.mainFunction b)

mainFunction
  :: ( AttachmentSym r att
     , OORenderMethod r vis md att
     , IC.ParameterSym r
     , UnRepr r TypeData
     , Monad r
     , VisibilitySym r vis
     )
  => VS (r TypeData) -> Label -> MSBody r -> MS (r md)
mainFunction s n = RG.intFunc True n public classLevel (mType IC.void)
  [IC.param (IC.var args (s >>= (\argT -> typeFromData (List String)
  (render (renderType argT) ++ array) (renderType argT <> array'))))]

-- | Used by the language renderers to build the module.
--   n is the module name
--   inc is the include
--   is is the import statements
--   ms is the class methods
--   cs is the classes
buildModule'
  :: (OORenderSym r vis smt md svr att, UnRepr r Doc)
  => Label
  -> (String -> r Doc)
  -> [Label]
  -> [MS (r md)]
  -> [SClass r]
  -> FS (r Module)
buildModule' n inc is ms cs = RG.modFromData n (do
  cls <- mapM (zoom lensFStoCS)
          (if null ms then cs else IG.buildClass Nothing [] [] ms : cs)
  lis <- getLangImports
  libis <- getLibImports
  mis <- getModuleImports
  return $ vibcat [
    vcat (map (RC.import' . inc) (lis ++ sort (is ++ libis) ++ mis)),
    vibcat (map RG.class' cls)])

-- Java and C++ --

-- | First parameter is language name, rest similar to call from RendererClassesCommon
call'
  :: (InternalVarElim r, RC.RenderValue r, RC.ValueElim r)
  => String -> Maybe Library -> Maybe Doc -> MixedCall r
call' l _ _ _ _ _ (_:_) = error $ namedArgError l
call' _ l o n t ps ns = call empty l o n t ps ns

namedArgError :: String -> String
namedArgError l = "Named arguments not supported in " ++ l

listSizeFunc :: (IG.OOFunctionSym r) => VS (r FuncData)
listSizeFunc = IG.func "size" IC.int []

listAccessFunc'
  :: (IG.OOFunctionSym r, TypeElim r)
  => Label -> VS (r TypeData) -> SValue r -> VS (r FuncData)
listAccessFunc' f t i = IG.func f t [intValue i]

-- C# and C++ --

stringRender :: String
stringRender = "string"

string :: (Monad r) => VS (r TypeData)
string = typeFromData String stringRender (text stringRender)

docInOutFunc
  :: (RenderMethod r md)
  => ([SVariable r] -> [SVariable r] -> [SVariable r] -> MSBody r -> MS (r md))
  -> String
  -> [(String, SVariable r)]
  -> [(String, SVariable r)]
  -> [(String, SVariable r)]
  -> MSBody r
  -> MS (r md)
docInOutFunc f desc is [o] [] b = docFuncRepr functionDox desc (map fst is)
  [fst o] (f (map snd is) [snd o] [] b)
docInOutFunc f desc is [] [both] b = docFuncRepr functionDox desc (map fst $
  both : is) [fst both] (f (map snd is) [] [snd both] b)
docInOutFunc f desc is os bs b = docFuncRepr functionDox desc (map fst $ bs ++
  is ++ os) [] (f (map snd is) (map snd os) (map snd bs) b)

-- Python, Java, C#, and Swift --

bindingError :: String -> String
bindingError l = "AttachmentTag unimplemented in " ++ l

notNull :: (Comparison r, IC.VariableValue r) => String -> SValue r -> SValue r
notNull nil v = v ?!= IC.valueOf (IC.var nil $ onStateValue valueType v)

listDecDef
  :: (IC.DeclStatement r smt, IC.Literal r, VariableElim r)
  => SVariable r -> r ScopeData -> [SValue r] -> MS (r smt)
listDecDef v scp vals = do
  vr <- zoom lensMStoVS v
  let lst = IC.litList (innerType $ return $ variableType vr) vals
  IC.varDecDef (return vr) scp lst

setDecDef
  :: (IC.DeclStatement r smt, IC.Literal r, VariableElim r)
  => SVariable r -> r ScopeData -> [SValue r] -> MS (r smt)
setDecDef v scp vals = do
  vr <- zoom lensMStoVS v
  let st = IC.litSet (innerType $ return $ variableType vr) vals
  IC.varDecDef (return vr) scp st

setDec
  :: (IC.DeclStatement r smt, RC.RenderStatement r smt, RC.StatementElim r smt)
  => (r Value -> Doc) -> SValue r -> SVariable r -> r ScopeData -> MS (r smt)
setDec f vl v scp = do
  sz <- zoom lensMStoVS vl
  vd <- IC.varDec v scp
  mkStmt (RC.statement vd <> f sz)

setMethodCall
  :: (IG.InternalValueExp r) => Label -> SValue r ->  SValue r -> SValue r
setMethodCall n a b = objMethodCall (innerType $ onStateValue valueType a) a n [b]

destructorError :: String -> String
destructorError l = "Destructors not allowed in " ++ l

stateVarDef
  :: (OORenderSym r vis smt md svr att, Monad r)
  => r vis -> r att -> SVariable r -> SValue r -> CS (r Doc)
stateVarDef s p vr vl = zoom lensCStoMS $ onStateValue (toCode . R.stateVar
  (RC.visibility  s) (RG.perm p) . RC.statement)
  (RC.stmt $ IC.varDecDef vr IC.local vl)

constVar :: (CommonRenderSym r vis smt md, Monad r) => Doc -> r vis ->
  SVariable r -> SValue r -> CS (r Doc)
constVar p s vr vl = zoom lensCStoMS $ onStateValue (toCode . R.stateVar
  (RC.visibility s) p . RC.statement) (RC.stmt $ IC.constDecDef vr IC.local vl)

-- Python, Java, C++, and Swift --

litArray
  :: (RC.RenderValue r, IC.TypeSym r, RC.ValueElim r)
  => (Doc -> Doc) -> VS (r TypeData) -> [SValue r] -> SValue r
litArray f t es = sequence es >>= (\elems -> mkStateVal (IC.arrayType t)
  (f $ valueList elems))

litSet
  :: (RC.RenderValue r, IC.TypeSym r, RC.ValueElim r)
  => (Doc -> Doc) -> (Doc -> Doc) -> VS (r TypeData) -> [SValue r] -> SValue r
litSet f1 f2 t es = sequence es >>= (\elems -> mkStateVal (IC.arrayType t)
  (f1 $ f2 $ valueList elems))

litSetFunc
  :: (RC.RenderValue r, IC.TypeSym r, RC.ValueElim r)
  => String -> VS (r TypeData) -> [SValue r] -> SValue r
litSetFunc s t es = sequence es >>= (\elems -> mkStateVal (IC.arrayType t)
  (text s <> parens (valueList elems)))

-- Python, C#, C++, and Swift--

extraClass
  :: (RG.RenderClass r vis md svr, VisibilitySym r vis)
  =>  Label -> Maybe Label -> [CSStateVar r svr] -> [MS (r md)] -> [MS (r md)] -> SClass r
extraClass n = RG.intClass n public . RG.inherit

-- Java, C#, and Swift --

doubleRender :: String
doubleRender = "Double"

double :: (Monad r) => VS (r TypeData)
double = typeFromData Double doubleRender (text doubleRender)

openFileR
  :: (IC.AssignStatement r smt)
  => (SValue r -> VS (r TypeData) -> SValue r)
  -> SVariable r
  -> SValue r
  -> MS (r smt)
openFileR f vr vl = vr &= f vl infile

openFileW
  :: (IC.AssignStatement r smt, IC.Literal r)
  => (SValue r -> VS (r TypeData) -> SValue r -> SValue r)
  -> SVariable r
  -> SValue r
  -> MS (r smt)
openFileW f vr vl = vr &= f vl outfile IC.litFalse

stateVar
  :: (Monad r, OORenderSym r vis smt md svr att)
  => r vis -> r att -> SVariable r -> CS (r Doc)
stateVar s p v = zoom lensCStoMS $ onStateValue (toCode . R.stateVar
  (RC.visibility s) (RG.perm p) . RC.statement) (RC.stmt $ IC.varDec v IC.local)

-- Python and Swift --

self :: (OOTypeSym r, RenderVariable r) => SVariable r
self = zoom lensVStoMS getClassName >>= (\l -> mkStateVar R.self (obj l)
  R.self')

multiAssign
  :: ( IC.AssignStatement r smt
     , InternalVarElim r
     , RC.RenderValue r
     , RC.RenderVariable r
     , RC.ValueElim r
     )
  => (Doc -> Doc) -> [SVariable r] -> [SValue r] -> MS (r smt)
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

multiReturn
  :: (IC.ControlStatement r smt, RC.RenderValue r, RC.ValueElim r)
  => (Doc -> Doc) -> [SValue r] -> MS (r smt)
multiReturn _ [] = error "Attempt to write return statement with no values."
multiReturn _ [v] = returnStmt v
multiReturn f vs = do
  vs' <- mapM (zoom lensMStoVS) vs
  returnStmt $ mkStateVal IC.void $ f $ valueList vs'

listDec
  :: (IC.DeclStatement r smt, IC.Literal r, VariableElim r)
  => SVariable r -> r ScopeData -> MS (r smt)
listDec v scp = listDecDef v scp []

funcDecDef
  :: (OORenderSym r vis smt md svr att)
  => SVariable r -> r ScopeData -> [SVariable r] -> MSBody r -> MS (r smt)
funcDecDef v scp ps b = do
  vr <- zoom lensMStoVS v
  modify $ useVarName $ variableName vr
  modify $ setVarScope (variableName vr) (scopeData scp)
  s <- get
  f <- function (variableName vr) private (return $ variableType vr)
    (map IC.param ps) b
  modify (L.set currParameters (s ^. currParameters))
  mkStmtNoEnd $ RC.method f

inOutCall
  :: (RC.InternalAssignStmt r smt, IC.StatementSym r smt, IC.VariableValue r)
  => (Label -> VS (r TypeData) -> [SValue r] -> SValue r)
  -> Label
  -> [SValue r]
  -> [SVariable r]
  -> [SVariable r]
  -> MS (r smt)
inOutCall f n ins [] [] = IC.valStmt $ f n IC.void ins
inOutCall f n ins outs both = RC.multiAssign rets [f n IC.void (map IC.valueOf
  both ++ ins)]
  where rets = both ++ outs

forLoopError :: String -> String
forLoopError l = "Classic for loops not available in " ++ l ++ ", use " ++
  "forRange, forEach, or while instead"

mainBody :: (RC.BodyElim r, RC.RenderMethod r md) => MSBody r -> MS (r md)
mainBody b = do
  modify setCurrMain
  bod <- b
  modify (setMainDoc $ RC.body bod)
  mthdFromData Pub empty

inOutFunc
  :: ( RC.InternalControlStmt r smt
     , IC.SharedStatement r smt
     , RenderBody r
     , RenderType r
     , VariableElim r
     )
  => (VS (r TypeData) -> [MS (r ParamData)] -> MSBody r -> MS (r md))
  -> [SVariable r]
  -> [SVariable r]
  -> [SVariable r]
  -> MSBody r
  -> MS (r md)
inOutFunc f ins [] [] b = f IC.void (map IC.param ins) b
inOutFunc f ins outs both b = f
  (multiType $ map (onStateValue variableType) rets)
  (map IC.pointerParam both ++ map IC.param ins)
  (multiBody [bodyStatements $ map (`IC.varDec` IC.local) outs, b,
    oneLiner $ RC.multiReturn $ map IC.valueOf rets])
  where rets = both ++ outs

docInOutFunc'
  :: (RenderMethod r md)
  => FuncDocRenderer
  -> ([SVariable r] -> [SVariable r] -> [SVariable r] -> MSBody r -> MS (r md))
  -> String
  -> [(String, SVariable r)]
  -> [(String, SVariable r)]
  -> [(String, SVariable r)]
  -> MSBody r -> MS (r md)
docInOutFunc' dfr f desc is os bs b = docFuncRepr dfr desc (map fst $ bs ++ is)
  (map fst $ bs ++ os) (f (map snd is) (map snd os) (map snd bs) b)

-- Java and Swift --

floatRender :: String
floatRender = "Float"

float :: (Monad r) => VS (r TypeData)
float = typeFromData Float floatRender (text floatRender)

stringRender' :: String
stringRender' = "String"

string' :: (Monad r) => VS (r TypeData)
string' = typeFromData String stringRender' (text stringRender')

-- C# and Swift --

inherit :: (Monad r) => Maybe Label -> r ParentSpec
inherit n = toCode $ maybe empty ((colon <+>) . text) n

implements :: (Monad r) => [Label] -> r ParentSpec
implements is = toCode $ colon <+> text (intercalate listSep is)

-- | Generates Markdown/DocC style module doc comment.  Useful for Swift, which follows
-- DocC, Julia, which uses Markdown, and any other language that doesn't have
-- Support for a document generator.
modDoc' :: ModuleDocRenderer
modDoc' desc watermark as date m = m : [desc | not (null desc)] ++
      [docField authorDoc (stringList as) | not (null as)] ++
      [docField dateDoc date | not (null date)] ++
      [docField noteDoc watermark]

-- | Creates an arbitrary Markdown/DocC style field for documentation.
-- Takes two strings, one for the field type ('ty'), and another
-- for the field documentation ('info')
docField :: String -> String -> String
docField ty info = docCommandInit ++ ty ++ docCommandSep ++ info

-- | Generates Markdown/DocC style function doc comment.
functionDoc :: FuncDocRenderer
functionDoc desc params returns = [desc | not (null desc)]
  ++ map (\(v, vDesc) -> docCommandInit ++ paramDoc ++ " " ++
    v ++ docCommandSep ++ vDesc) params
  ++ map ((docCommandInit ++ returnDoc ++ docCommandSep) ++) returns

docCommandInit, docCommandSep, authorDoc, dateDoc,
  noteDoc, paramDoc, returnDoc :: String
docCommandInit = "- "
docCommandSep = ": "
authorDoc = "Authors"
dateDoc = "Date"
noteDoc = "Note"
paramDoc = "Parameter"
returnDoc = "Returns"

-- | For declaring and optionally defining a variable in a language where
--   declaring a variable before defining it is not required.
--   v is the variable to declare, and e is Nothing if we are not defining it,
--   and (Just d) if d is the value we are defining it as.

fileOpen, fileR, fileW, fileA :: Label
fileOpen = "open"
fileR = "r"
fileW = "w"
fileA = "a"

openFileR', openFileW', openFileA'
  :: (IC.Literal r, IC.ValueExpression r) => SValue r -> SValue r
openFileR' n = funcApp fileOpen infile [n, IC.litString fileR]
openFileW' n = funcApp fileOpen infile [n, IC.litString fileW]
openFileA' n = funcApp fileOpen infile [n, IC.litString fileA]

argExists :: (IC.SharedStatement r smt) => Integer -> SValue r
argExists i = listSize IC.argsList ?> IC.litInt (fromIntegral $ i+1)

-- Python, C#, Swift, and Julia

listSet
  :: ( IC.AssignStatement r smt
     , IC.IndexTranslator r
     , RC.RenderVariable r
     , RC.ValueElim r
     )
  => SValue r -> SValue r -> SValue r -> MS (r smt)
listSet list idx val = do
  list' <- zoom lensMStoVS list
  idx' <- zoom lensMStoVS (IC.intToIndex idx)
  let listAccessVar = mkVar (render $ RC.value list') (valueType list')
                        (RC.value list' <> brackets (RC.value idx')) -- hack
  listAccessVar &= val

-- Julia and MATLAB --

-- | Convert an integer to an index in a 1-indexed language
--   Since GOOL is 0-indexed, we need to add 1
intToIndex'
  :: (IC.Literal r, IC.NumericExpression r, RC.RenderValue r, RC.ValueElim r)
  => SValue r -> SValue r
intToIndex' v = v `smartAdd` IC.litInt 1

-- | Convert an index to an integer in a 1-indexed language
--   Since GOOL is 0-indexed, we need to subtract 1
indexToInt'
  :: (IC.Literal r, IC.NumericExpression r, RC.RenderValue r, RC.ValueElim r)
  => SValue r -> SValue r
indexToInt' v = v `smartSub` IC.litInt 1
