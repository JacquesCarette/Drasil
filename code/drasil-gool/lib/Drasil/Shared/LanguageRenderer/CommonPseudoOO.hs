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
  extFuncAppMixedArgs,funcType, extVar, Label, Library, Variable, MixedCall,
  bodyStatements, oneLiner, TypeSym(infile, outfile, innerType), TypeElim(..),
  getCodeType, getTypeString, VariableElim(variableName, variableType),
  ValueSym(valueType), Comparison(..), (&=), ValueStatement(valStmt),
  ControlStatement(returnStmt), VisibilitySym(..), MethodSym(function),
  funcApp, listSize, BlockSym)
import qualified Drasil.Shared.InterfaceCommon as IC
import Drasil.GOOL.InterfaceGOOL (Class, CSStateVar, OOTypeSym(obj),
  AttachmentSym(..), Initializers, objMethodCallNoParams, objMethodCall)
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
  TypeData, FuncData)
import Drasil.Shared.State (MS, VS, FS, CS, lensFStoCS, lensFStoMS, lensCStoMS,
  lensMStoVS, lensVStoMS, currParameters, getClassName, getLangImports,
  getLibImports, getModuleImports, setClassName, setCurrMain, setMainDoc,
  useVarName, setVarScope)

import Prelude hiding (print,pi,(<>))
import qualified Prelude as P ((<>))
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
intToIndex :: VS (r val) -> VS (r val)
intToIndex = id

-- | Convert an index to an integer in a 0-indexed language
--   Since GOOL is 0-indexed, no adjustments need be made
indexToInt :: VS (r val) -> VS (r val)
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
  :: (OORenderSym r vis scope typ param val stmt mthd stvr attch file mod bod block)
  => Label
  -> [MS (r param)]
  -> Initializers r val
  -> MS (r bod)
  -> MS (r mthd)
constructor fName ps is b = getClassName >>= (\c -> intMethod False fName
  public instanceLevel (RG.construct c) ps (RC.multiBody [initStmts is, b]))

doxFunc
  :: (BlockCommentSym r, RenderMethod r mthd)
  => String -> [String] -> Maybe String -> MS (r mthd) -> MS (r mthd)
doxFunc = docFunc functionDox

doxClass
  :: (BlockCommentSym r, RG.RenderClass r vis mthd stvr)
  => String -> CS (r Class) -> CS (r Class)
doxClass = docClass classDox

doxMod
  :: (BlockCommentSym r, RG.RenderFile r file mod)
  => String
  -> String
  -> String
  -> [String]
  -> String
  -> FS (r file)
  -> FS (r file)
doxMod = docMod moduleDox

-- Python, Java, and C# --

classVarAccess
  ::
    ( InternalVarElim r
    , RenderVariable r TypeData
    , UnRepr r TypeData
    , VariableElim r TypeData
    )
  => (Doc -> Doc -> Doc) -> VS (r TypeData) -> VS (r Variable) -> VS (r Variable)
classVarAccess f c' v'= do
  c <- c'
  v <- v'
  vr <- varFromData
    (variableBind v) (getTypeString c `access` variableName v)
    (toState $ variableType v) (f (renderType c) (RC.variable v))
  toState $ classVarAccessCheck vr

indexOf
  :: (TypeSym r typ, IC.IndexTranslator r val, IG.OOFunctionSym r typ val)
  => Label -> VS (r val) -> VS (r val) -> VS (r val)
indexOf f l v = IC.indexToInt $ IG.objAccess l (IG.func f IC.int [v])

contains
  :: (TypeSym r typ, IG.OOFunctionSym r typ val)
  => Label -> VS (r val) -> VS (r val) -> VS (r val)
contains f s v = IG.objAccess s (IG.func f IC.bool [v])

containsInt
  :: (TypeSym r typ, Comparison r val, IG.OOFunctionSym r typ val)
  => Label -> Label -> VS (r val) -> VS (r val) -> VS (r val)
containsInt f fn s v = contains f s v ?!= IG.objAccess s (IG.func fn IC.bool [])

discardFileLine
  :: (TypeSym r typ, IG.InternalValueExp r typ val, ValueStatement r val stmt)
  => Label -> VS (r val) -> MS (r stmt)
discardFileLine n f = valStmt $ objMethodCallNoParams IC.string f n

-- | An internal function for creating a class.
--   Parameters: render function, class name, scope, parent, class variables,
--               constructor(s), methods
intClass
  :: (RC.MethodElim r mthd, Monad r, RG.StateVarElim r stvr, RC.VisibilityElim r vis)
  => (Label -> Doc -> Doc -> Doc -> Doc -> Doc)
  -> Label
  -> r vis
  -> r ParentSpec
  -> [CSStateVar r stvr]
  -> [MS (r mthd)]
  -> [MS (r mthd)]
  -> CS (r Doc)
intClass f n s i svrs cstrs mths = do
  modify (setClassName n)
  svs <- onStateList (R.stateVarList . fmap RG.stateVar) svrs
  ms <- onStateList (vibcat . fmap RC.method) (zoom lensCStoMS <$> (cstrs P.<> mths))
  pure $ onCodeValue (\p -> f n p (RC.visibility s) svs ms) i

-- Python and C++ --

-- Parameters: Module name, Doc for imports, Doc to put at top of module (but
-- after imports), Doc to put at bottom of module, methods, classes
-- Renamed top to topDoc to fix shadowing error with RendererClassesOO top
buildModule
  :: (RG.ClassElim r, RC.MethodElim r mthd, RG.RenderMod r mod)
  => Label
  -> FS Doc
  -> FS Doc
  -> FS Doc
  -> [MS (r mthd)]
  -> [CS (r Class)]
  -> FS (r mod)
buildModule n imps topDoc bot fs cs = RG.modFromData n (do
  cls <- mapM (zoom lensFStoCS) cs
  fns <- mapM (zoom lensFStoMS) fs
  is <- imps
  tp <- topDoc
  bt <- bot
  pure $ R.module' is (vibcat (tp : fmap RG.class' cls))
    (vibcat (fmap RC.method fns P.<> [bt])))

-- Java and C# --

arrayType
  :: (TypeElim r TypeData, Monad r, UnRepr r TypeData)
  => VS (r TypeData) -> VS (r TypeData)
arrayType t' = do
  t <- t'
  typeFromData (Array (getCodeType t))
    (getTypeString t P.<> array) (renderType t <> brackets empty)

pi :: (RC.RenderValue r typ val, TypeSym r typ) => VS (r val)
pi = mkStateVal IC.double (text $ mathFunc "PI")

printSt
  :: (RC.RenderStatement r stmt, RC.ValueElim r val)
  => VS (r val) -> VS (r val) -> MS (r stmt)
printSt va' vb' = do
  va <- zoom lensMStoVS va'
  vb <- zoom lensMStoVS vb'
  mkStmt (R.print va vb)

arrayDec
  :: ( TypeSym r TypeData
     , ScopeElim r ScopeData
     , UnRepr r TypeData
     , InternalVarElim r
     , RC.RenderStatement r stmt
     , RC.ValueElim r val
     , VariableElim r TypeData
     )
  => VS (r val) -> VS (r Variable) -> r ScopeData -> MS (r stmt)
arrayDec n vr scp = do
  sz <- zoom lensMStoVS n
  v <- zoom lensMStoVS vr
  modify $ useVarName $ variableName v
  modify $ setVarScope (variableName v) (scopeData scp)
  let tp = variableType v
  innerTp <- zoom lensMStoVS $ innerType $ pure tp
  mkStmt $ renderType tp <+> RC.variable v <+> equals <+> new' <+>
    renderType innerTp <> brackets (RC.value sz)

arrayDecDef
  :: ( IC.DeclStatement r scope val stmt bod
     , RC.RenderStatement r stmt
     , RC.StatementElim r stmt
     , RC.ValueElim r val
     )
  => VS (r Variable) -> r scope -> [VS (r val)] -> MS (r stmt)
arrayDecDef v' scp vals' = do
  vs <- mapM (zoom lensMStoVS) vals'
  vd <- IC.varDec v' scp
  mkStmt (RC.statement vd <+> equals <+> braces (valueList vs))

openFileA
  :: (IC.AssignStatement r val stmt, TypeSym r typ, IC.Literal r typ val)
  => (VS (r val) -> VS (r typ) -> VS (r val) -> VS (r val))
  -> VS (r Variable)
  -> VS (r val)
  -> MS (r stmt)
openFileA f vr vl = vr &= f vl outfile IC.litTrue

forEach
  ::
    ( RC.BodyElim r bod
    , InternalVarElim r
    , RC.RenderStatement r stmt
    , UnRepr r TypeData
    , RC.ValueElim r val
    , VariableElim r TypeData
    )
  => Doc
  -> Doc
  -> Doc
  -> Doc
  -> VS (r Variable)
  -> VS (r val)
  -> MS (r bod)
  -> MS (r stmt)
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

docMain
  :: (OORenderSym r vis scope typ param val stmt mthd stvr attch file mod bod block)
  => MS (r bod) -> MS (r mthd)
docMain b = commentedFunc (docComment $ toState $ functionDox
  mainDesc [(args, argsDesc)] []) (IC.mainFunction b)

mainFunction
  :: ( AttachmentSym r attch
     , TypeSym r TypeData
     , IC.VariableSym r TypeData
     , MethodTypeSym r TypeData
     , OORenderMethod r vis TypeData param mthd attch bod
     , IC.ParameterSym r param
     , UnRepr r TypeData
     , Monad r
     , VisibilitySym r vis
     )
  => VS (r TypeData) -> Label -> MS (r bod) -> MS (r mthd)
mainFunction s n = RG.intFunc True n public classLevel (mType IC.void)
  [IC.param (IC.var args (s >>= (\argT -> typeFromData (List String)
  (render (renderType argT) P.<> array) (renderType argT <> array'))))]

-- | Used by the language renderers to build the module.
--   n is the module name
--   inc is the include
--   is is the import statements
--   ms is the class methods
--   cs is the classes
buildModule'
  ::
    ( OORenderSym r vis scope typ param val stmt mthd stvr attch file mod bod block
    , UnRepr r Doc
    )
  => Label
  -> (String -> r Doc)
  -> [Label]
  -> [MS (r mthd)]
  -> [CS (r Class)]
  -> FS (r mod)
buildModule' n inc is ms cs = RG.modFromData n (do
  cls <- mapM (zoom lensFStoCS)
          (if null ms then cs else IG.buildClass Nothing [] [] ms : cs)
  lis <- getLangImports
  libis <- getLibImports
  mis <- getModuleImports
  pure $ vibcat [
    vcat (RC.import' . inc <$> (lis P.<> sort (is P.<> libis) P.<> mis)),
    vibcat (RG.class' <$> cls)])

-- Java and C++ --

-- | First parameter is language name, rest similar to call from RendererClassesCommon
call'
  :: (InternalVarElim r, RC.RenderValue r typ val, RC.ValueElim r val)
  => String -> Maybe Library -> Maybe Doc -> MixedCall r typ val
call' l _ _ _ _ _ (_:_) = error $ namedArgError l
call' _ l o n t ps ns = call empty l o n t ps ns

namedArgError :: String -> String
namedArgError l = "Named arguments not supported in " P.<> l

listSizeFunc :: (TypeSym r typ, IG.OOFunctionSym r typ val) => VS (r FuncData)
listSizeFunc = IG.func "size" IC.int []

listAccessFunc'
  :: (ValueSym r typ val, IG.OOFunctionSym r typ val, TypeElim r typ)
  => Label -> VS (r typ) -> VS (r val) -> VS (r FuncData)
listAccessFunc' f t i = IG.func f t [intValue i]

-- C# and C++ --

stringRender :: String
stringRender = "string"

string :: (Monad r) => VS (r TypeData)
string = typeFromData String stringRender (text stringRender)

docInOutFunc
  :: (BlockCommentSym r, RenderMethod r mthd)
  => ([VS (r Variable)] -> [VS (r Variable)] -> [VS (r Variable)] -> MS (r bod) -> MS (r mthd))
  -> String
  -> [(String, VS (r Variable))]
  -> [(String, VS (r Variable))]
  -> [(String, VS (r Variable))]
  -> MS (r bod)
  -> MS (r mthd)
docInOutFunc f desc is [o] [] b = docFuncRepr functionDox desc (fst <$> is)
  [fst o] (f (snd <$> is) [snd o] [] b)
docInOutFunc f desc is [] [both] b = docFuncRepr functionDox desc (fst <$>
  both : is) [fst both] (f (snd <$> is) [] [snd both] b)
docInOutFunc f desc is os bs b = docFuncRepr functionDox desc (fst <$> bs P.<>
  is P.<> os) [] (f (snd <$> is) (snd <$> os) (snd <$> bs) b)

-- Python, Java, C#, and Swift --

bindingError :: String -> String
bindingError l = "AttachmentTag unimplemented in " P.<> l

notNull
  ::
    ( ValueSym r typ val
    , Comparison r val
    , IC.VariableSym r typ
    , IC.VariableValue r val
    )
  => String -> VS (r val) -> VS (r val)
notNull nil v = v ?!= IC.valueOf (IC.var nil $ onStateValue valueType v)

listDecDef
  ::
    ( IC.DeclStatement r scope val stmt bod
    , TypeSym r typ
    , IC.Literal r typ val
    , VariableElim r typ
    )
  => VS (r Variable) -> r scope -> [VS (r val)] -> MS (r stmt)
listDecDef v scp vals = do
  vr <- zoom lensMStoVS v
  let lst = IC.litList (innerType $ pure $ variableType vr) vals
  IC.varDecDef (pure vr) scp lst

setDecDef
  ::
    ( IC.DeclStatement r scope val stmt bod
    , TypeSym r typ
    , IC.Literal r typ val
    , VariableElim r typ
    )
  => VS (r Variable) -> r scope -> [VS (r val)] -> MS (r stmt)
setDecDef v scp vals = do
  vr <- zoom lensMStoVS v
  let st = IC.litSet (innerType $ pure $ variableType vr) vals
  IC.varDecDef (pure vr) scp st

setDec
  ::
    ( IC.DeclStatement r scope val stmt bod
    , RC.RenderStatement r stmt
    , RC.StatementElim r stmt
    )
  => (r val -> Doc)
  -> VS (r val)
  -> VS (r Variable)
  -> r scope
  -> MS (r stmt)
setDec f vl v scp = do
  sz <- zoom lensMStoVS vl
  vd <- IC.varDec v scp
  mkStmt (RC.statement vd <> f sz)

setMethodCall
  :: (TypeSym r typ, ValueSym r typ val, IG.InternalValueExp r typ val)
  => Label -> VS (r val) ->  VS (r val) -> VS (r val)
setMethodCall n a b = objMethodCall (innerType $ onStateValue valueType a) a n [b]

destructorError :: String -> String
destructorError l = "Destructors not allowed in " P.<> l

stateVarDef
  ::
    ( OORenderSym r vis scope typ param val stmt mthd stvr attch file mod bod block
    , Monad r
    )
  => r vis -> r attch -> VS (r Variable) -> VS (r val) -> CS (r Doc)
stateVarDef s p vr vl = zoom lensCStoMS $ onStateValue (toCode . R.stateVar
  (RC.visibility  s) (RG.perm p) . RC.statement)
  (RC.stmt $ IC.varDecDef vr IC.local vl)

constVar
  :: (CommonRenderSym r vis scope typ param val stmt mthd bod block, Monad r)
  => Doc -> r vis -> VS (r Variable) -> VS (r val) -> CS (r Doc)
constVar p s vr vl = zoom lensCStoMS $ onStateValue (toCode . R.stateVar
  (RC.visibility s) p . RC.statement) (RC.stmt $ IC.constDecDef vr IC.local vl)

-- Python, Java, C++, and Swift --

litArray
  :: (RC.RenderValue r typ val, IC.TypeSym r typ, RC.ValueElim r val)
  => (Doc -> Doc) -> VS (r typ) -> [VS (r val)] -> VS (r val)
litArray f t es = sequence es >>= (\elems -> mkStateVal (IC.arrayType t)
  (f $ valueList elems))

litSet
  :: (RC.RenderValue r typ val, IC.TypeSym r typ, RC.ValueElim r val)
  => (Doc -> Doc) -> (Doc -> Doc) -> VS (r typ) -> [VS (r val)] -> VS (r val)
litSet f1 f2 t es = sequence es >>= (\elems -> mkStateVal (IC.arrayType t)
  (f1 $ f2 $ valueList elems))

litSetFunc
  :: (RC.RenderValue r typ val, IC.TypeSym r typ, RC.ValueElim r val)
  => String -> VS (r typ) -> [VS (r val)] -> VS (r val)
litSetFunc s t es = sequence es >>= (\elems -> mkStateVal (IC.arrayType t)
  (text s <> parens (valueList elems)))

-- Python, C#, C++, and Swift--

extraClass
  :: (RG.RenderClass r vis mthd stvr, VisibilitySym r vis)
  =>  Label
  -> Maybe Label
  -> [CSStateVar r stvr]
  -> [MS (r mthd)]
  -> [MS (r mthd)]
  -> CS (r Class)
extraClass n = RG.intClass n public . RG.inherit

-- Java, C#, and Swift --

doubleRender :: String
doubleRender = "Double"

double :: (Monad r) => VS (r TypeData)
double = typeFromData Double doubleRender (text doubleRender)

openFileR
  :: (TypeSym r typ, IC.AssignStatement r val stmt)
  => (VS (r val) -> VS (r typ) -> VS (r val))
  -> VS (r Variable)
  -> VS (r val)
  -> MS (r stmt)
openFileR f vr vl = vr &= f vl infile

openFileW
  :: (IC.AssignStatement r val stmt, TypeSym r typ, IC.Literal r typ val)
  => (VS (r val) -> VS (r typ) -> VS (r val) -> VS (r val))
  -> VS (r Variable)
  -> VS (r val)
  -> MS (r stmt)
openFileW f vr vl = vr &= f vl outfile IC.litFalse

stateVar
  :: (Monad r, OORenderSym r vis scope typ param val stmt mthd stvr attch file mod bod block)
  => r vis -> r attch -> VS (r Variable) -> CS (r Doc)
stateVar s p v = zoom lensCStoMS $ onStateValue (toCode . R.stateVar
  (RC.visibility s) (RG.perm p) . RC.statement) (RC.stmt $ IC.varDec v IC.local)

-- Python and Swift --

self :: (OOTypeSym r typ, RenderVariable r typ) => VS (r Variable)
self = zoom lensVStoMS getClassName >>= (\l -> mkStateVar R.self (obj l)
  R.self')

multiAssign
  :: ( TypeSym r typ
     , IC.AssignStatement r val stmt
     , InternalVarElim r
     , RC.RenderValue r typ val
     , RC.RenderVariable r typ
     , RC.ValueElim r val
     )
  => (Doc -> Doc) -> [VS (r Variable)] -> [VS (r val)] -> MS (r stmt)
multiAssign _ [] _ = error "Attempt to write assign statement for no variables."
multiAssign _ _ [] = error "Attempt to write assign statement with no values."
multiAssign f vars vals = if length vals /= 1 && length vars /= length vals
  then error $ "Attempted multiple assign statement with different number " P.<>
    "of variables than values"
  else do
  vrs <- mapM (zoom lensMStoVS) vars
  vls <- mapM (zoom lensMStoVS) vals
  let wrapIfMult :: [a] -> Doc -> Doc
      wrapIfMult l = if length l > 1 then f else id
  mkStateVar "" IC.void (wrapIfMult vrs (variableList vrs)) &=
    mkStateVal IC.void (wrapIfMult vls (valueList vls))

multiReturn
  ::
    ( TypeSym r typ
    , IC.ControlStatement r val stmt bod
    , RC.RenderValue r typ val
    , RC.ValueElim r val
    )
  => (Doc -> Doc) -> [VS (r val)] -> MS (r stmt)
multiReturn _ [] = error "Attempt to write return statement with no values."
multiReturn _ [v] = returnStmt v
multiReturn f vs = do
  vs' <- mapM (zoom lensMStoVS) vs
  returnStmt $ mkStateVal IC.void $ f $ valueList vs'

listDec
  ::
    ( IC.DeclStatement r scope val stmt bod
    , TypeSym r typ
    , IC.Literal r typ val
    , VariableElim r typ
    )
  => VS (r Variable) -> r scope -> MS (r stmt)
listDec v scp = listDecDef v scp []

funcDecDef
  :: (OORenderSym r vis ScopeData typ param val stmt mthd stvr attch file mod bod block)
  => VS (r Variable)
  -> r ScopeData
  -> [VS (r Variable)]
  -> MS (r bod)
  -> MS (r stmt)
funcDecDef v scp ps b = do
  vr <- zoom lensMStoVS v
  modify $ useVarName $ variableName vr
  modify $ setVarScope (variableName vr) (scopeData scp)
  s <- get
  f <- function (variableName vr) private (pure $ variableType vr)
    (IC.param <$> ps) b
  modify (L.set currParameters (s ^. currParameters))
  mkStmtNoEnd $ RC.method f

inOutCall
  ::
    ( TypeSym r typ
    , RC.InternalAssignStmt r val stmt
    , ValueStatement r val stmt
    , IC.VariableValue r val
    )
  => (Label -> VS (r typ) -> [VS (r val)] -> VS (r val))
  -> Label
  -> [VS (r val)]
  -> [VS (r Variable)]
  -> [VS (r Variable)]
  -> MS (r stmt)
inOutCall f n ins [] [] = IC.valStmt $ f n IC.void ins
inOutCall f n ins outs both = RC.multiAssign rets [f n IC.void (fmap IC.valueOf
  both P.<> ins)]
  where rets = both P.<> outs

forLoopError :: String -> String
forLoopError l = "Classic for loops not available in " P.<> l P.<> ", use " P.<>
  "forRange, forEach, or while instead"

mainBody
  :: (RC.BodyElim r bod, RC.RenderMethod r mthd) => MS (r bod) -> MS (r mthd)
mainBody b = do
  modify setCurrMain
  bod <- b
  modify (setMainDoc $ RC.body bod)
  mthdFromData Pub empty

inOutFunc
  ::
    ( IC.VariableValue r val
    , IC.ParameterSym r param
    , TypeSym r typ
    , IC.ScopeSym r scope
    , IC.DeclStatement r scope val stmt bod
    , BlockSym r block stmt
    , IC.BodySym r bod block
    , VariableElim r typ
    , RenderBody r bod
    , RenderType r typ
    , RC.InternalControlStmt r val stmt
    )
  => (VS (r typ) -> [MS (r param)] -> MS (r bod) -> MS (r mthd))
  -> [VS (r Variable)]
  -> [VS (r Variable)]
  -> [VS (r Variable)]
  -> MS (r bod)
  -> MS (r mthd)
inOutFunc f ins [] [] b = f IC.void (IC.param <$> ins) b
inOutFunc f ins outs both b = f
  (multiType $ onStateValue variableType <$> rets)
  (fmap IC.pointerParam both P.<> fmap IC.param ins)
  (multiBody [bodyStatements $ (`IC.varDec` IC.local) <$> outs, b,
    oneLiner $ RC.multiReturn $ IC.valueOf <$> rets])
  where rets = both P.<> outs

docInOutFunc'
  :: (BlockCommentSym r, RenderMethod r mthd)
  => FuncDocRenderer
  -> ([VS (r Variable)] -> [VS (r Variable)] -> [VS (r Variable)] -> MS (r bod) -> MS (r mthd))
  -> String
  -> [(String, VS (r Variable))]
  -> [(String, VS (r Variable))]
  -> [(String, VS (r Variable))]
  -> MS (r bod) -> MS (r mthd)
docInOutFunc' dfr f desc is os bs b = docFuncRepr dfr desc (fst <$> bs P.<> is)
  (fst <$> bs P.<> os) (f (snd <$> is) (snd <$> os) (snd <$> bs) b)

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
modDoc' desc watermark as date m = m : [desc | not (null desc)] P.<>
      [docField authorDoc (stringList as) | not (null as)] P.<>
      [docField dateDoc date | not (null date)] P.<>
      [docField noteDoc watermark]

-- | Creates an arbitrary Markdown/DocC style field for documentation.
-- Takes two strings, one for the field type ('ty'), and another
-- for the field documentation ('info')
docField :: String -> String -> String
docField ty info = docCommandInit P.<> ty P.<> docCommandSep P.<> info

-- | Generates Markdown/DocC style function doc comment.
functionDoc :: FuncDocRenderer
functionDoc desc params returns = [desc | not (null desc)]
  P.<> fmap (\(v, vDesc) -> docCommandInit P.<> paramDoc P.<> " " P.<>
    v P.<> docCommandSep P.<> vDesc) params
  P.<> fmap ((docCommandInit P.<> returnDoc P.<> docCommandSep) ++) returns

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
  :: (TypeSym r typ, IC.Literal r typ val, IC.ValueExpression r typ val)
  => VS (r val) -> VS (r val)
openFileR' n = funcApp fileOpen infile [n, IC.litString fileR]
openFileW' n = funcApp fileOpen infile [n, IC.litString fileW]
openFileA' n = funcApp fileOpen infile [n, IC.litString fileA]

argExists
  ::
    ( IC.Literal r typ val
    , IC.CommandLineArgs r val
    , Comparison r val
    , IC.List r val
    )
  => Integer -> VS (r val)
argExists i = listSize IC.argsList ?> IC.litInt (fromIntegral $ i+1)

-- Python, C#, Swift, and Julia

listSet
  :: ( IC.AssignStatement r val stmt
     , ValueSym r typ val
     , IC.IndexTranslator r val
     , RC.RenderVariable r typ
     , RC.ValueElim r val
     )
  => VS (r val) -> VS (r val) -> VS (r val) -> MS (r stmt)
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
  ::
    ( TypeSym r typ
    , IC.Literal r typ val
    , IC.NumericExpression r val
    , RC.RenderValue r typ val
    , RC.ValueElim r val
    )
  => VS (r val) -> VS (r val)
intToIndex' v = v `smartAdd` IC.litInt 1

-- | Convert an index to an integer in a 1-indexed language
--   Since GOOL is 0-indexed, we need to subtract 1
indexToInt'
  ::
    ( TypeSym r typ
    , IC.Literal r typ val
    , IC.NumericExpression r val
    , RC.RenderValue r typ val
    , RC.ValueElim r val
    )
  => VS (r val) -> VS (r val)
indexToInt' v = v `smartSub` IC.litInt 1
