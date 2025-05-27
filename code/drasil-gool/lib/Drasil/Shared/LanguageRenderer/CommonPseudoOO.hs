-- | Implementations defined here are valid in some, but not all, language renderers
module Drasil.Shared.LanguageRenderer.CommonPseudoOO where

import Utils.Drasil (indent, stringList)

import Drasil.Shared.CodeType (CodeType(..))

import Drasil.Shared.InterfaceCommon (Label, Library, Body, MSBody, VSFunction,
  VSType, Variable, SVariable, Value, SValue, MSStatement, MSParameter, SMethod,
  MixedCall, bodyStatements, oneLiner, TypeSym(infile, outfile, listInnerType),
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

import qualified Drasil.Shared.RendererClassesCommon as S 

import qualified Drasil.Shared.RendererClassesCommon as RC (ImportElim(..),
  BodyElim(..), InternalTypeElim(..), InternalVarElim(variable), ValueElim(..),
  StatementElim(statement), VisibilityElim(..), MethodElim(..), FunctionElim(..))

import Drasil.Shared.Helpers (vibcat, toCode, toState, onCodeValue, onStateValue,
  on2StateValues, onStateList)

import Drasil.GOOL.RendererClassesOO 

import qualified Drasil.GOOL.RendererClassesOO as S (OOMethodTypeSym(construct),
  OORenderMethod(intFunc), RenderClass(intClass, inherit),
  RenderMod(modFromData))

import qualified Drasil.GOOL.RendererClassesOO as RC (PermElim(..),
  StateVarElim(..), ClassElim(..))

import Drasil.Shared.LanguageRenderer (array', new', args, array, listSep, access,
  mathFunc, ModuleDocRenderer, FuncDocRenderer, functionDox, classDox,
  moduleDox, variableList, valueList, intValue)

import qualified Drasil.Shared.LanguageRenderer as R 

import Drasil.Shared.LanguageRenderer.Constructors (mkStmt, mkStmtNoEnd,
  mkStateVal, mkStateVar, mkVal, mkVal)

import Drasil.Shared.LanguageRenderer.LanguagePolymorphic (classVarCheckStatic,
  call, initStmts, docFunc, docFuncRepr, docClass, docMod, smartAdd, smartSub)

import Drasil.Shared.AST (VisibilityTag(..), ScopeTag(Global), ScopeData, sd)

import Drasil.Shared.State (FS, CS, lensFStoCS, lensFStoMS, lensCStoMS,
  lensMStoVS, lensVStoMS, currParameters, getClassName, getLangImports,
  getLibImports, getModuleImports, setClassName, setCurrMain, setMainDoc,
  useVarName, setVarScope)

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

int :: (CommonRenderSym r) => VSType r
int = typeFromData Integer intRender (text intRender)

constructor :: (OORenderSym r) => Label -> [MSParameter r] -> Initializers r ->
  MSBody r -> SMethod r
constructor fName ps is b = getClassName >>= (\c -> intMethod False fName
  public dynamic (S.construct c) ps (S.multiBody [initStmts is, b]))

doxFunc :: (CommonRenderSym r) => String -> [String] -> Maybe String -> SMethod r ->
  SMethod r
doxFunc = docFunc functionDox

doxClass :: (OORenderSym r) => String -> SClass r -> SClass r
doxClass = docClass classDox

doxMod :: (OORenderSym r) => String -> String -> [String] -> String -> SFile r ->
  SFile r
doxMod = docMod moduleDox

-- Python, Java, and C# --

classVar :: (CommonRenderSym r) => (Doc -> Doc -> Doc) -> VSType r -> SVariable r ->
  SVariable r
classVar f c' v'= do
  c <- c'
  v <- v'
  vr <- varFromData
    (variableBind v) (getTypeString c `access` variableName v)
    (toState $ variableType v) (f (RC.type' c) (RC.variable v))
  toState $ classVarCheckStatic vr

objVarSelf :: (OORenderSym r) => SVariable r -> SVariable r
objVarSelf = IG.objVar IG.self

indexOf :: (OORenderSym r) => Label -> SValue r -> SValue r -> SValue r
indexOf f l v = IC.indexToInt $ IG.objAccess l (IG.func f IC.int [v])

contains :: (OORenderSym r) => Label -> SValue r -> SValue r -> SValue r
contains f s v = IG.objAccess s (IG.func f IC.bool [v]) 

containsInt :: (OORenderSym r) => Label -> Label -> SValue r -> SValue r -> SValue r
containsInt f fn s v = contains f s v ?!= IG.objAccess s (IG.func fn IC.bool [])

listAddFunc :: (OORenderSym r) => Label -> SValue r -> SValue r -> VSFunction r
listAddFunc f i v = IG.func f (IC.listType $ onStateValue valueType v) 
  [i, v]

discardFileLine :: (OORenderSym r) => Label -> SValue r -> MSStatement r
discardFileLine n f = IC.valStmt $ objMethodCallNoParams IC.string f n 

-- | An internal function for creating a class.
--   Parameters: render function, class name, scope, parent, class variables,
--               constructor(s), methods
intClass :: (OORenderSym r, Monad r) => (Label -> Doc -> Doc -> Doc -> Doc ->
  Doc) -> Label -> r (Visibility r) -> r ParentSpec -> [CSStateVar r] ->
  [SMethod r]-> [SMethod r] -> CS (r Doc)
intClass f n s i svrs cstrs mths = do
  modify (setClassName n)
  svs <- onStateList (R.stateVarList . map RC.stateVar) svrs
  ms <- onStateList (vibcat . map RC.method) (map (zoom lensCStoMS) (cstrs ++ mths))
  return $ onCodeValue (\p -> f n p (RC.visibility s) svs ms) i


-- Python and C++ --

-- Parameters: Module name, Doc for imports, Doc to put at top of module (but 
-- after imports), Doc to put at bottom of module, methods, classes
buildModule :: (OORenderSym r) => Label -> FS Doc -> FS Doc -> FS Doc ->
  [SMethod r] -> [SClass r] -> FSModule r
buildModule n imps top bot fs cs = S.modFromData n (do
  cls <- mapM (zoom lensFStoCS) cs
  fns <- mapM (zoom lensFStoMS) fs
  is <- imps
  tp <- top
  bt <- bot
  return $ R.module' is (vibcat (tp : map RC.class' cls))
    (vibcat (map RC.method fns ++ [bt])))

-- Java and C# -- 

arrayType :: (CommonRenderSym r) => VSType r -> VSType r
arrayType t' = do
  t <- t'
  typeFromData (Array (getType t))
    (getTypeString t ++ array) (RC.type' t <> brackets empty)

pi :: (CommonRenderSym r) => SValue r
pi = mkStateVal IC.double (text $ mathFunc "PI")

printSt :: (CommonRenderSym r) => SValue r -> SValue r -> MSStatement r
printSt va' vb' = do
  va <- zoom lensMStoVS va'
  vb <- zoom lensMStoVS vb'
  mkStmt (R.print va vb)

arrayDec :: (CommonRenderSym r) => SValue r -> SVariable r -> r (Scope r)
  -> MSStatement r
arrayDec n vr scp = do
  sz <- zoom lensMStoVS n
  v <- zoom lensMStoVS vr
  modify $ useVarName $ variableName v
  modify $ setVarScope (variableName v) (scopeData scp)
  let tp = variableType v
  innerTp <- zoom lensMStoVS $ listInnerType $ return tp
  mkStmt $ RC.type' tp <+> RC.variable v <+> equals <+> new' <+>
    RC.type' innerTp <> brackets (RC.value sz)

arrayDecDef :: (CommonRenderSym r) => SVariable r -> r (Scope r) ->
  [SValue r] -> MSStatement r
arrayDecDef v' scp vals' = do
  vs <- mapM (zoom lensMStoVS) vals'
  vd <- IC.varDec v' scp
  mkStmt (RC.statement vd <+> equals <+> braces (valueList vs))

openFileA :: (CommonRenderSym r) => (SValue r -> VSType r -> SValue r -> SValue r) ->
  SVariable r -> SValue r -> MSStatement r
openFileA f vr vl = vr &= f vl outfile IC.litTrue

forEach :: (CommonRenderSym r) => Doc -> Doc -> Doc -> Doc -> SVariable r -> SValue r
  -> MSBody r -> MSStatement r
forEach bStart bEnd forEachLabel inLbl e' v' b' = do
  e <- zoom lensMStoVS e'
  v <- zoom lensMStoVS v'
  b <- b'
  mkStmtNoEnd $ vcat [
    forEachLabel <+> parens (RC.type' (variableType e) <+> RC.variable e <+>
      inLbl <+> RC.value v) <+> bStart,
    indent $ RC.body b,
    bEnd]

mainDesc, argsDesc :: String
mainDesc = "Controls the flow of the program"
argsDesc = "List of command-line arguments"

docMain :: (OORenderSym r) => MSBody r -> SMethod r
docMain b = commentedFunc (docComment $ toState $ functionDox 
  mainDesc [(args, argsDesc)] []) (IC.mainFunction b)

mainFunction :: (OORenderSym r) => VSType r -> Label -> MSBody r -> SMethod r
mainFunction s n = S.intFunc True n public static (mType IC.void)
  [IC.param (IC.var args (s >>= (\argT -> typeFromData (List String) 
  (render (RC.type' argT) ++ array) (RC.type' argT <> array'))))]

-- | Used by the language renderers to build the module.
--   n is the module name
--   inc is the include
--   is is the import statements
--   ms is the class methods
--   cs is the classes
buildModule' :: (OORenderSym r) => Label -> (String -> r (Import r)) -> [Label] 
  -> [SMethod r] -> [SClass r] -> FSModule r
buildModule' n inc is ms cs = S.modFromData n (do
  cls <- mapM (zoom lensFStoCS) 
          (if null ms then cs else IG.buildClass Nothing [] [] ms : cs)
  lis <- getLangImports
  libis <- getLibImports
  mis <- getModuleImports
  return $ vibcat [
    vcat (map (RC.import' . inc) (lis ++ sort (is ++ libis) ++ mis)),
    vibcat (map RC.class' cls)])

-- Java and C++ --

-- | First parameter is language name, rest similar to call from RendererClassesCommon
call' :: (CommonRenderSym r) => String -> Maybe Library -> Maybe Doc -> MixedCall r
call' l _ _ _ _ _ (_:_) = error $ namedArgError l
call' _ l o n t ps ns = call empty l o n t ps ns

namedArgError :: String -> String
namedArgError l = "Named arguments not supported in " ++ l

listSizeFunc :: (OORenderSym r) => VSFunction r
listSizeFunc = IG.func "size" IC.int []

listAccessFunc' :: (OORenderSym r) => Label -> VSType r -> SValue r ->
  VSFunction r
listAccessFunc' f t i = IG.func f t [intValue i]

-- C# and C++ --

stringRender :: String
stringRender = "string"

string :: (CommonRenderSym r) => VSType r
string = typeFromData String stringRender (text stringRender)

constDecDef :: (CommonRenderSym r) => SVariable r -> r (Scope r) -> SValue r
  -> MSStatement r
constDecDef vr' scp v'= do
  vr <- zoom lensMStoVS vr'
  v <- zoom lensMStoVS v'
  modify $ useVarName $ variableName vr
  modify $ setVarScope (variableName vr) (scopeData scp)
  mkStmt (R.constDecDef vr v)

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

-- Python, Java, C#, and Swift --

bindingError :: String -> String
bindingError l = "Binding unimplemented in " ++ l

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

setMethodCall :: (OORenderSym r) => Label -> SValue r ->  SValue r -> SValue r
setMethodCall n a b = objMethodCall (listInnerType $ onStateValue valueType a) a n [b] 

destructorError :: String -> String
destructorError l = "Destructors not allowed in " ++ l

stateVarDef :: (OORenderSym r, Monad r) => r (Visibility  r) -> r (Permanence r) ->
  SVariable r -> SValue r -> CS (r Doc)
stateVarDef s p vr vl = zoom lensCStoMS $ onStateValue (toCode . R.stateVar
  (RC.visibility  s) (RC.perm p) . RC.statement)
  (S.stmt $ IC.varDecDef vr IC.local vl)

constVar :: (CommonRenderSym r, Monad r) => Doc -> r (Visibility  r) -> SVariable r ->
  SValue r -> CS (r Doc)
constVar p s vr vl = zoom lensCStoMS $ onStateValue (toCode . R.stateVar 
  (RC.visibility s) p . RC.statement) (S.stmt $ IC.constDecDef vr IC.local vl)

-- Python, Java, C++, and Swift --

litArray :: (CommonRenderSym r) => (Doc -> Doc) -> VSType r -> [SValue r] -> SValue r
litArray f t es = sequence es >>= (\elems -> mkStateVal (IC.arrayType t) 
  (f $ valueList elems))

litSet :: (CommonRenderSym r) => (Doc -> Doc) -> (Doc -> Doc) -> VSType r -> [SValue r] -> SValue r
litSet f1 f2 t es = sequence es >>= (\elems -> mkStateVal (IC.arrayType t) 
  (f1 $ f2 $ valueList elems))

litSetFunc :: (CommonRenderSym r) => String -> VSType r -> [SValue r] -> SValue r
litSetFunc s t es = sequence es >>= (\elems -> mkStateVal (IC.arrayType t) 
  (text s <> parens (valueList elems)))

-- Python, C#, C++, and Swift--

extraClass :: (OORenderSym r) =>  Label -> Maybe Label -> [CSStateVar r] ->
  [SMethod r] -> [SMethod r] -> SClass r
extraClass n = S.intClass n public . S.inherit


-- Java, C#, and Swift --

doubleRender :: String
doubleRender = "Double"

double :: (CommonRenderSym r) => VSType r
double = typeFromData Double doubleRender (text doubleRender)

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

-- Python and Swift --

self :: (OORenderSym r) => SVariable r
self = zoom lensVStoMS getClassName >>= (\l -> mkStateVar R.self (obj l)
  R.self')

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

-- Java and Swift --

floatRender :: String
floatRender = "Float"

float :: (CommonRenderSym r) => VSType r
float = typeFromData Float floatRender (text floatRender)

stringRender' :: String
stringRender' = "String"

string' :: (CommonRenderSym r) => VSType r
string' = typeFromData String stringRender' (text stringRender')

-- C# and Swift --

inherit :: (Monad r) => Maybe Label -> r ParentSpec
inherit n = toCode $ maybe empty ((colon <+>) . text) n

implements :: (Monad r) => [Label] -> r ParentSpec
implements is = toCode $ colon <+> text (intercalate listSep is)


-- TODO: put docMod' back in Swift renderer, as it is no longer common.
docMod' :: (OORenderSym r) => String -> String -> [String] -> String -> SFile r -> SFile r
docMod' = docMod modDoc'

-- | Generates Markdown/DocC style module doc comment.  Useful for Swift, which follows
-- DocC, Julia, which uses Markdown, and any other language that doesn't have
-- Support for a document generator.
modDoc' :: ModuleDocRenderer
modDoc' desc as date m = m : [desc | not (null desc)] ++
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

-- Python and Julia --

-- | For declaring and optionally defining a variable in a language where
--   declaring a variable before defining it is not required.
--   v is the variable to declare, and e is Nothing if we are not defining it,
--   and (Just d) if d is the value we are defining it as.


fileOpen, fileR, fileW, fileA :: Label
fileOpen = "open"
fileR = "r"
fileW = "w"
fileA = "a"

openFileR', openFileW', openFileA' :: (CommonRenderSym r) => SValue r -> SValue r
openFileR' n = funcApp fileOpen infile [n, IC.litString fileR]
openFileW' n = funcApp fileOpen infile [n, IC.litString fileW]
openFileA' n = funcApp fileOpen infile [n, IC.litString fileA]

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