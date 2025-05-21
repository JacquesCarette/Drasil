-- | Implementations defined here are valid in some, but not all, language renderers
module Drasil.Shared.LanguageRenderer.CommonPseudoOO (
  int, constructor, doxFunc, doxClass, doxMod, docMod', modDoc', classVar, objVarSelf,
  indexOf, contains, containsInt, listAddFunc, discardFileLine, intClass, buildModule,
  arrayType, pi, printSt, arrayDec, arrayDecDef, openFileA, forEach,
  docMain, mainFunction, buildModule', call', listSizeFunc, listAccessFunc',
  string, constDecDef, bindingError, destructorError, stateVarDef, constVar, litSetFunc,
  extraClass, doubleRender, double, self, funcDecDef,
  floatRender, float, stringRender', string', inherit, implements, setMethodCall
) where


import Prelude hiding (print, pi, (<>))
import Data.List (sort, intercalate)
import Control.Monad (join)
import Control.Monad.State (get, modify)
import Control.Lens ((^.))
import qualified Control.Lens as L (set)
import Control.Lens.Zoom (zoom)
import Text.PrettyPrint.HughesPJ (Doc, text, empty, render, (<>), (<+>), parens, brackets, braces, colon, vcat, equals)

import Utils.Drasil (indent, stringList)

import Drasil.Shared.CodeType (CodeType(..))

import Drasil.Shared.InterfaceCommon
  ( private, public, Label, Library, MSBody, VSType, VSFunction, SVariable, SValue, Visibility, MSStatement, MSParameter, SMethod,
    MixedCall, TypeSym(infile, outfile, listInnerType), TypeElim(getType, getTypeString),
    VariableElim(variableName, variableType), ValueSym(valueType), ControlStatement(returnStmt),
    MethodSym(function), funcApp, ScopeSym(Scope), bodyStatements, oneLiner, (&=), Comparison(..) )

import qualified Drasil.Shared.InterfaceCommon as IC
  ( argsList, TypeSym(int, bool, double, string, listType, arrayType, void),
    VariableSym(var), Literal(litTrue, litFalse, litList, litSet, litInt, litString),
    VariableValue(valueOf), StatementSym(valStmt, emptyStmt),
    DeclStatement(varDec, varDecDef, constDecDef), List(intToIndex, indexToInt),
    ParameterSym(param, pointerParam), MethodSym(mainFunction), AssignStatement(assign),
    ScopeSym(..) )

import Drasil.GOOL.InterfaceGOOL
  ( SFile, FSModule, SClass, CSStateVar, OOTypeSym(obj), PermanenceSym(..),
    Initializers, objMethodCallNoParams, objMethodCall )

import qualified Drasil.GOOL.InterfaceGOOL as IG
  ( ClassSym(buildClass), OOVariableSym(self, objVar), OOFunctionSym(..) )

import Drasil.Shared.RendererClassesCommon
  ( CommonRenderSym, ImportSym(..), RenderBody(..), RenderType(..),
    RenderVariable(varFromData), InternalVarElim(variableBind), RenderFunction(funcFromData),
    MethodTypeSym(mType), RenderMethod(commentedFunc, mthdFromData),
    BlockCommentSym(..), ScopeElim(scopeData) )

import qualified Drasil.Shared.RendererClassesCommon as S
  ( RenderBody(multiBody), RenderValue(call), RenderStatement(stmt),
    InternalAssignStmt(multiAssign), InternalControlStmt(multiReturn),
    InternalListFunc(listSizeFunc, listAddFunc, listAppendFunc) )

import qualified Drasil.Shared.RendererClassesCommon as RC
  ( ImportElim(..), BodyElim(..), InternalTypeElim(..), InternalVarElim(variable),
    ValueElim(..), StatementElim(statement), VisibilityElim(..),
    MethodElim(..), FunctionElim(..) )

import Drasil.Shared.Helpers
  ( vibcat, toCode, toState, onCodeValue, onStateValue, on2StateValues, onStateList )

import Drasil.GOOL.RendererClassesOO
  ( OORenderSym, OORenderMethod(intMethod), ParentSpec )

import qualified Drasil.GOOL.RendererClassesOO as S
  ( OOMethodTypeSym(construct), OORenderMethod(intFunc), RenderClass(intClass, inherit),
    RenderMod(modFromData) )

import qualified Drasil.GOOL.RendererClassesOO as RC
  ( PermElim(..), StateVarElim(..), ClassElim(..) )

import Drasil.Shared.LanguageRenderer
  ( array', new', args, array, listSep, access, mathFunc, ModuleDocRenderer,
    FuncDocRenderer, functionDox, classDox, moduleDox, variableList, valueList, intValue )

import qualified Drasil.Shared.LanguageRenderer as R
  ( self, self', module', print, stateVar, stateVarList, constDecDef, extVar, listAccessFunc )

import Drasil.Shared.LanguageRenderer.Constructors
  ( mkStmt, mkStmtNoEnd, mkStateVal, mkStateVar, mkVal )

import Drasil.Shared.LanguageRenderer.LanguagePolymorphic
  ( classVarCheckStatic, call, initStmts, docFunc, docFuncRepr, docClass, docMod,
    smartAdd, smartSub )

import Drasil.Shared.AST (VisibilityTag(..), ScopeTag(Global), ScopeData, sd)

import Drasil.Shared.State
  ( FS, CS, lensFStoCS, lensFStoMS, lensCStoMS, lensMStoVS, lensVStoMS,
    currParameters, getClassName, getLangImports, getLibImports,
    getModuleImports, setClassName, setCurrMain, setMainDoc, useVarName, setVarScope )

import Metadata.Drasil.DrasilMetaCall (watermark)


-- Python, Java, C#, C++, and Swift --
-- | Convert an integer to an index in a 0-indexed language
--   Since GOOL is 0-indexed, no adjustments need be made
intToIndex :: SValue r -> SValue r
intToIndex = id

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

-- Python, C#, C++, and Swift--

extraClass :: (OORenderSym r) =>  Label -> Maybe Label -> [CSStateVar r] ->
  [SMethod r] -> [SMethod r] -> SClass r
extraClass n = S.intClass n public . S.inherit

listAccessFunc' :: (OORenderSym r) => Label -> VSType r -> SValue r ->
  VSFunction r
listAccessFunc' f t i = IG.func f t [intValue i]

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


-- Python, Java, C#, and Swift --

bindingError :: String -> String
bindingError l = "Binding unimplemented in " ++ l

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


litSetFunc :: (CommonRenderSym r) => String -> VSType r -> [SValue r] -> SValue r
litSetFunc s t es = sequence es >>= (\elems -> mkStateVal (IC.arrayType t) 
  (text s <> parens (valueList elems)))


-- Java, C#, and Swift --

doubleRender :: String
doubleRender = "Double"

double :: (CommonRenderSym r) => VSType r
double = typeFromData Double doubleRender (text doubleRender)

-- Python and Swift --

self :: (OORenderSym r) => SVariable r
self = zoom lensVStoMS getClassName >>= (\l -> mkStateVar R.self (obj l)
  R.self')

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

fileOpen, fileR, fileW, fileA :: Label
fileOpen = "open"
fileR = "r"
fileW = "w"
fileA = "a"

openFileR', openFileW', openFileA' :: (CommonRenderSym r) => SValue r -> SValue r
openFileR' n = funcApp fileOpen infile [n, IC.litString fileR]
openFileW' n = funcApp fileOpen infile [n, IC.litString fileW]
openFileA' n = funcApp fileOpen infile [n, IC.litString fileA]