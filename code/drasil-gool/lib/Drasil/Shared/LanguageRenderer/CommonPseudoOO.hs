-- | Implementations defined here are valid in some, but not all, language renderers
module Drasil.Shared.LanguageRenderer.CommonPseudoOO (int, constructor, doxFunc,
  doxClass, doxMod, docMod', modDoc', functionDoc, extVar, classVar, objVarSelf,
  indexOf, contains, containsInt, listAddFunc, discardFileLine, intClass, funcType, buildModule,
  arrayType, pi, printSt, arrayDec, arrayDecDef, openFileA, forEach,
  docMain, mainFunction, buildModule', call', listSizeFunc, listAccessFunc',
  string, constDecDef, docInOutFunc, bindingError, extFuncAppMixedArgs, notNull,
  listDecDef, destructorError, stateVarDef, constVar, litArray, litSet, listSetFunc, litSetFunc,
  extraClass, listAccessFunc, doubleRender, double, openFileR, openFileW,
  stateVar, self, multiAssign, multiReturn, listDec, funcDecDef, inOutCall,
  forLoopError, mainBody, inOutFunc, docInOutFunc', bool,
  floatRender, float, stringRender', string', inherit, implements, listSize, setDecDef, setDec,
  listAdd, listAppend, intToIndex, indexToInt, intToIndex', indexToInt',
  varDecDef, openFileR', openFileW', openFileA', argExists, global, setMethodCall) where

import Utils.Drasil (indent, stringList)

import Drasil.Shared.CodeType (CodeType(..))

import Drasil.Shared.InterfaceCommon (varDecDef, bool, extFuncAppMixedArgs,
  funcType, extVar, Label, Library, Value, MixedCall, bodyStatements, oneLiner,
  TypeSym(Type, infile, outfile, listInnerType), TypeElim(getType, getTypeString),
  VariableElim(variableName, variableType), ValueSym(valueType),
  Comparison(..), (&=), ControlStatement(returnStmt), ParameterSym(Parameter),
  VisibilitySym(..), MethodSym(Method, function), funcApp, ScopeSym(Scope), listSize,
  BodySym(Body), FunctionSym(Function), StatementSym(Statement),
  VariableSym(Variable))
import qualified Drasil.Shared.InterfaceCommon as IC (argsList,
  TypeSym(int, bool, double, string, listType, arrayType, void), VariableSym(var),
  Literal(litTrue, litFalse, litList, litSet, litInt, litString),
  VariableValue(valueOf), StatementSym(valStmt), DeclStatement(varDec,
  varDecDef, constDecDef), List(intToIndex, indexToInt), ParameterSym(param,
  pointerParam), MethodSym(mainFunction), ScopeSym(..))


import Drasil.GOOL.InterfaceGOOL (OOTypeSym(obj), PermanenceSym(..),
  StateVarSym(StateVar), Initializers, objMethodCallNoParams, objMethodCall,
  ClassSym(Class), FileSym(File), ModuleSym(Module))
import qualified Drasil.GOOL.InterfaceGOOL as IG (ClassSym(buildClass),
  OOVariableSym(self, objVar), OOFunctionSym(..))

import Drasil.Shared.RendererClassesCommon (CommonRenderSym, ImportSym(..),
  RenderBody(..), RenderType(..), RenderVariable(varFromData),
  InternalVarElim(variableBind),
  MethodTypeSym(mType), RenderMethod(commentedFunc, mthdFromData),
  BlockCommentSym(..), ScopeElim(scopeData))

import qualified Drasil.Shared.RendererClassesCommon as S 

import qualified Drasil.Shared.RendererClassesCommon as RC (ImportElim(..),
  BodyElim(..), InternalTypeElim(..), InternalVarElim(variable), ValueElim(..),
  StatementElim(statement), VisibilityElim(..), MethodElim(..), FunctionElim(..))

import Drasil.Shared.Helpers (vibcat, toCode, toState, onCodeValue, onStateValue, onStateList)

import Drasil.GOOL.RendererClassesOO (OORenderSym, OORenderMethod(intMethod),
  ParentSpec)

import qualified Drasil.GOOL.RendererClassesOO as S (OOMethodTypeSym(construct),
  OORenderMethod(intFunc), RenderClass(intClass, inherit),
  RenderMod(modFromData))

import qualified Drasil.GOOL.RendererClassesOO as RC (PermElim(..),
  StateVarElim(..), ClassElim(..))

import Drasil.Shared.LanguageRenderer (listAccessFunc, listSetFunc, array', new', args, array, listSep, access,
  mathFunc, ModuleDocRenderer, FuncDocRenderer, functionDox, classDox,
  moduleDox, variableList, valueList, intValue)

import qualified Drasil.Shared.LanguageRenderer as R (self, self', module',
  print, stateVar, stateVarList, constDecDef)
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
import Control.Monad.State (get, modify)
import Control.Lens ((^.), un)
import qualified Control.Lens as L (set)
import Control.Lens.Zoom (zoom)
import Text.PrettyPrint.HughesPJ (Doc, text, empty, render, (<>), (<+>), parens,
  brackets, braces, colon, vcat, equals)
import Drasil.Metadata (watermark)

-- Python, Java, C#, C++, and Swift --
-- | Convert an integer to an index in a 0-indexed language
--   Since GOOL is 0-indexed, no adjustments need be made
intToIndex :: Value r -> Value r
intToIndex = id

-- | Convert an index to an integer in a 0-indexed language
--   Since GOOL is 0-indexed, no adjustments need be made
indexToInt :: Value r -> Value r
indexToInt = id

-- Global for langauges that don't use declarations for them
global :: (Monad r) => r ScopeData
global = toCode $ sd Global

-- Python, Java, C#, and C++ --

intRender :: String
intRender = "int"

int :: (CommonRenderSym r) => Type r
int = typeFromData Integer intRender (text intRender)

constructor :: (OORenderSym r) => Label -> [Parameter r] -> Initializers r ->
  Body r -> Method r
constructor fName ps is b = undefined{-getClassName >>= (\c -> intMethod False fName
  public dynamic (S.construct c) ps (S.multiBody [initStmts is, b]))-}

doxFunc :: (CommonRenderSym r) => String -> [String] -> Maybe String -> Method r ->
  Method r
doxFunc = docFunc functionDox

doxClass :: (OORenderSym r) => String -> Class r -> Class r
doxClass = docClass classDox

doxMod :: (OORenderSym r) => String -> String -> [String] -> String -> File r ->
  File r
doxMod = docMod moduleDox

-- Python, Java, and C# --

classVar :: (CommonRenderSym r) => (Doc -> Doc -> Doc) -> Type r -> Variable r ->
  Variable r
classVar f c' v'= undefined{-do
  c <- c'
  v <- v'
  vr <- varFromData
    (variableBind v) (getTypeString c `access` variableName v)
    (toState $ variableType v) (f (RC.type' c) (RC.variable v))
  toState $ classVarCheckStatic vr-}

objVarSelf :: (OORenderSym r) => Variable r -> Variable r
objVarSelf = IG.objVar IG.self

indexOf :: (OORenderSym r) => Label -> Value r -> Value r -> Value r
indexOf f l v = IC.indexToInt $ IG.objAccess l (IG.func f IC.int [v])

contains :: (OORenderSym r) => Label -> Value r -> Value r -> Value r
contains f s v = IG.objAccess s (IG.func f IC.bool [v]) 

containsInt :: (OORenderSym r) => Label -> Label -> Value r -> Value r -> Value r
containsInt f fn s v = contains f s v ?!= IG.objAccess s (IG.func fn IC.bool [])

listAddFunc :: (OORenderSym r) => Label -> Value r -> Value r -> Function r
listAddFunc f i v = undefined{-} IG.func f (IC.listType $ onStateValue valueType v) 
  [i, v]-}

discardFileLine :: (OORenderSym r) => Label -> Value r -> Statement r
discardFileLine n f = IC.valStmt $ objMethodCallNoParams IC.string f n 

-- | An internal function for creating a class.
--   Parameters: render function, class name, scope, parent, class variables,
--               constructor(s), methods
intClass :: (OORenderSym r) => (Label -> Doc -> Doc -> Doc -> Doc ->
  Doc) -> Label -> Visibility r -> ParentSpec r -> [StateVar r] ->
  [Method r]-> [Method r] -> Doc
intClass f n s i svrs cstrs mths = undefined{-} do
  modify (setClassName n)
  svs <- onStateList (R.stateVarList . map RC.stateVar) svrs
  ms <- onStateList (vibcat . map RC.method) (map (zoom lensCStoMS) (cstrs ++ mths))
  return $ onCodeValue (\p -> f n p (RC.visibility s) svs ms) i-}


-- Python and C++ --

-- Parameters: Module name, Doc for imports, Doc to put at top of module (but 
-- after imports), Doc to put at bottom of module, methods, classes
-- Renamed top to topDoc to fix shadowing error with RendererClassesOO top
buildModule :: (OORenderSym r) => Label -> FS Doc -> FS Doc -> FS Doc ->
  [Method r] -> [Class r] -> Module r
buildModule n imps topDoc bot fs cs = undefined{-} S.modFromData n (do
  cls <- mapM (zoom lensFStoCS) cs
  fns <- mapM (zoom lensFStoMS) fs
  is <- imps
  tp <- topDoc
  bt <- bot
  return $ R.module' is (vibcat (tp : map RC.class' cls))
    (vibcat (map RC.method fns ++ [bt])))-}

-- Java and C# -- 

arrayType :: (CommonRenderSym r) => Type r -> Type r
arrayType t' = undefined{- do
  t <- t'
  typeFromData (Array (getType t))
    (getTypeString t ++ array) (RC.type' t <> brackets empty)-}

pi :: (CommonRenderSym r) => Value r
pi = mkStateVal IC.double (text $ mathFunc "PI")

printSt :: (CommonRenderSym r) => Value r -> Value r -> Statement r
printSt va' vb' = undefined {-do
  va <- zoom lensMStoVS va'
  vb <- zoom lensMStoVS vb'
  mkStmt (R.print va vb)-}

arrayDec :: (CommonRenderSym r) => Value r -> Variable r -> Scope r
  -> Statement r
arrayDec n vr scp = undefined{-do
  sz <- zoom lensMStoVS n
  v <- zoom lensMStoVS vr
  modify $ useVarName $ variableName v
  modify $ setVarScope (variableName v) (scopeData scp)
  let tp = variableType v
  innerTp <- zoom lensMStoVS $ listInnerType $ return tp
  mkStmt $ RC.type' tp <+> RC.variable v <+> equals <+> new' <+>
    RC.type' innerTp <> brackets (RC.value sz)-}

arrayDecDef :: (CommonRenderSym r) => Variable r -> Scope r ->
  [Value r] -> Statement r
arrayDecDef v' scp vals' = undefined{-do
  vs <- mapM (zoom lensMStoVS) vals'
  vd <- IC.varDec v' scp
  mkStmt (RC.statement vd <+> equals <+> braces (valueList vs))-}

openFileA :: (CommonRenderSym r) => (Value r -> Type r -> Value r -> Value r) ->
  Variable r -> Value r -> Statement r
openFileA f vr vl = vr &= f vl outfile IC.litTrue

forEach :: (CommonRenderSym r) => Doc -> Doc -> Doc -> Doc -> Variable r -> Value r
  -> Body r -> Statement r
forEach bStart bEnd forEachLabel inLbl e' v' b' = undefined{-do
  e <- zoom lensMStoVS e'
  v <- zoom lensMStoVS v'
  b <- b'
  mkStmtNoEnd $ vcat [
    forEachLabel <+> parens (RC.type' (variableType e) <+> RC.variable e <+>
      inLbl <+> RC.value v) <+> bStart,
    indent $ RC.body b,
    bEnd]-}

mainDesc, argsDesc :: String
mainDesc = "Controls the flow of the program"
argsDesc = "List of command-line arguments"

docMain :: (OORenderSym r) => Body r -> Method r
docMain b = undefined{- commentedFunc (docComment $ toState $ functionDox 
  mainDesc [(args, argsDesc)] []) (IC.mainFunction b)-}

mainFunction :: (OORenderSym r) => Type r -> Label -> Body r -> Method r
mainFunction s n = undefined {- S.intFunc True n public static (mType IC.void)
  [IC.param (IC.var args (s >>= (\argT -> typeFromData (List String) 
  (render (RC.type' argT) ++ array) (RC.type' argT <> array'))))]-}

-- | Used by the language renderers to build the module.
--   n is the module name
--   inc is the include
--   is is the import statements
--   ms is the class methods
--   cs is the classes
buildModule' :: (OORenderSym r) => Label -> (String -> Import r) -> [Label] 
  -> [Method r] -> [Class r] -> Module r
buildModule' n inc is ms cs = undefined{-S.modFromData n (do
  cls <- mapM (zoom lensFStoCS) 
          (if null ms then cs else IG.buildClass Nothing [] [] ms : cs)
  lis <- getLangImports
  libis <- getLibImports
  mis <- getModuleImports
  return $ vibcat [
    vcat (map (RC.import' . inc) (lis ++ sort (is ++ libis) ++ mis)),
    vibcat (map RC.class' cls)])-}

-- Java and C++ --

-- | First parameter is language name, rest similar to call from RendererClassesCommon
call' :: (CommonRenderSym r) => String -> Maybe Library -> Maybe Doc -> MixedCall r
call' l _ _ _ _ _ (_:_) = error $ namedArgError l
call' _ l o n t ps ns = call empty l o n t ps ns

namedArgError :: String -> String
namedArgError l = "Named arguments not supported in " ++ l

listSizeFunc :: (OORenderSym r) => Function r
listSizeFunc = IG.func "size" IC.int []

listAccessFunc' :: (OORenderSym r) => Label -> Type r -> Value r ->
  Function r
listAccessFunc' f t i = IG.func f t [intValue i]

-- C# and C++ --

stringRender :: String
stringRender = "string"

string :: (CommonRenderSym r) => Type r
string = typeFromData String stringRender (text stringRender)

constDecDef :: (CommonRenderSym r) => Variable r -> Scope r -> Value r
  -> Statement r
constDecDef vr' scp v'= undefined{-} do
  vr <- zoom lensMStoVS vr'
  v <- zoom lensMStoVS v'
  modify $ useVarName $ variableName vr
  modify $ setVarScope (variableName vr) (scopeData scp)
  mkStmt (R.constDecDef vr v)-}

docInOutFunc :: (CommonRenderSym r) => ([Variable r] -> [Variable r] ->
    [Variable r] -> Body r -> Method r) ->
  String -> [(String, Variable r)] -> [(String, Variable r)] ->
  [(String, Variable r)] -> Body r -> Method r
docInOutFunc f desc is [o] [] b = docFuncRepr functionDox desc (map fst is)
  [fst o] (f (map snd is) [snd o] [] b)
docInOutFunc f desc is [] [both] b = docFuncRepr functionDox desc (map fst $
  both : is) [fst both] (f (map snd is) [] [snd both] b)
docInOutFunc f desc is os bs b = docFuncRepr functionDox desc (map fst $ bs ++
  is ++ os) [] (f (map snd is) (map snd os) (map snd bs) b)

-- Python, Java, C#, and Swift --

bindingError :: String -> String
bindingError l = "Binding unimplemented in " ++ l

notNull :: (CommonRenderSym r) => String -> Value r -> Value r
notNull nil v = undefined{-v ?!= IC.valueOf (IC.var nil $ onStateValue valueType v)-}

listDecDef :: (CommonRenderSym r) => Variable r -> Scope r ->
  [Value r] -> Statement r
listDecDef v scp vals = undefined{-do
  vr <- zoom lensMStoVS v 
  let lst = IC.litList (listInnerType $ return $ variableType vr) vals
  IC.varDecDef (return vr) scp lst-}

setDecDef :: (CommonRenderSym r) => Variable r -> Scope r -> [Value r] -> Statement r
setDecDef v scp vals = undefined{-do
  vr <- zoom lensMStoVS v 
  let st = IC.litSet (listInnerType $ return $ variableType vr) vals
  IC.varDecDef (return vr) scp st-}

setDec :: (OORenderSym r) => (Value r -> Doc) -> Value r -> Variable r -> Scope r -> Statement r
setDec f vl v scp = undefined{-do 
  sz <- zoom lensMStoVS vl
  vd <- IC.varDec v scp
  mkStmt (RC.statement vd <> f sz)-}

setMethodCall :: (OORenderSym r) => Label -> Value r ->  Value r -> Value r
setMethodCall n a b = undefined{-objMethodCall (listInnerType $ onStateValue valueType a) a n [b] A-}

destructorError :: String -> String
destructorError l = "Destructors not allowed in " ++ l

stateVarDef :: (OORenderSym r) => Visibility r -> Permanence r ->
  Variable r -> Value r -> Doc
stateVarDef s p vr vl = undefined{-zoom lensCStoMS $ onStateValue (toCode . R.stateVar
  (RC.visibility  s) (RC.perm p) . RC.statement)
  (S.stmt $ IC.varDecDef vr IC.local vl)-}

constVar :: (CommonRenderSym r) => Doc -> Visibility r -> Variable r ->
  Value r -> Doc
constVar p s vr vl = undefined{-zoom lensCStoMS $ onStateValue (toCode . R.stateVar 
  (RC.visibility s) p . RC.statement) (S.stmt $ IC.constDecDef vr IC.local vl)-}

-- Python, Java, C++, and Swift --

litArray :: (CommonRenderSym r) => (Doc -> Doc) -> Type r -> [Value r] -> Value r
litArray f t es = undefined{-} sequence es >>= (\elems -> mkStateVal (IC.arrayType t) 
  (f $ valueList elems))-}

litSet :: (CommonRenderSym r) => (Doc -> Doc) -> (Doc -> Doc) -> Type r -> [Value r] -> Value r
litSet f1 f2 t es = undefined{-sequence es >>= (\elems -> mkStateVal (IC.arrayType t) 
  (f1 $ f2 $ valueList elems))-}

litSetFunc :: (CommonRenderSym r) => String -> Type r -> [Value r] -> Value r
litSetFunc s t es = undefined{-sequence es >>= (\elems -> mkStateVal (IC.arrayType t) 
  (text s <> parens (valueList elems)))-}

-- Python, C#, C++, and Swift--

extraClass :: (OORenderSym r) =>  Label -> Maybe Label -> [StateVar r] ->
  [Method r] -> [Method r] -> Class r
extraClass n = S.intClass n public . S.inherit


-- Java, C#, and Swift --

doubleRender :: String
doubleRender = "Double"

double :: (CommonRenderSym r) => Type r
double = typeFromData Double doubleRender (text doubleRender)

openFileR :: (CommonRenderSym r) => (Value r -> Type r -> Value r) -> Variable r
  -> Value r -> Statement r
openFileR f vr vl = vr &= f vl infile

openFileW :: (CommonRenderSym r) => (Value r -> Type r -> Value r -> Value r) ->
  Variable r -> Value r -> Statement r
openFileW f vr vl = vr &= f vl outfile IC.litFalse

stateVar :: (OORenderSym r) => Visibility r -> Permanence r ->
  Variable r -> Doc
stateVar s p v = undefined{-zoom lensCStoMS $ onStateValue (toCode . R.stateVar
  (RC.visibility s) (RC.perm p) . RC.statement) (S.stmt $ IC.varDec v IC.local)-}

-- Python and Swift --

self :: (OORenderSym r) => Variable r
self = undefined{-zoom lensVStoMS getClassName >>= (\l -> mkStateVar R.self (obj l)
  R.self')-}

multiAssign :: (CommonRenderSym r) => (Doc -> Doc) -> [Variable r] -> [Value r] ->
  Statement r
multiAssign _ [] _ = error "Attempt to write assign statement for no variables."
multiAssign _ _ [] = error "Attempt to write assign statement with no values."
multiAssign f vars vals = undefined{-} if length vals /= 1 && length vars /= length vals
  then error $ "Attempted multiple assign statement with different number " ++
    "of variables than values"
  else do
  vrs <- mapM (zoom lensMStoVS) vars
  vls <- mapM (zoom lensMStoVS) vals
  let wrapIfMult :: [a] -> Doc -> Doc
      wrapIfMult l = if length l > 1 then f else id
  mkStateVar "" IC.void (wrapIfMult vrs (variableList vrs)) &= 
    mkStateVal IC.void (wrapIfMult vls (valueList vls))-}

multiReturn :: (CommonRenderSym r) => (Doc -> Doc) -> [Value r] -> Statement r
multiReturn _ [] = error "Attempt to write return statement with no values."
multiReturn _ [v] = returnStmt v
multiReturn f vs = undefined{-do
  vs' <- mapM (zoom lensMStoVS) vs
  returnStmt $ mkStateVal IC.void $ f $ valueList vs'-}

listDec :: (CommonRenderSym r) => Variable r -> Scope r -> Statement r
listDec v scp = listDecDef v scp []

funcDecDef :: (OORenderSym r) => Variable r -> Scope r -> [Variable r] ->
  Body r -> Statement r
funcDecDef v scp ps b = undefined{-do
  vr <- zoom lensMStoVS v
  modify $ useVarName $ variableName vr
  modify $ setVarScope (variableName vr) (scopeData scp)
  s <- get
  f <- function (variableName vr) private (return $ variableType vr) 
    (map IC.param ps) b
  modify (L.set currParameters (s ^. currParameters))
  mkStmtNoEnd $ RC.method f-}

inOutCall :: (CommonRenderSym r) => (Label -> Type r -> [Value r] -> Value r) ->
  Label -> [Value r] -> [Variable r] -> [Variable r] -> Statement r
inOutCall f n ins [] [] = IC.valStmt $ f n IC.void ins
inOutCall f n ins outs both = S.multiAssign rets [f n IC.void (map IC.valueOf 
  both ++ ins)]
  where rets = both ++ outs

forLoopError :: String -> String
forLoopError l = "Classic for loops not available in " ++ l ++ ", use " ++
  "forRange, forEach, or while instead"

mainBody :: (CommonRenderSym r) => Body r -> Method r
mainBody b = undefined{-} do
  modify setCurrMain
  bod <- b
  modify (setMainDoc $ RC.body bod)
  mthdFromData Pub empty-}

inOutFunc :: (CommonRenderSym r) => (Type r -> [Parameter r] -> Body r ->
  Method r) -> [Variable r] -> [Variable r] -> [Variable r] -> Body r ->
  Method r
inOutFunc f ins [] [] b = f IC.void (map IC.param ins) b
inOutFunc f ins outs both b = undefined {-} f 
  (multiType $ map (onStateValue variableType) rets)  
  (map IC.pointerParam both ++ map IC.param ins) 
  (multiBody [bodyStatements $ map (`IC.varDec` IC.local) outs, b,
    oneLiner $ S.multiReturn $ map IC.valueOf rets])
  where rets = both ++ outs-}

docInOutFunc' :: (CommonRenderSym r) => FuncDocRenderer -> ([Variable r] ->
    [Variable r] -> [Variable r] -> Body r -> Method r) ->
  String -> [(String, Variable r)] -> [(String, Variable r)] ->
  [(String, Variable r)] -> Body r -> Method r
docInOutFunc' dfr f desc is os bs b = docFuncRepr dfr desc (map fst $ bs ++ is)
  (map fst $ bs ++ os) (f (map snd is) (map snd os) (map snd bs) b)

-- Java and Swift --

floatRender :: String
floatRender = "Float"

float :: (CommonRenderSym r) => Type r
float = typeFromData Float floatRender (text floatRender)

stringRender' :: String
stringRender' = "String"

string' :: (CommonRenderSym r) => Type r
string' = typeFromData String stringRender' (text stringRender')

-- C# and Swift --

inherit :: Monad r => Maybe String -> r Doc
inherit n = toCode $ maybe empty ((colon <+>) . text) n

implements :: Monad r => [[Char]] -> r Doc
implements is = toCode $ colon <+> text (intercalate listSep is)


-- TODO: put docMod' back in Swift renderer, as it is no longer common.
docMod' :: (OORenderSym r) => String -> String -> [String] -> String -> File r -> File r
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


-- | For declaring and optionally defining a variable in a language where
--   declaring a variable before defining it is not required.
--   v is the variable to declare, and e is Nothing if we are not defining it,
--   and (Just d) if d is the value we are defining it as.


fileOpen, fileR, fileW, fileA :: Label
fileOpen = "open"
fileR = "r"
fileW = "w"
fileA = "a"

openFileR', openFileW', openFileA' :: (CommonRenderSym r) => Value r -> Value r
openFileR' n = funcApp fileOpen infile [n, IC.litString fileR]
openFileW' n = funcApp fileOpen infile [n, IC.litString fileW]
openFileA' n = funcApp fileOpen infile [n, IC.litString fileA]

argExists :: (CommonRenderSym r) => Integer -> Value r
argExists i = listSize IC.argsList ?> IC.litInt (fromIntegral $ i+1)


-- Julia and MATLAB --

-- | Call to insert a value into a list in a language where this is not a method.
listAdd :: (CommonRenderSym r) => Value r -> Value r -> Value r -> Value r
listAdd l i v = undefined{-} do
  f <- S.listAddFunc l (IC.intToIndex i) v
  mkVal (RC.functionType f) (RC.function f)-}


-- | Call to append a value to a list in a language where this is not a method.
listAppend :: (CommonRenderSym r) => Value r -> Value r -> Value r
listAppend l v = undefined{-do
  f <- S.listAppendFunc l v
  mkVal (RC.functionType f) (RC.function f)-}

-- | Convert an integer to an index in a 1-indexed language
--   Since GOOL is 0-indexed, we need to add 1
intToIndex' :: (CommonRenderSym r) => Value r -> Value r
intToIndex' v = v `smartAdd` IC.litInt 1

-- | Convert an index to an integer in a 1-indexed language
--   Since GOOL is 0-indexed, we need to subtract 1
indexToInt' :: (CommonRenderSym r) => Value r -> Value r
indexToInt' v = v `smartSub` IC.litInt 1