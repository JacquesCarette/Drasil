-- | Implementations defined here are valid in some, but not all, language renderers
module GOOL.Drasil.LanguageRenderer.CommonPseudoOO (int, constructor, doxFunc,
  doxClass, doxMod, docMod', extVar, classVar, objVarSelf, indexOf, listAddFunc,
  discardFileLine, intClass, funcType, buildModule, arrayType, pi, printSt,
  arrayDec, arrayDecDef, openFileA, forEach, docMain, mainFunction,
  buildModule', call', listSizeFunc, listAccessFunc', string, constDecDef,
  docInOutFunc, bindingError, extFuncAppMixedArgs, notNull, listDecDef,
  destructorError, stateVarDef, constVar, litArray, listSetFunc, extraClass,
  listAccessFunc, doubleRender, double, openFileR, openFileW, stateVar, self,
  multiAssign, multiReturn, listDec, funcDecDef, inOutCall, forLoopError,
  mainBody, inOutFunc, docInOutFunc', boolRender, bool, floatRender, float, stringRender',
  string', inherit, implements, listSize, listAdd, listAppend, intToIndex,
  indexToInt, intToIndex', indexToInt'
) where

import Utils.Drasil (indent, stringList)

import GOOL.Drasil.CodeType (CodeType(..))
import GOOL.Drasil.ClassInterface (Label, Library, SFile, MSBody, VSType,
  SVariable, SValue, VSFunction, MSStatement, MSParameter, SMethod, CSStateVar,
  SClass, FSModule, Initializers, MixedCall, PermanenceSym(..), bodyStatements,
  oneLiner, TypeSym(infile, outfile, listInnerType, obj),
  TypeElim(getType, getTypeString), VariableElim(variableName, variableType),
  ValueSym(valueType), Comparison(..), objMethodCallNoParams, (&=),
  ControlStatement(returnStmt), ScopeSym(..), MethodSym(function),
  NumericExpression((#+), (#-)))
import qualified GOOL.Drasil.ClassInterface as S (
  TypeSym(int, double, string, listType, arrayType, void),
  VariableSym(var, self, objVar), Literal(litTrue, litFalse, litList, litInt),
  VariableValue(valueOf), FunctionSym(func, objAccess), StatementSym(valStmt),
  DeclStatement(varDec, varDecDef, constDecDef), List(intToIndex, indexToInt),
  ParameterSym(param, pointerParam), MethodSym(mainFunction),
  ClassSym(buildClass))
import GOOL.Drasil.RendererClasses (RenderSym, ImportSym(..), RenderBody(..), 
  RenderType(..), RenderVariable(varFromData), InternalVarElim(variableBind), 
  RenderFunction(funcFromData), MethodTypeSym(mType),
  RenderMethod(intMethod, commentedFunc, mthdFromData), ParentSpec, 
  BlockCommentSym(..))
import qualified GOOL.Drasil.RendererClasses as S (RenderBody(multiBody),
  RenderValue(call), RenderStatement(stmt), InternalAssignStmt(multiAssign),
  InternalControlStmt(multiReturn), MethodTypeSym(construct),
  RenderMethod(intFunc), RenderClass(intClass, inherit), RenderMod(modFromData),
  InternalListFunc(listSizeFunc, listAddFunc, listAppendFunc))
import qualified GOOL.Drasil.RendererClasses as RC (ImportElim(..),
  PermElim(..), BodyElim(..), InternalTypeElim(..), InternalVarElim(variable),
  ValueElim(..), StatementElim(statement), ScopeElim(..), MethodElim(..),
  StateVarElim(..), ClassElim(..), FunctionElim(..))
import GOOL.Drasil.Helpers (vibcat, toCode, toState, onCodeValue, onStateValue,
  on2StateValues, onStateList)
import GOOL.Drasil.LanguageRenderer (array', new', args, array, listSep, access,
  mathFunc, ModuleDocRenderer, FuncDocRenderer, functionDox, classDox,
  moduleDox, variableList, valueList, intValue)
import qualified GOOL.Drasil.LanguageRenderer as R (self, self', module',
  print, stateVar, stateVarList, constDecDef, extVar, listAccessFunc)
import GOOL.Drasil.LanguageRenderer.Constructors (mkStmt, mkStmtNoEnd, 
  mkStateVal, mkStateVar, mkVal)
import GOOL.Drasil.LanguageRenderer.LanguagePolymorphic (classVarCheckStatic,
  call, initStmts, docFunc, docFuncRepr, docClass, docMod)
import GOOL.Drasil.AST (ScopeTag(..))
import GOOL.Drasil.State (FS, CS, lensFStoCS, lensFStoMS, lensCStoMS,
  lensMStoVS, lensVStoMS, currParameters, getClassName, getLangImports,
  getLibImports, getModuleImports, setClassName, setCurrMain, setMainDoc,
  useVarName)

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

-- Python, Java, C#, and C++ --

intRender :: String
intRender = "int"

int :: (RenderSym r) => VSType r
int = typeFromData Integer intRender (text intRender)

constructor :: (RenderSym r) => Label -> [MSParameter r] -> Initializers r -> 
  MSBody r -> SMethod r
constructor fName ps is b = getClassName >>= (\c -> intMethod False fName 
  public dynamic (S.construct c) ps (S.multiBody [initStmts is, b]))

doxFunc :: (RenderSym r) => String -> [String] -> Maybe String -> SMethod r -> 
  SMethod r
doxFunc = docFunc functionDox

doxClass :: (RenderSym r) => String -> SClass r -> SClass r
doxClass = docClass classDox

doxMod :: (RenderSym r) => String -> String -> [String] -> String -> SFile r -> 
  SFile r
doxMod = docMod moduleDox

-- Python, Java, and C# --

extVar :: (RenderSym r) => Label -> Label -> VSType r -> SVariable r
extVar l n t = mkStateVar (l `access` n) t (R.extVar l n)

classVar :: (RenderSym r) => (Doc -> Doc -> Doc) -> VSType r -> SVariable r -> 
  SVariable r
classVar f c' v'= do 
  c <- c'
  v <- v'
  vr <- varFromData 
    (variableBind v) (getTypeString c `access` variableName v) 
    (toState $ variableType v) (f (RC.type' c) (RC.variable v))
  toState $ classVarCheckStatic vr
  
objVarSelf :: (RenderSym r) => SVariable r -> SVariable r
objVarSelf = S.objVar S.self

indexOf :: (RenderSym r) => Label -> SValue r -> SValue r -> SValue r
indexOf f l v = S.indexToInt $ S.objAccess l (S.func f S.int [v])

listAddFunc :: (RenderSym r) => Label -> SValue r -> SValue r -> VSFunction r
listAddFunc f i v = S.func f (S.listType $ onStateValue valueType v) 
  [i, v]
  
discardFileLine :: (RenderSym r) => Label -> SValue r -> MSStatement r
discardFileLine n f = S.valStmt $ objMethodCallNoParams S.string f n 

intClass :: (RenderSym r, Monad r) => (Label -> Doc -> Doc -> Doc -> Doc -> 
  Doc) -> Label -> r (Scope r) -> r ParentSpec -> [CSStateVar r] -> [SMethod r] 
  -> CS (r Doc)
intClass f n s i svrs mths = do
  modify (setClassName n) 
  svs <- onStateList (R.stateVarList . map RC.stateVar) svrs
  ms <- onStateList (vibcat . map RC.method) (map (zoom lensCStoMS) mths)
  return $ onCodeValue (\p -> f n p (RC.scope s) svs ms) i 

-- Python, Java, and C++ --

funcType :: (RenderSym r) => [VSType r] -> VSType r -> VSType r
funcType ps' r' =  do 
  ps <- sequence ps'
  r <- r'
  typeFromData (Func (map getType ps) (getType r)) "" empty

-- Python and C++ --

-- Parameters: Module name, Doc for imports, Doc to put at top of module (but 
-- after imports), Doc to put at bottom of module, methods, classes
buildModule :: (RenderSym r) => Label -> FS Doc -> FS Doc -> FS Doc -> 
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

arrayType :: (RenderSym r) => VSType r -> VSType r
arrayType t' = do 
  t <- t'
  typeFromData (Array (getType t)) 
    (getTypeString t ++ array) (RC.type' t <> brackets empty)
  
pi :: (RenderSym r) => SValue r
pi = mkStateVal S.double (text $ mathFunc "PI")

printSt :: (RenderSym r) => SValue r -> SValue r -> MSStatement r
printSt va' vb' = do
  va <- zoom lensMStoVS va'
  vb <- zoom lensMStoVS vb' 
  mkStmt (R.print va vb)

arrayDec :: (RenderSym r) => SValue r -> SVariable r -> MSStatement r
arrayDec n vr = do
  sz <- zoom lensMStoVS n 
  v <- zoom lensMStoVS vr 
  modify $ useVarName $ variableName v
  let tp = variableType v
  innerTp <- zoom lensMStoVS $ listInnerType $ return tp
  mkStmt $ RC.type' tp <+> RC.variable v <+> equals <+> new' <+> 
    RC.type' innerTp <> brackets (RC.value sz)

arrayDecDef :: (RenderSym r) => SVariable r -> [SValue r] -> MSStatement r
arrayDecDef v' vals' = do 
  vs <- mapM (zoom lensMStoVS) vals'
  vd <- S.varDec v'
  mkStmt (RC.statement vd <+> equals <+> braces (valueList vs))

openFileA :: (RenderSym r) => (SValue r -> VSType r -> SValue r -> SValue r) -> 
  SVariable r -> SValue r -> MSStatement r
openFileA f vr vl = vr &= f vl outfile S.litTrue

forEach :: (RenderSym r) => Doc -> Doc -> Doc -> Doc -> SVariable r -> SValue r 
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

docMain :: (RenderSym r) => MSBody r -> SMethod r
docMain b = commentedFunc (docComment $ toState $ functionDox 
  mainDesc [(args, argsDesc)] []) (S.mainFunction b)

mainFunction :: (RenderSym r) => VSType r -> Label -> MSBody r -> SMethod r
mainFunction s n = S.intFunc True n public static (mType S.void)
  [S.param (S.var args (s >>= (\argT -> typeFromData (List String) 
  (render (RC.type' argT) ++ array) (RC.type' argT <> array'))))]

buildModule' :: (RenderSym r) => Label -> (String -> r (Import r)) -> [Label] 
  -> [SMethod r] -> [SClass r] -> FSModule r
buildModule' n inc is ms cs = S.modFromData n (do
  cls <- mapM (zoom lensFStoCS) 
          (if null ms then cs else S.buildClass Nothing [] ms : cs) 
  lis <- getLangImports
  libis <- getLibImports
  mis <- getModuleImports
  return $ vibcat [
    vcat (map (RC.import' . inc) (lis ++ sort (is ++ libis) ++ mis)),
    vibcat (map RC.class' cls)])

-- Java and C++ --

-- | First parameter is language name, rest similar to call from ClassInterface
call' :: (RenderSym r) => String -> Maybe Library -> Maybe Doc -> MixedCall r
call' l _ _ _ _ _ (_:_) = error $ namedArgError l
call' _ l o n t ps ns = call empty l o n t ps ns

namedArgError :: String -> String
namedArgError l = "Named arguments not supported in " ++ l 

listSizeFunc :: (RenderSym r) => VSFunction r
listSizeFunc = S.func "size" S.int []

listAccessFunc' :: (RenderSym r) => Label -> VSType r -> SValue r -> 
  VSFunction r
listAccessFunc' f t i = S.func f t [intValue i]

-- C# and C++ --

stringRender :: String
stringRender = "string"

string :: (RenderSym r) => VSType r
string = typeFromData String stringRender (text stringRender)

constDecDef :: (RenderSym r) => SVariable r -> SValue r -> MSStatement r
constDecDef vr' v'= do
  vr <- zoom lensMStoVS vr'
  v <- zoom lensMStoVS v'
  modify $ useVarName $ variableName vr
  mkStmt (R.constDecDef vr v)
  
docInOutFunc :: (RenderSym r) => ([SVariable r] -> [SVariable r] -> 
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

extFuncAppMixedArgs :: (RenderSym r) => Library -> MixedCall r
extFuncAppMixedArgs l = S.call (Just l) Nothing

notNull :: (RenderSym r) => String -> SValue r -> SValue r
notNull nil v = v ?!= S.valueOf (S.var nil $ onStateValue valueType v)

listDecDef :: (RenderSym r) => SVariable r -> [SValue r] -> MSStatement r
listDecDef v vals = do
  vr <- zoom lensMStoVS v 
  let lst = S.litList (listInnerType $ return $ variableType vr) vals
  S.varDecDef (return vr) lst

destructorError :: String -> String
destructorError l = "Destructors not allowed in " ++ l

stateVarDef :: (RenderSym r, Monad r) => r (Scope r) -> r (Permanence r) -> 
  SVariable r -> SValue r -> CS (r Doc)
stateVarDef s p vr vl = zoom lensCStoMS $ onStateValue (toCode . R.stateVar 
  (RC.scope s) (RC.perm p) . RC.statement) (S.stmt $ S.varDecDef vr vl)
  
constVar :: (RenderSym r, Monad r) => Doc -> r (Scope r) -> SVariable r -> 
  SValue r -> CS (r Doc)
constVar p s vr vl = zoom lensCStoMS $ onStateValue (toCode . R.stateVar 
  (RC.scope s) p . RC.statement) (S.stmt $ S.constDecDef vr vl)

-- Python, Java, C++, and Swift --

litArray :: (RenderSym r) => (Doc -> Doc) -> VSType r -> [SValue r] -> SValue r
litArray f t es = sequence es >>= (\elems -> mkStateVal (S.arrayType t) 
  (f $ valueList elems))

-- Python, C#, C++, and Swift --

listSetFunc :: (RenderSym r) => (Doc -> Doc -> Doc) -> SValue r -> SValue r -> 
  SValue r -> VSFunction r
listSetFunc f v idx setVal = join $ on2StateValues (\i toVal -> funcFromData 
  (f (RC.value i) (RC.value toVal)) (onStateValue valueType v)) (intValue idx) 
  setVal

extraClass :: (RenderSym r) =>  Label -> Maybe Label -> [CSStateVar r] -> 
  [SMethod r] -> SClass r
extraClass n = S.intClass n public . S.inherit

-- Python, C#, and Swift --

listAccessFunc :: (RenderSym r) => VSType r -> SValue r -> VSFunction r
listAccessFunc t v = intValue v >>= ((`funcFromData` t) . R.listAccessFunc)

-- Java, C#, and Swift --

doubleRender :: String
doubleRender = "Double"

double :: (RenderSym r) => VSType r
double = typeFromData Double doubleRender (text doubleRender)

openFileR :: (RenderSym r) => (SValue r -> VSType r -> SValue r) -> SVariable r 
  -> SValue r -> MSStatement r
openFileR f vr vl = vr &= f vl infile

openFileW :: (RenderSym r) => (SValue r -> VSType r -> SValue r -> SValue r) -> 
  SVariable r -> SValue r -> MSStatement r
openFileW f vr vl = vr &= f vl outfile S.litFalse

stateVar :: (RenderSym r, Monad r) => r (Scope r) -> r (Permanence r) -> 
  SVariable r -> CS (r Doc)
stateVar s p v = zoom lensCStoMS $ onStateValue (toCode . R.stateVar 
  (RC.scope s) (RC.perm p) . RC.statement) (S.stmt $ S.varDec v)

-- Python and Swift --

self :: (RenderSym r) => SVariable r
self = zoom lensVStoMS getClassName >>= (\l -> mkStateVar R.self (obj l) 
  R.self')

multiAssign :: (RenderSym r) => (Doc -> Doc) -> [SVariable r] -> [SValue r] -> 
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
  mkStateVar "" S.void (wrapIfMult vrs (variableList vrs)) &= 
    mkStateVal S.void (wrapIfMult vls (valueList vls))

multiReturn :: (RenderSym r) => (Doc -> Doc) -> [SValue r] -> MSStatement r
multiReturn _ [] = error "Attempt to write return statement with no values."
multiReturn _ [v] = returnStmt v
multiReturn f vs = do
  vs' <- mapM (zoom lensMStoVS) vs
  returnStmt $ mkStateVal S.void $ f $ valueList vs'

listDec :: (RenderSym r) => SVariable r -> MSStatement r
listDec v = S.varDecDef v $ S.litList (onStateValue variableType v) []

funcDecDef :: (RenderSym r) => SVariable r -> [SVariable r] -> MSBody r -> 
  MSStatement r
funcDecDef v ps b = do
  vr <- zoom lensMStoVS v
  modify $ useVarName $ variableName vr
  s <- get
  f <- function (variableName vr) private (return $ variableType vr) 
    (map S.param ps) b
  modify (L.set currParameters (s ^. currParameters))
  mkStmtNoEnd $ RC.method f

inOutCall :: (RenderSym r) => (Label -> VSType r -> [SValue r] -> SValue r) -> 
  Label -> [SValue r] -> [SVariable r] -> [SVariable r] -> MSStatement r
inOutCall f n ins [] [] = S.valStmt $ f n S.void ins
inOutCall f n ins outs both = S.multiAssign rets [f n S.void (map S.valueOf 
  both ++ ins)]
  where rets = both ++ outs

forLoopError :: String -> String
forLoopError l = "Classic for loops not available in " ++ l ++ ", use " ++
  "forRange, forEach, or while instead"

mainBody :: (RenderSym r) => MSBody r -> SMethod r
mainBody b = do
  modify setCurrMain
  bod <- b
  modify (setMainDoc $ RC.body bod)
  mthdFromData Pub empty

inOutFunc :: (RenderSym r) => (VSType r -> [MSParameter r] -> MSBody r -> 
  SMethod r) -> [SVariable r] -> [SVariable r] -> [SVariable r] -> MSBody r -> 
  SMethod r
inOutFunc f ins [] [] b = f S.void (map S.param ins) b
inOutFunc f ins outs both b = f 
  (multiType $ map (onStateValue variableType) rets)  
  (map S.pointerParam both ++ map S.param ins) 
  (multiBody [bodyStatements $ map S.varDec outs, b, oneLiner $ S.multiReturn $ 
  map S.valueOf rets])
  where rets = both ++ outs

docInOutFunc' :: (RenderSym r) => FuncDocRenderer -> ([SVariable r] -> 
    [SVariable r] -> [SVariable r] -> MSBody r -> SMethod r) -> 
  String -> [(String, SVariable r)] -> [(String, SVariable r)] -> 
  [(String, SVariable r)] -> MSBody r -> SMethod r
docInOutFunc' dfr f desc is os bs b = docFuncRepr dfr desc (map fst $ bs ++ is)
  (map fst $ bs ++ os) (f (map snd is) (map snd os) (map snd bs) b)

-- Java and Swift --

floatRender :: String
floatRender = "Float"

float :: (RenderSym r) => VSType r
float = typeFromData Float floatRender (text floatRender)

stringRender' :: String
stringRender' = "String"

string' :: (RenderSym r) => VSType r
string' = typeFromData String stringRender' (text stringRender')

-- C# and Swift --

inherit :: (Monad r) => Maybe Label -> r ParentSpec
inherit n = toCode $ maybe empty ((colon <+>) . text) n

implements :: (Monad r) => [Label] -> r ParentSpec
implements is = toCode $ colon <+> text (intercalate listSep is)

-- Swift and Julia --
boolRender :: String
boolRender = "Bool"

bool :: (RenderSym r) => VSType r
bool = typeFromData Boolean boolRender (text boolRender)

docMod' :: (RenderSym r) => String -> String -> [String] -> String -> SFile r -> SFile r
docMod' = docMod modDoc'

-- | Generates Markdown/DocC style doc comment.  Useful for Swift, which follows
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

docCommandInit, docCommandSep, authorDoc, dateDoc, noteDoc :: String
docCommandInit = "- "
docCommandSep = ": "
authorDoc = "Authors"
dateDoc = "Date"
noteDoc = "Note"

-- Python, Julia, and MATLAB --
-- | Call to get the size of a list in a language where this is not a method.
listSize :: (RenderSym r) => SValue r -> SValue r
listSize l = do
  f <- S.listSizeFunc l
  mkVal (RC.functionType f) (RC.function f)

-- Julia and MATLAB --
-- | Call to insert a value into a list in a language where this is not a method.
listAdd :: (RenderSym r) => SValue r -> SValue r -> SValue r -> SValue r
listAdd l i v = do
  f <- S.listAddFunc l (S.intToIndex i) v
  mkVal (RC.functionType f) (RC.function f)

-- | Call to append a value to a list in a language where this is not a method.
listAppend :: (RenderSym r) => SValue r -> SValue r -> SValue r
listAppend l v = do
  f <- S.listAppendFunc l v
  mkVal (RC.functionType f) (RC.function f)

-- | Convert an integer to an index in a 1-indexed language
--   Since GOOL is 0-indexed, we need to add 1
intToIndex' :: (RenderSym r) => SValue r -> SValue r
intToIndex' = (#+ S.litInt 1)

-- | Convert an index to an integer in a 1-indexed language
--   Since GOOL is 0-indexed, we need to subtract 1
indexToInt' :: (RenderSym r) => SValue r -> SValue r
indexToInt' = (#- S.litInt 1)