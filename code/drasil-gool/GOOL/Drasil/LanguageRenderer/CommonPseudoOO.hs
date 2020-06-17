-- | Implementations defined here are valid in some, but not all, language renderers
module GOOL.Drasil.LanguageRenderer.CommonPseudoOO (
  bindingError, extVar, classVar, objVarSelf, iterVar, extFuncAppMixedArgs, 
  indexOf, listAddFunc, iterBeginError, iterEndError, listDecDef, 
  discardFileLine, destructorError, stateVarDef, constVar, intClass, objVar, 
  funcType, listSetFunc, listAccessFunc, buildModule, arrayType, pi, notNull, 
  printSt, arrayDec, arrayDecDef, openFileR, openFileW, openFileA, forEach, 
  docMain, mainFunction, stateVar, buildModule', litArray, call', listSizeFunc, 
  listAccessFunc', string, constDecDef, docInOutFunc
) where

import Utils.Drasil (indent)

import GOOL.Drasil.CodeType (CodeType(..))
import GOOL.Drasil.ClassInterface (Label, Library, MSBody, VSType, SVariable, 
  SValue, VSFunction, MSStatement, SMethod, CSStateVar, SClass, FSModule, 
  MixedCall, PermanenceSym(..), 
  TypeSym(infile, outfile, listInnerType, iterator), 
  TypeElim(getType, getTypeString), VariableElim(variableName, variableType), 
  ValueSym(valueType), Comparison(..), objMethodCallNoParams, (&=), 
  ScopeSym(..))
import qualified GOOL.Drasil.ClassInterface as S (
  TypeSym(int, double, string, listType, arrayType, void),
  VariableSym(var, self, objVar), Literal(litTrue, litFalse, litList), 
  VariableValue(valueOf), FunctionSym(func, objAccess), StatementSym(valStmt), 
  DeclStatement(varDec, varDecDef, constDecDef), ParameterSym(param), 
  MethodSym(mainFunction), ClassSym(buildClass))
import GOOL.Drasil.RendererClasses (RenderSym, ImportSym(..),  RenderType(..),
  RenderVariable(varFromData), InternalVarElim(variableBind), 
  RenderFunction(funcFromData), MethodTypeSym(mType),
  RenderMethod(commentedFunc), ParentSpec, BlockCommentSym(..))
import qualified GOOL.Drasil.RendererClasses as S (RenderValue(call), 
  RenderStatement(stmt), RenderMethod(intFunc), RenderMod(modFromData))
import qualified GOOL.Drasil.RendererClasses as RC (ImportElim(..), 
  PermElim(..), BodyElim(..), InternalTypeElim(..), InternalVarElim(variable), 
  ValueElim(value), StatementElim(statement), ScopeElim(..), MethodElim(..), 
  StateVarElim(..), ClassElim(..))
import GOOL.Drasil.Helpers (vibcat, toCode, toState, onCodeValue, onStateValue, 
  on2StateValues, onStateList, on2StateWrapped)
import GOOL.Drasil.LanguageRenderer (array', new', args, array, access, 
  mathFunc, functionDox, valueList, intValue)
import qualified GOOL.Drasil.LanguageRenderer as R (module', print, stateVar, 
  stateVarList, constDecDef, extVar, objVar, listAccessFunc)
import GOOL.Drasil.LanguageRenderer.Constructors (mkStmt, mkStmtNoEnd, 
  mkStateVal, mkStateVar, mkVar)
import GOOL.Drasil.LanguageRenderer.LanguagePolymorphic (classVarCheckStatic,
  call, docFuncRepr)
import GOOL.Drasil.State (FS, CS, lensFStoCS, lensFStoMS, lensCStoMS, 
  lensMStoVS, getLangImports, getLibImports, getModuleImports, setClassName)

import Prelude hiding (print,pi,(<>))
import Data.List (sort)
import Control.Monad (join)
import Control.Monad.State (modify)
import Control.Lens.Zoom (zoom)
import Text.PrettyPrint.HughesPJ (Doc, text, empty, render, (<>), (<+>), parens,
  brackets, braces, vcat, equals)

-- Python, C#, and Java --

bindingError :: String -> String
bindingError l = "Binding unimplemented in " ++ l

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

iterVar :: (RenderSym r) => Label -> VSType r -> SVariable r
iterVar n t = S.var n (iterator t)

extFuncAppMixedArgs :: (RenderSym r) => Library -> MixedCall r
extFuncAppMixedArgs l = S.call (Just l) Nothing

indexOf :: (RenderSym r) => Label -> SValue r -> SValue r -> SValue r
indexOf f l v = S.objAccess l (S.func f S.int [v])

listAddFunc :: (RenderSym r) => Label -> SValue r -> SValue r -> VSFunction r
listAddFunc f i v = S.func f (S.listType $ onStateValue valueType v) 
  [i, v]
  
iterBeginError :: String -> String
iterBeginError l = "Attempt to use iterBeginFunc in " ++ l ++ ", but " ++ l ++ 
  " has no iterators"

iterEndError :: String -> String
iterEndError l = "Attempt to use iterEndFunc in " ++ l ++ ", but " ++ l ++ 
  " has no iterators"
  
listDecDef :: (RenderSym r) => SVariable r -> [SValue r] -> MSStatement r
listDecDef v vals = do
  vr <- zoom lensMStoVS v 
  let lst = S.litList (listInnerType $ return $ variableType vr) vals
  S.varDecDef (return vr) lst
  
discardFileLine :: (RenderSym r) => Label -> SValue r -> MSStatement r
discardFileLine n f = S.valStmt $ objMethodCallNoParams S.string f n 

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

objVar :: (RenderSym r) => SVariable r -> SVariable r -> SVariable r
objVar = on2StateWrapped (\o v -> mkVar (variableName o `access` variableName v) 
    (variableType v) (R.objVar (RC.variable o) (RC.variable v))) 

-- Python, C#, and C++ --

listSetFunc :: (RenderSym r) => (Doc -> Doc -> Doc) -> SValue r -> SValue r -> 
  SValue r -> VSFunction r
listSetFunc f v idx setVal = join $ on2StateValues (\i toVal -> funcFromData 
  (f (RC.value i) (RC.value toVal)) (onStateValue valueType v)) (intValue idx) 
  setVal

-- Python and C# --

listAccessFunc :: (RenderSym r) => VSType r -> SValue r -> VSFunction r
listAccessFunc t v = intValue v >>= ((`funcFromData` t) . R.listAccessFunc)

-- Python and C++ --

buildModule :: (RenderSym r) => Label -> FS Doc -> FS Doc -> [SMethod r] -> 
  [SClass r] -> FSModule r
buildModule n imps bot fs cs = S.modFromData n (do
  cls <- mapM (zoom lensFStoCS) cs
  fns <- mapM (zoom lensFStoMS) fs
  is <- imps
  bt <- bot
  return $ R.module' is (vibcat (map RC.class' cls)) 
    (vibcat (map RC.method fns ++ [bt])))

-- Java and C# -- 

arrayType :: (RenderSym r) => VSType r -> VSType r
arrayType t' = do 
  t <- t'
  typeFromData (Array (getType t)) 
    (getTypeString t ++ array) (RC.type' t <> brackets empty)
  
pi :: (RenderSym r) => SValue r
pi = mkStateVal S.double (text $ mathFunc "PI")

notNull :: (RenderSym r) => SValue r -> SValue r
notNull v = v ?!= S.valueOf (S.var "null" $ onStateValue valueType v)

printSt :: (RenderSym r) => SValue r -> SValue r -> MSStatement r
printSt va' vb' = do
  va <- zoom lensMStoVS va'
  vb <- zoom lensMStoVS vb' 
  mkStmt (R.print va vb)

arrayDec :: (RenderSym r) => SValue r -> SVariable r -> MSStatement r
arrayDec n vr = do
  sz <- zoom lensMStoVS n 
  v <- zoom lensMStoVS vr 
  let tp = variableType v
  innerTp <- zoom lensMStoVS $ listInnerType $ return tp
  mkStmt $ RC.type' tp <+> RC.variable v <+> equals <+> new' <+> 
    RC.type' innerTp <> brackets (RC.value sz)

arrayDecDef :: (RenderSym r) => SVariable r -> [SValue r] -> MSStatement r
arrayDecDef v' vals' = do 
  vs <- mapM (zoom lensMStoVS) vals'
  vd <- S.varDec v'
  mkStmt (RC.statement vd <+> equals <+> braces (valueList vs))

openFileR :: (RenderSym r) => (SValue r -> VSType r -> SValue r) -> SVariable r 
  -> SValue r -> MSStatement r
openFileR f vr vl = vr &= f vl infile

openFileW :: (RenderSym r) => (SValue r -> VSType r -> SValue r -> SValue r) -> 
  SVariable r -> SValue r -> MSStatement r
openFileW f vr vl = vr &= f vl outfile S.litFalse

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

stateVar :: (RenderSym r, Monad r) => r (Scope r) -> r (Permanence r) -> 
  SVariable r -> CS (r Doc)
stateVar s p v = zoom lensCStoMS $ onStateValue (toCode . R.stateVar 
  (RC.scope s) (RC.perm p) . RC.statement) (S.stmt $ S.varDec v)

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

litArray :: (RenderSym r) => VSType r -> [SValue r] -> SValue r
litArray t es = sequence es >>= (\elems -> mkStateVal (S.arrayType t) 
  (braces $ valueList elems))

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
  mkStmt (R.constDecDef vr v)
  
docInOutFunc :: (RenderSym r) => ([SVariable r] -> [SVariable r] -> 
    [SVariable r] -> MSBody r -> SMethod r) -> 
  String -> [(String, SVariable r)] -> [(String, SVariable r)] -> 
  [(String, SVariable r)] -> MSBody r -> SMethod r
docInOutFunc f desc is [o] [] b = docFuncRepr desc (map fst is) [fst o] 
  (f (map snd is) [snd o] [] b)
docInOutFunc f desc is [] [both] b = docFuncRepr desc (map fst $ both : is) 
  [fst both] (f (map snd is) [] [snd both] b)
docInOutFunc f desc is os bs b = docFuncRepr desc (map fst $ bs ++ is ++ os)
  [] (f (map snd is) (map snd os) (map snd bs) b)