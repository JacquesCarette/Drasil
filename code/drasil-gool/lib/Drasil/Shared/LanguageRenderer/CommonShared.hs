module Drasil.Shared.LanguageRenderer.CommonShared (
  bool, boolRender, extVar, funcType, listDec, listDecDef, listAccessFunc,
  listSetFunc, notNull, extFuncAppMixedArgs, functionDoc, listSize, listAdd,
  listAppend, intToIndex', indexToInt', inOutFunc, docInOutFunc', forLoopError,
  varDecDef, openFileR', openFileW', openFileA', multiReturn, multiAssign,
  inOutCall, mainBody, argExists, forEach', litSet
) where

import qualified Drasil.GOOL.RendererClassesOO as RC (PermElim(..),
  StateVarElim(..), ClassElim(..))

import qualified Drasil.Shared.RendererClassesCommon as RC (ImportElim(..),
  BodyElim(..), InternalTypeElim(..), InternalVarElim(variable), ValueElim(..),
  StatementElim(statement), VisibilityElim(..), MethodElim(..), FunctionElim(..))

import qualified Drasil.Shared.InterfaceCommon as IC (argsList,
  TypeSym(int, bool, double, string, listType, arrayType, void), VariableSym(var),
  Literal(litTrue, litFalse, litList, litSet, litInt, litString),
  VariableValue(valueOf), StatementSym(valStmt, emptyStmt), DeclStatement(varDec,
  varDecDef, constDecDef), List(intToIndex, indexToInt), ParameterSym(param,
  pointerParam), MethodSym(mainFunction), AssignStatement(assign), ScopeSym(..))

import qualified Drasil.Shared.LanguageRenderer as R (self, self', module',
  print, stateVar, stateVarList, constDecDef, extVar, listAccessFunc)

import Drasil.Shared.RendererClassesCommon (listSetFunc, CommonRenderSym, ImportSym(..),
  RenderBody(..), RenderType(..), RenderVariable(varFromData),
  InternalVarElim(variableBind), RenderFunction(funcFromData),
  MethodTypeSym(mType), RenderMethod(commentedFunc, mthdFromData),
  BlockCommentSym(..), ScopeElim(scopeData))

import Drasil.Shared.InterfaceCommon (funcType, Label, Library, Body, MSBody, VSFunction,
  VSType, Variable, SVariable, Value, SValue, MSStatement, MSParameter, SMethod,
  MixedCall, bodyStatements, oneLiner, TypeSym(infile, outfile, listInnerType),
  TypeElim(getType, getTypeString), VariableElim(variableName, variableType),
  ValueSym(valueType), Comparison(..), (&=), ControlStatement(returnStmt),
  VisibilitySym(..), MethodSym(function), funcApp, ScopeSym(Scope))

import qualified Drasil.Shared.RendererClassesCommon as S (RenderBody(multiBody),
  RenderValue(call), RenderStatement(stmt),
  InternalAssignStmt(multiAssign), InternalControlStmt(multiReturn),
  InternalListFunc(listSizeFunc, listAddFunc, listAppendFunc))

import Drasil.Shared.LanguageRenderer (array', new', args, array, listSep, access,
  mathFunc, ModuleDocRenderer, FuncDocRenderer, functionDox, classDox,
  moduleDox, variableList, valueList, intValue)

import Drasil.Shared.LanguageRenderer.CommonPseudoOO ( funcType, listSetFunc )

-- Python, Java, C#, and Julia --

extVar :: (CommonRenderSym r) => Label -> Label -> VSType r -> SVariable r
extVar l n t = mkStateVar (l `access` n) t (R.extVar l n)

-- Python, Java, C++, and Julia --

funcType :: (CommonRenderSym r) => [VSType r] -> VSType r -> VSType r
funcType ps' r' = do
  ps <- sequence ps'
  r <- r'
  typeFromData (Func (map getType ps) (getType r)) "" empty

-- Python, Java, C#, Swift, and Julia --

extFuncAppMixedArgs :: (CommonRenderSym r) => Library -> MixedCall r
extFuncAppMixedArgs l = S.call (Just l) Nothing

-- Python, C#, Swift, and Julia --

listAccessFunc :: (CommonRenderSym r) => VSType r -> SValue r -> VSFunction r
listAccessFunc t v = intValue v >>= ((`funcFromData` t) . R.listAccessFunc)

listSetFunc :: (CommonRenderSym r) => (Doc -> Doc -> Doc) -> SValue r -> SValue r -> SValue r -> VSFunction r
listSetFunc f v idx setVal = join $ on2StateValues (\i toVal -> funcFromData
  (f (RC.value i) (RC.value toVal)) (onStateValue valueType v)) (intValue idx)
  setVal

notNull :: (CommonRenderSym r) => String -> SValue r -> SValue r
notNull nil v = v ?!= IC.valueOf (IC.var nil $ onStateValue valueType v)

functionDoc :: FuncDocRenderer
functionDoc desc params returns = [desc | not (null desc)]
  ++ map (\(v, vDesc) -> "- Parameter: " ++ v ++ ": " ++ vDesc) params
  ++ map ("- Returns: " ++) returns

listSize :: (CommonRenderSym r) => SValue r -> SValue r
listSize l = do
  f <- S.listSizeFunc l
  mkVal (RC.functionType f) (RC.function f)

listAdd :: (CommonRenderSym r) => SValue r -> SValue r -> SValue r -> SValue r
listAdd l i v = do
  f <- S.listAddFunc l (IC.intToIndex i) v
  mkVal (RC.functionType f) (RC.function f)

listAppend :: (CommonRenderSym r) => SValue r -> SValue r -> SValue r
listAppend l v = do
  f <- S.listAppendFunc l v
  mkVal (RC.functionType f) (RC.function f)

intToIndex' :: (CommonRenderSym r) => SValue r -> SValue r
intToIndex' v = v `smartAdd` IC.litInt 1

indexToInt' :: (CommonRenderSym r) => SValue r -> SValue r
indexToInt' v = v `smartSub` IC.litInt 1

inOutFunc :: (CommonRenderSym r) => (VSType r -> [MSParameter r] -> MSBody r -> SMethod r)
          -> [SVariable r] -> [SVariable r] -> [SVariable r] -> MSBody r -> SMethod r
inOutFunc f ins outs both b = f (multiType $ map (onStateValue variableType) rets)
  (map IC.pointerParam both ++ map IC.param ins)
  (multiBody [bodyStatements $ map (`IC.varDec` IC.local) outs, b,
    oneLiner $ S.multiReturn $ map IC.valueOf rets])
  where rets = both ++ outs

docInOutFunc' :: (CommonRenderSym r) => FuncDocRenderer
              -> ([SVariable r] -> [SVariable r] -> [SVariable r] -> MSBody r -> SMethod r)
              -> String -> [(String, SVariable r)] -> [(String, SVariable r)]
              -> [(String, SVariable r)] -> MSBody r -> SMethod r
docInOutFunc' dfr f desc is os bs b = docFuncRepr dfr desc (map fst $ bs ++ is)
  (map fst $ bs ++ os) (f (map snd is) (map snd os) (map snd bs) b)

forLoopError :: String -> String
forLoopError l = "Classic for loops not available in " ++ l ++ ", use forRange, forEach, or while instead"

varDecDef :: (CommonRenderSym r) => SVariable r -> r (Scope r) -> Maybe (SValue r) -> MSStatement r
varDecDef v scp e = do
  v' <- zoom lensMStoVS v
  modify $ useVarName (variableName v')
  modify $ setVarScope (variableName v') (scopeData scp)
  maybe IC.emptyStmt (IC.assign v) e

openFileR', openFileW', openFileA' :: (CommonRenderSym r) => SValue r -> SValue r
openFileR' n = funcApp "open" infile [n, IC.litString "r"]
openFileW' n = funcApp "open" infile [n, IC.litString "w"]
openFileA' n = funcApp "open" infile [n, IC.litString "a"]

multiReturn :: (CommonRenderSym r) => (Doc -> Doc) -> [SValue r] -> MSStatement r
multiReturn _ [] = error "Empty return statement"
multiReturn _ [v] = returnStmt v
multiReturn f vs = do
  vs' <- mapM (zoom lensMStoVS) vs
  returnStmt $ mkStateVal IC.void $ f $ valueList vs'

multiAssign :: (CommonRenderSym r) => (Doc -> Doc) -> [SVariable r] -> [SValue r] -> MSStatement r
multiAssign _ [] _ = error "No vars in assignment"
multiAssign _ _ [] = error "No vals in assignment"
multiAssign f vars vals
  | length vals /= 1 && length vars /= length vals =
      error "Var/Val count mismatch"
  | otherwise = do
      vrs <- mapM (zoom lensMStoVS) vars
      vls <- mapM (zoom lensMStoVS) vals
      let wrap = if length vrs > 1 then f else id
      mkStateVar "" IC.void (wrap $ variableList vrs) &= mkStateVal IC.void (wrap $ valueList vls)

inOutCall :: (CommonRenderSym r) => (Label -> VSType r -> [SValue r] -> SValue r)
          -> Label -> [SValue r] -> [SVariable r] -> [SVariable r] -> MSStatement r
inOutCall f n ins [] [] = IC.valStmt $ f n IC.void ins
inOutCall f n ins outs both =
  S.multiAssign (both ++ outs) [f n IC.void (map IC.valueOf (both ++ ins))]

mainBody :: (CommonRenderSym r) => MSBody r -> SMethod r
mainBody b = do
  modify setCurrMain
  bod <- b
  modify (setMainDoc $ RC.body bod)
  mthdFromData Pub empty

argExists :: (CommonRenderSym r) => Integer -> SValue r
argExists i = listSize IC.argsList ?> IC.litInt (fromIntegral $ i + 1)

forEach' :: (CommonRenderSym r)
         => (r (Variable r) -> r (Value r) -> r (Body r) -> Doc)
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
