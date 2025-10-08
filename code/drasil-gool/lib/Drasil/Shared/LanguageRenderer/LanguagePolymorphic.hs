{-# LANGUAGE BlockArguments, PostfixOperators, TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas, #-}

-- | Implementations defined here are valid for any language renderer.
module Drasil.Shared.LanguageRenderer.LanguagePolymorphic (fileFromData,
  multiBody, block, multiBlock, listInnerType, obj, negateOp, csc, sec,
  cot, equalOp, notEqualOp, greaterOp, greaterEqualOp, lessOp, lessEqualOp,
  plusOp, minusOp, multOp, divideOp, moduloOp, var, staticVar, objVar,
  classVarCheckStatic, arrayElem, local, litChar, litDouble, litInt, litString,
  valueOf, arg, argsList, call, funcAppMixedArgs, selfFuncAppMixedArgs,
  newObjMixedArgs, lambda, objAccess, objMethodCall, func, get, set, listAdd,
  listAppend, listAccess, listSet, getFunc, setFunc,
  listAppendFunc, stmt, loopStmt, emptyStmt, assign, subAssign, increment,
  objDecNew, print, closeFile, returnStmt, valStmt, comment, throw, ifCond,
  tryCatch, construct, param, method, getMethod, setMethod, initStmts,
  function, docFuncRepr, docFunc, buildClass, implementingClass, docClass,
  commentedClass, modFromData, fileDoc, docMod, OptionalSpace(..),
  defaultOptSpace, smartAdd, smartSub
) where

import Utils.Drasil (indent)

import Drasil.Shared.CodeType (CodeType(..), ClassName)
import Drasil.Shared.InterfaceCommon (Label, Library, NamedArgs, MixedCall,
  MixedCtorCall, BodySym(Body), StatementSym(Statement), MethodSym(Method),
  FunctionSym(Function), ParameterSym(Parameter), bodyStatements, oneLiner,
  BlockSym(Block), TypeSym(Type), TypeElim(getType, getTypeString),
  VariableSym(Variable), VisibilitySym(..), VariableElim(variableName,
  variableType), ValueSym(Value, valueType), NumericExpression((#+), (#-), (#/),
  sin, cos, tan), Comparison(..), funcApp, StatementSym(multi),
  AssignStatement((&++)), (&=), IOStatement(printStr, printStrLn, printFile,
  printFileStr, printFileStrLn), ifNoElse, ScopeSym(Scope), convType)
import qualified Drasil.Shared.InterfaceCommon as IC (TypeSym(int, double, char,
  string, listType, arrayType, listInnerType, funcType, void), VariableSym(var),
  Literal(litInt, litFloat, litDouble, litString), VariableValue(valueOf),
  List(listSize, listAccess), StatementSym(valStmt), DeclStatement(varDecDef),
  IOStatement(print), ControlStatement(returnStmt, for, forEach),
  ParameterSym(param), List(intToIndex), ScopeSym(local))
import Drasil.GOOL.InterfaceGOOL (Initializers, FileSym(File), ClassSym(Class),
  StateVarSym(StateVar), ModuleSym(Module), newObj, objMethodCallNoParams,
  ($.), PermanenceSym(..), convTypeOO)
import qualified Drasil.GOOL.InterfaceGOOL as IG (OOVariableSym(objVarSelf),
  OOMethodSym(method), OOFunctionSym(func))
import Drasil.Shared.RendererClassesCommon (CommonRenderSym, RenderType(..),
  InternalVarElim(variableBind), RenderValue(valFromData),
  RenderFunction(funcFromData), FunctionElim(functionType),
  RenderStatement(stmtFromData), StatementElim(statementTerm),
  MethodTypeSym(mType), RenderParam(paramFromData), RenderMethod(commentedFunc),
  BlockCommentSym(..))
import qualified Drasil.Shared.RendererClassesCommon as S (RenderValue(call),
  InternalListFunc (listAddFunc, listAppendFunc, listAccessFunc, listSetFunc),
  RenderStatement(stmt), InternalIOStmt(..))
import qualified Drasil.Shared.RendererClassesCommon as RC (BodyElim(..),
  BlockElim(..), InternalVarElim(variable), ValueElim(value, valueInt),
  FunctionElim(..), StatementElim(statement), BlockCommentElim(..))
import Drasil.GOOL.RendererClassesOO (OORenderSym, RenderFile(commentedMod),
  OORenderMethod(intMethod), RenderClass(inherit, implements),
  RenderMod(updateModuleDoc))
import qualified Drasil.GOOL.RendererClassesOO as S (RenderFile(fileFromData),
  InternalGetSet(getFunc, setFunc), OORenderMethod(intFunc),
  RenderClass(intClass, commentedClass))
import qualified Drasil.GOOL.RendererClassesOO as RC (ClassElim(..),
  ModuleElim(..))
import Drasil.Shared.AST (Binding(..), Terminator(..), isSource, ScopeTag(Local),
  ScopeData, sd, OpData)
import Drasil.Shared.Helpers (doubleQuotedText, vibcat, emptyIfEmpty, toCode,
  toState, onStateValue, on2StateValues, onStateList, getInnerType, getNestDegree,
  on2StateWrapped)
import Drasil.Shared.LanguageRenderer (dot, ifLabel, elseLabel, access, addExt,
  FuncDocRenderer, ClassDocRenderer, ModuleDocRenderer, getterName, setterName,
  valueList, namedArgList)
import qualified Drasil.Shared.LanguageRenderer as R (file, block, assign,
  addAssign, subAssign, return', comment, getTerm, var, objVar, arg, func,
  objAccess, commentedItem)
import Drasil.Shared.LanguageRenderer.Constructors (mkStmt, mkStmtNoEnd,
  mkStateVal, mkVal, mkStateVar, mkVar, mkStaticVar, unOpPrec,
  compEqualPrec, compPrec, addPrec, multPrec)
import Drasil.Shared.State (FS, CS, MS, lensFStoGS, lensMStoVS, lensCStoFS,
  currMain, currFileType, addFile, setMainMod, setModuleName, getModuleName,
  addParameter, getParameters, useVarName)

import Prelude hiding (print,sin,cos,tan,(<>))
import Data.Maybe (fromMaybe, maybeToList)
import Control.Monad.State (State, modify)
import Control.Lens ((^.), over)
import Control.Lens.Zoom (zoom)
import Text.PrettyPrint.HughesPJ (Doc, text, empty, render, (<>), (<+>), ($+$),
  parens, brackets, integer, vcat, comma, isEmpty, space)
import qualified Text.PrettyPrint.HughesPJ as D (char, double)

-- Bodies --

multiBody :: (CommonRenderSym r) => [Body r] -> Method r
multiBody bs = undefined {-onStateList (toCode . vibcat) $ map (onStateValue RC.body) bs-}

-- Blocks --

block :: (CommonRenderSym r) => [Statement r] -> Method r
block sts = undefined{-} onStateList (toCode . R.block . map RC.statement) (map S.stmt sts)-}

multiBlock :: (CommonRenderSym r) => [Block r] -> Method r
multiBlock bs = undefined{-} onStateList (toCode . vibcat) $ map (onStateValue RC.block) bs-}

-- Types --

listInnerType :: (OORenderSym r) => Type r -> Type r
listInnerType = convTypeOO . getInnerType . getType

obj :: (CommonRenderSym r) => ClassName -> Type r
obj n = typeFromData (Object n) n (text n)

-- Unary Operators --

negateOp :: OpData
negateOp = unOpPrec "-"

csc :: (CommonRenderSym r) => Value r -> Value r
csc v = valOfOne (valueType v) #/ sin v

sec :: (CommonRenderSym r) => Value r -> Value r
sec v = valOfOne (valueType v) #/ cos v

cot :: (CommonRenderSym r) => Value r -> Value r
cot v = valOfOne (valueType v) #/ tan v

valOfOne :: (CommonRenderSym r) => Type r -> Value r
valOfOne = getVal . getType
  where getVal Float = IC.litFloat 1.0
        getVal _ = IC.litDouble 1.0

-- Binary Operators --

smartAdd :: (CommonRenderSym r) => Value r -> Value r -> Value r
smartAdd v1 v2 = case (RC.valueInt v1, RC.valueInt v2) of
    (Just i1, Just i2) -> litInt (i1 + i2)
    _                  -> v1 #+ v2

smartSub :: (CommonRenderSym r) => Value r -> Value r -> Value r
smartSub v1 v2 =
  case (RC.valueInt v1, RC.valueInt v2) of
    (Just i1, Just i2) -> litInt (i1 - i2)
    _                  -> v1 #- v2

equalOp :: OpData
equalOp = compEqualPrec "=="

notEqualOp :: OpData
notEqualOp = compEqualPrec "!="

greaterOp :: OpData
greaterOp = compPrec ">"

greaterEqualOp :: OpData
greaterEqualOp = compPrec ">="

lessOp :: OpData
lessOp = compPrec "<"

lessEqualOp :: OpData
lessEqualOp = compPrec "<="

plusOp :: OpData
plusOp = addPrec "+"

minusOp :: OpData
minusOp = addPrec "-"

multOp :: OpData
multOp = multPrec "*"

divideOp :: OpData
divideOp = multPrec "/"

moduloOp :: OpData
moduloOp = multPrec "%"

-- Variables --

var :: (CommonRenderSym r) => Label -> Type r -> Variable r
var n t = mkStateVar n t (R.var n)

staticVar :: (CommonRenderSym r) => Label -> Type r -> Variable r
staticVar n t = mkStaticVar n t (R.var n)

-- | To be used in classVar implementations. Throws an error if the variable is 
-- not static since classVar is for accessing static variables from a class
classVarCheckStatic :: (CommonRenderSym r) => Variable r -> Variable r
classVarCheckStatic v = classVarCS (variableBind v)
  where classVarCS Dynamic = error
          "classVar can only be used to access static variables"
        classVarCS Static = v

objVar :: (CommonRenderSym r) => Variable r -> Variable r -> Variable r
objVar o' v' =
  let objVar' Static = error
        "Cannot access static variables through an object, use classVar instead"
      objVar' Dynamic = mkVar (variableName o' `access` variableName v')
        (variableType v') (R.objVar (RC.variable o') (RC.variable v'))
  in objVar' (variableBind v')

arrayElem :: (OORenderSym r) => Value r -> Variable r -> Variable r
arrayElem i' v' =
  let
    i = IC.intToIndex i'
    vName = variableName v' ++ "[" ++ render (RC.value i) ++ "]"
    vType = listInnerType $ variableType v'
    vRender = RC.variable v' <> brackets (RC.value i)
  in mkStateVar vName vType vRender

-- Scope --
local :: ScopeData
local = sd Local

-- Values --

litChar :: (CommonRenderSym r) => (Doc -> Doc) -> Char -> Value r
litChar f c = mkStateVal IC.char (f $ if c == '\n' then text "\\n" else D.char c)

litDouble :: (CommonRenderSym r) => Double -> Value r
litDouble d = mkStateVal IC.double (D.double d)

litInt :: (CommonRenderSym r) => Integer -> Value r
litInt i = valFromData Nothing (Just i) IC.int (integer i)

litString :: (CommonRenderSym r) => String -> Value r
litString s = mkStateVal IC.string (doubleQuotedText s)

valueOf :: (CommonRenderSym r) => Variable r -> Value r
valueOf v' = mkVal (variableType v') (RC.variable v')

arg :: (CommonRenderSym r) => Value r -> Value r -> Value r
arg n args =
  let s = IC.string
  in mkVal s (R.arg n args)

argsList :: (CommonRenderSym r) => String -> Value r
argsList l = mkStateVal (IC.arrayType IC.string) (text l)

-- | First parameter is separator between name and value for named arguments, 
-- rest similar to call from RendererClasses
call :: (CommonRenderSym r) => Doc -> Maybe Library -> Maybe Doc -> MixedCall r
call sep lib o n t pas nas =
  let
    nms = map fst nas
    nargs = map snd nas
    libDoc = maybe (text n) (text . (`access` n)) lib
    obDoc = fromMaybe empty o
  in mkStateVal t $ obDoc <> libDoc <> parens (valueList pas <>
    (if null pas || null nas then empty else comma) <+> namedArgList sep
    (zip nms nargs))

funcAppMixedArgs :: (CommonRenderSym r) => MixedCall r
funcAppMixedArgs = S.call Nothing Nothing

selfFuncAppMixedArgs :: (CommonRenderSym r) => Doc -> Variable r -> MixedCall r
selfFuncAppMixedArgs d slf = S.call Nothing (Just $ RC.variable slf <> d)

newObjMixedArgs :: (CommonRenderSym r) => String -> MixedCtorCall r
newObjMixedArgs s tp vs ns = undefined{-} do
  t <- tp
  S.call Nothing Nothing (s ++ getTypeString t) (return t) vs ns-}

lambda :: (CommonRenderSym r) => ([Variable r] -> Value r -> Doc) ->
  [Variable r] -> Value r -> Value r
lambda f ps ex =
  let ft = IC.funcType (map variableType ps) (valueType ex)
  in valFromData (Just 0) Nothing ft (f ps ex)

objAccess :: (CommonRenderSym r) => Value r -> Function r -> Value r
objAccess v f = mkVal (functionType f)
  (R.objAccess (RC.value v) (RC.function f))

objMethodCall :: (CommonRenderSym r) => Label -> Type r -> Value r -> [Value r]
  -> NamedArgs r -> Value r
objMethodCall f t ob vs ns = (\o -> S.call Nothing
  (Just $ RC.value o <> dot) f t vs ns) ob

-- Functions --

func :: (CommonRenderSym r) => Label -> Type r -> [Value r] -> Function r
func l t vs =  ((`funcFromData` t) . R.func . RC.value) (funcApp l t vs)

get :: (OORenderSym r) => Value r -> Variable r -> Value r
get v vToGet = v $. S.getFunc vToGet

set :: (OORenderSym r) => Value r -> Variable r -> Value r -> Value r
set v vToSet toVal = v $. S.setFunc (valueType v) vToSet toVal

listAdd :: (OORenderSym r) => Value r -> Value r -> Value r -> Value r
listAdd v i vToAdd = v $. S.listAddFunc v (IC.intToIndex i) vToAdd

listAppend :: (OORenderSym r) => Value r -> Value r -> Value r
listAppend v vToApp = v $. S.listAppendFunc v vToApp

listAccess :: (CommonRenderSym r) => Value r -> Value r -> Value r
listAccess v i =
  let i' = IC.intToIndex i
      t  = IC.listInnerType $  valueType v
      checkType (List _) = S.listAccessFunc t i'
      checkType (Set _) = S.listAccessFunc t i'
      checkType (Array _) = (\ix -> funcFromData (brackets (RC.value ix)) t) i'
      checkType _ = error "listAccess called on non-list-type value"
      f = checkType (getType (valueType v))
  in mkVal (RC.functionType f) (RC.value v <> RC.function f)

listSet :: (CommonRenderSym r) => Value r -> Value r -> Value r -> Value r
listSet v i toVal =
  let f = S.listSetFunc v (IC.intToIndex i) toVal
  in mkVal (RC.functionType f) (RC.value v <> RC.function f)

getFunc :: (OORenderSym r) => Variable r -> Function r
getFunc vr = IG.func (getterName $ variableName vr)
  (variableType vr) []

setFunc :: (OORenderSym r) => Type r -> Variable r -> Value r -> Function r
setFunc t v toVal = (\vr -> IG.func (setterName $ variableName vr) t [toVal]) v

listAppendFunc :: (OORenderSym r) => Label -> Value r -> Function r
listAppendFunc f v = IG.func f (IC.listType $ valueType v) [v]

-- Statements --

stmt :: (CommonRenderSym r) => Statement r -> Statement r
stmt s = mkStmtNoEnd (RC.statement s <> R.getTerm (statementTerm s))

loopStmt :: (CommonRenderSym r) => Statement r -> Statement r
loopStmt = S.stmt . setEmpty

emptyStmt :: (CommonRenderSym r) => Statement r
emptyStmt = mkStmtNoEnd empty

assign :: (CommonRenderSym r) => Terminator -> Variable r -> Value r ->
  Statement r
assign t vr v = stmtFromData (R.assign vr v) t

subAssign :: (CommonRenderSym r) => Terminator -> Variable r -> Value r ->
  Statement r
subAssign t vr v = stmtFromData (R.subAssign vr v) t

increment :: (CommonRenderSym r) => Variable r -> Value r -> Statement r
increment vr v = do mkStmt $ R.addAssign vr v

objDecNew :: (OORenderSym r) => Variable r -> Scope r -> [Value r]
  -> Statement r
objDecNew v scp vs = IC.varDecDef v scp (newObj (variableType v) vs)

printList :: (CommonRenderSym r) => Integer -> Value r -> (Value r -> Statement r)
  -> (String -> Statement r) -> (String -> Statement r) -> Statement r
printList n v prFn prStrFn prLnFn = multi [prStrFn "[",
  IC.for (IC.varDecDef i IC.local (IC.litInt 0))
    (IC.valueOf i ?< (IC.listSize v #- IC.litInt 1)) (i &++)
    (bodyStatements [prFn (IC.listAccess v (IC.valueOf i)), prStrFn ", "]),
  ifNoElse [(IC.listSize v ?> IC.litInt 0, oneLiner $
    prFn (IC.listAccess v (IC.listSize v #- IC.litInt 1)))],
  prLnFn "]"]
  where l_i = "list_i" ++ show n
        i = IC.var l_i IC.int

printSet :: (CommonRenderSym r) => Integer -> Value r -> (Value r -> Statement r)
  -> (String -> Statement r) -> (String -> Statement r) -> Type r -> Statement r
printSet n v prFn prStrFn prLnFn s = multi [prStrFn "{ ",
  IC.forEach i v
    (bodyStatements [prFn (IC.valueOf i),prStrFn " "]),
  prLnFn "}"]
  where set_i = "set_i" ++ show n
        i = IC.var set_i s

printObj :: ClassName -> (String -> Statement r) -> Statement r
printObj n prLnFn = prLnFn $ "Instance of " ++ n ++ " object"

print :: (CommonRenderSym r) => Bool -> Maybe (Value r) -> Value r -> Value r ->
  Statement r
print newLn f printFn v = (print' . getType . valueType) v
  where print' (List t) = printList (getNestDegree 1 t) v prFn prStrFn
          prLnFn
        print' (Object n) = printObj n prLnFn
        print' (Set t) = printSet (getNestDegree 1 t) v prFn prStrFn prLnFn (convType t)
        print' _ = S.printSt newLn f printFn v
        prFn = maybe IC.print printFile f
        prStrFn = maybe printStr printFileStr f
        prLnFn = if newLn then maybe printStrLn printFileStrLn f else maybe
          printStr printFileStr f

closeFile :: (OORenderSym r) => Label -> Value r -> Statement r
closeFile n f = IC.valStmt $ objMethodCallNoParams IC.void f n

returnStmt :: (CommonRenderSym r) => Terminator -> Value r -> Statement r
returnStmt t v = stmtFromData (R.return' [v]) t

valStmt :: (CommonRenderSym r) => Terminator -> Value r -> Statement r
valStmt t v = stmtFromData (RC.value v) t

comment :: (CommonRenderSym r) => Doc -> Label -> Statement r
comment cs c = mkStmtNoEnd (R.comment c cs)

throw :: (CommonRenderSym r) => (Value r -> Doc) -> Terminator -> Label ->
  Statement r
throw f t l =
  let msg = IC.litString l
  in stmtFromData (f msg) t

newtype OptionalSpace = OSpace {oSpace :: Doc}

defaultOptSpace :: OptionalSpace
defaultOptSpace = OSpace {oSpace = space}

optSpaceDoc :: OptionalSpace -> Doc
optSpaceDoc OSpace {oSpace = sp} = sp

-- ControlStatements --

-- 1st parameter is a Doc function to use on the render of each condition (i.e. parens)
-- 2nd parameter is the syntax for starting a block in an if-condition
-- 3rd parameter is the keyword for an else-if statement
-- 4th parameter is the syntax for ending a block in an if-condition
-- 5th parameter is the syntax for ending an if-statement
ifCond :: (CommonRenderSym r) => (Doc -> Doc) -> Doc -> OptionalSpace -> Doc -> Doc ->
  Doc -> [(Value r, Body r)] -> Body r -> Statement r
ifCond _ _ _ _ _ _ [] _ = error "if condition created with no cases"
ifCond f ifStart os elif bEnd ifEnd (c:cs) eBody =
    let ifSect (v, b) = (\val bd -> vcat [
          ifLabel <+> f (RC.value val) <> optSpaceDoc os <> ifStart,
          indent $ RC.body bd,
          bEnd]) v b
        elseIfSect (v, b) = (\val bd -> vcat [
          elif <+> f (RC.value val) <> optSpaceDoc os <> ifStart,
          indent $ RC.body bd,
          bEnd]) v b
        elseSect = (\bd -> emptyIfEmpty (RC.body bd) (vcat [
          elseLabel <> optSpaceDoc os <> ifStart,
          indent $ RC.body bd,
          bEnd]) $+$ ifEnd) eBody
    in (mkStmtNoEnd . vcat) (ifSect c : map elseIfSect cs ++ [elseSect])


tryCatch :: (CommonRenderSym r) => (Body r -> Body r -> Doc) -> Body r ->
  Body r -> Statement r
tryCatch f tb1 tb2 = mkStmtNoEnd (f tb1 tb2)

-- Methods --

construct :: (CommonRenderSym r) => Label -> Type r
construct n = typeFromData (Object n) n empty

param :: (CommonRenderSym r) => (Variable r -> Doc) -> Variable r ->
  Parameter r
param f v' =undefined{-} do
  v <- zoom lensMStoVS v'
  let n = variableName v
  modify $ addParameter n
  modify $ useVarName n
  paramFromData v' $ f v-}

method :: (OORenderSym r) => Label -> Visibility r -> Permanence r -> Type r
  -> [Parameter r] -> Body r -> Method r
method n s p t = intMethod False n s p (mType t)

getMethod :: (OORenderSym r) => Variable r -> Method r
getMethod v = (\vr -> IG.method (getterName $ variableName
  vr) public dynamic (variableType vr) [] getBody) v
  where getBody = oneLiner $ IC.returnStmt (IC.valueOf $ IG.objVarSelf v)

setMethod :: (OORenderSym r) => Variable r -> Method r
setMethod v = (\vr -> IG.method (setterName $ variableName
  vr) public dynamic IC.void [IC.param v] setBody) v
  where setBody = oneLiner $ IG.objVarSelf v &= IC.valueOf v

initStmts :: (OORenderSym r) => Initializers r -> Body r
initStmts = bodyStatements . map (\(vr, vl) -> IG.objVarSelf vr &= vl)

function :: (OORenderSym r) => Label -> Visibility r -> Type r ->
  [Parameter r] -> Body r -> Method r
function n s t = S.intFunc False n s static (mType t)

docFuncRepr :: (CommonRenderSym r) => FuncDocRenderer -> String -> [String] ->
  [String] -> Method r -> Method r
docFuncRepr f desc pComms rComms = undefined{-} commentedFunc (docComment $ onStateValue
  (\ps -> f desc (zip ps pComms) rComms) getParameters)-}

docFunc :: (CommonRenderSym r) => FuncDocRenderer -> String -> [String] ->
  Maybe String -> Method r -> Method r
docFunc f desc pComms rComm = docFuncRepr f desc pComms (maybeToList rComm)

-- Classes --

buildClass :: (OORenderSym r) =>  Maybe Label -> [StateVar r] ->
  [Method r] -> [Method r] -> Class r
buildClass p stVars constructors methods = undefined{-} do
  n <- zoom lensCStoFS getModuleName
  S.intClass n public (inherit p) stVars constructors methods-}

implementingClass :: (OORenderSym r) => Label -> [Label] -> [StateVar r] ->
  [Method r] -> [Method r] -> Class r
implementingClass n is = S.intClass n public (implements is)

docClass :: (OORenderSym r) => ClassDocRenderer -> String -> Class r -> Class r
docClass cdr d = undefined{-} S.commentedClass (docComment $ toState $ cdr d)-}

commentedClass :: (OORenderSym r) => CS (BlockComment r) -> Class r
  -> Class r
commentedClass = undefined{-} on2StateValues (\cmt cs -> toCode $ R.commentedItem
  (RC.blockComment' cmt) (RC.class' cs))-}

-- Modules --

modFromData :: Label -> (Doc -> Module r) -> FS Doc -> Module r
modFromData n f d = undefined{-} modify (setModuleName n) >> onStateValue f d-}

-- Files --

fileDoc :: (OORenderSym r) => String -> (Module r -> Block r) ->
  Block r -> Module r -> File r
fileDoc ext topb botb mdl = undefined{-} do
  m <- mdl
  nm <- getModuleName
  let fp = addExt ext nm
      updm = updateModuleDoc (\d -> emptyIfEmpty d
        (R.file (RC.block $ topb m) d (RC.block botb))) m
  S.fileFromData fp (toState updm)-}

-- | Generates a file for a documented module.
--   mdr is a function that takes description, author, and module name and 
--                                                     returns a doc comment
--   e is the file extension
--   d is the description (I think)
--   a is a list of authors
--   dt is the date
--   fl is the file
docMod :: (OORenderSym r) => ModuleDocRenderer -> String -> String ->
  [String] -> String -> File r -> File r
docMod mdr e d a dt fl = undefined{-} commentedMod fl (docComment $ mdr d a dt . addExt e
  <$> getModuleName)-}

fileFromData 
  :: (OORenderSym r, Module r ~ State s mod, File r ~ State s file) 
  => (FilePath -> Module r -> File r)
  -> FilePath -> Module r -> File r
fileFromData f fpath mdl' = undefined{-do
  -- Add this file to list of files as long as it is not empty
  mdl <- mdl'
  modify \s -> 
    if __ then -- (\s -> if isEmpty (RC.module' mdl)
      s
    else 
      over lensFStoGS (addFile (s ^. currFileType) fpath) $
      -- If this is the main source file, set it as the main module in the state
      if s ^. currMain && isSource (s ^. currFileType) then 
        over lensFStoGS (setMainMod fpath) s
      else 
        s
  return $ f fpath mdl-}

-- Helper functions

setEmpty :: (CommonRenderSym r) => Statement r -> Statement r
setEmpty = mkStmtNoEnd . RC.statement
