{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}

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
import Drasil.Shared.InterfaceCommon (Label, Library, MSBody, MSBlock, VSFunction,
  VSType, SVariable, SValue, MSStatement, MSParameter, SMethod, NamedArgs,
  MixedCall, MixedCtorCall, BodySym(Body), bodyStatements, oneLiner,
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
  IOStatement(print), ControlStatement(returnStmt, for, forEach), ParameterSym(param),
  List(intToIndex), ScopeSym(local))
import Drasil.GOOL.InterfaceGOOL (SFile, FSModule, SClass, Initializers,
  CSStateVar, FileSym(File), ModuleSym(Module), newObj, objMethodCallNoParams,
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
  ScopeData, sd)
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
  mkStateVal, mkVal, mkStateVar, mkVar, mkStaticVar, VSOp, unOpPrec, 
  compEqualPrec, compPrec, addPrec, multPrec)
import Drasil.Shared.State (FS, CS, MS, lensFStoGS, lensMStoVS, lensCStoFS, 
  currMain, currFileType, addFile, setMainMod, setModuleName, getModuleName,
  addParameter, getParameters, useVarName)

import Prelude hiding (print,sin,cos,tan,(<>))
import Data.Maybe (fromMaybe, maybeToList)
import Control.Monad.State (modify)
import Control.Lens ((^.), over)
import Control.Lens.Zoom (zoom)
import Text.PrettyPrint.HughesPJ (Doc, text, empty, render, (<>), (<+>), ($+$),
  parens, brackets, integer, vcat, comma, isEmpty, space)
import qualified Text.PrettyPrint.HughesPJ as D (char, double)

-- Bodies --

multiBody :: (CommonRenderSym r, Monad r) => [MSBody r] -> MS (r Doc)
multiBody bs = onStateList (toCode . vibcat) $ map (onStateValue RC.body) bs

-- Blocks --

block :: (CommonRenderSym r, Monad r) => [MSStatement r] -> MS (r Doc)
block sts = onStateList (toCode . R.block . map RC.statement) (map S.stmt sts)

multiBlock :: (CommonRenderSym r, Monad r) => [MSBlock r] -> MS (r Doc)
multiBlock bs = onStateList (toCode . vibcat) $ map (onStateValue RC.block) bs

-- Types --

listInnerType :: (OORenderSym r) => VSType r -> VSType r
listInnerType t = t >>= (convTypeOO . getInnerType . getType)

obj :: (CommonRenderSym r) => ClassName -> VSType r
obj n = typeFromData (Object n) n (text n)

-- Unary Operators --

negateOp :: (Monad r) => VSOp r
negateOp = unOpPrec "-"

csc :: (CommonRenderSym r) => SValue r -> SValue r
csc v = valOfOne (fmap valueType v) #/ sin v

sec :: (CommonRenderSym r) => SValue r -> SValue r
sec v = valOfOne (fmap valueType v) #/ cos v

cot :: (CommonRenderSym r) => SValue r -> SValue r
cot v = valOfOne (fmap valueType v) #/ tan v

valOfOne :: (CommonRenderSym r) => VSType r -> SValue r
valOfOne t = t >>= (getVal . getType)
  where getVal Float = IC.litFloat 1.0
        getVal _ = IC.litDouble 1.0

-- Binary Operators --

smartAdd :: (CommonRenderSym r) => SValue r -> SValue r -> SValue r
smartAdd v1 v2 = do
  v1' <- v1
  v2' <- v2
  case (RC.valueInt v1', RC.valueInt v2') of
    (Just i1, Just i2) -> litInt (i1 + i2)
    _                  -> v1 #+ v2

smartSub :: (CommonRenderSym r) => SValue r -> SValue r -> SValue r
smartSub v1 v2 = do
  v1' <- v1
  v2' <- v2
  case (RC.valueInt v1', RC.valueInt v2') of
    (Just i1, Just i2) -> litInt (i1 - i2)
    _                  -> v1 #- v2

equalOp :: (Monad r) => VSOp r
equalOp = compEqualPrec "=="

notEqualOp :: (Monad r) => VSOp r
notEqualOp = compEqualPrec "!="

greaterOp :: (Monad r) => VSOp r
greaterOp = compPrec ">"

greaterEqualOp :: (Monad r) => VSOp r
greaterEqualOp = compPrec ">="

lessOp :: (Monad r) => VSOp r
lessOp = compPrec "<"

lessEqualOp :: (Monad r) => VSOp r
lessEqualOp = compPrec "<="

plusOp :: (Monad r) => VSOp r
plusOp = addPrec "+"

minusOp :: (Monad r) => VSOp r
minusOp = addPrec "-"

multOp :: (Monad r) => VSOp r
multOp = multPrec "*"

divideOp :: (Monad r) => VSOp r
divideOp = multPrec "/"

moduloOp :: (Monad r) => VSOp r
moduloOp = multPrec "%"

-- Variables --

var :: (CommonRenderSym r) => Label -> VSType r -> SVariable r
var n t = mkStateVar n t (R.var n)

staticVar :: (CommonRenderSym r) => Label -> VSType r -> SVariable r
staticVar n t = mkStaticVar n t (R.var n)

-- | To be used in classVar implementations. Throws an error if the variable is 
-- not static since classVar is for accessing static variables from a class
classVarCheckStatic :: (CommonRenderSym r) => r (Variable r) -> r (Variable r)
classVarCheckStatic v = classVarCS (variableBind v)
  where classVarCS Dynamic = error
          "classVar can only be used to access static variables"
        classVarCS Static = v

objVar :: (CommonRenderSym r) => SVariable r -> SVariable r -> SVariable r
objVar o' v' = do
  o <- o'
  v <- v'
  let objVar' Static = error 
        "Cannot access static variables through an object, use classVar instead"
      objVar' Dynamic = mkVar (variableName o `access` variableName v) 
        (variableType v) (R.objVar (RC.variable o) (RC.variable v))
  objVar' (variableBind v)

arrayElem :: (OORenderSym r) => SValue r -> SVariable r -> SVariable r
arrayElem i' v' = do
  i <- IC.intToIndex i'
  v <- v'
  let vName = variableName v ++ "[" ++ render (RC.value i) ++ "]"
      vType = listInnerType $ return $ variableType v
      vRender = RC.variable v <> brackets (RC.value i)
  mkStateVar vName vType vRender

-- Scope --
local :: (Monad r) => r ScopeData
local = toCode $ sd Local

-- Values --

litChar :: (CommonRenderSym r) => (Doc -> Doc) -> Char -> SValue r
litChar f c = mkStateVal IC.char (f $ if c == '\n' then text "\\n" else D.char c)

litDouble :: (CommonRenderSym r) => Double -> SValue r
litDouble d = mkStateVal IC.double (D.double d)

litInt :: (CommonRenderSym r) => Integer -> SValue r
litInt i = valFromData Nothing (Just i) IC.int (integer i)

litString :: (CommonRenderSym r) => String -> SValue r
litString s = mkStateVal IC.string (doubleQuotedText s)

valueOf :: (CommonRenderSym r) => SVariable r -> SValue r
valueOf v' = do 
  v <- v'
  mkVal (variableType v) (RC.variable v)

arg :: (CommonRenderSym r) => SValue r -> SValue r -> SValue r
arg n' args' = do 
  n <- n'
  args <- args'
  s <- IC.string
  mkVal s (R.arg n args)

argsList :: (CommonRenderSym r) => String -> SValue r
argsList l = mkStateVal (IC.arrayType IC.string) (text l)

-- | First parameter is separator between name and value for named arguments, 
-- rest similar to call from RendererClasses
call :: (CommonRenderSym r) => Doc -> Maybe Library -> Maybe Doc -> MixedCall r
call sep lib o n t pas nas = do
  pargs <- sequence pas
  nms <- mapM fst nas
  nargs <- mapM snd nas
  let libDoc = maybe (text n) (text . (`access` n)) lib
      obDoc = fromMaybe empty o
  mkStateVal t $ obDoc <> libDoc <> parens (valueList pargs <> 
    (if null pas || null nas then empty else comma) <+> namedArgList sep 
    (zip nms nargs))

funcAppMixedArgs :: (CommonRenderSym r) => MixedCall r
funcAppMixedArgs = S.call Nothing Nothing

selfFuncAppMixedArgs :: (CommonRenderSym r) => Doc -> SVariable r -> MixedCall r
selfFuncAppMixedArgs d slf n t vs ns = do
  s <- slf 
  S.call Nothing (Just $ RC.variable s <> d) n t vs ns

newObjMixedArgs :: (CommonRenderSym r) => String -> MixedCtorCall r
newObjMixedArgs s tp vs ns = do
  t <- tp 
  S.call Nothing Nothing (s ++ getTypeString t) (return t) vs ns

lambda :: (CommonRenderSym r) => ([r (Variable r)] -> r (Value r) -> Doc) -> 
  [SVariable r] -> SValue r -> SValue r
lambda f ps' ex' = do
  ps <- sequence ps'
  ex <- ex'
  let ft = IC.funcType (map (return . variableType) ps) (return $ valueType ex)
  valFromData (Just 0) Nothing ft (f ps ex)

objAccess :: (CommonRenderSym r) => SValue r -> VSFunction r -> SValue r
objAccess = on2StateWrapped (\v f-> mkVal (functionType f) 
  (R.objAccess (RC.value v) (RC.function f)))

objMethodCall :: (CommonRenderSym r) => Label -> VSType r -> SValue r -> [SValue r] 
  -> NamedArgs r -> SValue r
objMethodCall f t ob vs ns = ob >>= (\o -> S.call Nothing 
  (Just $ RC.value o <> dot) f t vs ns)

-- Functions --

func :: (CommonRenderSym r) => Label -> VSType r -> [SValue r] -> VSFunction r
func l t vs = funcApp l t vs >>= ((`funcFromData` t) . R.func . RC.value)

get :: (OORenderSym r) => SValue r -> SVariable r -> SValue r
get v vToGet = v $. S.getFunc vToGet

set :: (OORenderSym r) => SValue r -> SVariable r -> SValue r -> SValue r
set v vToSet toVal = v $. S.setFunc (onStateValue valueType v) vToSet toVal

listAdd :: (OORenderSym r) => SValue r -> SValue r -> SValue r -> SValue r
listAdd v i vToAdd = v $. S.listAddFunc v (IC.intToIndex i) vToAdd

listAppend :: (OORenderSym r) => SValue r -> SValue r -> SValue r
listAppend v vToApp = v $. S.listAppendFunc v vToApp

listAccess :: (CommonRenderSym r) => SValue r -> SValue r -> SValue r
listAccess v i = do
  v' <- v
  let i' = IC.intToIndex i
      t  = IC.listInnerType $ return $ valueType v'
      checkType (List _) = S.listAccessFunc t i'
      checkType (Set _) = S.listAccessFunc t i'
      checkType (Array _) = i' >>= 
                              (\ix -> funcFromData (brackets (RC.value ix)) t)
      checkType _ = error "listAccess called on non-list-type value"
  f <- checkType (getType (valueType v'))
  mkVal (RC.functionType f) (RC.value v' <> RC.function f)

listSet :: (CommonRenderSym r) => SValue r -> SValue r -> SValue r -> SValue r
listSet v i toVal = do
  v' <- v
  f <- S.listSetFunc v (IC.intToIndex i) toVal
  mkVal (RC.functionType f) (RC.value v' <> RC.function f)

getFunc :: (OORenderSym r) => SVariable r -> VSFunction r
getFunc v = v >>= (\vr -> IG.func (getterName $ variableName vr) 
  (toState $ variableType vr) [])

setFunc :: (OORenderSym r) => VSType r -> SVariable r -> SValue r -> VSFunction r
setFunc t v toVal = v >>= (\vr -> IG.func (setterName $ variableName vr) t 
  [toVal])

listAppendFunc :: (OORenderSym r) => Label -> SValue r -> VSFunction r
listAppendFunc f v = IG.func f (IC.listType $ onStateValue valueType v) [v]

-- Statements --

stmt :: (CommonRenderSym r) => MSStatement r -> MSStatement r
stmt s' = do 
  s <- s'
  mkStmtNoEnd (RC.statement s <> R.getTerm (statementTerm s))

loopStmt :: (CommonRenderSym r) => MSStatement r -> MSStatement r
loopStmt = S.stmt . setEmpty

emptyStmt :: (CommonRenderSym r) => MSStatement r
emptyStmt = mkStmtNoEnd empty

assign :: (CommonRenderSym r) => Terminator -> SVariable r -> SValue r -> 
  MSStatement r
assign t vr' v' = do 
  vr <- zoom lensMStoVS vr'
  v <- zoom lensMStoVS v'
  stmtFromData (R.assign vr v) t

subAssign :: (CommonRenderSym r) => Terminator -> SVariable r -> SValue r -> 
  MSStatement r
subAssign t vr' v' = do 
  vr <- zoom lensMStoVS vr'
  v <- zoom lensMStoVS v'
  stmtFromData (R.subAssign vr v) t

increment :: (CommonRenderSym r) => SVariable r -> SValue r -> MSStatement r
increment vr' v'= do 
  vr <- zoom lensMStoVS vr'
  v <- zoom lensMStoVS v'
  mkStmt $ R.addAssign vr v

objDecNew :: (OORenderSym r) => SVariable r -> r (Scope r) -> [SValue r]
  -> MSStatement r
objDecNew v scp vs = IC.varDecDef v scp (newObj (onStateValue variableType v) vs)

printList :: (CommonRenderSym r) => Integer -> SValue r -> (SValue r -> MSStatement r)
  -> (String -> MSStatement r) -> (String -> MSStatement r) -> MSStatement r
printList n v prFn prStrFn prLnFn = multi [prStrFn "[", 
  IC.for (IC.varDecDef i IC.local (IC.litInt 0)) 
    (IC.valueOf i ?< (IC.listSize v #- IC.litInt 1)) (i &++) 
    (bodyStatements [prFn (IC.listAccess v (IC.valueOf i)), prStrFn ", "]), 
  ifNoElse [(IC.listSize v ?> IC.litInt 0, oneLiner $
    prFn (IC.listAccess v (IC.listSize v #- IC.litInt 1)))], 
  prLnFn "]"]
  where l_i = "list_i" ++ show n
        i = IC.var l_i IC.int

printSet :: (CommonRenderSym r) => Integer -> SValue r -> (SValue r -> MSStatement r)
  -> (String -> MSStatement r) -> (String -> MSStatement r) -> VSType r -> MSStatement r
printSet n v prFn prStrFn prLnFn s = multi [prStrFn "{ ", 
  IC.forEach i v
    (bodyStatements [prFn (IC.valueOf i),prStrFn " "]),
  prLnFn "}"]
  where set_i = "set_i" ++ show n
        i = IC.var set_i s

printObj :: ClassName -> (String -> MSStatement r) -> MSStatement r
printObj n prLnFn = prLnFn $ "Instance of " ++ n ++ " object"

print :: (CommonRenderSym r) => Bool -> Maybe (SValue r) -> SValue r -> SValue r ->
  MSStatement r
print newLn f printFn v = zoom lensMStoVS v >>= print' . getType . valueType
  where print' (List t) = printList (getNestDegree 1 t) v prFn prStrFn 
          prLnFn
        print' (Object n) = printObj n prLnFn
        print' (Set t) = printSet (getNestDegree 1 t) v prFn prStrFn prLnFn (convType t)
        print' _ = S.printSt newLn f printFn v
        prFn = maybe IC.print printFile f
        prStrFn = maybe printStr printFileStr f
        prLnFn = if newLn then maybe printStrLn printFileStrLn f else maybe 
          printStr printFileStr f 

closeFile :: (OORenderSym r) => Label -> SValue r -> MSStatement r
closeFile n f = IC.valStmt $ objMethodCallNoParams IC.void f n

returnStmt :: (CommonRenderSym r) => Terminator -> SValue r -> MSStatement r
returnStmt t v' = do 
  v <- zoom lensMStoVS v'
  stmtFromData (R.return' [v]) t

valStmt :: (CommonRenderSym r) => Terminator -> SValue r -> MSStatement r
valStmt t v' = do 
  v <- zoom lensMStoVS v'
  stmtFromData (RC.value v) t

comment :: (CommonRenderSym r) => Doc -> Label -> MSStatement r
comment cs c = mkStmtNoEnd (R.comment c cs)

throw :: (CommonRenderSym r) => (r (Value r) -> Doc) -> Terminator -> Label -> 
  MSStatement r
throw f t l = do 
  msg <- zoom lensMStoVS (IC.litString l)
  stmtFromData (f msg) t

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
  Doc -> [(SValue r, MSBody r)] -> MSBody r -> MSStatement r
ifCond _ _ _ _ _ _ [] _ = error "if condition created with no cases"
ifCond f ifStart os elif bEnd ifEnd (c:cs) eBody =
    let ifSect (v, b) = on2StateValues (\val bd -> vcat [
          ifLabel <+> f (RC.value val) <> optSpaceDoc os <> ifStart,
          indent $ RC.body bd,
          bEnd]) (zoom lensMStoVS v) b
        elseIfSect (v, b) = on2StateValues (\val bd -> vcat [
          elif <+> f (RC.value val) <> optSpaceDoc os <> ifStart,
          indent $ RC.body bd,
          bEnd]) (zoom lensMStoVS v) b
        elseSect = onStateValue (\bd -> emptyIfEmpty (RC.body bd) (vcat [
          elseLabel <> optSpaceDoc os <> ifStart,
          indent $ RC.body bd,
          bEnd]) $+$ ifEnd) eBody
    in sequence (ifSect c : map elseIfSect cs ++ [elseSect]) 
      >>= (mkStmtNoEnd . vcat)

tryCatch :: (CommonRenderSym r) => (r (Body r) -> r (Body r) -> Doc) -> MSBody r -> 
  MSBody r -> MSStatement r
tryCatch f = on2StateWrapped (\tb1 tb2 -> mkStmtNoEnd (f tb1 tb2))

-- Methods --

construct :: (CommonRenderSym r) => Label -> MS (r (Type r))
construct n = zoom lensMStoVS $ typeFromData (Object n) n empty

param :: (CommonRenderSym r) => (r (Variable r) -> Doc) -> SVariable r -> 
  MSParameter r
param f v' = do
  v <- zoom lensMStoVS v'
  let n = variableName v
  modify $ addParameter n
  modify $ useVarName n
  paramFromData v' $ f v

method :: (OORenderSym r) => Label -> r (Visibility r) -> r (Permanence r) -> VSType r 
  -> [MSParameter r] -> MSBody r -> SMethod r
method n s p t = intMethod False n s p (mType t)

getMethod :: (OORenderSym r) => SVariable r -> SMethod r
getMethod v = zoom lensMStoVS v >>= (\vr -> IG.method (getterName $ variableName 
  vr) public dynamic (toState $ variableType vr) [] getBody)
  where getBody = oneLiner $ IC.returnStmt (IC.valueOf $ IG.objVarSelf v)

setMethod :: (OORenderSym r) => SVariable r -> SMethod r
setMethod v = zoom lensMStoVS v >>= (\vr -> IG.method (setterName $ variableName 
  vr) public dynamic IC.void [IC.param v] setBody)
  where setBody = oneLiner $ IG.objVarSelf v &= IC.valueOf v

initStmts :: (OORenderSym r) => Initializers r -> MSBody r
initStmts = bodyStatements . map (\(vr, vl) -> IG.objVarSelf vr &= vl)

function :: (OORenderSym r) => Label -> r (Visibility r) -> VSType r -> 
  [MSParameter r] -> MSBody r -> SMethod r
function n s t = S.intFunc False n s static (mType t)
  
docFuncRepr :: (CommonRenderSym r) => FuncDocRenderer -> String -> [String] -> 
  [String] -> SMethod r -> SMethod r
docFuncRepr f desc pComms rComms = commentedFunc (docComment $ onStateValue 
  (\ps -> f desc (zip ps pComms) rComms) getParameters)

docFunc :: (CommonRenderSym r) => FuncDocRenderer -> String -> [String] -> 
  Maybe String -> SMethod r -> SMethod r
docFunc f desc pComms rComm = docFuncRepr f desc pComms (maybeToList rComm)

-- Classes --

buildClass :: (OORenderSym r) =>  Maybe Label -> [CSStateVar r] -> 
  [SMethod r] -> [SMethod r] -> SClass r
buildClass p stVars constructors methods = do 
  n <- zoom lensCStoFS getModuleName
  S.intClass n public (inherit p) stVars constructors methods

implementingClass :: (OORenderSym r) => Label -> [Label] -> [CSStateVar r] -> 
  [SMethod r] -> [SMethod r] -> SClass r
implementingClass n is = S.intClass n public (implements is)

docClass :: (OORenderSym r) => ClassDocRenderer -> String -> SClass r -> SClass r
docClass cdr d = S.commentedClass (docComment $ toState $ cdr d)

commentedClass :: (OORenderSym r, Monad r) => CS (r (BlockComment r)) -> SClass r 
  -> CS (r Doc)
commentedClass = on2StateValues (\cmt cs -> toCode $ R.commentedItem 
  (RC.blockComment' cmt) (RC.class' cs))

-- Modules --

modFromData :: Label -> (Doc -> r (Module r)) -> FS Doc -> FSModule r
modFromData n f d = modify (setModuleName n) >> onStateValue f d

-- Files --

fileDoc :: (OORenderSym r) => String -> (r (Module r) -> r (Block r)) -> 
  r (Block r) -> FSModule r -> SFile r
fileDoc ext topb botb mdl = do
  m <- mdl
  nm <- getModuleName
  let fp = addExt ext nm
      updm = updateModuleDoc (\d -> emptyIfEmpty d 
        (R.file (RC.block $ topb m) d (RC.block botb))) m
  S.fileFromData fp (toState updm)

-- | Generates a file for a documented module.
--   mdr is a function that takes description, author, and module name and 
--                                                     returns a doc comment
--   e is the file extension
--   d is the description (I think)
--   a is a list of authors
--   dt is the date
--   fl is the file
docMod :: (OORenderSym r) => ModuleDocRenderer -> String -> String -> 
  [String] -> String -> SFile r -> SFile r
docMod mdr e d a dt fl = commentedMod fl (docComment $ mdr d a dt . addExt e 
  <$> getModuleName)

fileFromData :: (OORenderSym r) => (FilePath -> r (Module r) -> r (File r)) 
  -> FilePath -> FSModule r -> SFile r
fileFromData f fpath mdl' = do
  -- Add this file to list of files as long as it is not empty
  mdl <- mdl'
  modify (\s -> if isEmpty (RC.module' mdl) 
    then s
    else over lensFStoGS (addFile (s ^. currFileType) fpath) $ 
      -- If this is the main source file, set it as the main module in the state
      if s ^. currMain && isSource (s ^. currFileType) 
        then over lensFStoGS (setMainMod fpath) s
        else s)
  return $ f fpath mdl

-- Helper functions

setEmpty :: (CommonRenderSym r) => MSStatement r -> MSStatement r
setEmpty s' = s' >>= mkStmtNoEnd . RC.statement
