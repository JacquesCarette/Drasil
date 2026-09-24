{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- | Implementations defined here are valid for any language renderer.
module Drasil.Shared.LanguageRenderer.LanguagePolymorphic (fileFromData,
  multiBody, block, multiBlock, obj, negateOp, csc, sec, cot, equalOp,
  notEqualOp, greaterOp, greaterEqualOp, lessOp, lessEqualOp, plusOp, minusOp,
  multOp, divideOp, moduloOp, var, classVar, instanceVarAccess,
  classVarAccessCheck, arrayElem, local, litChar, litDouble, litInt, litString,
  valueOf, arg, argsList, call, funcAppMixedArgs, newObjMixedArgs, lambda,
  objAccess, objMethodCall, func, get, set, listAccess, getFunc, setFunc, stmt,
  loopStmt, emptyStmt, assign, subAssign, objDecNew, print, closeFile,
  returnStmt, valStmt, comment, throw, ifCond, tryCatch, construct, param,
  method, getMethod, setMethod, initStmts, function, docFuncRepr, docFunc,
  buildClass, implementingClass, docClass, commentedClass, modFromData, fileDoc,
  docMod, OptionalSpace(..), defaultOptSpace, smartAdd, smartSub
) where

import Drasil.FileHandling.Legacy (indent)

import Drasil.Shared.CodeType (CodeType(..), ClassName)
import Drasil.Shared.InterfaceCommon (UnRepr(..), Label, Library, Variable,
  SVariable, NamedArgs, MixedCall, MixedCtorCall, bodyStatements, oneLiner,
  VisibilitySym(..), VariableElim(variableName, variableType),
  ValueSym(valueType), NumericExpression((#+), (#-), (#/), sin, cos, tan),
  Comparison(..), funcApp, MultiStatement(multi), AssignStatement((&++)), (&=),
  TypeElim(..), PrintConsole(printStr, printStrLn),
  PrintFile(printFile, printFileStr, printFileStrLn), ifNoElse, convType,
  VSBinder, BinderElim(..), getCodeType, getTypeString, ValueExpression,
  VariableValue, BlockSym, BodySym)
import qualified Drasil.Shared.InterfaceCommon as IC
import Drasil.GOOL.InterfaceGOOL (Class, Initializers, CSStateVar, newObj,
  objMethodCallNoParams, ($.), AttachmentSym(..), SelfSym, OOVariableSym)
import qualified Drasil.GOOL.InterfaceGOOL as IG
import Drasil.Shared.RendererClassesCommon (InternalVarElim(variableBind),
  RenderValue(valFromData), RenderFunction(funcFromData),
  FunctionElim(functionType), RenderStatement(stmtFromData),
  StatementElim(statementTerm), MethodTypeSym(mType), RenderParam(paramFromData),
  RenderMethod(commentedFunc), BlockCommentSym(..), ValueElim (value),
  RenderVariable)
import qualified Drasil.Shared.RendererClassesCommon as RC
import Drasil.GOOL.RendererClassesOO (OORenderSym, RenderFile(commentedMod),
  OORenderMethod(intMethod), RenderClass(inherit, implements),
  RenderMod(updateModuleDoc))
import qualified Drasil.GOOL.RendererClassesOO as RO
import Drasil.Shared.AST (AttachmentTag(..), Terminator(..), isSource,
  ScopeTag(Local), ScopeData, sd, TypeData(..), BinderD, ParamData, FuncData)
import Drasil.Shared.Helpers (doubleQuotedText, vibcat, emptyIfEmpty, toCode,
  toState, onStateValue, on2StateValues, onStateList, getNestDegree,
  on2StateWrapped)
import Drasil.Shared.LanguageRenderer (dot, ifLabel, elseLabel, access, addExt,
  FuncDocRenderer, ClassDocRenderer, ModuleDocRenderer, getterName, setterName,
  valueList, namedArgList)
import qualified Drasil.Shared.LanguageRenderer as R
import Drasil.Shared.LanguageRenderer.Constructors (mkStmtNoEnd, mkStateVal,
  mkVal, mkStateVar, mkVar, mkClassVar, VSOp, unOpPrec, compEqualPrec, compPrec,
  addPrec, multPrec, typeFromData)
import Drasil.Shared.State (VS, FS, CS, MS, lensFStoGS, lensMStoVS, lensCStoFS,
  currMain, currFileType, addFile, setMainMod, setModuleName, getModuleName,
  addParameter, getParameters, useVarName)

import Prelude hiding (print,sin,cos,tan,(<>))
import Data.Maybe (fromMaybe, maybeToList)
import Control.Monad.State (modify)
import Control.Lens ((^.), over)
import Control.Lens.Zoom (zoom)
import Text.PrettyPrint.HughesPJ (Doc, text, empty, render, (<>), (<+>), ($+$),
  parens, brackets, integer, vcat, comma, isEmpty, space)
import qualified Text.PrettyPrint.HughesPJ as D

-- Bodies --

multiBody :: (RC.BodyElim r bod, Monad r) => [MS (r bod)] -> MS (r Doc)
multiBody bs = onStateList (toCode . vibcat) $ map (onStateValue RC.body) bs

-- Blocks --

block
  :: (Monad r, RenderStatement r stmt, StatementElim r stmt)
  => [MS (r stmt)] -> MS (r Doc)
block sts = onStateList (toCode . R.block . map RC.statement) (map RC.stmt sts)

multiBlock :: (RC.BlockElim r block, Monad r) => [MS (r block)] -> MS (r Doc)
multiBlock bs = onStateList (toCode . vibcat) $ map (onStateValue RC.block) bs

-- Types --

obj :: (Monad r) => ClassName -> VS (r TypeData)
obj n = typeFromData (Object n) n (text n)

-- Unary Operators --

negateOp :: (Monad r) => VSOp r
negateOp = unOpPrec "-"

csc
  ::
    ( ValueSym r typ val
    , IC.Literal r typ val
    , IC.NumericExpression r val
    , TypeElim r typ
    )
  => VS (r val) -> VS (r val)
csc v = valOfOne (fmap valueType v) #/ sin v

sec
  ::
    ( ValueSym r typ val
    , IC.Literal r typ val
    , IC.NumericExpression r val
    , TypeElim r typ
    )
  => VS (r val) -> VS (r val)
sec v = valOfOne (fmap valueType v) #/ cos v

cot
  ::
    ( ValueSym r typ val
    , IC.Literal r typ val
    , IC.NumericExpression r val
    , TypeElim r typ
    )
  => VS (r val) -> VS (r val)
cot v = valOfOne (fmap valueType v) #/ tan v

valOfOne :: (IC.Literal r typ val, TypeElim r typ) => VS (r typ) -> VS (r val)
valOfOne t = t >>= (getVal . getCodeType)
  where getVal Float = IC.litFloat 1.0
        getVal _ = IC.litDouble 1.0

-- Binary Operators --

smartAdd
  ::
    ( IC.TypeSym r typ
    , NumericExpression r val
    , RenderValue r typ val
    , ValueElim r val
    )
  => VS (r val) -> VS (r val) -> VS (r val)
smartAdd v1 v2 = do
  v1' <- v1
  v2' <- v2
  case (RC.valueInt v1', RC.valueInt v2') of
    (Just i1, Just i2) -> litInt (i1 + i2)
    (_, Just i2) | i2 < 0 -> v1 #- litInt (negate i2)
    _                  -> v1 #+ v2

smartSub
  ::
    ( IC.TypeSym r typ
    , IC.NumericExpression r val
    , RenderValue r typ val
    , ValueElim r val
    )
  => VS (r val) -> VS (r val) -> VS (r val)
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

var :: (RenderVariable r typ) => Label -> VS (r typ) -> SVariable r
var n t = mkStateVar n t (R.var n)

classVar :: (RenderVariable r typ) => Label -> VS (r typ) -> SVariable r
classVar n t = mkClassVar n t (R.var n)

-- | To be used in classVarAccess implementations. Throws an error if the variable is
-- not class-level since classVarAccess is for accessing class-level variables from a class
classVarAccessCheck :: (InternalVarElim r) => r Variable -> r Variable
classVarAccessCheck v = classVarCS (variableBind v)
  where classVarCS InstanceLevel = error
          "classVarAccess can only be used to access class-level variables"
        classVarCS ClassLevel = v

instanceVarAccess
  ::
    ( InternalVarElim r
    , RenderVariable r typ
    , ValueElim r val
    , VariableElim r typ
    )
  => VS (r val) -> SVariable r -> SVariable r
instanceVarAccess o' v' = do
  o <- o'
  v <- v'
  let instanceVarAccess' ClassLevel = error
        "Cannot access class-level variables through an object, use classVarAccess instead"
      instanceVarAccess' InstanceLevel = mkVar (render (value o) `access` variableName v)
        (variableType v) (R.instanceVarAccess (RC.value o) (RC.variable v))
  instanceVarAccess' (variableBind v)

arrayElem
  ::
    ( IC.TypeSym r typ
    , ValueSym r typ val
    , IC.IndexTranslator r val
    , RenderVariable r typ
    , ValueElim r val
    )
  => VS (r val) -> VS (r val) -> SVariable r
arrayElem arr' i' = do
  i <- IC.intToIndex i'
  arr <- arr'
  let vName = render (RC.value arr) ++ "[" ++ render (RC.value i) ++ "]"
      vType = IC.innerType $ pure $ valueType arr
      vRender = RC.value arr <> brackets (RC.value i)
  mkStateVar vName vType vRender

-- Scope --
local :: (Monad r) => r ScopeData
local = toCode $ sd Local

-- Values --

litChar
  :: (RenderValue r typ val, IC.TypeSym r typ)
  => (Doc -> Doc) -> Char -> VS (r val)
litChar f c = mkStateVal IC.char (f $ if c == '\n' then text "\\n" else D.char c)

litDouble :: (RenderValue r typ val, IC.TypeSym r typ) => Double -> VS (r val)
litDouble d = mkStateVal IC.double (D.double d)

litInt :: (RenderValue r typ val, IC.TypeSym r typ) => Integer -> VS (r val)
litInt i = valFromData Nothing (Just i) IC.int (integer i)

litString :: (RenderValue r typ val, IC.TypeSym r typ) => String -> VS (r val)
litString s = mkStateVal IC.string (doubleQuotedText s)

valueOf
  :: (InternalVarElim r, RenderValue r typ val, VariableElim r typ)
  => SVariable r -> VS (r val)
valueOf v' = do
  v <- v'
  mkVal (variableType v) (RC.variable v)

arg
  :: (RenderValue r typ val, IC.TypeSym r typ, ValueElim r val)
  => VS (r val) -> VS (r val) -> VS (r val)
arg n' args' = do
  n <- n'
  args <- args'
  s <- IC.string
  mkVal s (R.arg n args)

argsList :: (RenderValue r typ val, IC.TypeSym r typ) => String -> VS (r val)
argsList l = mkStateVal (IC.arrayType IC.string) (text l)

-- | First parameter is separator between name and value for named arguments,
-- rest similar to call from RendererClasses
call
  :: (InternalVarElim r, RenderValue r typ val, ValueElim r val)
  => Doc -> Maybe Library -> Maybe Doc -> MixedCall r typ val
call sep lib o n t pas nas = do
  pargs <- sequence pas
  nms <- mapM fst nas
  nargs <- mapM snd nas
  let libDoc = maybe (text n) (text . (`access` n)) lib
      obDoc = fromMaybe empty o
  mkStateVal t $ obDoc <> libDoc <> parens (valueList pargs <>
    (if null pas || null nas then empty else comma) <+> namedArgList sep
    (zip nms nargs))

funcAppMixedArgs :: (RenderValue r typ val) => MixedCall r typ val
funcAppMixedArgs = RC.call Nothing Nothing

newObjMixedArgs
  :: (RenderValue r TypeData val, UnRepr r TypeData)
  => String -> MixedCtorCall r TypeData val
newObjMixedArgs s tp vs ns = do
  t <- tp
  RC.call Nothing Nothing (s ++ getTypeString t) (pure t) vs ns

lambda
  ::
    ( BinderElim r typ
    , RenderValue r typ val
    , IC.TypeSym r typ
    , ValueSym r typ val
    )
  => ([r BinderD] -> r val -> Doc)
  -> [VSBinder r]
  -> VS (r val)
  -> VS (r val)
lambda f ps' ex' = do
  ps <- sequence ps'
  ex <- ex'
  let ft = IC.funcType (map (pure . binderType) ps) (pure $ valueType ex)
  valFromData (Just 0) Nothing ft (f ps ex)

objAccess
  :: (FunctionElim r typ, RenderValue r typ val, ValueElim r val)
  => VS (r val) -> VS (r FuncData) -> VS (r val)
objAccess = on2StateWrapped (\v f-> mkVal (functionType f)
  (R.objAccess (RC.value v) (RC.function f)))

objMethodCall
  :: (RenderValue r typ val, ValueElim r val)
  => Label
  -> VS (r typ)
  -> VS (r val)
  -> [VS (r val)]
  -> NamedArgs r val
  -> VS (r val)
objMethodCall f t ob vs ns = ob >>= (\o -> RC.call Nothing
  (Just $ RC.value o <> dot) f t vs ns)

-- Functions --

func
  :: (RenderFunction r typ, ValueElim r val, ValueExpression r typ val)
  => Label -> VS (r typ) -> [VS (r val)] -> VS (r FuncData)
func l t vs = funcApp l t vs >>= ((`funcFromData` t) . R.func . RC.value)

get
  :: (RO.InternalGetSet r typ val, IG.OOFunctionSym r typ val)
  => VS (r val) -> SVariable r -> VS (r val)
get v vToGet = v $. RO.getFunc vToGet

set
  ::
    ( ValueSym r typ val
    , RO.InternalGetSet r typ val
    , IG.OOFunctionSym r typ val
    )
  => VS (r val) -> SVariable r -> VS (r val) -> VS (r val)
set v vToSet toVal = v $. RO.setFunc (onStateValue valueType v) vToSet toVal

-- TODO [Brandon Bosman, 06/10/2026]: Figure out what to do with this
listAccess
  :: ( IC.TypeSym r typ
     , ValueSym r typ val
     , IC.IndexTranslator r val
     , RC.InternalListFunc r typ val
     , FunctionElim r typ
     , RenderFunction r typ
     , RenderValue r typ val
     , TypeElim r typ
     , ValueElim r val
     )
  => VS (r val) -> VS (r val) -> VS (r val)
listAccess v i = do
  v' <- v
  let i' = IC.intToIndex i
      t  = IC.innerType $ pure $ valueType v'
      checkType (List _) = RC.listAccessFunc t i'
      checkType (Set _) = RC.listAccessFunc t i'
      checkType (Array _) = i' >>=
                              (\ix -> funcFromData (brackets (RC.value ix)) t)
      checkType _ = error "listAccess called on non-list-type value"
  f <- checkType (getCodeType (valueType v'))
  mkVal (RC.functionType f) (RC.value v' <> RC.function f)

getFunc
  :: (IG.OOFunctionSym r typ val, VariableElim r typ)
  => SVariable r -> VS (r FuncData)
getFunc v = v >>= (\vr -> IG.func (getterName $ variableName vr)
  (toState $ variableType vr) [])

setFunc
  :: (IG.OOFunctionSym r typ val, VariableElim r typ)
  => VS (r typ) -> SVariable r -> VS (r val) -> VS (r FuncData)
setFunc t v toVal = v >>= (\vr -> IG.func (setterName $ variableName vr) t
  [toVal])

-- Statements --

stmt
  :: (RenderStatement r stmt, StatementElim r stmt)
  => MS (r stmt) -> MS (r stmt)
stmt s' = do
  s <- s'
  mkStmtNoEnd (RC.statement s <> R.getTerm (statementTerm s))

loopStmt
  :: (RenderStatement r stmt, StatementElim r stmt)
  => MS (r stmt) -> MS (r stmt)
loopStmt = RC.stmt . setEmpty

emptyStmt :: (RenderStatement r stmt) => MS (r stmt)
emptyStmt = mkStmtNoEnd empty

assign
  :: (InternalVarElim r, RenderStatement r stmt, ValueElim r val)
  => Terminator -> SVariable r -> VS (r val) -> MS (r stmt)
assign t vr' v' = do
  vr <- zoom lensMStoVS vr'
  v <- zoom lensMStoVS v'
  stmtFromData (R.assign vr v) t

subAssign
  :: (InternalVarElim r, RenderStatement r stmt, ValueElim r val)
  => Terminator -> SVariable r -> VS (r val) -> MS (r stmt)
subAssign t vr' v' = do
  vr <- zoom lensMStoVS vr'
  v <- zoom lensMStoVS v'
  stmtFromData (R.subAssign vr v) t

objDecNew
  ::
    ( IC.DeclStatement r val stmt bod
    , IG.OOValueExpression r typ val
    , VariableElim r typ
    )
  => SVariable r -> r ScopeData -> [VS (r val)] -> MS (r stmt)
objDecNew v scp vs = IC.varDecDef v scp (newObj (onStateValue variableType v) vs)

printList
  ::
    ( BlockSym r block stmt
    , BodySym r bod block
    , MultiStatement r stmt
    , IC.ScopeSym r
    , IC.DeclStatement r val stmt bod
    , AssignStatement r val stmt
    , IC.ControlStatement r val stmt bod
    , IC.TypeSym r typ
    , IC.Literal r typ val
    , NumericExpression r val
    , Comparison r val
    , IC.VariableSym r typ
    , IC.VariableValue r val
    , IC.List r val
    )
  => Integer
  -> VS (r val)
  -> (VS (r val) -> MS (r stmt))
  -> (String -> MS (r stmt))
  -> (String -> MS (r stmt))
  -> MS (r stmt)
printList n v prFn prStrFn prLnFn = multi [prStrFn "[",
  IC.for (IC.varDecDef i IC.local (IC.litInt 0))
    (IC.valueOf i ?< (IC.listSize v #- IC.litInt 1)) (i &++)
    (bodyStatements [prFn (IC.listAccess v (IC.valueOf i)), prStrFn ", "]),
  ifNoElse [(IC.listSize v ?> IC.litInt 0, oneLiner $
    prFn (IC.listAccess v (IC.listSize v #- IC.litInt 1)))],
  prLnFn "]"]
  where l_i = "list_i" ++ show n
        i = IC.var l_i IC.int

printSet
  ::
    ( BlockSym r block stmt
    , BodySym r bod block
    , MultiStatement r stmt
    , IC.ControlStatement r val stmt bod
    , IC.VariableSym r typ
    , IC.VariableValue r val
    )
  => Integer
  -> VS (r val)
  -> (VS (r val) -> MS (r stmt))
  -> (String -> MS (r stmt))
  -> (String -> MS (r stmt))
  -> VS (r typ)
  -> MS (r stmt)
printSet n v prFn prStrFn prLnFn s = multi [prStrFn "{ ",
  IC.forEach i v
    (bodyStatements [prFn (IC.valueOf i),prStrFn " "]),
  prLnFn "}"]
  where set_i = "set_i" ++ show n
        i = IC.var set_i s

printObj :: ClassName -> (String -> MS (r stmt)) -> MS (r stmt)
printObj n prLnFn = prLnFn $ "Instance of " ++ n ++ " object"

print
  ::
    ( BlockSym r block stmt
    , BodySym r bod block
    , MultiStatement r stmt
    , PrintConsole r val stmt
    , PrintFile r val stmt
    , IC.ScopeSym r
    , IC.DeclStatement r val stmt bod
    , AssignStatement r val stmt
    , IC.ControlStatement r val stmt bod
    , ValueSym r typ val
    , IC.Literal r typ val
    , NumericExpression r val
    , Comparison r val
    , IC.VariableSym r typ
    , IC.VariableValue r val
    , IC.List r val
    , IC.TypeSym r typ
    , TypeElim r typ
    , RC.InternalIOStmt r val stmt
    )
  => Bool -> Maybe (VS (r val)) -> VS (r val) -> VS (r val) -> MS (r stmt)
print newLn f printFn v = zoom lensMStoVS v >>= print' . getCodeType . valueType
  where print' (List t) = printList (getNestDegree 1 t) v prFn prStrFn prLnFn
        print' (Object n) = printObj n prLnFn
        print' (Set t) = printSet (getNestDegree 1 t) v prFn prStrFn prLnFn (convType t)
        print' _ = RC.printSt newLn f printFn v
        prFn = maybe IC.print printFile f
        prStrFn = maybe printStr printFileStr f
        prLnFn = if newLn then maybe printStrLn printFileStrLn f else maybe
          printStr printFileStr f

closeFile
  ::
    ( IC.TypeSym r typ
    , IG.InternalValueExp r typ val
    , IC.ValueStatement r val stmt
    )
  => Label -> VS (r val) -> MS (r stmt)
closeFile n f = IC.valStmt $ objMethodCallNoParams IC.void f n

returnStmt
  :: (RenderStatement r stmt, ValueElim r val)
  => Terminator -> VS (r val) -> MS (r stmt)
returnStmt t v' = do
  v <- zoom lensMStoVS v'
  stmtFromData (R.return' [v]) t

valStmt
  :: (RenderStatement r stmt, ValueElim r val)
  => Terminator -> VS (r val) -> MS (r stmt)
valStmt t v' = do
  v <- zoom lensMStoVS v'
  stmtFromData (RC.value v) t

comment :: (RenderStatement r stmt) => Doc -> Label -> MS (r stmt)
comment cs c = mkStmtNoEnd (R.comment c cs)

throw
  :: (IC.Literal r typ val, RenderStatement r stmt)
  => (r val -> Doc) -> Terminator -> Label -> MS (r stmt)
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
ifCond
  :: (RC.BodyElim r bod, RenderStatement r stmt, ValueElim r val)
  => (Doc -> Doc)
  -> Doc
  -> OptionalSpace
  -> Doc
  -> Doc
  -> Doc
  -> [(VS (r val), MS (r bod))]
  -> MS (r bod)
  -> MS (r stmt)
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

tryCatch :: (RenderStatement r stmt) => (r bod -> r bod -> Doc) ->
  MS (r bod) -> MS (r bod) -> MS (r stmt)
tryCatch f = on2StateWrapped (\tb1 tb2 -> mkStmtNoEnd (f tb1 tb2))

-- Methods --

construct :: (Monad r) => Label -> MS (r TypeData)
construct n = zoom lensMStoVS $ typeFromData (Object n) n empty

param
  :: (RenderParam r, VariableElim r typ)
  => (r Variable -> Doc) -> SVariable r -> MS (r ParamData)
param f v' = do
  v <- zoom lensMStoVS v'
  let n = variableName v
  modify $ addParameter n
  modify $ useVarName n
  paramFromData v' $ f v

method
  :: (MethodTypeSym r typ, OORenderMethod r vis typ mthd attch bod)
  => Label
  -> r vis
  -> r attch
  -> VS (r typ)
  -> [MS (r ParamData)]
  -> MS (r bod)
  -> MS (r mthd)
method n s p t = intMethod False n s p (mType t)

getMethod
  :: (OORenderSym r vis typ val stmt mthd stvr attch file mod bod block)
  => SVariable r -> MS (r mthd)
getMethod v = zoom lensMStoVS v >>= (\vr -> method (getterName $ variableName
  vr) public instanceLevel (toState $ variableType vr) [] getBody)
  where getBody = oneLiner $ IC.returnStmt (IC.valueOf $ IG.instanceVarSelf v)

setMethod
  :: (OORenderSym r vis typ val stmt mthd stvr attch file mod bod block)
  => SVariable r -> MS (r mthd)
setMethod v = zoom lensMStoVS v >>= (\vr -> method (setterName $ variableName
  vr) public instanceLevel IC.void [IC.param v] setBody)
  where setBody = oneLiner $ IG.instanceVarSelf v &= IC.valueOf v

initStmts
  ::
    ( OOVariableSym r typ val
    , VariableValue r val
    , SelfSym r
    , AssignStatement r val stmt
    , BlockSym r block stmt
    , BodySym r bod block
    )
  => Initializers r val -> MS (r bod)
initStmts = bodyStatements . map (\(vr, vl) -> IG.instanceVarSelf vr &= vl)

function
  ::
    ( AttachmentSym r attch
    , MethodTypeSym r typ
    , OORenderMethod r vis typ mthd attch bod
    )
  => Label -> r vis -> VS (r typ) -> [MS (r ParamData)] -> MS (r bod) -> MS (r mthd)
function n s t = RO.intFunc False n s classLevel (mType t)

docFuncRepr
  :: (BlockCommentSym r, RenderMethod r mthd)
  => FuncDocRenderer
  -> String
  -> [String]
  -> [String]
  -> MS (r mthd)
  -> MS (r mthd)
docFuncRepr f desc pComms rComms = commentedFunc (docComment $ onStateValue
  (\ps -> f desc (zip ps pComms) rComms) getParameters)

docFunc
  :: (BlockCommentSym r, RenderMethod r mthd)
  => FuncDocRenderer
  -> String
  -> [String]
  -> Maybe String
  -> MS (r mthd)
  -> MS (r mthd)
docFunc f desc pComms rComm = docFuncRepr f desc pComms (maybeToList rComm)

-- Classes --

buildClass
  :: (RenderClass r vis mthd stvr, VisibilitySym r vis)
  =>  Maybe Label -> [CSStateVar r stvr] -> [MS (r mthd)] -> [MS (r mthd)] -> CS (r Class)
buildClass p stVars constructors methods = do
  n <- zoom lensCStoFS getModuleName
  RO.intClass n public (inherit p) stVars constructors methods

implementingClass :: (RenderClass r vis mthd stvr, VisibilitySym r vis) => Label -> [Label] ->
  [CSStateVar r stvr] -> [MS (r mthd)] -> [MS (r mthd)] -> CS (r Class)
implementingClass n is = RO.intClass n public (implements is)

docClass
  :: (BlockCommentSym r, RenderClass r vis mthd stvr)
  => ClassDocRenderer -> String -> CS (r Class) -> CS (r Class)
docClass cdr d = RO.commentedClass (docComment $ toState $ cdr d)

commentedClass
  :: (RC.BlockCommentElim r, RO.ClassElim r, Monad r)
  => CS (r Doc) -> CS (r Class) -> CS (r Doc)
commentedClass = on2StateValues (\cmt cs -> toCode $ R.commentedItem
  (RC.blockComment' cmt) (RO.class' cs))

-- Modules --

modFromData :: Label -> (Doc -> r mod) -> FS Doc -> FS (r mod)
modFromData n f d = modify (setModuleName n) >> onStateValue f d

-- Files --

fileDoc
  :: (RC.BlockElim r block, RenderMod r mod, RenderFile r file mod)
  => String -> (r mod -> r block) -> r block -> FS (r mod) -> FS (r file)
fileDoc ext topb botb mdl = do
  m <- mdl
  nm <- getModuleName
  let fp = addExt ext nm
      updm = updateModuleDoc (\d -> emptyIfEmpty d
        (R.file (RC.block $ topb m) d (RC.block botb))) m
  RO.fileFromData fp (toState updm)

-- | Generates a file for a documented module.
--   mdr is a function that takes description, author, and module name and
--                                                     returns a doc comment
--   e is the file extension
--   d is the description (I think)
--   a is a list of authors
--   dt is the date
--   fl is the file
docMod
  :: (BlockCommentSym r, RenderFile r file mod)
  => ModuleDocRenderer
  -> String
  -> String
  -> String
  -> [String]
  -> String
  -> FS (r file)
  -> FS (r file)
docMod mdr e wm d a dt fl = commentedMod fl (docComment $ mdr wm d a dt . addExt e
  <$> getModuleName)

fileFromData
  :: (RO.ModuleElim r mod)
  => (FilePath -> r mod -> r file) -> FilePath -> FS (r mod) -> FS (r file)
fileFromData f fpath mdl' = do
  -- Add this file to list of files as long as it is not empty
  mdl <- mdl'
  modify (\s -> if isEmpty (RO.module' mdl)
    then s
    else over lensFStoGS (addFile (s ^. currFileType) fpath) $
      -- If this is the main source file, set it as the main module in the state
      if s ^. currMain && isSource (s ^. currFileType)
        then over lensFStoGS (setMainMod fpath) s
        else s)
  pure $ f fpath mdl

-- Helper functions

setEmpty :: (RenderStatement r stmt, StatementElim r stmt) => MS (r stmt) -> MS (r stmt)
setEmpty s' = s' >>= mkStmtNoEnd . RC.statement
