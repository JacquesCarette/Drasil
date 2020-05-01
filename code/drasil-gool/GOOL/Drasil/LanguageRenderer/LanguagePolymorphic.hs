{-# LANGUAGE PostfixOperators #-}

-- | Implementations defined here are valid for any language renderer.
module GOOL.Drasil.LanguageRenderer.LanguagePolymorphic (fileFromData,
  multiBody, block, multiBlock, int, listInnerType, obj, funcType, negateOp, 
  csc, sec, cot, equalOp, notEqualOp, greaterOp, greaterEqualOp, lessOp, 
  lessEqualOp, plusOp, minusOp, multOp, divideOp, moduloOp, var, staticVar, 
  classVarCheckStatic, arrayElem, litChar, litDouble, litInt, litString, 
  valueOf, arg, argsList, call, funcAppMixedArgs, selfFuncAppMixedArgs, 
  newObjMixedArgs, lambda, objAccess, objMethodCall, func, get, set, listAdd, 
  listAppend, iterBegin, iterEnd, listAccess, listSet, getFunc, setFunc, 
  listAppendFunc, stmt, loopStmt, emptyStmt, assign, increment, objDecNew, 
  print, closeFile, returnStmt, valStmt, comment, throw, ifCond, tryCatch, 
  construct, param, method, getMethod, setMethod, constructor, function, 
  docFuncRepr, docFunc, buildClass, implementingClass, docClass, 
  commentedClass, modFromData, fileDoc, docMod
) where

import Utils.Drasil (indent)

import GOOL.Drasil.CodeType (CodeType(..), ClassName)
import GOOL.Drasil.ClassInterface (Label, Library, SFile, MSBody, MSBlock, 
  VSType, SVariable, SValue, VSFunction, MSStatement, MSParameter, SMethod, 
  CSStateVar, SClass, FSModule, NamedArgs, Initializers, MixedCall, 
  MixedCtorCall, FileSym(File), BodySym(Body), bodyStatements, oneLiner, 
  BlockSym(Block), PermanenceSym(..), TypeSym(Type), 
  TypeElim(getType, getTypeString), VariableSym(Variable), 
  VariableElim(variableName, variableType), ValueSym(Value, valueType), 
  NumericExpression((#-), (#/), sin, cos, tan), Comparison(..), funcApp, 
  newObj, objMethodCallNoParams, ($.), StatementSym(multi), 
  AssignStatement((&++)), (&=), 
  IOStatement(printStr, printStrLn, printFile, printFileStr, printFileStrLn),
  ifNoElse, ScopeSym(..), ModuleSym(Module), convType)
import qualified GOOL.Drasil.ClassInterface as S (
  TypeSym(int, double, char, string, listType, arrayType, listInnerType, void), 
  VariableSym(var, objVarSelf), Literal(litInt, litFloat, litDouble, litString),
  VariableValue(valueOf), FunctionSym(func), List(listSize, listAccess), 
  StatementSym(valStmt), DeclStatement(varDecDef), IOStatement(print),
  ControlStatement(returnStmt, for), ParameterSym(param), MethodSym(method))
import GOOL.Drasil.RendererClasses (RenderSym, RenderFile(commentedMod),  
  RenderType(..), InternalVarElim(variableBind), RenderValue(valFromData),
  InternalIterator(iterBeginFunc, iterEndFunc), RenderFunction(funcFromData), 
  FunctionElim(functionType), RenderStatement(stmtFromData), 
  StatementElim(statementTerm), MethodTypeSym(mType), 
  RenderParam(paramFromData), RenderMethod(intMethod, commentedFunc), 
  RenderClass(inherit, implements), RenderMod(updateModuleDoc), 
  BlockCommentSym(..))
import qualified GOOL.Drasil.RendererClasses as S (RenderFile(fileFromData), 
  RenderBody(multiBody), RenderValue(call), InternalGetSet(getFunc, setFunc),
  InternalListFunc(listAddFunc, listAppendFunc, listAccessFunc, listSetFunc),
  RenderStatement(stmt), InternalIOStmt(..), MethodTypeSym(construct), 
  RenderMethod(intFunc), RenderClass(intClass, commentedClass))
import qualified GOOL.Drasil.RendererClasses as RC (BodyElim(..), BlockElim(..),
  InternalVarElim(variable), ValueElim(value), FunctionElim(function), 
  StatementElim(statement), ClassElim(..), ModuleElim(..), BlockCommentElim(..))
import GOOL.Drasil.AST (Binding(..), Terminator(..), isSource)
import GOOL.Drasil.Helpers (doubleQuotedText, vibcat, emptyIfEmpty, toCode, 
  toState, onStateValue, on2StateValues, on3StateValues, onStateList, 
  getInnerType, getNestDegree)
import GOOL.Drasil.LanguageRenderer (dot, addExt, functionDox, classDox, 
  moduleDox, getterName, setterName, valueList, namedArgList)
import qualified GOOL.Drasil.LanguageRenderer as R (file, block, assign, 
  addAssign, return', comment, getTerm, var, arg, func, objAccess, 
  commentedItem)
import GOOL.Drasil.LanguageRenderer.Constructors (mkStmt, mkStmtNoEnd, 
  mkStateVal, mkVal, mkStateVar, mkStaticVar, VSOp, unOpPrec, compEqualPrec, 
  compPrec, addPrec, multPrec)
import GOOL.Drasil.State (FS, CS, MS, lensFStoGS, lensMStoVS, currMain, 
  currFileType, modifyReturnFunc, addFile, setMainMod, setModuleName, 
  getModuleName, getClassName, addParameter, getParameters)

import Prelude hiding (print,sin,cos,tan,(<>))
import Data.Maybe (fromMaybe, maybeToList)
import Control.Monad.State (modify)
import Control.Lens ((^.), over)
import Control.Lens.Zoom (zoom)
import Text.PrettyPrint.HughesPJ (Doc, text, empty, render, (<>), (<+>), parens,
  brackets, quotes, integer, vcat, comma, isEmpty)
import qualified Text.PrettyPrint.HughesPJ as D (char, double)

-- Bodies --

multiBody :: (RenderSym r, Monad r) => [MSBody r] -> MS (r Doc)
multiBody bs = onStateList (toCode . vibcat) $ map (onStateValue RC.body) bs

-- Blocks --

block :: (RenderSym r, Monad r) => [MSStatement r] -> MS (r Doc)
block sts = onStateList (toCode . R.block . map RC.statement) (map S.stmt sts)

multiBlock :: (RenderSym r, Monad r) => [MSBlock r] -> MS (r Doc)
multiBlock bs = onStateList (toCode . vibcat) $ map (onStateValue RC.block) bs

-- Types --

int :: (RenderSym r) => VSType r
int = toState $ typeFromData Integer "int" (text "int")

listInnerType :: (RenderSym r) => VSType r -> VSType r
listInnerType t = t >>= (convType . getInnerType . getType)

obj :: (RenderSym r) => ClassName -> VSType r
obj n = toState $ typeFromData (Object n) n (text n)

funcType :: (RenderSym r) => [VSType r] -> VSType r -> VSType r
funcType ps' = on2StateValues (\ps r -> typeFromData (Func (map getType ps) 
  (getType r)) "" empty) (sequence ps')

-- Unary Operators --

negateOp :: (Monad r) => VSOp r
negateOp = unOpPrec "-"

csc :: (RenderSym r) => SValue r -> SValue r
csc v = valOfOne (fmap valueType v) #/ sin v

sec :: (RenderSym r) => SValue r -> SValue r
sec v = valOfOne (fmap valueType v) #/ cos v

cot :: (RenderSym r) => SValue r -> SValue r
cot v = valOfOne (fmap valueType v) #/ tan v

valOfOne :: (RenderSym r) => VSType r -> SValue r
valOfOne t = t >>= (getVal . getType)
  where getVal Float = S.litFloat 1.0
        getVal _ = S.litDouble 1.0

-- Binary Operators --

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

var :: (RenderSym r) => Label -> VSType r -> SVariable r
var n t = mkStateVar n t (R.var n)

staticVar :: (RenderSym r) => Label -> VSType r -> SVariable r
staticVar n t = mkStaticVar n t (R.var n)

-- | To be used in classVar implementations. Throws an error if the variable is 
-- not static since classVar is for accessing static variables from a class
classVarCheckStatic :: (RenderSym r) => r (Variable r) -> r (Variable r)
classVarCheckStatic v = classVarCS (variableBind v)
  where classVarCS Dynamic = error
          "classVar can only be used to access static variables"
        classVarCS Static = v

arrayElem :: (RenderSym r) => SValue r -> SVariable r -> SVariable r
arrayElem i' v' = do
  i <- i'
  v <- v'
  let vName = variableName v ++ "[" ++ render (RC.value i) ++ "]"
      vType = listInnerType $ toState $ variableType v
      vRender = RC.variable v <> brackets (RC.value i)
  mkStateVar vName vType vRender

-- Values --

litChar :: (RenderSym r) => Char -> SValue r
litChar c = mkStateVal S.char (quotes $ D.char c)

litDouble :: (RenderSym r) => Double -> SValue r
litDouble d = mkStateVal S.double (D.double d)

litInt :: (RenderSym r) => Integer -> SValue r
litInt i = mkStateVal S.int (integer i)

litString :: (RenderSym r) => String -> SValue r
litString s = mkStateVal S.string (doubleQuotedText s)

valueOf :: (RenderSym r) => SVariable r -> SValue r
valueOf = onStateValue (\v -> mkVal (variableType v) (RC.variable v))

arg :: (RenderSym r) => SValue r -> SValue r -> SValue r
arg = on3StateValues (\s n args -> mkVal s (R.arg n args)) S.string

argsList :: (RenderSym r) => String -> SValue r
argsList l = mkStateVal (S.arrayType S.string) (text l)

-- | First parameter is separator between name and value for named arguments, 
-- rest similar to call from ClassInterface
call :: (RenderSym r) => Doc -> Maybe Library -> Maybe Doc -> MixedCall r
call sep lib o n t pas nas = do
  pargs <- sequence pas
  nms <- mapM fst nas
  nargs <- mapM snd nas
  let libDoc = maybe empty (text . (++ ".")) lib
      obDoc = fromMaybe empty o
  mkStateVal t $ obDoc <> libDoc <> text n <> parens (valueList pargs <+> 
    (if null pas || null nas then empty else comma) <+> namedArgList sep 
    (zip nms nargs))

funcAppMixedArgs :: (RenderSym r) => MixedCall r
funcAppMixedArgs = S.call Nothing Nothing

selfFuncAppMixedArgs :: (RenderSym r) => Doc -> SVariable r -> MixedCall r
selfFuncAppMixedArgs d slf n t vs ns = do
  s <- slf 
  S.call Nothing (Just $ RC.variable s <> d) n t vs ns

newObjMixedArgs :: (RenderSym r) => String -> MixedCtorCall r
newObjMixedArgs s tp vs ns = do
  t <- tp 
  S.call Nothing Nothing (s ++ getTypeString t) (toState t) vs ns

lambda :: (RenderSym r) => ([r (Variable r)] -> r (Value r) -> Doc) -> 
  [SVariable r] -> SValue r -> SValue r
lambda f ps' ex' = do
  ps <- sequence ps'
  ex <- ex'
  ft <- funcType (map (toState . variableType) ps) (toState $ valueType ex)
  toState $ valFromData (Just 0) ft (f ps ex)

objAccess :: (RenderSym r) => SValue r -> VSFunction r -> SValue r
objAccess = on2StateValues (\v f -> mkVal (functionType f) (R.objAccess 
  (RC.value v) (RC.function f)))

objMethodCall :: (RenderSym r) => Label -> VSType r -> SValue r -> [SValue r] 
  -> NamedArgs r -> SValue r
objMethodCall f t ob vs ns = ob >>= (\o -> S.call Nothing 
  (Just $ RC.value o <> dot) f t vs ns)

-- Functions --

func :: (RenderSym r) => Label -> VSType r -> [SValue r] -> VSFunction r
func l t vs = funcApp l t vs >>= ((`funcFromData` t) . R.func . RC.value)

get :: (RenderSym r) => SValue r -> SVariable r -> SValue r
get v vToGet = v $. S.getFunc vToGet

set :: (RenderSym r) => SValue r -> SVariable r -> SValue r -> SValue r
set v vToSet toVal = v $. S.setFunc (onStateValue valueType v) vToSet toVal

listAdd :: (RenderSym r) => SValue r -> SValue r -> SValue r -> SValue r
listAdd v i vToAdd = v $. S.listAddFunc v i vToAdd

listAppend :: (RenderSym r) => SValue r -> SValue r -> SValue r
listAppend v vToApp = v $. S.listAppendFunc vToApp

iterBegin :: (RenderSym r) => SValue r -> SValue r
iterBegin v = v $. iterBeginFunc (S.listInnerType $ onStateValue valueType v)

iterEnd :: (RenderSym r) => SValue r -> SValue r
iterEnd v = v $. iterEndFunc (S.listInnerType $ onStateValue valueType v)

listAccess :: (RenderSym r) => SValue r -> SValue r -> SValue r
listAccess v i = do
  v' <- v
  let checkType (List _) = S.listAccessFunc (S.listInnerType $ return $ 
        valueType v') i
      checkType (Array _) = i >>= (\ix -> funcFromData (brackets (RC.value ix)) 
        (S.listInnerType $ return $ valueType v'))
      checkType _ = error "listAccess called on non-list-type value"
  v $. checkType (getType (valueType v'))

listSet :: (RenderSym r) => SValue r -> SValue r -> SValue r -> SValue r
listSet v i toVal = v $. S.listSetFunc v i toVal

getFunc :: (RenderSym r) => SVariable r -> VSFunction r
getFunc v = v >>= (\vr -> S.func (getterName $ variableName vr) 
  (toState $ variableType vr) [])

setFunc :: (RenderSym r) => VSType r -> SVariable r -> SValue r -> VSFunction r
setFunc t v toVal = v >>= (\vr -> S.func (setterName $ variableName vr) t 
  [toVal])

listAppendFunc :: (RenderSym r) => Label -> SValue r -> VSFunction r
listAppendFunc f v = S.func f (S.listType $ onStateValue valueType v) [v]

-- Statements --

stmt :: (RenderSym r) => MSStatement r -> MSStatement r
stmt = onStateValue (\s -> mkStmtNoEnd (RC.statement s <> R.getTerm 
  (statementTerm s)))
  
loopStmt :: (RenderSym r) => MSStatement r -> MSStatement r
loopStmt = S.stmt . setEmpty

emptyStmt :: (RenderSym r) => MSStatement r
emptyStmt = toState $ mkStmtNoEnd empty

assign :: (RenderSym r) => Terminator -> SVariable r -> SValue r -> 
  MSStatement r
assign t vr vl = zoom lensMStoVS $ on2StateValues (\vr' vl' -> stmtFromData 
  (R.assign vr' vl') t) vr vl

increment :: (RenderSym r) => SVariable r -> SValue r -> MSStatement r
increment vr vl = zoom lensMStoVS $ on2StateValues (\vr' -> mkStmt . 
  R.addAssign vr') vr vl

objDecNew :: (RenderSym r) => SVariable r -> [SValue r] -> MSStatement r
objDecNew v vs = S.varDecDef v (newObj (onStateValue variableType v) vs)

printList :: (RenderSym r) => Integer -> SValue r -> (SValue r -> MSStatement r)
  -> (String -> MSStatement r) -> (String -> MSStatement r) -> MSStatement r
printList n v prFn prStrFn prLnFn = multi [prStrFn "[", 
  S.for (S.varDecDef i (S.litInt 0)) 
    (S.valueOf i ?< (S.listSize v #- S.litInt 1)) (i &++) 
    (bodyStatements [prFn (S.listAccess v (S.valueOf i)), prStrFn ", "]), 
  ifNoElse [(S.listSize v ?> S.litInt 0, oneLiner $
    prFn (S.listAccess v (S.listSize v #- S.litInt 1)))], 
  prLnFn "]"]
  where l_i = "list_i" ++ show n
        i = S.var l_i S.int

printObj :: ClassName -> (String -> MSStatement r) -> MSStatement r
printObj n prLnFn = prLnFn $ "Instance of " ++ n ++ " object"

print :: (RenderSym r) => Bool -> Maybe (SValue r) -> SValue r -> SValue r -> 
  MSStatement r
print newLn f printFn v = zoom lensMStoVS v >>= print' . getType . valueType
  where print' (List t) = printList (getNestDegree 1 t) v prFn prStrFn 
          prLnFn
        print' (Object n) = printObj n prLnFn
        print' _ = S.printSt newLn f printFn v
        prFn = maybe S.print printFile f
        prStrFn = maybe printStr printFileStr f
        prLnFn = if newLn then maybe printStrLn printFileStrLn f else maybe 
          printStr printFileStr f 

closeFile :: (RenderSym r) => Label -> SValue r -> MSStatement r
closeFile n f = S.valStmt $ objMethodCallNoParams S.void f n

returnStmt :: (RenderSym r) => Terminator -> SValue r -> MSStatement r
returnStmt t v' = zoom lensMStoVS $ onStateValue (\v -> stmtFromData 
  (R.return' [v]) t) v'

valStmt :: (RenderSym r) => Terminator -> SValue r -> MSStatement r
valStmt t v' = zoom lensMStoVS $ onStateValue (\v -> stmtFromData (RC.value v)
  t) v'

comment :: (RenderSym r) => Doc -> Label -> MSStatement r
comment cs c = toState $ mkStmtNoEnd (R.comment c cs)

throw :: (RenderSym r) => (r (Value r) -> Doc) -> Terminator -> Label -> 
  MSStatement r
throw f t = onStateValue (\msg -> stmtFromData (f msg) t) . zoom lensMStoVS . 
  S.litString

-- ControlStatements --

ifCond :: (RenderSym r) => Doc -> Doc -> Doc -> [(SValue r, MSBody r)] -> 
  MSBody r -> MSStatement r
ifCond _ _ _ [] _ = error "if condition created with no cases"
ifCond ifStart elif bEnd (c:cs) eBody =
    let ifSect (v, b) = on2StateValues (\val bd -> vcat [
          text "if" <+> parens (RC.value val) <+> ifStart,
          indent $ RC.body bd,
          bEnd]) (zoom lensMStoVS v) b
        elseIfSect (v, b) = on2StateValues (\val bd -> vcat [
          elif <+> parens (RC.value val) <+> ifStart,
          indent $ RC.body bd,
          bEnd]) (zoom lensMStoVS v) b
        elseSect = onStateValue (\bd -> emptyIfEmpty (RC.body bd) $ vcat [
          text "else" <+> ifStart,
          indent $ RC.body bd,
          bEnd]) eBody
    in onStateList (mkStmtNoEnd . vcat)
      (ifSect c : map elseIfSect cs ++ [elseSect])

tryCatch :: (RenderSym r) => (r (Body r) -> r (Body r) -> Doc) -> MSBody r -> 
  MSBody r -> MSStatement r
tryCatch f = on2StateValues (\tb -> mkStmtNoEnd . f tb)

-- Methods --

construct :: (RenderSym r) => Label -> MS (r (Type r))
construct n = toState $ typeFromData (Object n) n empty

param :: (RenderSym r) => (r (Variable r) -> Doc) -> SVariable r -> 
  MSParameter r
param f v' = modifyReturnFunc (\v s -> addParameter (variableName v) s) 
  (\v -> paramFromData v (f v)) (zoom lensMStoVS v')

method :: (RenderSym r) => Label -> r (Scope r) -> r (Permanence r) -> VSType r 
  -> [MSParameter r] -> MSBody r -> SMethod r
method n s p t = intMethod False n s p (mType t)

getMethod :: (RenderSym r) => SVariable r -> SMethod r
getMethod v = zoom lensMStoVS v >>= (\vr -> S.method (getterName $ variableName 
  vr) public dynamic (toState $ variableType vr) [] getBody)
  where getBody = oneLiner $ S.returnStmt (S.valueOf $ S.objVarSelf v)

setMethod :: (RenderSym r) => SVariable r -> SMethod r
setMethod v = zoom lensMStoVS v >>= (\vr -> S.method (setterName $ variableName 
  vr) public dynamic S.void [S.param v] setBody)
  where setBody = oneLiner $ S.objVarSelf v &= S.valueOf v

constructor :: (RenderSym r) => Label -> [MSParameter r] -> Initializers r -> 
  MSBody r -> SMethod r
constructor fName ps is b = getClassName >>= (\c -> intMethod False fName 
  public dynamic (S.construct c) ps (S.multiBody [ib, b]))
  where ib = bodyStatements (zipWith (\vr vl -> S.objVarSelf vr &= vl) 
          (map fst is) (map snd is))

function :: (RenderSym r) => Label -> r (Scope r) -> r (Permanence r) -> 
  VSType r -> [MSParameter r] -> MSBody r -> SMethod r
function n s p t = S.intFunc False n s p (mType t)
  
docFuncRepr :: (RenderSym r) => String -> [String] -> [String] -> SMethod r -> 
  SMethod r
docFuncRepr desc pComms rComms = commentedFunc (docComment $ onStateValue 
  (\ps -> functionDox desc (zip ps pComms) rComms) getParameters)

docFunc :: (RenderSym r) => String -> [String] -> Maybe String -> SMethod r -> 
  SMethod r
docFunc desc pComms rComm = docFuncRepr desc pComms (maybeToList rComm)

-- Classes --

buildClass :: (RenderSym r) => Label -> Maybe Label -> [CSStateVar r] -> 
  [SMethod r] -> SClass r
buildClass n = S.intClass n public . inherit

implementingClass :: (RenderSym r) => Label -> [Label] -> [CSStateVar r] -> 
  [SMethod r] -> SClass r
implementingClass n is = S.intClass n public (implements is)

docClass :: (RenderSym r) => String -> SClass r -> SClass r
docClass d = S.commentedClass (docComment $ toState $ classDox d)

commentedClass :: (RenderSym r, Monad r) => CS (r (BlockComment r)) -> SClass r 
  -> CS (r Doc)
commentedClass = on2StateValues (\cmt cs -> toCode $ R.commentedItem 
  (RC.blockComment' cmt) (RC.class' cs))

-- Modules --

modFromData :: Label -> (Doc -> r (Module r)) -> FS Doc -> FSModule r
modFromData n f d = modify (setModuleName n) >> onStateValue f d

-- Files --

fileDoc :: (RenderSym r) => String -> (r (Module r) -> r (Block r)) -> 
  r (Block r) -> FSModule r -> SFile r
fileDoc ext topb botb mdl = do
  m <- mdl
  nm <- getModuleName
  let fp = toState $ addExt ext nm
      updm = toState $ updateModuleDoc (\d -> emptyIfEmpty d 
        (R.file (RC.block $ topb m) d (RC.block botb))) m
  S.fileFromData fp updm

docMod :: (RenderSym r) => String -> String -> [String] -> String -> SFile r -> 
  SFile r
docMod e d a dt = commentedMod (docComment $ moduleDox d a dt . addExt e <$> 
  getModuleName)

fileFromData :: (RenderSym r) => (FilePath -> r (Module r) -> r (File r)) 
  -> FS FilePath -> FSModule r -> SFile r
fileFromData f fp m = do
  mdl <- m
  fpath <- fp
  modify (\s -> if isEmpty (RC.module' mdl) 
    then s
    else over lensFStoGS (addFile (s ^. currFileType) fpath) $ 
      if s ^. currMain && isSource (s ^. currFileType) 
        then over lensFStoGS (setMainMod fpath) s
        else s)
  toState $ f fpath mdl

-- Helper functions

setEmpty :: (RenderSym r) => MSStatement r -> MSStatement r
setEmpty = onStateValue (mkStmtNoEnd . RC.statement)
