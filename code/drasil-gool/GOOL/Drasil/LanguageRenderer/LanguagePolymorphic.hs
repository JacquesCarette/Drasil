{-# LANGUAGE PostfixOperators #-}

-- | The structure for a class of renderers is defined here.
module GOOL.Drasil.LanguageRenderer.LanguagePolymorphic (fileFromData, block, 
  pi, varDec, varDecDef, listDec, listDecDef, objDecNew, objDecNewNoParams, 
  comment, ifCond, for, forEach, while, construct, method, getMethod, setMethod,
  privMethod, pubMethod, constructor, docMain, function, mainFunction, docFunc, 
  docInOutFunc, intFunc, stateVar,stateVarDef, constVar, privMVar, pubMVar, 
  pubGVar, buildClass, enum, privClass, pubClass, docClass, commentedClass, 
  buildModule, buildModule', fileDoc, docMod
) where

import Utils.Drasil (indent)

import GOOL.Drasil.CodeType (CodeType(..), isObject)
import GOOL.Drasil.Symantics (Label, KeywordSym(..),
  RenderSym(RenderFile, commentedMod), BlockSym(Block), 
  InternalBlock(..), BodySym(..), PermanenceSym(..), InternalPerm(..), 
  TypeSym(..), InternalType(..), VariableSym(..), 
  ValueSym(Value, valueOf, valueDoc),
  ValueExpression(..), InternalValue(..), InternalStatement(..), 
  StatementSym(Statement, (&=), constDecDef, returnState), ScopeSym(..), 
  InternalScope(..), MethodTypeSym(mType), ParameterSym(..), 
  MethodTypeSym(MethodType), MethodSym(Method), 
  InternalMethod(intMethod, commentedFunc, isMainMethod, methodDoc), 
  StateVarSym(StateVar), InternalStateVar(..), ClassSym(Class), 
  InternalClass(..), ModuleSym(Module, moduleName), InternalMod(..), 
  BlockComment(..))
import qualified GOOL.Drasil.Symantics as S (InternalFile(fileFromData), 
  StatementSym(varDec, varDecDef), MethodTypeSym(construct), 
  MethodSym(method, mainFunction), InternalMethod(intFunc), 
  StateVarSym(stateVar), ClassSym(buildClass, commentedClass))
import GOOL.Drasil.Data (Binding(..), Terminator(..), TypeData(..), td, 
  FileType, FileData(..), fileD, ModData(..))
import GOOL.Drasil.Helpers (vibcat, vmap, emptyIfEmpty, liftList)
import GOOL.Drasil.LanguageRenderer (forLabel, addExt, blockDocD, stateVarDocD, 
  stateVarListDocD, methodListDocD, enumDocD, enumElementsDocD, moduleDocD, 
  fileDoc', docFuncRepr, commentDocD, commentedItem, functionDox, classDox, 
  moduleDox, getterName, setterName)
import GOOL.Drasil.State (GOOLState, hasMain, mainMod, getPutReturnFunc, 
  addFile, setMainMod, setFilePath, getFilePath)

import Prelude hiding (break,print,last,mod,pi,(<>))
import Data.Maybe (maybeToList, isNothing)
import Control.Lens ((^.))
import Control.Applicative (liftA2)
import Control.Monad.State (State)
import Text.PrettyPrint.HughesPJ (Doc, text, empty, render, (<>), (<+>), parens,
  vcat, semi, equals, isEmpty)

fileFromData :: FileType -> FilePath -> State GOOLState ModData -> 
  State GOOLState FileData
fileFromData ft fp sm = getPutReturnFunc sm (\s m -> (if isEmpty (modDoc m) 
  then id else (if s ^. hasMain && isNothing (s ^. mainMod) then setMainMod fp 
  else id) . addFile ft fp . setFilePath fp) s) (fileD fp)

block :: (RenderSym repr) => repr (Keyword repr) -> [repr (Statement repr)] -> 
  repr (Block repr)
block end sts = docBlock $ blockDocD (keyDoc end) (map (statementDoc . state) 
  sts)

pi :: (RenderSym repr) => repr (Value repr)
pi = valFromData Nothing float (text "Math.PI")

varDec :: (RenderSym repr) => repr (Permanence repr) -> repr (Permanence repr) 
  -> repr (Variable repr) -> repr (Statement repr)
varDec s d v = stateFromData (permDoc (bind $ variableBind v) <+> getTypeDoc 
  (variableType v) <+> variableDoc v) Semi
  where bind Static = s
        bind Dynamic = d

varDecDef :: (RenderSym repr) => repr (Variable repr) -> repr (Value repr) -> 
  repr (Statement repr)
varDecDef vr vl = stateFromData (statementDoc (S.varDec vr) <+> equals 
  <+> valueDoc vl) Semi

listDec :: (RenderSym repr) => (repr (Value repr) -> Doc) -> 
  repr (Value repr) -> repr (Variable repr) -> repr (Statement repr)
listDec f sz v = stateFromData (statementDoc (S.varDec v) <> f sz) Semi

listDecDef :: (RenderSym repr) => ([repr (Value repr)] -> Doc) -> 
  repr (Variable repr) -> [repr (Value repr)] -> repr (Statement repr)
listDecDef f v vs = stateFromData (statementDoc (S.varDec v) <> f vs) Semi

objDecNew :: (RenderSym repr) => repr (Variable repr) -> [repr (Value repr)] -> 
  repr (Statement repr)
objDecNew v vs = S.varDecDef v (newObj (variableType v) vs)

objDecNewNoParams :: (RenderSym repr) => repr (Variable repr) -> 
  repr (Statement repr)
objDecNewNoParams v = objDecNew v []

comment :: (RenderSym repr) => repr (Keyword repr) -> Label -> 
  repr (Statement repr)
comment cs c = stateFromData (commentDocD c (keyDoc cs)) Empty

ifCond :: (RenderSym repr) => repr (Keyword repr) -> repr (Keyword repr) -> 
  repr (Keyword repr) -> [(repr (Value repr), repr (Body repr))] -> 
  repr (Body repr) -> repr (Statement repr)
ifCond _ _ _ [] _ = error "if condition created with no cases"
ifCond ifst elseif blEnd (c:cs) eBody = 
    let ifStart = keyDoc ifst
        elif = keyDoc elseif
        bEnd = keyDoc blEnd
        elseBody = bodyDoc eBody
        ifSect (v, b) = vcat [
          text "if" <+> parens (valueDoc v) <+> ifStart,
          indent $ bodyDoc b,
          bEnd]
        elseIfSect (v, b) = vcat [
          elif <+> parens (valueDoc v) <+> ifStart,
          indent $ bodyDoc b,
          bEnd]
        elseSect = emptyIfEmpty elseBody $ vcat [
          text "else" <+> ifStart,
          indent elseBody,
          bEnd]
    in stateFromData (vcat [
      ifSect c,
      vmap elseIfSect cs,
      elseSect]) Empty

for :: (RenderSym repr) => repr (Keyword repr) -> repr (Keyword repr) -> 
  repr (Statement repr) -> repr (Value repr) -> repr (Statement repr) -> 
  repr (Body repr) -> repr (Statement repr)
for bStart bEnd sInit vGuard sUpdate b = stateFromData (vcat [
  forLabel <+> parens (statementDoc (loopState sInit) <> semi <+> valueDoc 
    vGuard <> semi <+> statementDoc (loopState sUpdate)) <+> keyDoc bStart,
  indent $ bodyDoc b,
  keyDoc bEnd]) Empty

forEach :: (RenderSym repr) => repr (Keyword repr) -> repr (Keyword repr) -> 
  repr (Keyword repr) -> repr (Keyword repr) -> repr (Variable repr) -> 
  repr (Value repr) -> repr (Body repr) -> repr (Statement repr)
forEach bStart bEnd forEachLabel inLbl e v b = stateFromData
  (vcat [keyDoc forEachLabel <+> parens (getTypeDoc (variableType e) <+> 
    variableDoc e <+> keyDoc inLbl <+> valueDoc v) <+> keyDoc bStart,
  indent $ bodyDoc b,
  keyDoc bEnd]) Empty

while :: (RenderSym repr) => repr (Keyword repr) -> repr (Keyword repr) -> 
  repr (Value repr) -> repr (Body repr) -> repr (Statement repr)
while bStart bEnd v b = stateFromData (vcat [
  text "while" <+> parens (valueDoc v) <+> keyDoc bStart,
  indent $ bodyDoc b,
  keyDoc bEnd]) Empty

construct :: Label -> TypeData
construct n = td (Object n) n empty

method :: (RenderSym repr) => Label -> Label -> repr (Scope repr) -> 
  repr (Permanence repr) -> repr (Type repr) -> [repr (Parameter repr)] -> 
  repr (Body repr) -> State GOOLState (repr (Method repr))
method n c s p t = intMethod False n c s p (mType t)

getMethod :: (RenderSym repr) => Label -> repr (Variable repr) -> 
  State GOOLState (repr (Method repr))
getMethod c v = S.method (getterName $ variableName v) c public dynamic_ 
    (variableType v) [] getBody
    where getBody = oneLiner $ returnState (valueOf $ objVarSelf c v)

setMethod :: (RenderSym repr) => Label -> repr (Variable repr) -> 
  State GOOLState (repr (Method repr))
setMethod c v = S.method (setterName $ variableName v) c public dynamic_ void 
  [param v] setBody
  where setBody = oneLiner $ objVarSelf c v &= valueOf v

privMethod :: (RenderSym repr) => Label -> Label -> repr (Type repr) -> 
  [repr (Parameter repr)] -> repr (Body repr) -> 
  State GOOLState (repr (Method repr))
privMethod n c = S.method n c private dynamic_

pubMethod :: (RenderSym repr) => Label -> Label -> repr (Type repr) -> 
  [repr (Parameter repr)] -> repr (Body repr) -> 
  State GOOLState (repr (Method repr))
pubMethod n c = S.method n c public dynamic_

constructor :: (RenderSym repr) => Label -> Label -> [repr (Parameter repr)] -> 
  repr (Body repr) -> State GOOLState (repr (Method repr))
constructor fName n = intMethod False fName n public dynamic_ (S.construct n)

docMain :: (RenderSym repr) => repr (Body repr) -> 
  State GOOLState (repr (Method repr))
docMain b = commentedFunc (docComment $ return $ functionDox 
  "Controls the flow of the program" 
  [("args", "List of command-line arguments")] []) (S.mainFunction b)

function :: (RenderSym repr) => Label -> repr (Scope repr) -> 
  repr (Permanence repr) -> repr (Type repr) -> [repr (Parameter repr)] -> 
  repr (Body repr) -> State GOOLState (repr (Method repr))
function n s p t = S.intFunc False n s p (mType t)

mainFunction :: (RenderSym repr) => repr (Type repr) -> Label -> 
  repr (Body repr) -> State GOOLState (repr (Method repr))
mainFunction s n = S.intFunc True n public static_ (mType void)
  [param (var "args" (typeFromData (List String) (render (getTypeDoc s) ++ 
  "[]") (getTypeDoc s <> text "[]")))]

docFunc :: (RenderSym repr) => String -> [String] -> Maybe String -> 
  State GOOLState (repr (Method repr)) -> State GOOLState (repr (Method repr))
docFunc desc pComms rComm = docFuncRepr desc pComms (maybeToList rComm)

docInOutFunc :: (RenderSym repr) => (repr (Scope repr) -> repr (Permanence repr)
    -> [repr (Variable repr)] -> [repr (Variable repr)] -> 
    [repr (Variable repr)] -> repr (Body repr) -> 
    State GOOLState (repr (Method repr)))
  -> repr (Scope repr) -> repr (Permanence repr) -> String -> 
  [(String, repr (Variable repr))] -> [(String, repr (Variable repr))] -> 
  [(String, repr (Variable repr))] -> repr (Body repr) -> 
  State GOOLState (repr (Method repr))
docInOutFunc f s p desc is [o] [] b = docFuncRepr desc (map fst is) [fst o] 
  (f s p (map snd is) [snd o] [] b)
docInOutFunc f s p desc is [] [both] b = docFuncRepr desc (map fst $ both : is) 
  [fst both | not ((isObject . getType . variableType . snd) both)] 
  (f s p (map snd is) [] [snd both] b)
docInOutFunc f s p desc is os bs b = docFuncRepr desc (map fst $ bs ++ is ++ os)
  [] (f s p (map snd is) (map snd os) (map snd bs) b)

intFunc :: (RenderSym repr) => Bool -> Label -> repr (Scope repr) -> 
  repr (Permanence repr) -> repr (MethodType repr) -> [repr (Parameter repr)] 
  -> repr (Body repr) -> State GOOLState (repr (Method repr))
intFunc m n = intMethod m n ""

stateVar :: (RenderSym repr) => repr (Scope repr) -> repr (Permanence repr) ->
  repr (Variable repr) -> State GOOLState (repr (StateVar repr))
stateVar s p v = stateVarFromData $ stateVarDocD (scopeDoc s) (permDoc p) 
  (statementDoc (state $ S.varDec v))

stateVarDef :: (RenderSym repr) => repr (Scope repr) -> repr (Permanence repr) 
  -> repr (Variable repr) -> repr (Value repr) -> 
  State GOOLState (repr (StateVar repr))
stateVarDef s p vr vl = stateVarFromData $ stateVarDocD (scopeDoc s) (permDoc p)
  (statementDoc (state $ S.varDecDef vr vl))

constVar :: (RenderSym repr) => Doc -> repr (Scope repr) ->
  repr (Variable repr) -> repr (Value repr) -> 
  State GOOLState (repr (StateVar repr))
constVar p s vr vl = stateVarFromData $ stateVarDocD (scopeDoc s) p 
  (statementDoc (state $ constDecDef vr vl))

privMVar :: (RenderSym repr) => repr (Variable repr) -> 
  State GOOLState (repr (StateVar repr))
privMVar = S.stateVar private dynamic_

pubMVar :: (RenderSym repr) => repr (Variable repr) -> 
  State GOOLState (repr (StateVar repr))
pubMVar = S.stateVar public dynamic_

pubGVar :: (RenderSym repr) => repr (Variable repr) -> 
  State GOOLState (repr (StateVar repr))
pubGVar = S.stateVar public static_

buildClass :: (RenderSym repr) => (Label -> Doc -> Doc -> Doc -> Doc -> Doc) -> 
  (Label -> repr (Keyword repr)) -> Label -> Maybe Label -> repr (Scope repr) 
  -> [State GOOLState (repr (StateVar repr))] -> 
  [State GOOLState (repr (Method repr))] -> State GOOLState (repr (Class repr))
buildClass f i n p s vs fs = classFromData (liftA2 (f n parent (scopeDoc s)) 
  (liftList (stateVarListDocD . map stateVarDoc) vs) 
  (liftList (methodListDocD . map methodDoc) fs))
  where parent = case p of Nothing -> empty
                           Just pn -> keyDoc $ i pn

enum :: (RenderSym repr) => Label -> [Label] -> repr (Scope repr) -> 
  State GOOLState (repr (Class repr))
enum n es s = classFromData (return $ enumDocD n (enumElementsDocD es False) 
  (scopeDoc s))

privClass :: (RenderSym repr) => Label -> Maybe Label -> 
  [State GOOLState (repr (StateVar repr))] -> 
  [State GOOLState (repr (Method repr))] -> State GOOLState (repr (Class repr))
privClass n p = S.buildClass n p private

pubClass :: (RenderSym repr) => Label -> Maybe Label -> 
  [State GOOLState (repr (StateVar repr))] -> 
  [State GOOLState (repr (Method repr))] -> State GOOLState (repr (Class repr))
pubClass n p = S.buildClass n p public

docClass :: (RenderSym repr) => String -> State GOOLState (repr (Class repr))
  -> State GOOLState (repr (Class repr))
docClass d = S.commentedClass (docComment $ return $ classDox d)

commentedClass :: (RenderSym repr) => State GOOLState (repr (BlockComment repr))
  -> State GOOLState (repr (Class repr)) -> State GOOLState (repr (Class repr))
commentedClass cmt cs = classFromData (liftA2 (\cmt' cs' -> commentedItem 
  (blockCommentDoc cmt') (classDoc cs')) cmt cs)

buildModule :: (RenderSym repr) => Label -> [repr (Keyword repr)] -> 
  [State GOOLState (repr (Method repr))] -> 
  [State GOOLState (repr (Class repr))] -> State GOOLState (repr (Module repr))
buildModule n ls ms cs = modFromData n (liftList (any isMainMethod) ms) 
  (liftA2 (moduleDocD (vcat $ map keyDoc ls)) (liftList (vibcat . map classDoc) 
  cs) (liftList (methodListDocD . map methodDoc) ms))

buildModule' :: (RenderSym repr) => Label -> 
  [State GOOLState (repr (Method repr))] -> 
  [State GOOLState (repr (Class repr))] -> State GOOLState (repr (Module repr))
buildModule' n ms cs = modFromData n (liftList (any isMainMethod) ms) 
  (liftList (vibcat . map classDoc) (if null ms then cs else pubClass n Nothing 
  [] ms : cs))

fileDoc :: (RenderSym repr) => FileType -> String -> repr (Block repr) -> 
  repr (Block repr) -> State GOOLState (repr (Module repr)) -> 
  repr (RenderFile repr)
fileDoc ft ext topb botb m = S.fileFromData ft (fmap (addExt ext . moduleName) 
  m) (updateModuleDoc (\d -> emptyIfEmpty d (fileDoc' (blockDoc topb) d 
  (blockDoc botb))) m)

docMod :: (RenderSym repr) => String -> [String] -> String -> 
  repr (RenderFile repr) -> repr (RenderFile repr)
docMod d a dt m = commentedMod m (docComment $ moduleDox d a dt <$> getFilePath)
