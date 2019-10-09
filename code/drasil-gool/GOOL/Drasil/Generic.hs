{-# LANGUAGE PostfixOperators #-}

-- | The structure for a class of renderers is defined here.
module GOOL.Drasil.Generic (
  construct, method, getMethod, setMethod, privMethod, pubMethod, constructor, 
  docMain, function, mainFunction, docFunc, docInOutFunc, intFunc, stateVar,
  stateVarDef, constVar, privMVar, pubMVar, pubGVar, buildClass, enum, 
  privClass, pubClass, docClass, commentedClass, buildModule, buildModule'
) where

import GOOL.Drasil.CodeType (CodeType(..), isObject)
import GOOL.Drasil.Symantics (Label, KeywordSym(..), 
  RenderSym(..), BodySym(..), PermanenceSym(..), InternalPerm(..), TypeSym(..), 
  InternalType(..), VariableSym(..), ValueSym(..), InternalStatement(..), 
  StatementSym(..), ScopeSym(..), InternalScope(..), MethodTypeSym(mType), 
  ParameterSym(..), MethodTypeSym(MethodType), MethodSym(Method, inOutFunc), 
  InternalMethod(intMethod, commentedFunc, isMainMethod, methodDoc), 
  StateVarSym(StateVar), 
  InternalStateVar(..), ClassSym(Class), InternalClass(..), ModuleSym(Module),
  InternalMod(..), BlockComment(..))
import qualified GOOL.Drasil.Symantics as S (MethodTypeSym(construct), 
  MethodSym(method, mainFunction), InternalMethod(intFunc), 
  StateVarSym(stateVar), ClassSym(buildClass, commentedClass))
import GOOL.Drasil.Data (TypeData(..), td)
import GOOL.Drasil.Helpers (vibcat)
import GOOL.Drasil.LanguageRenderer (stateVarDocD, stateVarListDocD, 
  methodListDocD, enumDocD, enumElementsDocD, moduleDocD, docFuncRepr, 
  commentedItem, functionDox, classDox, getterName, setterName)

import Prelude hiding (break,print,last,mod,(<>))
import Data.Maybe (maybeToList)
import Text.PrettyPrint.HughesPJ (Doc, text, empty, render, (<>), (<+>), ($+$),
  brackets, parens, isEmpty, rbrace, lbrace, vcat, char, double, quotes, 
  integer, semi, equals, braces, int, comma, colon, hcat)

construct :: Label -> TypeData
construct n = td (Object n) n empty

method :: (RenderSym repr) => Label -> Label -> repr (Scope repr) -> 
  repr (Permanence repr) -> repr (Type repr) -> 
  [repr (Parameter repr)] -> repr (Body repr) -> repr (Method repr)
method n c s p t = intMethod False n c s p (mType t)

getMethod :: (RenderSym repr) => Label -> repr (Variable repr) -> 
  repr (Method repr)
getMethod c v = S.method (getterName $ variableName v) c public dynamic_ 
    (variableType v) [] getBody
    where getBody = oneLiner $ returnState (valueOf $ objVarSelf c 
            (variableName v) (variableType v))

setMethod :: (RenderSym repr) => Label -> repr (Variable repr) -> 
  repr (Method repr)
setMethod c v = S.method (setterName $ variableName v) c public dynamic_ void 
  [param v] setBody
  where setBody = oneLiner $ objVarSelf c (variableName v) (variableType v) 
          &= valueOf v

privMethod :: (RenderSym repr) => Label -> Label -> repr (Type repr) -> 
  [repr (Parameter repr)] -> repr (Body repr) -> repr (Method repr)
privMethod n c = S.method n c private dynamic_

pubMethod :: (RenderSym repr) => Label -> Label -> repr (Type repr) -> 
  [repr (Parameter repr)] -> repr (Body repr) -> repr (Method repr)
pubMethod n c = S.method n c public dynamic_

constructor :: (RenderSym repr) => Label -> Label -> [repr (Parameter repr)] -> 
  repr (Body repr) -> repr (Method repr)
constructor fName n = intMethod False fName n public dynamic_ (S.construct n)

docMain :: (RenderSym repr) => repr (Body repr) -> repr (Method repr)
docMain b = commentedFunc (docComment $ functionDox 
  "Controls the flow of the program" 
  [("args", "List of command-line arguments")] []) (S.mainFunction b)

function :: (RenderSym repr) => Label -> repr (Scope repr) -> 
  repr (Permanence repr) -> repr (Type repr) -> [repr (Parameter repr)] -> 
  repr (Body repr) -> repr (Method repr) 
function n s p t = S.intFunc False n s p (mType t)

mainFunction :: (RenderSym repr) => repr (Type repr) -> Label -> 
  repr (Body repr) -> repr (Method repr)
mainFunction s n = S.intFunc True n public static_ (mType void)
  [param (var "args" (typeFromData (List String) (render (getTypeDoc s) ++ 
  "[]") (getTypeDoc s <> text "[]")))]

docFunc :: (RenderSym repr) => String -> [String] -> Maybe String -> 
  repr (Method repr) -> repr (Method repr)
docFunc desc pComms rComm = docFuncRepr desc pComms (maybeToList rComm)

docInOutFunc :: (RenderSym repr) => Label -> repr (Scope repr) -> 
  repr (Permanence repr) -> String -> [(String, repr (Variable repr))] -> 
  [(String, repr (Variable repr))] -> [(String, repr (Variable repr))] -> 
  repr (Body repr) -> repr (Method repr)
docInOutFunc n s p desc is [o] [] b = docFuncRepr desc (map fst is) [fst o] 
  (inOutFunc n s p (map snd is) [snd o] [] b)
docInOutFunc n s p desc is [] [both] b = docFuncRepr desc (map fst $ both : 
  is) [fst both | not ((isObject . getType . variableType . snd) both)] 
  (inOutFunc n s p (map snd is) [] [snd both] b)
docInOutFunc n s p desc is os bs b = docFuncRepr desc (map fst $ bs ++ is ++ 
  os) [] (inOutFunc n s p (map snd is) (map snd os) (map snd bs) b)

intFunc :: (RenderSym repr) => Bool -> Label -> repr (Scope repr) -> 
  repr (Permanence repr) -> repr (MethodType repr) -> [repr (Parameter repr)] 
  -> repr (Body repr) -> repr (Method repr)
intFunc m n = intMethod m n ""

stateVar :: (RenderSym repr) => repr (Scope repr) -> repr (Permanence repr) ->
  repr (Variable repr) -> repr (StateVar repr)
stateVar s p v = stateVarFromData $ stateVarDocD (scopeDoc s) (permDoc p) 
  (statementDoc (state $ varDec v))

stateVarDef :: (RenderSym repr) => repr (Scope repr) -> repr (Permanence repr) 
  -> repr (Variable repr) -> repr (Value repr) -> repr (StateVar repr)
stateVarDef s p vr vl = stateVarFromData $ stateVarDocD (scopeDoc s) (permDoc p)
  (statementDoc (state $ varDecDef vr vl))

constVar :: (RenderSym repr) => Doc -> repr (Scope repr) ->
  repr (Variable repr) -> repr (Value repr) -> repr (StateVar repr)
constVar p s vr vl = stateVarFromData $ stateVarDocD (scopeDoc s) p 
  (statementDoc (state $ constDecDef vr vl))

privMVar :: (RenderSym repr) => Int -> repr (Variable repr) -> 
  repr (StateVar repr)
privMVar del = S.stateVar del private dynamic_

pubMVar :: (RenderSym repr) => Int -> repr (Variable repr) -> 
  repr (StateVar repr)
pubMVar del = S.stateVar del public dynamic_

pubGVar :: (RenderSym repr) => Int -> repr (Variable repr) -> 
  repr (StateVar repr)
pubGVar del = S.stateVar del public static_

buildClass :: (RenderSym repr) => (Label -> Doc -> Doc -> Doc -> Doc -> Doc) -> 
  (Label -> repr (Keyword repr)) -> Label -> Maybe Label -> repr (Scope repr) 
  -> [repr (StateVar repr)] -> [repr (Method repr)] -> repr (Class repr)
buildClass f i n p s vs fs = classFromData (f n parent (scopeDoc s) 
  (stateVarListDocD (map stateVarDoc vs)) (methodListDocD (map methodDoc fs)))
  where parent = case p of Nothing -> empty
                           Just pn -> keyDoc $ i pn

enum :: (RenderSym repr) => Label -> [Label] -> repr (Scope repr) -> 
  repr (Class repr)
enum n es s = classFromData (enumDocD n (enumElementsDocD es False) 
  (scopeDoc s))

privClass :: (RenderSym repr) => Label -> Maybe Label -> [repr (StateVar repr)] 
  -> [repr (Method repr)] -> repr (Class repr)
privClass n p = S.buildClass n p private

pubClass :: (RenderSym repr) => Label -> Maybe Label -> [repr (StateVar repr)] 
  -> [repr (Method repr)] -> repr (Class repr)
pubClass n p = S.buildClass n p public

docClass :: (RenderSym repr) => String -> repr (Class repr) -> repr (Class repr)
docClass d = S.commentedClass (docComment $ classDox d)

commentedClass :: (RenderSym repr) => repr (BlockComment repr) -> 
  repr (Class repr) -> repr (Class repr)
commentedClass cmt cs = classFromData (commentedItem (blockCommentDoc cmt) 
  (classDoc cs))

buildModule :: (RenderSym repr) => Label -> [repr (Keyword repr)] -> 
  [repr (Method repr)] -> [repr (Class repr)] -> repr (Module repr)
buildModule n ls ms cs = modFromData n (any isMainMethod ms) (moduleDocD 
  (vcat $ map keyDoc ls) (methodListDocD $ map methodDoc ms) 
  (vibcat $ map classDoc cs))

buildModule' :: (RenderSym repr) => Label -> [repr (Method repr)] -> 
  [repr (Class repr)] -> repr (Module repr)
buildModule' n ms cs = modFromData n (any isMainMethod ms) (vibcat $ map 
  classDoc $ if null ms then cs else pubClass n Nothing [] ms : cs)