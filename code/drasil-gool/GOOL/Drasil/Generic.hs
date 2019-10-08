{-# LANGUAGE PostfixOperators #-}

-- | The structure for a class of renderers is defined here.
module GOOL.Drasil.Generic (
  construct, method, getMethod, setMethod, privMethod, pubMethod, constructor, docMain, function, mainFunction, docFunc, docInOutFunc, intFunc
) where

import GOOL.Drasil.CodeType (CodeType(..), isObject)
import GOOL.Drasil.Symantics (Label,
  RenderSym(..), BodySym(..), PermanenceSym(..), TypeSym(..), InternalType(..), 
  VariableSym(..), ValueSym(..), StatementSym(..), ScopeSym(..), 
  MethodTypeSym(mType), ParameterSym(..), MethodTypeSym(MethodType), 
  MethodSym(Method, inOutFunc), InternalMethod(intMethod, commentedFunc))
import qualified GOOL.Drasil.Symantics as S (MethodTypeSym(construct), 
  MethodSym(method, mainFunction), InternalMethod(intFunc))
import GOOL.Drasil.Data (TypeData(..), td)
import GOOL.Drasil.LanguageRenderer (docFuncRepr, functionDox, getterName, 
  setterName)

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
