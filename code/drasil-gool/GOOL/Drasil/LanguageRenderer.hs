{-# LANGUAGE PostfixOperators #-}

-- | The structure for a class of renderers is defined here.
module GOOL.Drasil.LanguageRenderer (
  -- * Common Syntax
  classDec, dot, commentStart, elseIfLabel, forLabel, inLabel, new,
  blockCmtStart, blockCmtEnd, docCmtStart, bodyStart, bodyEnd, endStatement, 
  addExt,
  
  -- * Default Functions available for use in renderers
  packageDocD, fileDoc', moduleDocD, classDocD, multiStateDocD, blockDocD, 
  bodyDocD, outDoc, printDoc, printFileDocD, destructorError, paramDocD, 
  methodDocD, stateVarDocD, constVarDocD, stateVarListDocD, switchDocD, 
  assignDocD, multiAssignDoc, plusEqualsDocD, plusPlusDocD, listDecDocD, 
  statementDocD, getTermDoc, returnDocD, commentDocD, mkSt, mkStNoEnd, 
  mkStateVal, mkVal, mkStateVar, mkVar, mkStaticVar, varDocD, extVarDocD, 
  selfDocD, argDocD, classVarCheckStatic, classVarDocD, objVarDocD, 
  funcAppDocD, newObjDocD, newObjDocD', constDecDefDocD, funcDocD, castDocD, 
  listAccessFuncDocD, listSetFuncDocD, objAccessDocD, castObjDocD, breakDocD, 
  continueDocD, staticDocD, dynamicDocD, bindingError, privateDocD, publicDocD, 
  blockCmtDoc, docCmtDoc, commentedItem, addCommentsDocD, functionDox, 
  classDox, moduleDox, commentedModD, docFuncRepr, valueList, variableList, 
  parameterList, prependToBody, appendToBody, surroundBody, getterName, 
  setterName, intValue
) where

import Utils.Drasil (blank, capitalize, indent, indentList, stringList)

import GOOL.Drasil.CodeType (CodeType(..), ClassName)
import GOOL.Drasil.ClassInterface (Label, Library, VSType, SVariable, SValue, 
  MSStatement, SMethod, BodySym(..), bodyStatements, oneLiner, 
  PermanenceSym(..), TypeSym(Type, getType), VariableSym(..), ValueSym(..), Literal(..), VariableValue(..),
  NumericExpression(..), Comparison(..), List(..),
  StatementSym(..), AssignStatement(..), 
  DeclStatement(..), IOStatement(..), ControlStatement(..), 
  ifNoElse, ScopeSym(..), ParameterSym(..))
import qualified GOOL.Drasil.ClassInterface as S (TypeSym(int))
import GOOL.Drasil.RendererClasses (RenderSym, InternalBody(..), 
  InternalPerm(..), InternalType(..), InternalVariable(..), InternalValue(..), 
  InternalIOStmt(..), InternalStatement(..), InternalScope(..), InternalParam(..), 
  InternalMethod(..), BlockCommentSym(..))
import GOOL.Drasil.AST (Terminator(..), FileData(..), fileD, updateFileMod, 
  updateMod, TypeData(..), Binding(..), VarData(..))
import GOOL.Drasil.Helpers (hicat, vibcat, vmap, emptyIfEmpty, emptyIfNull,
  onStateValue, getNestDegree)
import GOOL.Drasil.State (lensMStoVS, getParameters)

import Control.Lens.Zoom (zoom)
import Data.List (last)
import Prelude hiding (break,print,last,mod,(<>))
import Text.PrettyPrint.HughesPJ (Doc, text, empty, render, (<>), (<+>), ($+$),
  space, brackets, parens, isEmpty, rbrace, lbrace, vcat, semi, equals, colon)

----------------------------------------
-- Syntax common to several renderers --
----------------------------------------

classDec, dot, commentStart, elseIfLabel, forLabel, inLabel, new, blockCmtStart,
  blockCmtEnd, docCmtStart, bodyStart, bodyEnd, endStatement :: Doc
classDec = text "class"
dot = text "."
commentStart = text "//"
elseIfLabel = text "else if"
forLabel = text "for"
inLabel = text "in"
new = text "new"
blockCmtStart = text "/*"
blockCmtEnd = text "*/"
docCmtStart = text "/**"
bodyStart = lbrace
bodyEnd = rbrace
endStatement = semi

addExt :: String -> String -> String
addExt ext nm = nm ++ "." ++ ext

----------------------------------
-- Functions for rendering code --
----------------------------------

packageDocD :: Label -> Doc -> FileData -> FileData
packageDocD n end f = fileD (n ++ "/" ++ filePath f) (updateMod 
  (\d -> emptyIfEmpty d (vibcat [text "package" <+> text n <> end, d])) 
  (fileMod f))

fileDoc' :: Doc -> Doc -> Doc -> Doc
fileDoc' t m b = vibcat [
  t,
  m,
  b]

-----------------------------------------------
-- 'Default' functions used in the renderers --
-----------------------------------------------

-- Module --

moduleDocD :: Doc -> Doc -> Doc -> Doc
moduleDocD ls fs cs = emptyIfEmpty (fs <> cs) (vibcat (filter (not . isEmpty) 
  [ls, fs, cs]))

-- Class --

classDocD :: Label -> Doc -> Doc -> Doc -> Doc -> Doc
classDocD n p s vs fs = vcat [
  s <+> classDec <+> text n <+> p <+> lbrace, 
  indentList [
    vs,
    blank,
    fs],
  rbrace]

-- Groupings --

multiStateDocD :: [(Doc, Terminator)] -> (Doc, Terminator)
multiStateDocD sts = (vcat (applyEnd statements), needsEnd statements)
  where applyEnd [] = []
        applyEnd [(s, _)] = [s]
        applyEnd ((s, t):ss) = (s <> getTermDoc t) : applyEnd ss
        needsEnd [] = Empty
        needsEnd ss = snd (last ss)
        statements = filter notNullStatement sts
        notNullStatement s = not (isEmpty (fst s))

blockDocD :: [Doc] -> Doc
blockDocD sts = vcat $ filter (not . isEmpty) sts

bodyDocD :: [Doc] -> Doc
bodyDocD bs = vibcat $ filter (not . isEmpty) bs

-- IO --

printDoc :: (RenderSym repr) => repr (Value repr) -> repr (Value repr) -> Doc
printDoc printFn v = valueDoc printFn <> parens (valueDoc v)

printListDoc :: (RenderSym repr) => Integer -> SValue repr -> (SValue repr -> 
  MSStatement repr) -> (String -> MSStatement repr) -> 
  (String -> MSStatement repr) -> MSStatement repr
printListDoc n v prFn prStrFn prLnFn = multi [prStrFn "[", 
  for (varDecDef i (litInt 0)) (valueOf i ?< (listSize v #- litInt 1))
    (i &++) (bodyStatements [prFn (listAccess v (valueOf i)), prStrFn ", "]), 
  ifNoElse [(listSize v ?> litInt 0, oneLiner $
    prFn (listAccess v (listSize v #- litInt 1)))], 
  prLnFn "]"]
  where l_i = "list_i" ++ show n
        i = var l_i S.int

printObjDoc :: ClassName -> (String -> MSStatement repr) -> MSStatement repr
printObjDoc n prLnFn = prLnFn $ "Instance of " ++ n ++ " object"

outDoc :: (RenderSym repr) => Bool -> Maybe (SValue repr) -> SValue repr -> 
  SValue repr -> MSStatement repr
outDoc newLn f printFn v = zoom lensMStoVS v >>= outDoc' . getType . valueType
  where outDoc' (List t) = printListDoc (getNestDegree 1 t) v prFn prStrFn 
          prLnFn
        outDoc' (Object n) = printObjDoc n prLnFn
        outDoc' _ = printSt newLn f printFn v
        prFn = maybe print printFile f
        prStrFn = maybe printStr printFileStr f
        prLnFn = if newLn then maybe printStrLn printFileStrLn f else maybe 
          printStr printFileStr f 

printFileDocD :: Label -> Doc -> Doc
printFileDocD fn f = f <> dot <> text fn

-- Parameters --

paramDocD :: (RenderSym repr) => repr (Variable repr) -> Doc
paramDocD v = getTypeDoc (variableType v) <+> variableDoc v

-- Method --

methodDocD :: (RenderSym repr) => Label -> repr (Scope repr) -> 
  repr (Permanence repr) -> repr (Type repr) -> [repr (Parameter repr)] -> 
  repr (Body repr) -> Doc
methodDocD n s p t ps b = vcat [
  scopeDoc s <+> permDoc p <+> getTypeDoc t <+> text n <> 
    parens (parameterList ps) <+> lbrace,
  indent (bodyDoc b),
  rbrace]
  
destructorError :: String -> String
destructorError l = "Destructors not allowed in " ++ l

-- StateVar --

stateVarDocD :: Doc -> Doc -> Doc -> Doc
stateVarDocD s p dec = s <+> p <+> dec

constVarDocD :: Doc -> Doc -> Doc -> VarData -> Doc
constVarDocD s end p v = s <+> p <+> text "const" <+> typeDoc (varType v) <+>
  varDoc v <> end

stateVarListDocD :: [Doc] -> Doc
stateVarListDocD = vcat

-- Controls --

switchDocD :: (RenderSym repr) => repr (Statement repr) -> repr (Value repr) -> 
  repr (Body repr) -> [(repr (Value repr), repr (Body repr))] -> Doc
switchDocD breakState v defBody cs = 
  let caseDoc (l, result) = vcat [
        text "case" <+> valueDoc l <> colon,
        indentList [
          bodyDoc result,
          statementDoc breakState]]
      defaultSection = vcat [
        text "default" <> colon,
        indentList [
          bodyDoc defBody,
          statementDoc breakState]]
  in vcat [
      text "switch" <> parens (valueDoc v) <+> lbrace,
      indentList [
        vmap caseDoc cs,
        defaultSection],
      rbrace]

-- Statements --

assignDocD :: (RenderSym repr) => repr (Variable repr) -> repr (Value repr) -> 
  Doc
assignDocD vr vl = variableDoc vr <+> equals <+> valueDoc vl

multiAssignDoc :: (RenderSym repr) => [repr (Variable repr)] -> 
  [repr (Value repr)] -> Doc
multiAssignDoc vrs vls = variableList vrs <+> equals <+> valueList vls

plusEqualsDocD :: (RenderSym repr) => repr (Variable repr) -> repr (Value repr) 
  -> Doc
plusEqualsDocD vr vl = variableDoc vr <+> text "+=" <+> valueDoc vl

plusPlusDocD :: (RenderSym repr) => repr (Variable repr) -> Doc
plusPlusDocD v = variableDoc v <> text "++"

listDecDocD :: (RenderSym repr) => repr (Variable repr) -> repr (Value repr) -> 
  Doc
listDecDocD v n = space <> equals <+> new <+> getTypeDoc (variableType v) 
  <> parens (valueDoc n)

constDecDefDocD :: (RenderSym repr) => repr (Variable repr) -> 
  repr (Value repr) -> Doc
constDecDefDocD v def = text "const" <+> getTypeDoc (variableType v) <+> 
  variableDoc v <+> equals <+> valueDoc def

returnDocD :: (RenderSym repr) => [repr (Value repr)] -> Doc
returnDocD vs = text "return" <+> valueList vs

commentDocD :: Label -> Doc -> Doc
commentDocD cmt cStart = cStart <+> text cmt

statementDocD :: (Doc, Terminator) -> (Doc, Terminator)
statementDocD (s, t) = (s <> getTermDoc t, Empty)

getTermDoc :: Terminator -> Doc
getTermDoc Semi = semi
getTermDoc Empty = empty

mkSt :: (RenderSym repr) => Doc -> repr (Statement repr)
mkSt = flip stateFromData Semi

mkStNoEnd :: (RenderSym repr) => Doc -> repr (Statement repr)
mkStNoEnd = flip stateFromData Empty

mkStateVal :: (RenderSym repr) => VSType repr -> Doc -> SValue repr
mkStateVal t d = onStateValue (\tp -> valFromData Nothing tp d) t

mkVal :: (RenderSym repr) => repr (Type repr) -> Doc -> repr (Value repr)
mkVal = valFromData Nothing

mkStateVar :: (RenderSym repr) => String -> VSType repr -> Doc -> SVariable repr
mkStateVar n t d = onStateValue (\tp -> varFromData Dynamic n tp d) t

mkVar :: (RenderSym repr) => String -> repr (Type repr) -> Doc -> 
  repr (Variable repr)
mkVar = varFromData Dynamic

mkStaticVar :: (RenderSym repr) => String -> VSType repr -> Doc -> 
  SVariable repr
mkStaticVar n t d = onStateValue (\tp -> varFromData Static n tp d) t

-- Value Printers --

varDocD :: Label -> Doc
varDocD = text

extVarDocD :: Library -> Label -> Doc
extVarDocD l n = text l <> dot <> text n

selfDocD :: Doc
selfDocD = text "this"

argDocD :: (RenderSym repr) => repr (Value repr) -> repr (Value repr) -> Doc
argDocD n args = valueDoc args <> brackets (valueDoc n)

-- enumElemDocD :: Label -> Label -> Doc
-- enumElemDocD en e = text en <> dot <> text e

classVarCheckStatic :: (RenderSym repr) => repr (Variable repr) -> 
  repr (Variable repr)
classVarCheckStatic v = classVarCS (variableBind v)
  where classVarCS Dynamic = error
          "classVar can only be used to access static variables"
        classVarCS Static = v

classVarDocD :: Doc -> Doc -> Doc
classVarDocD c v = c <> dot <> v

objVarDocD :: Doc -> Doc ->  Doc
objVarDocD n1 n2 = n1 <> dot <> n2

funcAppDocD :: (RenderSym repr) => Label -> [repr (Value repr)] -> Doc
funcAppDocD n vs = text n <> parens (valueList vs)

newObjDocD :: (RenderSym repr) => repr (Type repr) -> Doc -> Doc
newObjDocD st vs = new <+> newObjDocD' st vs

newObjDocD' :: (RenderSym repr) => repr (Type repr) -> Doc -> Doc
newObjDocD' st vs = getTypeDoc st <> parens vs

-- Functions --

funcDocD :: Doc -> Doc
funcDocD fnApp = dot <> fnApp

castDocD :: Doc -> Doc
castDocD = parens

listAccessFuncDocD :: (RenderSym repr) => repr (Value repr) -> Doc
listAccessFuncDocD v = brackets $ valueDoc v

listSetFuncDocD :: Doc -> Doc -> Doc
listSetFuncDocD i v = brackets i <+> equals <+> v

objAccessDocD :: Doc -> Doc -> Doc
objAccessDocD v f = v <> f

castObjDocD :: Doc -> Doc -> Doc
castObjDocD t v = t <> parens v

-- Permanence --

staticDocD :: Doc
staticDocD = text "static"

dynamicDocD :: Doc
dynamicDocD = empty

bindingError :: String -> String
bindingError l = "Binding unimplemented in " ++ l

-- Jumps --

breakDocD :: Doc
breakDocD = text "break"

continueDocD :: Doc
continueDocD = text "continue"

-- Scope --

privateDocD :: Doc
privateDocD = text "private"

publicDocD :: Doc
publicDocD = text "public"

-- Comment Functions -- 

blockCmtDoc :: [String] -> Doc -> Doc -> Doc
blockCmtDoc lns start end = start <+> vcat (map text lns) <+> end

docCmtDoc :: [String] -> Doc -> Doc -> Doc
docCmtDoc lns start end = emptyIfNull lns $
  vcat $ start : map (indent . text) lns ++ [end]

commentedItem :: Doc -> Doc -> Doc
commentedItem cmt itm = emptyIfEmpty itm cmt $+$ itm

commentLength :: Int
commentLength = 75

endCommentLabel :: Label
endCommentLabel = "End"

addCommentsDocD :: Label -> Doc -> Doc -> Doc
addCommentsDocD c cStart b = vcat [
  commentDelimit c cStart,
  b,
  endCommentDelimit c cStart]

commentDelimit :: Label -> Doc -> Doc
commentDelimit c cStart = 
  let com = cStart <> text (" " ++ c ++ " ")
  in com <> text (dashes (render com) commentLength)

endCommentDelimit :: Label -> Doc -> Doc
endCommentDelimit c = commentDelimit (endCommentLabel ++ " " ++ c)

dashes :: String -> Int -> String
dashes s l = replicate (l - length s) '-'

functionDox :: String -> [(String, String)] -> [String] -> [String]
functionDox desc params returns = [doxBrief ++ desc | not (null desc)]
  ++ map (\(v, vDesc) -> doxParam ++ v ++ " " ++ vDesc) params
  ++ map (doxReturn ++) returns

classDox :: String -> [String]
classDox desc = [doxBrief ++ desc | not (null desc)]

moduleDox :: String -> [String] -> String -> String -> [String]
moduleDox desc as date m = (doxFile ++ m) : 
  [doxAuthor ++ stringList as | not (null as)] ++
  [doxDate ++ date | not (null date)] ++ 
  [doxBrief ++ desc | not (null desc)]

commentedModD :: FileData -> Doc -> FileData
commentedModD m cmt = updateFileMod (updateMod (commentedItem cmt) (fileMod m)) m

docFuncRepr :: (RenderSym repr) => String -> [String] -> [String] -> 
  SMethod repr -> SMethod repr
docFuncRepr desc pComms rComms = commentedFunc (docComment $ onStateValue 
  (\ps -> functionDox desc (zip ps pComms) rComms) getParameters)

-- Helper Functions --

valueList :: (RenderSym repr) => [repr (Value repr)] -> Doc
valueList = hicat (text ", ") . map valueDoc

variableList :: (RenderSym repr) => [repr (Variable repr)] -> Doc
variableList = hicat (text ", ") . map variableDoc

parameterList :: (RenderSym repr) => [repr (Parameter repr)] -> Doc
parameterList = hicat (text ", ") . map parameterDoc

prependToBody :: (Doc, Terminator) -> Doc -> Doc
prependToBody s b = vcat [fst $ statementDocD s, maybeBlank, b]
  where maybeBlank = emptyIfEmpty (fst s) (emptyIfEmpty b blank)

appendToBody :: Doc -> (Doc, Terminator) -> Doc
appendToBody b s = vcat [b, maybeBlank, fst $ statementDocD s]
  where maybeBlank = emptyIfEmpty b (emptyIfEmpty (fst s) blank)

surroundBody :: (Doc, Terminator) -> Doc -> (Doc, Terminator) -> Doc
surroundBody p b a = prependToBody p (appendToBody b a)

getterName :: String -> String
getterName s = "get" ++ capitalize s

setterName :: String -> String
setterName s = "set" ++ capitalize s

intValue :: (RenderSym repr) => SValue repr -> SValue repr
intValue i = i >>= intValue' . getType . valueType
  where intValue' Integer = i
        -- intValue' (Enum _) = cast S.int i
        intValue' _ = error "Value passed must be Integer or Enum"

doxCommand, doxBrief, doxParam, doxReturn, doxFile, doxAuthor, doxDate :: String
doxCommand = "\\"
doxBrief = doxCommand ++ "brief "
doxParam = doxCommand ++ "param "
doxReturn = doxCommand ++ "return "
doxFile = doxCommand  ++ "file "
doxAuthor = doxCommand ++ "author "
doxDate = doxCommand ++ "date "
