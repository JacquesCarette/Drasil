{-# LANGUAGE PostfixOperators #-}

-- | The structure for a class of renderers is defined here.
module GOOL.Drasil.LanguageRenderer (
  -- * Common Syntax
  classDec, dot, commentStart, elseIfLabel, forLabel, inLabel, new,
  blockCmtStart, blockCmtEnd, docCmtStart, bodyStart, bodyEnd, endStatement, 
  addExt,
  
  -- * Default Functions available for use in renderers
  package, file, module', class', multiStmt, block, body, print, printFile, 
  param, method, stateVar, constVar, stateVarList, switch, assign, multiAssign, 
  addAssign, increment, listDec, getTerm, return', comment, var, extVar, self, 
  arg, classVar, objVar, constDecDef, func, cast, listAccessFunc, listSetFunc, 
  objAccess, castObj, break, continue, static, dynamic, private, public, 
  blockCmt, docCmt, commentedItem, addComments, functionDox, classDox, 
  moduleDox, commentedMod, valueList, variableList, parameterList, 
  prependToBody, appendToBody, surroundBody, getterName, setterName, intValue
) where

import Utils.Drasil (blank, capitalize, indent, indentList, stringList)

import GOOL.Drasil.CodeType (CodeType(..))
import GOOL.Drasil.ClassInterface (Label, Library, SValue, BodySym(Body), 
  PermanenceSym(Permanence), TypeSym(Type), TypeElim(..), VariableSym(Variable),
  VariableElim(..), ValueSym(..), StatementSym(Statement), ScopeSym(Scope), 
  ParameterSym(Parameter))
import GOOL.Drasil.RendererClasses (RenderSym)
import qualified GOOL.Drasil.RendererClasses as RC (PermElim(..), BodyElim(..),
  InternalTypeElim(..), InternalVarElim(..), ValueElim(..), StatementElim(..),
  ScopeElim(..), ParamElim(..))
import GOOL.Drasil.AST (Terminator(..), FileData(..), fileD, updateFileMod, 
  updateMod, TypeData(..), VarData(..))
import GOOL.Drasil.Helpers (hicat, vibcat, vmap, emptyIfEmpty, emptyIfNull)

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

package :: Label -> Doc -> FileData -> FileData
package n end f = fileD (n ++ "/" ++ filePath f) (updateMod 
  (\d -> emptyIfEmpty d (vibcat [text "package" <+> text n <> end, d])) 
  (fileMod f))

file :: Doc -> Doc -> Doc -> Doc
file t m b = vibcat [
  t,
  m,
  b]

-- Many function names in this module conflict with names of functions that are 
-- part of GOOL's interface. This module is thus intended to be imported 
-- qualified.

-----------------------------------------------
-- 'Default' functions used in the renderers --
-----------------------------------------------

-- Module --

module' :: Doc -> Doc -> Doc -> Doc
module' ls fs cs = emptyIfEmpty (fs <> cs) (vibcat (filter (not . isEmpty) 
  [ls, fs, cs]))

-- Class --

class' :: Label -> Doc -> Doc -> Doc -> Doc -> Doc
class' n p s vs fs = vcat [
  s <+> classDec <+> text n <+> p <+> lbrace, 
  indentList [
    vs,
    blank,
    fs],
  rbrace]

-- Groupings --

multiStmt :: [(Doc, Terminator)] -> (Doc, Terminator)
multiStmt sts = (vcat (applyEnd statements), needsEnd statements)
  where applyEnd [] = []
        applyEnd [(s, _)] = [s]
        applyEnd ((s, t):ss) = (s <> getTerm t) : applyEnd ss
        needsEnd [] = Empty
        needsEnd ss = snd (last ss)
        statements = filter notNullStatement sts
        notNullStatement s = not (isEmpty (fst s))

block :: [Doc] -> Doc
block sts = vcat $ filter (not . isEmpty) sts

body :: [Doc] -> Doc
body bs = vibcat $ filter (not . isEmpty) bs

-- IO --

print :: (RenderSym r) => r (Value r) -> r (Value r) -> Doc
print printFn v = RC.value printFn <> parens (RC.value v)

printFile :: Label -> Doc -> Doc
printFile fn f = f <> dot <> text fn

-- Parameters --

param :: (RenderSym r) => r (Variable r) -> Doc
param v = RC.type' (variableType v) <+> RC.variable v

-- Method --

method :: (RenderSym r) => Label -> r (Scope r) -> r (Permanence r) -> 
  r (Type r) -> [r (Parameter r)] -> r (Body r) -> Doc
method n s p t ps b = vcat [
  RC.scope s <+> RC.perm p <+> RC.type' t <+> text n <> 
    parens (parameterList ps) <+> lbrace,
  indent (RC.body b),
  rbrace]

-- StateVar --

stateVar :: Doc -> Doc -> Doc -> Doc
stateVar s p dec = s <+> p <+> dec

constVar :: Doc -> Doc -> Doc -> VarData -> Doc
constVar s end p v = s <+> p <+> text "const" <+> typeDoc (varType v) <+>
  varDoc v <> end

stateVarList :: [Doc] -> Doc
stateVarList = vcat

-- Controls --

switch :: (RenderSym r) => r (Statement r) -> r (Value r) -> r (Body r) -> 
  [(r (Value r), r (Body r))] -> Doc
switch breakState v defBody cs = 
  let caseDoc (l, result) = vcat [
        text "case" <+> RC.value l <> colon,
        indentList [
          RC.body result,
          RC.statement breakState]]
      defaultSection = vcat [
        text "default" <> colon,
        indentList [
          RC.body defBody,
          RC.statement breakState]]
  in vcat [
      text "switch" <> parens (RC.value v) <+> lbrace,
      indentList [
        vmap caseDoc cs,
        defaultSection],
      rbrace]

-- Statements --

assign :: (RenderSym r) => r (Variable r) -> r (Value r) -> Doc
assign vr vl = RC.variable vr <+> equals <+> RC.value vl

multiAssign :: (RenderSym r) => [r (Variable r)] -> [r (Value r)] -> Doc
multiAssign vrs vls = variableList vrs <+> equals <+> valueList vls

addAssign :: (RenderSym r) => r (Variable r) -> r (Value r) -> Doc
addAssign vr vl = RC.variable vr <+> text "+=" <+> RC.value vl

increment :: (RenderSym r) => r (Variable r) -> Doc
increment v = RC.variable v <> text "++"

listDec :: (RenderSym r) => r (Variable r) -> r (Value r) -> Doc
listDec v n = space <> equals <+> new <+> RC.type' (variableType v) 
  <> parens (RC.value n)

constDecDef :: (RenderSym r) => r (Variable r) -> r (Value r) -> Doc
constDecDef v def = text "const" <+> RC.type' (variableType v) <+> 
  RC.variable v <+> equals <+> RC.value def

return' :: (RenderSym r) => [r (Value r)] -> Doc
return' vs = text "return" <+> valueList vs

comment :: Label -> Doc -> Doc
comment cmt cStart = cStart <+> text cmt

statement :: (Doc, Terminator) -> (Doc, Terminator)
statement (s, t) = (s <> getTerm t, Empty)

getTerm :: Terminator -> Doc
getTerm Semi = semi
getTerm Empty = empty

-- Value Printers --

var :: Label -> Doc
var = text

extVar :: Library -> Label -> Doc
extVar l n = text l <> dot <> text n

self :: Doc
self = text "this"

arg :: (RenderSym r) => r (Value r) -> r (Value r) -> Doc
arg n args = RC.value args <> brackets (RC.value n)

classVar :: Doc -> Doc -> Doc
classVar c v = c <> dot <> v

objVar :: Doc -> Doc ->  Doc
objVar n1 n2 = n1 <> dot <> n2

-- Functions --

func :: Doc -> Doc
func fnApp = dot <> fnApp

cast :: Doc -> Doc
cast = parens

listAccessFunc :: (RenderSym r) => r (Value r) -> Doc
listAccessFunc v = brackets $ RC.value v

listSetFunc :: Doc -> Doc -> Doc
listSetFunc i v = brackets i <+> equals <+> v

objAccess :: Doc -> Doc -> Doc
objAccess v f = v <> f

castObj :: Doc -> Doc -> Doc
castObj t v = t <> parens v

-- Permanence --

static :: Doc
static = text "static"

dynamic :: Doc
dynamic = empty

-- Jumps --

break :: Doc
break = text "break"

continue :: Doc
continue = text "continue"

-- Scope --

private :: Doc
private = text "private"

public :: Doc
public = text "public"

-- Comment Functions -- 

blockCmt :: [String] -> Doc -> Doc -> Doc
blockCmt lns start end = start <+> vcat (map text lns) <+> end

docCmt :: [String] -> Doc -> Doc -> Doc
docCmt lns start end = emptyIfNull lns $
  vcat $ start : map (indent . text) lns ++ [end]

commentedItem :: Doc -> Doc -> Doc
commentedItem cmt itm = emptyIfEmpty itm cmt $+$ itm

commentLength :: Int
commentLength = 75

endCommentLabel :: Label
endCommentLabel = "End"

addComments :: Label -> Doc -> Doc -> Doc
addComments c cStart b = vcat [
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

commentedMod :: FileData -> Doc -> FileData
commentedMod m cmt = updateFileMod (updateMod (commentedItem cmt) (fileMod m)) m

-- Helper Functions --

valueList :: (RenderSym r) => [r (Value r)] -> Doc
valueList = hicat (text ", ") . map RC.value

variableList :: (RenderSym r) => [r (Variable r)] -> Doc
variableList = hicat (text ", ") . map RC.variable

parameterList :: (RenderSym r) => [r (Parameter r)] -> Doc
parameterList = hicat (text ", ") . map RC.parameter

prependToBody :: (Doc, Terminator) -> Doc -> Doc
prependToBody s b = vcat [fst $ statement s, maybeBlank, b]
  where maybeBlank = emptyIfEmpty (fst s) (emptyIfEmpty b blank)

appendToBody :: Doc -> (Doc, Terminator) -> Doc
appendToBody b s = vcat [b, maybeBlank, fst $ statement s]
  where maybeBlank = emptyIfEmpty b (emptyIfEmpty (fst s) blank)

surroundBody :: (Doc, Terminator) -> Doc -> (Doc, Terminator) -> Doc
surroundBody p b a = prependToBody p (appendToBody b a)

getterName :: String -> String
getterName s = "get" ++ capitalize s

setterName :: String -> String
setterName s = "set" ++ capitalize s

intValue :: (RenderSym r) => SValue r -> SValue r
intValue i = i >>= intValue' . getType . valueType
  where intValue' Integer = i
        intValue' _ = error "Value passed to intValue must be Integer"

doxCommand, doxBrief, doxParam, doxReturn, doxFile, doxAuthor, doxDate :: String
doxCommand = "\\"
doxBrief = doxCommand ++ "brief "
doxParam = doxCommand ++ "param "
doxReturn = doxCommand ++ "return "
doxFile = doxCommand  ++ "file "
doxAuthor = doxCommand ++ "author "
doxDate = doxCommand ++ "date "
