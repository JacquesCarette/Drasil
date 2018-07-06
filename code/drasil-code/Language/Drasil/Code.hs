-- | re-export smart constructors for external code writing
module Language.Drasil.Code (
  Body, Class, StateVar, Value, Parameter, Module, FunctionDecl,
  Label, StateType, Library, Statement,
  bool, int, float, char, string, infile, outfile, listT, obj,
  arg, self,
  methodType, methodTypeVoid, block, defaultValue, true, false,
  pubClass, privClass, privMVar, pubMVar, pubGVar, privMethod, pubMethod, constructor,
  mainMethod,
  (?!), (?<), (?<=), (?>), (?>=), (?==), (?!=), (?&&), (?||),
  (#~), (#/^), (#|), (#+), (#-), (#*), (#/), (#%), (#^),
  (&=), (&.=), (&=.), (&+=), (&-=), (&++), (&~-), (&.+=), (&.-=), (&.++), (&.~-),
  ($->), ($.), ($:), log, exp,
  alwaysDel, neverDel, assign, at, binExpr, break, cast, constDecDef, extends, for,
  forEach, ifCond, ifExists, listDec, listDec', listDecValues, listOf, litBool, litChar,
  litFloat, litInt, litObj, litObj', litString, noElse, noParent, objDecDef, oneLiner, param,
  params, paramToVar, print, printLn, printStr, printStrLn, printFile, printFileLn, printFileStr,
  openFileR, openFileW, closeFile, getInput, getFileInput, getFileInputAll, getFileInputLine,
  printFileStrLn, return, returnVar, switch, throw, tryCatch, typ, varDec, varDecDef,
  while, zipBlockWith, zipBlockWith4, addComments, comment, commentDelimit,
  endCommentDelimit, prefixFirstBlock, getterName, setterName, convertToClass,
  convertToMethod, bodyReplace, funcReplace, valListReplace, objDecNew,
  objDecNewVoid, objDecNew', objDecNewVoid',
  listSize, listAccess, listAppend, listSlice, stringSplit,
  var, svToVar, objMethodCall, objMethodCallVoid, valStmt, funcApp, funcApp', func, continue,
  cSharpLabel, cppLabel, goolLabel, javaLabel, objectiveCLabel, pythonLabel, luaLabel,
  makeCode, createCodeFiles, toAbsCode, getClassName, buildModule, moduleName,
  Options(..),
  ($:=), Choices(..), CodeSpec, Comments(CommentNone), ConstraintBehaviour(..), Func, 
  FuncStmt(..), ImplementationType(..), Lang(..), Logging(LogNone), Mod(Mod), Structure(..),
  asExpr, asVC, codeSpec, fdec, ffor, funcData, funcDef, packmod, relToQD,
  gen, genCode, 
  Ind(..), junk, junkLine, listEntry, multiLine, repeated, singleLine, singleton
) where

import Prelude hiding (break, print, return, log, exp)
import Language.Drasil.Code.Imperative.AST (alwaysDel, neverDel, assign, at, binExpr, 
  break, cast, constDecDef, extends, for,
  forEach, ifCond, ifExists, listDec, listDec', listDecValues, listOf, litBool, litChar,
  litFloat, litInt, litObj, litObj', litString, noElse, noParent, objDecDef, oneLiner, param,
  params, paramToVar, print, printLn, printStr, printStrLn, printFile, printFileLn, printFileStr,
  openFileR, openFileW, closeFile, getInput, getFileInput, getFileInputAll, getFileInputLine,
  printFileStrLn, return, returnVar, switch, throw, tryCatch, typ, varDec, varDecDef,
  while, zipBlockWith, zipBlockWith4, addComments, comment, commentDelimit,
  endCommentDelimit, prefixFirstBlock, getterName, setterName, convertToClass,
  convertToMethod, bodyReplace, funcReplace, valListReplace, objDecNew,
  objDecNewVoid, objDecNew', objDecNewVoid',
  listSize, listAccess, listAppend, listSlice, stringSplit,
  var, svToVar, objMethodCall, objMethodCallVoid, valStmt, funcApp, funcApp', func, continue,
  toAbsCode, getClassName, buildModule, moduleName, 
  (?!), (?<), (?<=), (?>), (?>=), (?==), (?!=), (?&&), (?||),
  (#~), (#/^), (#|), (#+), (#-), (#*), (#/), (#%), (#^),
  (&=), (&.=), (&=.), (&+=), (&-=), (&++), (&~-), (&.+=), (&.-=), (&.++), (&.~-),
  ($->), ($.), ($:), log, exp,
  Body, Class, StateVar, Value, Parameter, Module, FunctionDecl,
  Label, StateType, Library, Statement,
  bool, int, float, char, string, infile, outfile, listT, obj,
  arg, self,
  methodType, methodTypeVoid, block, defaultValue, true, false,
  pubClass, privClass, privMVar, pubMVar, pubGVar, privMethod, pubMethod, constructor,
  mainMethod)

import Language.Drasil.Code.Imperative.LanguageRenderer (Options(..))

import Language.Drasil.Code.Imperative.Parsers.ConfigParser (cSharpLabel, cppLabel, 
  goolLabel, javaLabel, objectiveCLabel, pythonLabel, luaLabel)

import Language.Drasil.Code.CodeGeneration (makeCode, createCodeFiles)

import Language.Drasil.Code.DataDesc (Ind(..), junk, junkLine, listEntry, multiLine, repeated, singleLine, singleton)

import Language.Drasil.CodeSpec (($:=), Choices(..), CodeSpec, Comments(..), ConstraintBehaviour(..), 
  Func, FuncStmt(..), ImplementationType(..), Lang(..), Logging(..), Mod(Mod), Structure(..), 
  asExpr, asVC, codeSpec, fdec, ffor, funcData, funcDef, packmod, relToQD,
  )

import Language.Drasil.Generate (gen, genCode)
