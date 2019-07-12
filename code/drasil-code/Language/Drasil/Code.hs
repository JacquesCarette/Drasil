-- | re-export smart constructors for external code writing
module Language.Drasil.Code (
  makeCode, createCodeFiles, 
  generator, generateCode,
  ($:=), Choices(..), CodeSpec, Comments(CommentNone), ConstraintBehaviour(..), Func, 
  FuncStmt(..), ImplementationType(..), Lang(..), Logging(LogNone, LogAll), Mod(Mod), Structure(..),
  asExpr, asExpr', asVC, asVC', codeSpec, fdec, ffor, funcData, funcDef, packmod, relToQD,
  junkLine, multiLine, repeated, singleLine, singleton,
  PackageSym(..), RenderSym(..), 
  PermanenceSym(..), BodySym(..), BlockSym(..), ControlBlockSym(..), 
  StateTypeSym(..), StatementSym(..), ControlStatementSym(..),  ValueSym(..), 
  NumericExpression(..), BooleanExpression(..), ValueExpression(..), 
  Selector(..), FunctionSym(..), SelectorFunction(..),
  MethodSym(..), ModuleSym(..), BlockCommentSym(..),
  ModData(..),
  JavaCode(..), PythonCode(..), CSharpCode(..), CppSrcCode(..), CppHdrCode(..),
  unSrc, unHdr
) where

import Prelude hiding (break, print, return, log, exp)

import Language.Drasil.Code.Imperative.Import (generator, generateCode)

import Language.Drasil.Code.CodeGeneration (makeCode, createCodeFiles)

import Language.Drasil.Code.DataDesc (junkLine, multiLine, repeated, singleLine,
  singleton)

import Language.Drasil.CodeSpec (($:=), Choices(..), CodeSpec, Comments(..), ConstraintBehaviour(..), 
  Func, FuncStmt(..), ImplementationType(..), Lang(..), Logging(..), Mod(Mod), Structure(..), 
  asExpr, asExpr', asVC, asVC', codeSpec, fdec, ffor, funcData, funcDef, packmod, relToQD,
  )

import Language.Drasil.Code.Imperative.Symantics (PackageSym(..), RenderSym(..), 
  PermanenceSym(..), BodySym(..), BlockSym(..), ControlBlockSym(..), 
  StateTypeSym(..), StatementSym(..), ControlStatementSym(..),  ValueSym(..), 
  NumericExpression(..), BooleanExpression(..), ValueExpression(..), 
  Selector(..), FunctionSym(..), SelectorFunction(..),
  MethodSym(..), ModuleSym(..), BlockCommentSym(..))

import Language.Drasil.Code.Imperative.Data (ModData(..))

import Language.Drasil.Code.Imperative.LanguageRenderer.JavaRenderer 
  (JavaCode (..))
import Language.Drasil.Code.Imperative.LanguageRenderer.PythonRenderer 
  (PythonCode(..))
import Language.Drasil.Code.Imperative.LanguageRenderer.CSharpRenderer 
  (CSharpCode(..))
import Language.Drasil.Code.Imperative.LanguageRenderer.CppRenderer 
  (CppSrcCode(..), CppHdrCode(..), unSrc, unHdr)
