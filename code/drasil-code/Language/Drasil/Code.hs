-- | re-export smart constructors for external code writing
module Language.Drasil.Code (
  makeCode, createCodeFiles, 
  generator, generateCode,
  ($:=), Choices(..), CodeSpec, Comments(CommentNone), ConstraintBehaviour(..), Func, 
  FuncStmt(..), ImplementationType(..), Lang(..), Logging(LogNone), Mod(Mod), Structure(..),
  asExpr, asExpr', asVC, asVC', codeSpec, fdec, ffor, funcData, funcDef, packmod, relToQD,
  Ind(..), junk, junkLine, listEntry, multiLine, repeated, singleLine, singleton,
  unJC, unPC, unCSC, unSrc, unHdr
) where

import Prelude hiding (break, print, return, log, exp)

import Language.Drasil.Code.Imperative.Import (generator, generateCode)

import Language.Drasil.Code.CodeGeneration (makeCode, createCodeFiles)

import Language.Drasil.Code.DataDesc (Ind(..), junk, junkLine, listEntry, multiLine, repeated, singleLine, singleton)

import Language.Drasil.CodeSpec (($:=), Choices(..), CodeSpec, Comments(..), ConstraintBehaviour(..), 
  Func, FuncStmt(..), ImplementationType(..), Lang(..), Logging(..), Mod(Mod), Structure(..), 
  asExpr, asExpr', asVC, asVC', codeSpec, fdec, ffor, funcData, funcDef, packmod, relToQD,
  )

import Language.Drasil.Code.Imperative.LanguageRenderer.NewJavaRenderer (unJC)
import Language.Drasil.Code.Imperative.LanguageRenderer.NewPythonRenderer (unPC)
import Language.Drasil.Code.Imperative.LanguageRenderer.NewCSharpRenderer (unCSC)
import Language.Drasil.Code.Imperative.LanguageRenderer.NewCppRenderer (unSrc, unHdr)
