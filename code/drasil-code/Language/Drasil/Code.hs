-- | re-export smart constructors for external code writing
module Language.Drasil.Code (
  makeCode, createCodeFiles, 
  generator, generateCode,
  readWithDataDesc, sampleInputDD,
  ($:=), Choices(..), CodeSpec(..), CodeSystInfo(..), Comments(..), 
  ConstraintBehaviour(..), Func, FuncStmt(..), ImplementationType(..), Lang(..),
  Logging(LogNone, LogAll), Mod(Mod), Structure(..), ConstantStructure(..), 
  ConstantRepr(..), InputModule(..), CodeConcept(..), matchConcepts, 
  AuxFile(..), Visibility(..),
  asExpr, asExpr', asVC, asVC', codeSpec, fdec, ffor, funcData, funcDef, 
  packmod, relToQD,
  junkLine, multiLine, repeated, singleLine, singleton,
  PackageSym(..),
  quantvar,
  unPP, unJP, unCSP, unCPPP
) where

import Prelude hiding (break, print, return, log, exp)

import Language.Drasil.Code.Imperative.Generator (generator, generateCode)

import Language.Drasil.Code.Imperative.ReadInput (readWithDataDesc, 
  sampleInputDD)

import Language.Drasil.Code.CodeGeneration (makeCode, createCodeFiles)

import Language.Drasil.Code.DataDesc (junkLine, multiLine, repeated, singleLine,
  singleton)

import Language.Drasil.CodeSpec (($:=), Choices(..), CodeSpec(..), 
  CodeSystInfo(..), Comments(..), ConstraintBehaviour(..), Func, FuncStmt(..),
  ImplementationType(..), Lang(..), Logging(..), Mod(Mod), Structure(..), 
  ConstantStructure(..), ConstantRepr(..), InputModule(..), CodeConcept(..), 
  matchConcepts, AuxFile(..), Visibility(..), asExpr, asExpr', asVC, asVC', 
  codeSpec, fdec, ffor, funcData, funcDef, packmod, relToQD)

import Language.Drasil.Code.Imperative.GOOL.Symantics (PackageSym(..))

import Language.Drasil.Chunk.Code (quantvar)

import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.PythonRenderer (unPP)
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.JavaRenderer (unJP)
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.CSharpRenderer (unCSP)
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.CppRenderer (unCPPP)
