{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render C++ code is contained in this module
module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.CppRenderer (
  -- * C++ Code Configuration -- defines syntax of all C++ code
  CppSrcCode(..), CppHdrCode(..), CppCode(..), unCPPC
) where

import Utils.Drasil (indent, indentList)

import Language.Drasil.Code.Code (CodeType(..), isObject)
import Language.Drasil.Code.Imperative.GOOL.Symantics (Label, PackageSym(..), 
  ProgramSym(..), RenderSym(..), InternalFile(..), AuxiliarySym(..), 
  KeywordSym(..), PermanenceSym(..), BodySym(..), BlockSym(..), 
  ControlBlockSym(..), StateTypeSym(..), UnaryOpSym(..), BinaryOpSym(..), 
  VariableSym(..), ValueSym(..), NumericExpression(..), BooleanExpression(..), 
  ValueExpression(..), InternalValue(..), Selector(..), FunctionSym(..), 
  SelectorFunction(..), InternalFunction(..), InternalStatement(..), 
  StatementSym(..), ControlStatementSym(..), ScopeSym(..), InternalScope(..), 
  MethodTypeSym(..), ParameterSym(..), MethodSym(..), StateVarSym(..), 
  ClassSym(..), ModuleSym(..), BlockCommentSym(..))
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer (addExt,
  fileDoc', enumElementsDocD, multiStateDocD, blockDocD, bodyDocD, outDoc,
  intTypeDocD, charTypeDocD, stringTypeDocD, typeDocD, enumTypeDocD, 
  listTypeDocD, voidDocD, constructDocD, stateParamDocD, paramListDocD, mkParam,
  methodListDocD, stateVarDocD, stateVarDefDocD, constVarDocD, alwaysDel, 
  ifCondDocD, switchDocD, forDocD, whileDocD, stratDocD, assignDocD, 
  plusEqualsDocD, plusPlusDocD, varDecDocD, varDecDefDocD, objDecDefDocD, 
  constDecDefDocD, statementDocD, returnDocD, commentDocD, freeDocD, mkSt, 
  mkStNoEnd, stringListVals', stringListLists', unOpPrec, notOpDocD, 
  negateOpDocD, sqrtOpDocD, absOpDocD, expOpDocD, sinOpDocD, cosOpDocD, 
  tanOpDocD, asinOpDocD, acosOpDocD, atanOpDocD, unExpr, unExpr', typeUnExpr, 
  equalOpDocD, notEqualOpDocD, greaterOpDocD, greaterEqualOpDocD, lessOpDocD, 
  lessEqualOpDocD, plusOpDocD, minusOpDocD, multOpDocD, divideOpDocD, 
  moduloOpDocD, powerOpDocD, andOpDocD, orOpDocD, binExpr, binExpr', 
  typeBinExpr, mkVal, mkVar, mkStaticVar, litTrueD, litFalseD, litCharD, 
  litFloatD, litIntD, litStringD, varDocD, selfDocD, argDocD, 
  classVarCheckStatic, objVarDocD, inlineIfD, funcAppDocD, funcDocD, castDocD, 
  objAccessDocD, castObjDocD, breakDocD, continueDocD, staticDocD, dynamicDocD,
  privateDocD, publicDocD, classDec, dot, blockCmtStart, blockCmtEnd, 
  docCmtStart, observerListName, doxConfigName, makefileName, sampleInputName, 
  doubleSlash, blockCmtDoc, docCmtDoc, commentedItem, addCommentsDocD, 
  functionDoc, classDoc, moduleDoc, docFuncRepr, valList, appendToBody, 
  surroundBody, getterName, setterName, setEmpty, intValue, filterOutObjs)
import Language.Drasil.Code.Imperative.GOOL.Data (Pair(..), pairList, 
  Terminator(..), ScopeTag(..), Binding(..), AuxData(..), ad, emptyAux, 
  BindData(..), bd, FileData(..), srcFile, hdrFile, updateFileMod, FuncData(..),
  fd, ModData(..), md, updateModDoc, OpData(..), od, PackData(..), packD, 
  emptyPack, ParamData(..), pd, ProgData(..), progD, emptyProg, 
  StateVarData(..), svd, TypeData(..), td, ValData(..), VarData(..), vard)
import Language.Drasil.Code.Imperative.Doxygen.Import (makeDoxConfig)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig, Runnable, 
  asFragment, buildAll, cppCompiler, nativeBinary)
import Language.Drasil.Code.Imperative.Build.Import (makeBuild)
import Language.Drasil.Code.Imperative.GOOL.Helpers (angles, blank, doubleQuotedText,
  emptyIfEmpty, mapPairFst, mapPairSnd, vibcat, liftA4, liftA5, liftA6, liftA8,
  liftList, lift2Lists, lift1List, lift3Pair, lift4Pair, liftPair, liftPairFst, 
  getInnerType, convType, checkParams)
import Language.Drasil.Code.Imperative.WriteInput (makeInputFile)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor,const,log,exp)
import qualified Data.Map as Map (fromList,lookup)
import Data.Maybe (fromMaybe, maybeToList)
import Control.Applicative (Applicative, liftA2, liftA3)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), braces, parens, comma,
  empty, equals, semi, vcat, lbrace, rbrace, quotes, render, colon, isEmpty)

instance (Pair p) => PackageSym (p CppSrcCode CppHdrCode) where
  type Package (p CppSrcCode CppHdrCode) = PackData
  package p aux = pair (package (pfst p) (map pfst aux)) (return emptyPack)

instance (Pair p) => AuxiliarySym (p CppSrcCode CppHdrCode) where
  type Auxiliary (p CppSrcCode CppHdrCode) = AuxData
  doxConfig pName p = pair (doxConfig pName $ pfst p) (return emptyAux)
  sampleInput db d sd = pair (sampleInput db d sd) (return emptyAux)

  optimizeDox = pair optimizeDox (return empty)

  makefile cms p = pair (makefile cms $ pfst p) (return emptyAux)

-----------------
-- Source File --
-----------------

instance PackageSym CppSrcCode where
  type Package CppSrcCode = PackData
  package = lift1List packD

instance AuxiliarySym CppSrcCode where
  type Auxiliary CppSrcCode = AuxData
  doxConfig pName p = fmap (ad doxConfigName) (liftA2 (makeDoxConfig pName)
    optimizeDox p)
  sampleInput db d sd = return $ ad sampleInputName (makeInputFile db d sd)

  optimizeDox = return $ text "NO"
  
  makefile cms = fmap (ad makefileName . makeBuild cms cppBuildConfig 
    cppRunnable)

-- helpers

cppBuildConfig :: Maybe BuildConfig
cppBuildConfig = buildAll $ \i o -> cppCompiler : i ++ map asFragment
  ["--std=c++11", "-o"] ++ [o]

cppRunnable :: Runnable
cppRunnable = nativeBinary