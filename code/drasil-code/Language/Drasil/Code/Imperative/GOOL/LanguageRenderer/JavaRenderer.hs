{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render Java code is contained in this module
module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.JavaRenderer (
  -- * Java Code Configuration -- defines syntax of all Java code
  JavaCode(..), jNameOpts
) where

import Utils.Drasil (indent)

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
  packageDocD, fileDoc', moduleDocD, classDocD, enumDocD, enumElementsDocD, 
  multiStateDocD, blockDocD, bodyDocD, outDoc, printDoc, printFileDocD, 
  boolTypeDocD, intTypeDocD, charTypeDocD, typeDocD, enumTypeDocD, listTypeDocD,
  voidDocD, constructDocD, stateParamDocD, paramListDocD, mkParam, 
  methodListDocD, stateVarDocD, stateVarDefDocD, stateVarListDocD, ifCondDocD, 
  switchDocD, forDocD, forEachDocD, whileDocD, stratDocD, assignDocD, 
  plusEqualsDocD, plusPlusDocD, varDecDocD, varDecDefDocD, listDecDocD, 
  objDecDefDocD, statementDocD, returnDocD, commentDocD, mkSt, mkStNoEnd, 
  stringListVals', stringListLists', unOpPrec, notOpDocD, negateOpDocD, unExpr, 
  unExpr', typeUnExpr, powerPrec, equalOpDocD, notEqualOpDocD, greaterOpDocD, 
  greaterEqualOpDocD, lessOpDocD, lessEqualOpDocD, plusOpDocD, minusOpDocD, 
  multOpDocD, divideOpDocD, moduloOpDocD, andOpDocD, orOpDocD, binExpr, 
  binExpr', typeBinExpr, mkVal, mkVar, mkStaticVar, litTrueD, litFalseD, 
  litCharD, litFloatD, litIntD, litStringD, varDocD, extVarDocD, selfDocD, 
  argDocD, enumElemDocD, classVarCheckStatic, classVarD, classVarDocD, 
  objVarDocD, inlineIfD, funcAppDocD, extFuncAppDocD, stateObjDocD, 
  listStateObjDocD, notNullDocD, funcDocD, castDocD, objAccessDocD, castObjDocD,
  breakDocD, continueDocD, staticDocD, dynamicDocD, privateDocD, publicDocD, 
  dot, new, forLabel, blockCmtStart, blockCmtEnd, docCmtStart, observerListName,
  doxConfigName, makefileName, sampleInputName, doubleSlash, blockCmtDoc, 
  docCmtDoc, commentedItem, addCommentsDocD, functionDoc, classDoc, moduleDoc, 
  docFuncRepr, valList, appendToBody, surroundBody, getterName, setterName, 
  setMainMethod, setEmpty, intValue, filterOutObjs)
import Language.Drasil.Code.Imperative.GOOL.Data (Terminator(..), AuxData(..), 
  ad, FileData(..), file, updateFileMod, FuncData(..), fd, ModData(..), md, 
  updateModDoc, MethodData(..), mthd, OpData(..), ParamData(..), pd, 
  PackData(..), packD, ProgData(..), progD, TypeData(..), td, ValData(..), 
  VarData(..), vard)
import Language.Drasil.Code.Imperative.Doxygen.Import (makeDoxConfig)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig, Runnable, 
  NameOpts(NameOpts), asFragment, buildSingle, includeExt, inCodePackage, 
  interp, mainModule, mainModuleFile, packSep, withExt)
import Language.Drasil.Code.Imperative.Build.Import (makeBuild)
import Language.Drasil.Code.Imperative.GOOL.Helpers (angles, emptyIfEmpty, 
  liftA4, liftA5, liftA6, liftA7, liftList, lift1List, lift3Pair, 
  lift4Pair, liftPair, liftPairFst, getInnerType, convType, checkParams)
import Language.Drasil.Code.Imperative.WriteInput (makeInputFile)

import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import qualified Data.Map as Map (fromList,lookup)
import Data.Maybe (fromMaybe, maybeToList)
import Control.Applicative (Applicative, liftA2, liftA3)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), parens, empty, equals,
  semi, vcat, lbrace, rbrace, render, colon, comma, render)

jNameOpts :: NameOpts
jNameOpts = NameOpts {
  packSep = ".",
  includeExt = False
}

instance PackageSym JavaCode where
  type Package JavaCode = PackData
  package = lift1List packD

instance AuxiliarySym JavaCode where
  type Auxiliary JavaCode = AuxData
  doxConfig pName p = fmap (ad doxConfigName) (liftA2 (makeDoxConfig pName)
    optimizeDox p)
  sampleInput db d sd = return $ ad sampleInputName (makeInputFile db d sd)

  optimizeDox = return $ text "YES"

  makefile cms = fmap (ad makefileName . makeBuild cms jBuildConfig jRunnable)

jBuildConfig :: Maybe BuildConfig
jBuildConfig = buildSingle (\i _ -> asFragment "javac" : i) $
  inCodePackage mainModuleFile

jRunnable :: Runnable
jRunnable = interp (flip withExt ".class" $ inCodePackage mainModule) 
  jNameOpts "java"