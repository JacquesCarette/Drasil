{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render C# code is contained in this module
module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.CSharpRenderer (
  -- * C# Code Configuration -- defines syntax of all C# code
  CSharpCode(..)
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
  fileDoc', moduleDocD, classDocD, enumDocD, enumElementsDocD, multiStateDocD,
  blockDocD, bodyDocD, printDoc, outDoc, printFileDocD, boolTypeDocD, 
  intTypeDocD, charTypeDocD, stringTypeDocD, typeDocD, enumTypeDocD, 
  listTypeDocD, voidDocD, constructDocD, stateParamDocD, paramListDocD, mkParam,
  methodDocD, methodListDocD, stateVarDocD, stateVarDefDocD, stateVarListDocD, 
  ifCondDocD, switchDocD, forDocD, forEachDocD, whileDocD, stratDocD, 
  assignDocD, plusEqualsDocD, plusPlusDocD, varDecDocD, varDecDefDocD, 
  listDecDocD, listDecDefDocD, objDecDefDocD, constDecDefDocD, statementDocD, 
  returnDocD, mkSt, mkStNoEnd, stringListVals', stringListLists', commentDocD, 
  unOpPrec, notOpDocD, negateOpDocD, unExpr, unExpr', typeUnExpr, powerPrec, 
  equalOpDocD, notEqualOpDocD, greaterOpDocD, greaterEqualOpDocD, lessOpDocD, 
  lessEqualOpDocD, plusOpDocD, minusOpDocD, multOpDocD, divideOpDocD, 
  moduloOpDocD, andOpDocD, orOpDocD, binExpr, binExpr', typeBinExpr, mkVal, 
  mkVar, mkStaticVar, litTrueD, litFalseD, litCharD, litFloatD, litIntD, 
  litStringD, varDocD, extVarDocD, selfDocD, argDocD, enumElemDocD, 
  classVarCheckStatic, classVarD, classVarDocD, objVarDocD, inlineIfD, 
  funcAppDocD, extFuncAppDocD, stateObjDocD, listStateObjDocD, notNullDocD, 
  funcDocD, castDocD, listSetFuncDocD, listAccessFuncDocD, objAccessDocD, 
  castObjDocD, breakDocD, continueDocD, staticDocD, dynamicDocD, privateDocD, 
  publicDocD, dot, new, blockCmtStart, blockCmtEnd, docCmtStart, 
  observerListName, doxConfigName, makefileName, sampleInputName, doubleSlash, 
  blockCmtDoc, docCmtDoc, commentedItem, addCommentsDocD, functionDoc, classDoc,
  moduleDoc, docFuncRepr, valList, appendToBody, surroundBody, getterName, 
  setterName, setMainMethod, setEmpty, intValue, filterOutObjs)
import Language.Drasil.Code.Imperative.GOOL.Data (Terminator(..), AuxData(..), 
  ad, FileData(..), file, updateFileMod, FuncData(..), fd, ModData(..), md, 
  updateModDoc, MethodData(..), mthd, OpData(..), PackData(..), packD, 
  ParamData(..), pd, updateParamDoc, ProgData(..), progD, TypeData(..), td, 
  ValData(..), updateValDoc, Binding(..), VarData(..), vard)
import Language.Drasil.Code.Imperative.Doxygen.Import (makeDoxConfig)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig, Runnable, 
  asFragment, buildAll, nativeBinary, osClassDefault)
import Language.Drasil.Code.Imperative.Build.Import (makeBuild)
import Language.Drasil.Code.Imperative.GOOL.Helpers (emptyIfEmpty, liftA4, 
  liftA5, liftA6, liftA7, liftList, lift1List, lift3Pair, lift4Pair,
  liftPair, liftPairFst, getInnerType, convType, checkParams)
import Language.Drasil.Code.Imperative.WriteInput (makeInputFile)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor)
import qualified Prelude as P ((<>))
import qualified Data.Map as Map (fromList,lookup)
import Data.Maybe (fromMaybe, maybeToList)
import Control.Applicative (Applicative, liftA2, liftA3)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), parens, comma, empty,
  semi, vcat, lbrace, rbrace, colon)

instance PackageSym CSharpCode where
  type Package CSharpCode = PackData
  package = lift1List packD

instance AuxiliarySym CSharpCode where
  type Auxiliary CSharpCode = AuxData
  doxConfig pName p = fmap (ad doxConfigName) (liftA2 (makeDoxConfig pName)
    optimizeDox p)
  sampleInput db d sd = return $ ad sampleInputName (makeInputFile db d sd)

  optimizeDox = return $ text "NO"

  makefile cms = fmap (ad makefileName . makeBuild cms csBuildConfig csRunnable)

csBuildConfig :: Maybe BuildConfig
csBuildConfig = buildAll $ \i o -> [osClassDefault "CSC" "csc" "mcs", 
  asFragment "-out:" P.<> o] ++ i

csRunnable :: Runnable
csRunnable = nativeBinary