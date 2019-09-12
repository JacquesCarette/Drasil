{-# LANGUAGE TypeFamilies #-}

-- | The logic to render Python code is contained in this module
module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.PythonRenderer (
  -- * Python Code Configuration -- defines syntax of all Python code
  PythonCode(..)
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
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer (addExt, fileDoc', 
  enumElementsDocD', multiStateDocD, blockDocD, bodyDocD, outDoc, intTypeDocD, 
  floatTypeDocD, typeDocD, enumTypeDocD, constructDocD, paramListDocD, mkParam,
  methodListDocD, stateVarListDocD, ifCondDocD, stratDocD, assignDocD, 
  multiAssignDoc, plusEqualsDocD', plusPlusDocD', statementDocD, returnDocD, 
  commentDocD, mkStNoEnd, stringListVals', stringListLists', unOpPrec, 
  notOpDocD', negateOpDocD, sqrtOpDocD', absOpDocD', expOpDocD', sinOpDocD', 
  cosOpDocD', tanOpDocD', asinOpDocD', acosOpDocD', atanOpDocD', unExpr, 
  unExpr', typeUnExpr, powerPrec, multPrec, andPrec, orPrec, equalOpDocD, 
  notEqualOpDocD, greaterOpDocD, greaterEqualOpDocD, lessOpDocD, 
  lessEqualOpDocD, plusOpDocD, minusOpDocD, multOpDocD, divideOpDocD, 
  moduloOpDocD, binExpr, typeBinExpr, mkVal, mkVar, mkStaticVar, litCharD, 
  litFloatD, litIntD, litStringD, varDocD, extVarDocD, argDocD, enumElemDocD, 
  classVarCheckStatic, classVarD, objVarDocD, funcAppDocD, extFuncAppDocD, 
  funcDocD, listSetFuncDocD, listAccessFuncDocD, objAccessDocD, castObjDocD, 
  breakDocD, continueDocD, dynamicDocD, classDec, dot, forLabel, 
  observerListName, doxConfigName, makefileName, sampleInputName, commentedItem,
  addCommentsDocD, classDoc, moduleDoc, docFuncRepr, valList, surroundBody, 
  getterName, setterName, filterOutObjs)
import Language.Drasil.Code.Imperative.GOOL.Data (Terminator(..), AuxData(..), 
  ad, FileData(..), file, updateFileMod, FuncData(..), fd, ModData(..), md, 
  updateModDoc, MethodData(..), mthd, OpData(..), PackData(..), packD, 
  ParamData(..), ProgData(..), progD, TypeData(..), td, ValData(..), vd,
  VarData(..), vard)
import Language.Drasil.Code.Imperative.Doxygen.Import (makeDoxConfig)
import Language.Drasil.Code.Imperative.Build.AST (Runnable, interpMM)
import Language.Drasil.Code.Imperative.Build.Import (makeBuild)
import Language.Drasil.Code.Imperative.GOOL.Helpers (blank, vibcat, 
  emptyIfEmpty, liftA4, liftA5, liftList, lift1List, lift2Lists, lift4Pair, 
  liftPair, liftPairFst, getInnerType, convType, checkParams)
import Language.Drasil.Code.Imperative.WriteInput (makeInputFile)

import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import qualified Data.Map as Map (fromList,lookup)
import Data.Maybe (fromMaybe, maybeToList)
import Control.Applicative (Applicative, liftA2, liftA3)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), ($+$), parens, empty,
  equals, vcat, colon, brackets, isEmpty)

instance PackageSym PythonCode where
  type Package PythonCode = PackData
  package = lift1List packD

instance AuxiliarySym PythonCode where
  type Auxiliary PythonCode = AuxData
  doxConfig pName p = fmap (ad doxConfigName) (liftA2 (makeDoxConfig pName)
    optimizeDox p)
  sampleInput db d sd = return $ ad sampleInputName (makeInputFile db d sd)

  optimizeDox = return $ text "YES"

  makefile cms = fmap (ad makefileName . makeBuild cms Nothing pyRunnable)

pyRunnable :: Runnable
pyRunnable = interpMM "python"
