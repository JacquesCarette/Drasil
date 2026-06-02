{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

-- | MVP renderer for logging statements.

module Drasil.Shared.LanguageRenderer.Logger (LoggerCode(..)) where

import Drasil.Shared.InterfaceCommon (VSType, TypeSym(..), VariableSym(..),
  ValueSym(..), Literal(..), IndexTranslator(..), Array(..), VariableElim(..))
import Drasil.GOOL.InterfaceGOOL (OOTypeSym(..), OOVariableSym(..), SelfSym(..),
  InstanceVarSelfSym(..), convTypeOO)
import Drasil.Shared.AST (TypeData(..), td)
import Drasil.Shared.CodeType (CodeType(..))
import Drasil.GOOL.CodeInfoOO (CodeInfoOO)
import Drasil.GOOL.LanguageRenderer.JavaRenderer (JavaCode)
import Drasil.GOOL.LanguageRenderer.CSharpRenderer (CSharpCode)
import Drasil.GOOL.LanguageRenderer.CppRenderer (CppCode, CppSrcCode, CppHdrCode)
import Drasil.GOOL.LanguageRenderer.PythonRenderer (PythonCode)
import Drasil.GOOL.LanguageRenderer.SwiftRenderer (SwiftCode)
import Drasil.Shared.LanguageRenderer (dot)

import Text.PrettyPrint.HughesPJ (Doc, text, empty, comma, space, brackets,
  braces, punctuate, hcat)
import qualified Text.PrettyPrint.HughesPJ as P (char, integer, float, double)
import Data.List (intercalate)
import Data.Kind (Type)

newtype LoggerCode (lang :: Type -> Type) a = LC {unLC :: a} deriving Functor

instance Applicative (LoggerCode lang) where
  pure = LC
  (LC f) <*> (LC x) = LC (f x)

instance Monad (LoggerCode lang) where
  LC x >>= f = f x

instance VariableSym (LoggerCode lang) where
  type Variable (LoggerCode lang) = Doc
  var n _ = return $ return $ text n
  constant n _ = return $ return $ text n
  extVar l n _ = return $ return $ text l <> dot <> text n

instance OOVariableSym (LoggerCode lang) where
  classVar = var
  classConst = constant
  classVarAccess cls vr = do
    cls' <- cls
    vr' <- vr
    let clsDoc = (typeDoc . unLC) cls'
        vrDoc = unLC vr'
    return $ return $ clsDoc <> dot <> vrDoc
  extClassVarAccess = classVarAccess
  instanceVarAccess ob vr = do
    ob' <- ob
    vr' <- vr
    return $ return $ unLC ob' <> dot <> unLC vr'

instance SelfSym (LoggerCode CodeInfoOO) where
  self = return $ return empty

instance InstanceVarSelfSym (LoggerCode CodeInfoOO) where
  instanceVarSelf _ = return $ return empty

instance SelfSym (LoggerCode JavaCode) where
  self = return $ return $ text "this"

instance InstanceVarSelfSym (LoggerCode JavaCode) where
  instanceVarSelf vr = do
    vr' <- vr
    self' <- self @(LoggerCode JavaCode)
    return $ return $ unLC self' <> dot <> unLC vr'

instance SelfSym (LoggerCode CSharpCode) where
  self = return $ return $ text "this"

instance InstanceVarSelfSym (LoggerCode CSharpCode) where
  instanceVarSelf vr = do
    vr' <- vr
    self' <- self @(LoggerCode CSharpCode)
    return $ return $ unLC self' <> dot <> unLC vr'

instance SelfSym (LoggerCode (CppCode CppSrcCode CppHdrCode)) where
  self = return $ return $ text "self"

instance InstanceVarSelfSym (LoggerCode (CppCode CppSrcCode CppHdrCode)) where
  instanceVarSelf vr = do
    vr' <- vr
    self' <- self @(LoggerCode (CppCode CppSrcCode CppHdrCode))
    return $ return $ unLC self' <> text "->" <> unLC vr'

instance SelfSym (LoggerCode PythonCode) where
  self = return $ return $ text "self"

instance InstanceVarSelfSym (LoggerCode PythonCode) where
  instanceVarSelf vr = do
    vr' <- vr
    self' <- self @(LoggerCode PythonCode)
    return $ return $ unLC self' <> dot <> unLC vr'

instance SelfSym (LoggerCode SwiftCode) where
  self = return $ return $ text "self"

instance InstanceVarSelfSym (LoggerCode SwiftCode) where
  instanceVarSelf vr = do
    vr' <- vr
    self' <- self @(LoggerCode SwiftCode)
    return $ return $ unLC self' <> dot <> unLC vr'

instance ValueSym (LoggerCode lang) where
  type Value (LoggerCode lang) = Doc
  valueType = error "Not implemented"

instance TypeSym (LoggerCode lang) where
  bool = typeFromData Boolean "Boolean"
  int = typeFromData Integer "Integer"
  float = typeFromData Float "Float"
  double = typeFromData Double "Double"
  char = typeFromData Char "Char"
  string = typeFromData String "String"
  infile = typeFromData InFile "InFile"
  outfile = typeFromData OutFile "OutFile"
  listType tp = do
    tp' <- tp
    let tpData = unLC tp'
    typeFromData (List (cType tpData)) ("List<" ++ typeString tpData ++ ">")
  setType tp = do
    tp' <- tp
    let tpData = unLC tp'
    typeFromData (Set (cType tpData)) ("Set<" ++ typeString tpData ++ ">")
  arrayType tp = do
    tp' <- tp
    let tpData = unLC tp'
    typeFromData (Array (cType tpData)) ("Array<" ++ typeString tpData ++ ">")
  listInnerType tp = tp >>= (convTypeOO . cType . unLC)
  funcType inTps outTp = do
    inTps' <- sequence inTps
    outTp' <- outTp
    let inTpsData = map unLC inTps'
        outTpData = unLC outTp'
    typeFromData (Func (map cType inTpsData) (cType outTpData))
      ("(" ++ intercalate " × " (map typeString inTpsData) ++ ") → " ++ typeString outTpData)
  void = typeFromData Void "Void"

instance OOTypeSym (LoggerCode lang) where
  obj nm = typeFromData (Object nm) ("Object<" ++ nm ++ ">")

typeFromData :: CodeType -> String -> VSType (LoggerCode lang)
typeFromData tp str = return $ return $ td tp str (text str)

instance Literal (LoggerCode lang) where
  litTrue = litString "True"
  litFalse = litString "False"
  litChar = return . return . P.char
  litDouble = return . return . P.double
  litFloat = return . return . P.float
  litInt = return . return . P.integer
  litString = return . return . text
  litArray _ vs = do
    vs' <- sequence vs
    let docs = map unLC vs'
    return $ return $ brackets $ hcat $ punctuate (comma <> space) docs
  litList = litArray
  litSet _ vs = do
    vs' <- sequence vs
    let docs = map unLC vs'
    return $ return $ braces $ hcat $ punctuate (comma <> space) docs

instance IndexTranslator (LoggerCode lang) where
  intToIndex = id
  indexToInt = id

instance Array (LoggerCode lang) where
  arrayElem idx' vr' = do
    idx <- idx'
    vr <- vr'
    return $ return $ unLC idx <> brackets (unLC vr)

-- Not Implemented
instance VariableElim (LoggerCode lang) where
  variableName = undefined
  variableType = undefined
