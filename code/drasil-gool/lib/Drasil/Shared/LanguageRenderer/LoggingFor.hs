{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

-- | MVP renderer for logging statements.

module Drasil.Shared.LanguageRenderer.LoggingFor (LoggingFor(..)) where

import Drasil.Shared.InterfaceCommon (VSType, TypeSym(..), VariableSym(..),
  VariableValue(..), ValueSym(..), Literal(..), IndexTranslator(..), Array(..),
  VariableElim(..))
import Drasil.GOOL.InterfaceGOOL (OOTypeSym(..), OOVariableSym(..), SelfSym(..),
  InstanceVarSelfSym(..))
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
import Data.Kind (Type)

newtype LoggingFor (lang :: Type -> Type) a = LC {unLC :: a} deriving Functor

instance Applicative (LoggingFor lang) where
  pure = LC
  (LC f) <*> (LC x) = LC (f x)

instance Monad (LoggingFor lang) where
  LC x >>= f = f x

instance VariableSym (LoggingFor lang) where
  type Variable (LoggingFor lang) = Doc
  var n _ = return $ return $ text n
  constant n _ = return $ return $ text n
  extVar l n _ = return $ return $ text l <> dot <> text n

instance OOVariableSym (LoggingFor lang) where
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

instance SelfSym (LoggingFor CodeInfoOO) where
  self = return $ return empty

instance InstanceVarSelfSym (LoggingFor CodeInfoOO) where
  instanceVarSelf _ = return $ return empty

instance SelfSym (LoggingFor JavaCode) where
  self = return $ return $ text "this"

instance InstanceVarSelfSym (LoggingFor JavaCode) where
  instanceVarSelf vr = do
    vr' <- vr
    self' <- self @(LoggingFor JavaCode)
    return $ return $ unLC self' <> dot <> unLC vr'

instance SelfSym (LoggingFor CSharpCode) where
  self = return $ return $ text "this"

instance InstanceVarSelfSym (LoggingFor CSharpCode) where
  instanceVarSelf vr = do
    vr' <- vr
    self' <- self @(LoggingFor CSharpCode)
    return $ return $ unLC self' <> dot <> unLC vr'

instance SelfSym (LoggingFor (CppCode CppSrcCode CppHdrCode)) where
  self = return $ return $ text "self"

instance InstanceVarSelfSym (LoggingFor (CppCode CppSrcCode CppHdrCode)) where
  instanceVarSelf vr = do
    vr' <- vr
    self' <- self @(LoggingFor (CppCode CppSrcCode CppHdrCode))
    return $ return $ unLC self' <> text "->" <> unLC vr'

instance SelfSym (LoggingFor PythonCode) where
  self = return $ return $ text "self"

instance InstanceVarSelfSym (LoggingFor PythonCode) where
  instanceVarSelf vr = do
    vr' <- vr
    self' <- self @(LoggingFor PythonCode)
    return $ return $ unLC self' <> dot <> unLC vr'

instance SelfSym (LoggingFor SwiftCode) where
  self = return $ return $ text "self"

instance InstanceVarSelfSym (LoggingFor SwiftCode) where
  instanceVarSelf vr = do
    vr' <- vr
    self' <- self @(LoggingFor SwiftCode)
    return $ return $ unLC self' <> dot <> unLC vr'

instance ValueSym (LoggingFor lang) where
  type Value (LoggingFor lang) = Doc
  valueType = error "Not implemented"

instance TypeSym (LoggingFor lang) where
  bool = bool
  int = int
  float = float
  double = double
  char = char
  string = string
  infile = infile
  outfile = outfile
  listType = listType
  setType = setType
  arrayType = arrayType
  innerType = innerType
  funcType = funcType
  void = void

instance OOTypeSym (LoggingFor lang) where
  obj nm = typeFromData (Object nm) ("Object<" ++ nm ++ ">")

typeFromData :: CodeType -> String -> VSType (LoggingFor lang)
typeFromData tp str = return $ return $ td tp str (text str)

instance Literal (LoggingFor lang) where
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

instance IndexTranslator (LoggingFor lang) where
  intToIndex = id
  indexToInt = id

instance Array (LoggingFor lang) where
  arrayElem idx' vr' = do
    idx <- idx'
    vr <- vr'
    return $ return $ unLC idx <> brackets (unLC vr)
  arrayLength = undefined
  arrayCopy = undefined

-- Not Implemented
instance VariableElim (LoggingFor lang) where
  variableName = undefined
  variableType = undefined

instance VariableValue (LoggingFor lang) where
  valueOf = id
