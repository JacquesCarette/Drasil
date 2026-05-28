{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

-- | MVP renderer for logging statements.

module Drasil.Shared.LanguageRenderer.Logger (LoggerCode(..)) where

import Drasil.Shared.InterfaceCommon (TypeSym(..), VariableSym(..),
  ValueSym(..), Literal(..))
import Drasil.GOOL.InterfaceGOOL (OOTypeSym(..), OOVariableSym(..))
import Drasil.Shared.AST (TypeData(..))

import Text.PrettyPrint.HughesPJ (Doc, text, comma, space, brackets, braces,
  punctuate, hcat)
import qualified Text.PrettyPrint.HughesPJ as P (char, integer, float, double)

newtype LoggerCode a = LC {unLC :: a} deriving Functor

instance Applicative LoggerCode where
  pure = LC
  (LC f) <*> (LC x) = LC (f x)

instance Monad LoggerCode where
  LC x >>= f = f x

instance VariableSym LoggerCode where
  type Variable LoggerCode = Doc
  var n _ = return $ return $ text n
  constant n _ = return $ return $ text n
  extVar l n _ = return $ return $ text l <> text "." <> text n
  arrayElem idx' vr' = do
    idx <- idx'
    vr <- vr'
    return $ return $ unLC idx <> brackets (unLC vr)

instance OOVariableSym LoggerCode where
  classVar = var
  classConst = constant
  self = return $ return $ text "self"
  classVarAccess cls vr = do
    cls' <- cls
    vr' <- vr
    let clsDoc = (typeDoc . unLC) cls'
        vrDoc = unLC vr'
    return $ return $ clsDoc <> text "." <> vrDoc
  extClassVarAccess = classVarAccess
  instanceVarAccess ob vr = do
    ob' <- ob
    vr' <- vr
    return $ return $ unLC ob' <> text "." <> unLC vr'
  instanceVarSelf vr = do
    vr' <- vr
    return $ return $ text "self." <> unLC vr'

instance ValueSym LoggerCode where
  type Value LoggerCode = Doc
  valueType = error "Not implemented"

instance TypeSym LoggerCode where
  bool = error "Not implemented"
  int = error "Not implemented"
  float = error "Not implemented"
  double = error "Not implemented"
  char = error "Not implemented"
  string = error "Not implemented"
  infile = error "Not implemented"
  outfile = error "Not implemented"
  listType = error "Not implemented"
  setType = error "Not implemented"
  arrayType = error "Not implemented"
  listInnerType = error "Not implemented"
  funcType = error "Not implemented"
  void = error "Not implemented"

instance OOTypeSym LoggerCode where
  obj = error "Not Implemented"

instance Literal LoggerCode where
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
