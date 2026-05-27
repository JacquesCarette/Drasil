{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

-- | MVP renderer for logging statements.

module Drasil.Shared.LanguageRenderer.Logger (LoggerCode(..)) where

import Drasil.Shared.InterfaceCommon (TypeSym(..), VariableSym(..), ValueSym(..))

import Text.PrettyPrint.HughesPJ (Doc, text, brackets)

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

