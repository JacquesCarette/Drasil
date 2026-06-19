{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | MVP renderer for logging statements.

module Drasil.Shared.LanguageRenderer.LoggingFor (LoggingFor(..)) where

import Drasil.Shared.InterfaceCommon
import Drasil.Shared.State

import Prelude hiding (print)
import Control.Lens (zoom)
import Data.Kind (Type)

newtype LoggingFor (lang :: Type -> Type) a = LC {unLC :: lang a}
  deriving newtype (Functor, Applicative, Monad)

-- instance (SharedProg lang) => SharedProg (LoggingFor lang)

instance (Argument lang) => Argument (LoggingFor lang) where
  pointerArg = pointerArg

instance (IndexTranslator lang) => IndexTranslator (LoggingFor lang) where
  intToIndex = intToIndex
  indexToInt = indexToInt

instance (VariableSym lang) => VariableSym (LoggingFor lang) where
  type Variable (LoggingFor lang) = Variable lang
  var = var
  constant = constant
  extVar = extVar

instance (TypeSym lang) => TypeSym (LoggingFor lang) where
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
  listInnerType = listInnerType
  funcType = funcType
  void = void

instance (ValueSym lang) => ValueSym (LoggingFor lang) where
  type Value (LoggingFor lang) = Value lang
  valueType = valueType

instance StatementSym lang => StatementSym (LoggingFor lang) where
  type Statement (LoggingFor lang) = Statement lang
  valStmt = valStmt
  emptyStmt = emptyStmt
  multi = multi

-- TODO [Brandon Bosman, 06/19/2026]: This should be passed down from drasil-code
varLogFile :: (VariableSym r) => SVariable r
varLogFile = var "outfile" outfile

valLogFile :: (VariableValue r) => SValue r
valLogFile = valueOf varLogFile

-- TODO [Brandon Bosman, 06/19/2026]: This should be passed down from drasil-code
logName :: (Literal r) => SValue r
logName = litString "log.txt"

instance (SharedProg lang) => AssignStatement (LoggingFor lang) where
  (&-=) = (&-=)
  (&+=) = (&+=)
  (&++) = (&++)
  (&--) = (&--)
  assign x e = do
    modName <- zoom lensMStoFS getModuleName
    LC <$> multi
      [ openFileA (unLC <$> varLogFile) logName
      , assign (unLC <$> x) (unLC <$> e)
      , do
          x' <- variableName . unLC <$> zoom lensMStoVS x
          printFileStr valLogFile $ "var '" <> x' <> "' assigned"
      , printFile valLogFile $ valueOf (unLC <$> x)
      , printLn $ litString $ " in module " <> modName
      ]

