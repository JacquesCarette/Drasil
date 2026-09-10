{-# Language TemplateHaskell #-}

module Language.Drasil.ReasonableValue (
  -- * Reasonable Value
  ReasonableValue,

  -- ** Constructors
  reasonableValue,

  -- ** Lenses
  reasV, rationale
) where

import Control.Lens (makeLenses)

import Language.Drasil.Expr.Lang (Expr(..))
import Language.Drasil.Sentence (Sentence)

-- | Represents a reasonable value ('Expr') with an optional
-- rationale ('Maybe Sentence').
--
-- A reasonable value is an example of a value that is physically
-- admissible.
data ReasonableValue = RV { _reasV     :: Expr
                          , _rationale :: Maybe Sentence
                          }
makeLenses ''ReasonableValue

reasonableValue :: Expr -> Maybe Sentence -> ReasonableValue
reasonableValue = RV
