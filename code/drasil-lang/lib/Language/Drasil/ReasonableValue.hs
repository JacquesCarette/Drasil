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
import qualified Data.Set as S (union)

import Drasil.Database (HasChunkRefs(..))
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

-- | Both the value and its rationale can mention other chunks.
instance HasChunkRefs ReasonableValue where
  chunkRefs (RV val why) = chunkRefs val `S.union` chunkRefs why
  {-# INLINABLE chunkRefs #-}

reasonableValue :: Expr -> Maybe Sentence -> ReasonableValue
reasonableValue = RV
