{-# LANGUAGE TemplateHaskell #-}
-- | Defines types and functions to gather all the information needed for
-- printing.
module Language.Drasil.Printing.PrintingInformation (
    PrintingInformation
  , Notation(..)
  , sysdb, stg, notation
  , piSys
) where

import Control.Lens (makeLenses)

import Drasil.Database (ChunkDB)
import Language.Drasil (Stage(..))

-- | Notation can be scientific or for engineering.
data Notation = Scientific
              | Engineering

-- | Printing information contains a database, a stage, and a printing configuration.
data PrintingInformation =
  PI { _sysdb :: ChunkDB
     , _stg :: Stage
     , _notation :: Notation
     }
makeLenses ''PrintingInformation

-- | Builds a document's printing information based on the system information.
piSys :: ChunkDB -> Stage -> Notation -> PrintingInformation
piSys = PI
