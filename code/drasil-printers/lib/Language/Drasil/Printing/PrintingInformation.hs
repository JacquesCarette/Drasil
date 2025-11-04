{-# LANGUAGE TemplateHaskell #-}
-- | Defines types and functions to gather all the information needed for printing.
module Language.Drasil.Printing.PrintingInformation (
    Notation(..), HasPrintingOptions(..)
  , PrintingConfiguration, notation
  , PrintingInformation, ckdb, stg, configuration
  , piSys
  , defaultConfiguration, plainConfiguration
) where

import Control.Lens (makeLenses, Lens')

import Database.Drasil (ChunkDB)

import Language.Drasil (Stage(..))

-- | Notation can be scientific or for engineering.
data Notation = Scientific
              | Engineering

-- | Able to be printed.
class HasPrintingOptions c where
    -- | Holds the printing notation.
    getSetting :: Lens' c Notation

-- | Holds the printing configuration.
newtype PrintingConfiguration = PC { _notation :: Notation }
makeLenses ''PrintingConfiguration

-- | Finds the notation used for the 'PrintingConfiguration'.
instance HasPrintingOptions  PrintingConfiguration where getSetting = notation

-- | Printing information contains a database, a stage, and a printing configuration.
data PrintingInformation = PI
                         { _ckdb :: ChunkDB
                         , _stg :: Stage
                         , _configuration :: PrintingConfiguration
                         }
makeLenses ''PrintingInformation

-- | Finds the notation used for the 'PrintingConfiguration' within the 'PrintingInformation'.
instance HasPrintingOptions  PrintingInformation where getSetting  = configuration . getSetting

-- | Builds a document's printing information based on the system information.
piSys :: ChunkDB -> Stage -> PrintingConfiguration -> PrintingInformation
piSys = PI

-- | Default configuration is for engineering.
defaultConfiguration :: PrintingConfiguration
defaultConfiguration = PC Engineering

-- | Simple printing configuration is scientific.
plainConfiguration :: PrintingConfiguration
plainConfiguration = PC Scientific
