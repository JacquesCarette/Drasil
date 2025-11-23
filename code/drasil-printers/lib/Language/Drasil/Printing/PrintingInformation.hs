{-# LANGUAGE TemplateHaskell #-}
-- | Defines types and functions to gather all the information needed for printing.
module Language.Drasil.Printing.PrintingInformation (
    Notation(..), HasPrintingOptions(..)
  , PrintingConfiguration, notation
  , PrintingInformation
  , syst, stg, configuration
  , piSys, refFind
  , defaultConfiguration, plainConfiguration
) where

import Control.Lens (makeLenses, Lens', (^.))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

import Database.Drasil (ChunkDB, UID)
import Language.Drasil (Stage(..), Reference)
import Drasil.System (System, refTable)

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
                         { _syst :: System
                         , _stg :: Stage
                         , _configuration :: PrintingConfiguration
                         }
makeLenses ''PrintingInformation

-- | Finds the notation used for the 'PrintingConfiguration' within the 'PrintingInformation'.
instance HasPrintingOptions  PrintingInformation where getSetting  = configuration . getSetting

-- | Builds a document's printing information based on the system information.
piSys :: System -> Stage -> PrintingConfiguration -> PrintingInformation
piSys = PI

refFind :: UID -> PrintingInformation -> Reference
refFind u pinfo = fromMaybe (error $ "`" ++ show u ++ "` not found in Reference table!!!")
  $ M.lookup u $ pinfo ^. syst . refTable

-- | Default configuration is for engineering.
defaultConfiguration :: PrintingConfiguration
defaultConfiguration = PC Engineering

-- | Simple printing configuration is scientific.
plainConfiguration :: PrintingConfiguration
plainConfiguration = PC Scientific
