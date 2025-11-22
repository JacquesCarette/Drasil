{-# LANGUAGE TemplateHaskell #-}
-- | Defines types and functions to gather all the information needed for printing.
module Language.Drasil.Printing.PrintingInformation (
    Notation(..), HasPrintingOptions(..)
  , PrintingConfiguration, notation
  , PrintingInformation
  , ckdb, stg, configuration, refTable, refbyTable, traceTable
  , piSys, refFind
  , defaultConfiguration, plainConfiguration
) where

import Control.Lens (makeLenses, Lens', (^.))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

import Database.Drasil (ChunkDB, UID)
import Language.Drasil (Stage(..), Reference)

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
                         , _refTable :: M.Map UID Reference
                         , _refbyTable :: M.Map UID [UID]
                         , _traceTable :: M.Map UID [UID]
                         , _stg :: Stage
                         , _configuration :: PrintingConfiguration
                         }
makeLenses ''PrintingInformation

-- | Finds the notation used for the 'PrintingConfiguration' within the 'PrintingInformation'.
instance HasPrintingOptions  PrintingInformation where getSetting  = configuration . getSetting

-- | Builds a document's printing information based on the system information.
piSys :: ChunkDB -> Stage -> PrintingConfiguration -> PrintingInformation
piSys db = PI db mempty mempty mempty

refFind :: UID -> PrintingInformation -> Reference
refFind u pinfo = fromMaybe (error $ "`" ++ show u ++ "` not found in Reference table!!!")
  $ M.lookup u $ pinfo ^. refTable

-- | Default configuration is for engineering.
defaultConfiguration :: PrintingConfiguration
defaultConfiguration = PC Engineering

-- | Simple printing configuration is scientific.
plainConfiguration :: PrintingConfiguration
plainConfiguration = PC Scientific
