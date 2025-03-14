{-# LANGUAGE TemplateHaskell #-}
-- | Defines types and functions to gather all the information needed for printing.
module Language.Drasil.Printing.PrintingInformation where

import Control.Lens (makeLenses, Lens', (^.))

import SysInfo.Drasil (sysinfodb, SystemInformation)
import Database.Drasil
import Language.Drasil

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

-- | Printing information contains the chunk database, printing stage, printing
-- configuration, and a database of chunks rendered (as references).
data PrintingInformation = PI
                         { _ckdb :: ChunkDB
                         , _stg :: Stage
                         , _configuration :: PrintingConfiguration
                         , _references :: UMap Reference
                         }
makeLenses ''PrintingInformation

-- | Finds the notation used for the 'PrintingConfiguration' within the
-- 'PrintingInformation'.
instance HasPrintingOptions PrintingInformation where
    getSetting = configuration . getSetting

-- | Builds a document's printing information based on the system information.
piSys :: SystemInformation -> Stage -> PrintingConfiguration -> [Reference] -> PrintingInformation
piSys si st pc = PI (si ^. sysinfodb) st pc . idMap

-- | Default configuration is for engineering.
defaultConfiguration :: PrintingConfiguration
defaultConfiguration = PC Engineering

-- | Looks up a 'UID' in the reference table from the 'ChunkDB'. If nothing is found, an error is thrown.
refResolve :: UID -> PrintingInformation -> Reference
refResolve u pi = uMapLookup "Reference" "ReferenceMap" u (pi ^. references)
