{-# LANGUAGE TemplateHaskell #-}
-- | Defines types and functions to gather all the information needed for
-- printing.
module Language.Drasil.Printing.PrintingInformation (
    PrintingInformation
  , Notation(..)
  , sysdb, stg, notation
  , piSys, refFind
) where

import Control.Lens (makeLenses, (^.))
import Data.Maybe (fromMaybe)

import Drasil.Database (UID, ChunkDB)
import Drasil.Database.SearchTools (refResolve)
import Language.Drasil (Stage(..))
import Language.Drasil.Document (Reference)

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

refFind :: UID -> PrintingInformation -> Reference
refFind u pinfo = go $ refResolve (pinfo ^. sysdb) u
  where go = fromMaybe (error $ "`" ++ show u ++ "` not found in Reference table!!!")
