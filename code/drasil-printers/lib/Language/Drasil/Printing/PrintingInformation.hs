{-# LANGUAGE TemplateHaskell #-}
-- | Defines types and functions to gather all the information needed for
-- printing.
module Language.Drasil.Printing.PrintingInformation (
    PrintingInformation
  , Notation(..)
  , sysdb, refTable, stg, notation
  , piSys, refFind
) where

import Control.Lens (makeLenses, (^.))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

import Drasil.Database (UID, ChunkDB)
import Language.Drasil (Stage(..))
import Language.Drasil.Docs (Reference)

-- | Notation can be scientific or for engineering.
data Notation = Scientific
              | Engineering

-- | Printing information contains a database, a stage, and a printing configuration.
data PrintingInformation =
  PI { _sysdb :: ChunkDB
     , _refTable :: M.Map UID Reference
     , _stg :: Stage
     , _notation :: Notation
     }
makeLenses ''PrintingInformation

-- | Builds a document's printing information based on the system information.
piSys :: ChunkDB -> M.Map UID Reference -> Stage -> Notation -> PrintingInformation
piSys = PI

refFind :: UID -> PrintingInformation -> Reference
refFind u pinfo = fromMaybe (error $ "`" ++ show u ++ "` not found in Reference table!!!")
  $ M.lookup u $ pinfo ^. refTable
