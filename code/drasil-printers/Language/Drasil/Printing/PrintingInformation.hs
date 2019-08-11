{-# LANGUAGE TemplateHaskell #-}

module Language.Drasil.Printing.PrintingInformation where

import Control.Lens (makeLenses, Lens')

import Database.Drasil (ChunkDB)

import Language.Drasil (Stage(..))

data Notation = Scientific
              | Engineering

class HasPrintingOptions c where
    getSetting :: Lens' c Notation

newtype PrintingConfiguration = PC { _notation :: Notation }
makeLenses ''PrintingConfiguration

instance HasPrintingOptions  PrintingConfiguration where getSetting = notation


data PrintingInformation = PI
                         { _ckdb :: ChunkDB
                         , _stg :: Stage
                         , _configuration :: PrintingConfiguration
                         }
makeLenses ''PrintingInformation

instance HasPrintingOptions  PrintingInformation where getSetting  = configuration . getSetting

defaultConfiguration :: PrintingConfiguration
defaultConfiguration = PC Engineering
