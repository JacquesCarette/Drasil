{-# LANGUAGE TemplateHaskell #-}

module Language.Drasil.Printing.PrintingInformation where

import Control.Lens (makeLenses, Lens')

import Language.Drasil (ChunkDB , HasTermTable(..), HasDefinitionTable(..)
  , HasUnitTable(..)
  )

data Notation = Scientific
              | Engineering

class HasPrintingOptions c where
    getSetting :: Lens' c Notation

data PrintingConfiguration = PC { _notation :: Notation }
makeLenses ''PrintingConfiguration

instance HasPrintingOptions  PrintingConfiguration where getSetting = notation


data PrintingInformation = PI
                         { _ckdb :: ChunkDB
                         , _configuration :: PrintingConfiguration
                         }
makeLenses ''PrintingInformation

instance HasTermTable        PrintingInformation where termTable    = ckdb . termTable
instance HasDefinitionTable  PrintingInformation where defTable     = ckdb . defTable
instance HasUnitTable        PrintingInformation where unitTable    = ckdb . unitTable
instance HasPrintingOptions  PrintingInformation where getSetting  = configuration . getSetting

defaultConfiguration :: PrintingConfiguration
defaultConfiguration = PC Engineering
