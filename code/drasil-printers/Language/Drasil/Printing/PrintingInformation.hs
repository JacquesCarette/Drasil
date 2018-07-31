{-# LANGUAGE GADTs, TemplateHaskell, TypeFamilies #-}

module Language.Drasil.Printing.PrintingInformation where

import Control.Lens ((^.), makeLenses, view)

import Language.Drasil (ChunkDB, HasSymbolTable(..)
  , HasTermTable(..), HasDefinitionTable(..)
  , HasUnitTable(..)
  )

data PrintingInformation = PI
                         { _ckdb :: ChunkDB
                         , _scientificSetting :: String
                         }
makeLenses ''PrintingInformation

instance HasSymbolTable     PrintingInformation where symbolTable = ckdb . symbolTable
instance HasTermTable       PrintingInformation where termTable   = ckdb . termTable
instance HasDefinitionTable PrintingInformation where defTable    = ckdb . defTable
instance HasUnitTable       PrintingInformation where unitTable   = ckdb . unitTable

getSetting :: PrintingInformation -> String
getSetting u = view scientificSetting u