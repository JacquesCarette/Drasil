{-# LANGUAGE GADTs, TemplateHaskell, TypeFamilies #-}

module Language.Drasil.PrintingInformation where

import Control.Lens ((^.), makeLenses, view)

import Language.Drasil.ChunkDB (ChunkDB, cdb
  , HasSymbolTable(..), symbolMap, symbLookup, getUnitLup
  , HasTermTable(..), termLookup
  , HasDefinitionTable(..), conceptMap, defLookup
  , HasUnitTable(..), unitMap, collectUnits
  )

-- | Data structure for holding all of the requisite information about a system
-- to be used in artefact generation
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