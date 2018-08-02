{-# LANGUAGE GADTs, TemplateHaskell, TypeFamilies #-}

module Language.Drasil.Printing.PrintingInformation where

import Control.Lens ((^.), makeLenses, view)

import Language.Drasil (ChunkDB, HasSymbolTable(..)
  , HasTermTable(..), HasDefinitionTable(..)
  , HasUnitTable(..)
  )

data Notation = Scientific
               |Engineering

class HaveNotationSetting c where
	getSetting :: c -> Notation 

data PrintingInformation = PI
                         { _ckdb :: ChunkDB
                         , _setting :: Notation
                         }
makeLenses ''PrintingInformation

instance HasSymbolTable      PrintingInformation where symbolTable = ckdb . symbolTable
instance HasTermTable        PrintingInformation where termTable   = ckdb . termTable
instance HasDefinitionTable  PrintingInformation where defTable    = ckdb . defTable
instance HasUnitTable        PrintingInformation where unitTable   = ckdb . unitTable
instance HaveNotationSetting PrintingInformation where getSetting c = c ^. setting 
