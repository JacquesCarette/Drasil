{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.NamedArgument (NamedArgument(..), narg) where

import Language.Drasil.Classes.Core (HasUID(uid), HasSymbol(symbol))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA), HasSpace(typ), 
  Quantity, IsArgumentName)
import Language.Drasil.Chunk.Quantity (QuantityDict, qw)
import Language.Drasil.Chunk.UnitDefn(MayHaveUnit(getUnit))

import Control.Lens ((^.), makeLenses, view)

-- | Any quantity can be a named argument (wrapper for 'QuantityDict')
newtype NamedArgument = NA {_qtd :: QuantityDict}
makeLenses ''NamedArgument

instance HasUID         NamedArgument where uid = qtd . uid
instance NamedIdea      NamedArgument where term = qtd . term
instance Idea           NamedArgument where getA = getA . view qtd
instance HasSpace       NamedArgument where typ = qtd . typ
instance HasSymbol      NamedArgument where symbol = symbol . view qtd
instance Quantity       NamedArgument where 
instance IsArgumentName NamedArgument where
instance Eq             NamedArgument where a == b = (a ^. uid) == (b ^. uid)
instance MayHaveUnit    NamedArgument where getUnit = getUnit . view qtd
  
-- | Smart constructor for 'NamedArgument' 
narg :: (Quantity q, MayHaveUnit q) => q -> NamedArgument
narg = NA . qw
