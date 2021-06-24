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

-- | Finds the 'UID' of the 'QuantityDict' used to make the 'NamedArgument'.
instance HasUID         NamedArgument where uid = qtd . uid
-- | Finds the term ('NP') of the 'QuantityDict' used to make the 'NamedArgument'.
instance NamedIdea      NamedArgument where term = qtd . term
-- | Finds the idea contained in the 'QuantityDict' used to make the 'NamedArgument'.
instance Idea           NamedArgument where getA = getA . view qtd
-- | Finds the 'Space' of the 'QuantityDict' used to make the 'NamedArgument'.
instance HasSpace       NamedArgument where typ = qtd . typ
-- | Finds the 'Symbol' of the 'QuantityDict' used to make the 'NamedArgument'.
instance HasSymbol      NamedArgument where symbol = symbol . view qtd
-- | 'NamedArgument's have a 'Quantity'.
instance Quantity       NamedArgument where
-- | 'NamedArgument's have an argument name.
instance IsArgumentName NamedArgument where
-- | Equal if 'UID's are equal.
instance Eq             NamedArgument where a == b = (a ^. uid) == (b ^. uid)
-- | Finds the units of the 'QuantityDict' used to make the 'NamedArgument'.
instance MayHaveUnit    NamedArgument where getUnit = getUnit . view qtd
  
-- | Smart constructor for 'NamedArgument' .
narg :: (Quantity q, MayHaveUnit q) => q -> NamedArgument
narg = NA . qw
