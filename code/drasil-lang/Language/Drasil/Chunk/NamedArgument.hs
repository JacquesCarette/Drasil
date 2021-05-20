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
-- ^ Finds the 'UID' of the 'QuantityDict' used to make the 'NamedArgument'.
instance NamedIdea      NamedArgument where term = qtd . term
-- ^ Finds the term ('NP') of the 'QuantityDict' used to make the 'NamedArgument'.
instance Idea           NamedArgument where getA = getA . view qtd
-- ^ Finds the idea contained in the 'QuantityDict' used to make the 'NamedArgument'.
instance HasSpace       NamedArgument where typ = qtd . typ
-- ^ Finds the 'Space' of the 'QuantityDict' used to make the 'NamedArgument'.
instance HasSymbol      NamedArgument where symbol = symbol . view qtd
-- ^ Finds the 'Symbol' of the 'QuantityDict' used to make the 'NamedArgument'.
instance Quantity       NamedArgument where 
instance IsArgumentName NamedArgument where
instance Eq             NamedArgument where a == b = (a ^. uid) == (b ^. uid)
-- ^ Equal if 'UID's are equal.
instance MayHaveUnit    NamedArgument where getUnit = getUnit . view qtd
-- ^ Finds the units of the 'QuantityDict' used to make the 'NamedArgument'.
  
-- | Smart constructor for 'NamedArgument' 
narg :: (Quantity q, MayHaveUnit q) => q -> NamedArgument
narg = NA . qw
