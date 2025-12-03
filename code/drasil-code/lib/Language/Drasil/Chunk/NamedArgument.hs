{-# LANGUAGE TemplateHaskell #-}
-- | Named arguments used in generating code.
module Language.Drasil.Chunk.NamedArgument (
  -- * Chunk Type
  NamedArgument(..),
  -- * Constructor
  narg) where

import Control.Lens ((^.), makeLenses, view)

import Language.Drasil (HasSpace(..), HasSymbol(..), HasUID(..),
  Idea(..), MayHaveUnit(..), NamedIdea(..), Quantity,
  DefinedQuantityDict, Concept, dqdWr, Definition (defn), ConceptDomain (cdom))

import Drasil.Code.Classes (IsArgumentName)

-- | Any quantity can be a named argument (wrapper for 'DefinedQuantityDict'),
-- but with more of a focus on generating code arguments.
newtype NamedArgument = NA {_qtd :: DefinedQuantityDict}
makeLenses ''NamedArgument

-- | Finds the 'UID' of the 'DefinedQuantityDict' used to make the 'NamedArgument'.
instance HasUID         NamedArgument where uid = qtd . uid
-- | Finds the term ('NP') of the 'DefinedQuantityDict' used to make the 'NamedArgument'.
instance NamedIdea      NamedArgument where term = qtd . term
-- | Finds the idea contained in the 'DefinedQuantityDict' used to make the 'NamedArgument'.
instance Idea           NamedArgument where getA = getA . view qtd

instance Definition     NamedArgument where defn = qtd . defn

instance ConceptDomain  NamedArgument where cdom = cdom . view qtd
-- | Finds the 'Space' of the 'DefinedQuantityDict' used to make the 'NamedArgument'.
instance HasSpace       NamedArgument where typ = qtd . typ
-- | Finds the 'Symbol' of the 'DefinedQuantityDict' used to make the 'NamedArgument'.
instance HasSymbol      NamedArgument where symbol = symbol . view qtd
-- | 'NamedArgument's have a 'Quantity'.
instance Quantity       NamedArgument where
-- | 'NamedArgument's have an argument name.
instance IsArgumentName NamedArgument where
-- | Equal if 'UID's are equal.
instance Eq             NamedArgument where a == b = (a ^. uid) == (b ^. uid)
-- | Finds the units of the 'DefinedQuantityDict' used to make the 'NamedArgument'.
instance MayHaveUnit    NamedArgument where getUnit = getUnit . view qtd

-- | Smart constructor for 'NamedArgument' .
narg :: (Quantity q, MayHaveUnit q, Concept q) => q -> NamedArgument
narg = NA . dqdWr
