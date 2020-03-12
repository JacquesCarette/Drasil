{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.CodeQuantity 
  (CodeQuantityDict, cqd, implCQD, cqw) where

import Control.Lens ((^.), makeLenses, view)

import Language.Drasil

data CodeQuantityDict = CQD { _id' :: IdeaDict
                            , _typ' :: Space
                            , _symb' :: Stage -> Symbol
                            , _unit' :: Maybe UnitDefn
                            }
makeLenses ''CodeQuantityDict

instance HasUID      CodeQuantityDict where uid = id' . uid
instance NamedIdea   CodeQuantityDict where term = id' . term
instance Idea        CodeQuantityDict where getA  qd = getA (qd ^. id')
instance HasSymbol   CodeQuantityDict where symbol = view symb'
instance HasSpace    CodeQuantityDict where typ = typ'
instance Quantity    CodeQuantityDict
instance Eq          CodeQuantityDict where a == b = (a ^. uid) == (b ^. uid)
instance MayHaveUnit CodeQuantityDict where getUnit = view unit'

cqd :: String -> NP -> Maybe String -> Space -> (Stage -> Symbol) -> 
  Maybe UnitDefn -> CodeQuantityDict
cqd s np a = CQD (mkIdea s np a)

-- For CodeQuantityDict with implementation-only symbol
implCQD :: String -> NP -> Maybe String -> Space -> Symbol -> 
  Maybe UnitDefn -> CodeQuantityDict
implCQD s np a t sym = cqd s np a t f
  where f :: Stage -> Symbol
        f Implementation = sym
        f Equational = Empty

cqw :: (Quantity q, MayHaveUnit q) => q -> CodeQuantityDict
cqw q = CQD (nw q) (q^.typ) (symbol q) (getUnit q)
