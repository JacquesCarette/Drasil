{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.CodeQuantity 
  (HasCodeType(ctyp), CodeQuantityDict, cqd, implCQD, cqw) where

import Control.Lens (Lens', (^.), makeLenses, view)

import Language.Drasil

import Language.Drasil.Code.Code (CodeType, spaceToCodeType)

-- | HasCodeType is anything which has a CodeType
class HasCodeType c where
  ctyp      :: Lens' c CodeType

data CodeQuantityDict = CQD { _id' :: IdeaDict
                            , _typ' :: CodeType
                            , _symb' :: Stage -> Symbol
                            , _unit' :: Maybe UnitDefn
                            }
makeLenses ''CodeQuantityDict

instance HasUID      CodeQuantityDict where uid = id' . uid
instance NamedIdea   CodeQuantityDict where term = id' . term
instance Idea        CodeQuantityDict where getA  qd = getA (qd ^. id')
instance HasSymbol   CodeQuantityDict where symbol = view symb'
instance HasCodeType CodeQuantityDict where ctyp = typ'
instance Eq          CodeQuantityDict where a == b = (a ^. uid) == (b ^. uid)
instance MayHaveUnit CodeQuantityDict where getUnit = view unit'

cqd :: String -> NP -> Maybe String -> CodeType -> (Stage -> Symbol) -> 
  Maybe UnitDefn -> CodeQuantityDict
cqd s np a = CQD (mkIdea s np a)

-- For CodeQuantityDict with implementation-only symbol
implCQD :: String -> NP -> Maybe String -> CodeType -> Symbol -> 
  Maybe UnitDefn -> CodeQuantityDict
implCQD s np a t sym = cqd s np a t f
  where f :: Stage -> Symbol
        f Implementation = sym
        f Equational = Empty

cqw :: (Quantity q, MayHaveUnit q) => q -> CodeQuantityDict
cqw q = CQD (nw q) (spaceToCodeType $ q^.typ) (symbol q) (getUnit q)
