{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.CodeQuantity 
  (HasCodeType(ctyp), CodeQuantityDict, cqw) where

import Control.Lens (Lens', (^.), makeLenses, view)

import Language.Drasil

import Language.Drasil.Chunk.Code (spaceToCodeType)
import Language.Drasil.Code.Code (CodeType)

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

cqw :: (Quantity q, MayHaveUnit q) => q -> CodeQuantityDict
cqw q = CQD (nw q) (spaceToCodeType $ q^.typ) (symbol q) (getUnit q)