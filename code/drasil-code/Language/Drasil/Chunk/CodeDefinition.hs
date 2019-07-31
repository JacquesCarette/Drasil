{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.CodeDefinition (
  CodeDefinition, qtoc, qtov, codeEquat
) where

import Language.Drasil
import Language.Drasil.Chunk.Code (CodeChunk, CodeIdea(codeName, codeChunk), 
  quantvar, quantfunc)
import Language.Drasil.Chunk.CodeQuantity (HasCodeType(ctyp))

import Control.Lens ((^.), makeLenses, view)

data CodeDefinition = CD { _cchunk :: CodeChunk
                         , _def :: Expr
                         }
makeLenses ''CodeDefinition

instance HasUID       CodeDefinition where uid = cchunk . uid
instance NamedIdea    CodeDefinition where term = cchunk . term
instance Idea         CodeDefinition where getA = getA . view cchunk
instance HasCodeType  CodeDefinition where ctyp = cchunk . ctyp
instance HasSymbol    CodeDefinition where symbol c = symbol (c ^. cchunk)
instance CodeIdea     CodeDefinition where 
  codeName = codeName . view cchunk
  codeChunk = view cchunk
instance Eq           CodeDefinition where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance MayHaveUnit  CodeDefinition where getUnit = getUnit . view cchunk
instance DefiningExpr CodeDefinition where defnExpr = def

qtoc :: (Quantity q, DefiningExpr q, MayHaveUnit q) => q -> CodeDefinition
qtoc q = CD (quantfunc q) (q ^. defnExpr)

qtov :: QDefinition -> CodeDefinition
qtov q = CD (quantvar q) (q ^. defnExpr)

codeEquat :: CodeDefinition -> Expr
codeEquat cd = cd ^. def