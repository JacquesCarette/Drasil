{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Language.Drasil.Chunk.Function where

import Control.Lens

import Language.Drasil.Chunk.UnitDefn
import Language.Drasil.Classes.Core
import Language.Drasil.Classes
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Expr.Display
import Language.Drasil.Expr
import Language.Drasil.Expr.Math
import Language.Drasil.NounPhrase.Core
import Language.Drasil.Space
import Language.Drasil.Sentence
import Language.Drasil.Stages
import Language.Drasil.Symbol
import Language.Drasil.UID

-- TODO: Should we have named arguments for functions? Probably in CodeExpr, but don't think so in Expr
data FuncDefn = FD {
  _qua    :: QuantityDict,
  _inputs :: [UID],
  _defn'  :: Sentence,
  _equat  :: Expr,
  _spc    :: Space,
  cd      :: [UID] -- TODO: make use of this later
}
makeLenses ''FuncDefn

instance HasUID        FuncDefn where uid = qua . uid
instance NamedIdea     FuncDefn where term = qua . term
instance Idea          FuncDefn where getA c = getA $ c ^. qua
instance HasSpace      FuncDefn where typ = spc
instance HasSymbol     FuncDefn where symbol e = symbol (e ^. qua)  -- TODO: what should the symbol be?
instance Definition    FuncDefn where defn = defn'
instance Quantity      FuncDefn where
instance DefiningExpr  FuncDefn where defnExpr = equat
instance Eq            FuncDefn where a == b = (a ^. uid) == (b ^. uid)
instance MayHaveUnit   FuncDefn where getUnit = getUnit . view qua
instance Display       FuncDefn where toDispExpr q = defines (FCall (q ^. uid) (map C $ q ^. inputs) []) (q ^. defnExpr)
instance ConceptDomain FuncDefn where cdom = cd

mkFuncDefn :: (HasUID f, HasSymbol f, HasSpace f, HasUID i, HasSymbol i, HasSpace i, IsUnit u) =>
  f -> NP -> Sentence -> u -> [i] -> Expr -> FuncDefn
mkFuncDefn f n s u is e = FD (mkQuant' (f ^. uid) n Nothing (f ^. typ) (symbol f) (Just $ unitWrapper u)) (map (^. uid) is) s e (mkPrimitiveMapping (map (^. typ) is) (f ^. typ)) []

mkFuncDefn' :: (HasUID f, HasSymbol f, HasSpace f, HasUID i, HasSymbol i, HasSpace i) =>
  f -> NP -> Sentence -> [i] -> Expr -> FuncDefn
mkFuncDefn' f n s is e = FD (mkQuant' (f ^. uid) n Nothing (f ^. typ) (symbol f) Nothing) (map (^. uid) is) s e (mkPrimitiveMapping (map (^. typ) is) (f ^. typ)) []
