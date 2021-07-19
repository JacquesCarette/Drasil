{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Language.Drasil.Chunk.Function (
  FuncDefn(..),
  mkFuncDefn, mkFuncDefn', mkFuncDefnByQ
) where

import Control.Lens ((^.), view, makeLenses)

import Language.Drasil.Chunk.UnitDefn (unitWrapper, IsUnit, MayHaveUnit(..), UnitDefn)
import Language.Drasil.Classes.Core (HasSymbol(..), HasUID(..))
import Language.Drasil.Classes (ConceptDomain(..), Display(..), DefiningExpr(..),
  Quantity, Definition(..), HasSpace(..), Idea(..), NamedIdea(..))
import Language.Drasil.Chunk.Quantity (mkQuant', QuantityDict)
import Language.Drasil.Expr.Display (defines)
import Language.Drasil.Expr (Expr(FCall, C))
import Language.Drasil.NounPhrase.Core (NP)
import Language.Drasil.Space (mkPrimitiveMapping, Space)
import Language.Drasil.Sentence (Sentence(EmptyS))
import Language.Drasil.UID (UID)

-- TODO: Should we have named arguments for functions? Probably in CodeExpr, but don't think so in Expr. Perhaps the FCalls need to be different!
data FuncDefn = FD {
  _qua    :: QuantityDict,
  _inputs :: [UID],
  _defn'  :: Sentence,
  _ex     :: Expr,
  _spc    :: Space,
  cd      :: [UID] -- FIXME: unused!
}
makeLenses ''FuncDefn

instance HasUID        FuncDefn where uid = qua . uid
instance NamedIdea     FuncDefn where term = qua . term
instance Idea          FuncDefn where getA = getA . (^. qua)
instance HasSpace      FuncDefn where typ = spc
instance HasSymbol     FuncDefn where symbol = symbol . (^. qua)  -- TODO: what should the symbol be? "F(X,Y,Z)" or just "F"? Leaving as "F" for now since we match stable
instance Definition    FuncDefn where defn = defn'
instance Quantity      FuncDefn where
instance DefiningExpr  FuncDefn where defnExpr = ex
instance Eq            FuncDefn where a == b = (a ^. uid) == (b ^. uid)
instance MayHaveUnit   FuncDefn where getUnit = getUnit . view qua
instance ConceptDomain FuncDefn where cdom = cd
instance Display       FuncDefn where
  toDispExpr q = defines (FCall (q ^. uid) (map C $ q ^. inputs) []) (q ^. defnExpr)

-- | Factored version of 'mkFuncDefn' functions
mkFuncDefn0 :: (HasUID f, HasSymbol f, HasSpace f,
                HasUID i, HasSymbol i, HasSpace i) =>
  f -> NP -> Sentence -> Maybe UnitDefn -> [i] -> Expr -> FuncDefn
mkFuncDefn0 f n s u is e = FD
  (mkQuant' (f ^. uid) n Nothing (f ^. typ) (symbol f) u)
  (map (^. uid) is) s e
  (mkPrimitiveMapping (map (^. typ) is) (f ^. typ)) []

-- | Create 'FuncDefn' with a symbol, name, term, list of inputs, resultant units, and a defining Expr
mkFuncDefn :: (HasUID f, HasSymbol f, HasSpace f,
               HasUID i, HasSymbol i, HasSpace i,
               IsUnit u) =>
  f -> NP -> Sentence -> u -> [i] -> Expr -> FuncDefn
mkFuncDefn f n s u = mkFuncDefn0 f n s (Just $ unitWrapper u)

-- | Create 'FuncDefn' with a symbol, name, term, list of inputs, and a defining Expr
mkFuncDefn' :: (HasUID f, HasSymbol f, HasSpace f,
                HasUID i, HasSymbol i, HasSpace i) =>
  f -> NP -> Sentence -> [i] -> Expr -> FuncDefn
mkFuncDefn' f n s = mkFuncDefn0 f n s Nothing

-- | Create 'FuncDefn's using a symbol, list of inputs, and a defining Expr
mkFuncDefnByQ :: (Quantity c, MayHaveUnit c, HasSpace c,
                  Quantity i, HasSpace i) =>
  c -> [i] -> Expr -> FuncDefn
mkFuncDefnByQ f = case getUnit f of
  Just u  -> mkFuncDefn  f (f ^. term) EmptyS u
  Nothing -> mkFuncDefn' f (f ^. term) EmptyS
