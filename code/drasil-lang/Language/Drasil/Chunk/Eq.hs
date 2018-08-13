{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Language.Drasil.Chunk.Eq 
  (QDefinition, fromEqn, fromEqn', fromEqn'', equat, getVC
  , ec, qua, fromEqn''', fromEqn'''') where

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  HasSymbol(symbol), IsUnit, ExprRelat(relat),
  ConceptDomain, HasLabel(getLabel))
import Language.Drasil.Chunk.Quantity (HasSpace(typ), QuantityDict,
  mkQuant, qw)
import Language.Drasil.Chunk.VarChunk (VarChunk, vcSt)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Space (Space(Real))
import Language.Drasil.Chunk.ShortName (HasShortName(shortname))
import Language.Drasil.Label.Core (Label)
import Language.Drasil.Label (mkLabelSame)

import Control.Lens ((^.), makeLenses, view)

import Language.Drasil.RefTypes(RefType(..), DType(..))
import Language.Drasil.Chunk.Quantity (Quantity)
import Language.Drasil.Development.Unit(unitWrapper, MayHaveUnit(getUnit))
import Language.Drasil.Expr (Expr)
import Language.Drasil.NounPhrase (NP)
import Language.Drasil.Spec (Sentence)

-- | A QDefinition is a 'Quantity' with a defining equation.
data QDefinition = EC
          { _qua :: QuantityDict
          , _equat :: Expr
          , _lb :: Label -- FIXME: to be removed
          }
makeLenses ''QDefinition

-- this works because UnitalChunk is a Chunk
instance HasUID        QDefinition where uid = qua . uid
instance NamedIdea     QDefinition where term = qua . term
instance Idea          QDefinition where getA c = getA $ c ^. qua
instance HasSpace      QDefinition where typ = qua . typ
instance HasSymbol     QDefinition where symbol e st = symbol (e^.qua) st
instance Quantity      QDefinition where 
instance ExprRelat     QDefinition where relat = equat
instance Eq            QDefinition where a == b = (a ^. uid) == (b ^. uid)
instance HasLabel      QDefinition where getLabel = lb
instance MayHaveUnit   QDefinition where getUnit = getUnit . view qua
instance HasShortName  QDefinition where -- FIXME: This could lead to trouble; need
                                         -- to ensure sanity checking when building
                                         -- Refs. Double-check QDef is a DD before allowing
  shortname = lb . shortname
 

-- | Create a 'QDefinition' with a uid, noun phrase (term), definition, symbol,
-- unit, and defining equation.  And it ignores the definition...
--FIXME: Space hack
fromEqn :: (IsUnit u, ConceptDomain u) => 
  String -> NP -> Sentence -> Symbol -> u -> Expr -> Label -> QDefinition
fromEqn nm desc _ symb un eqn lbe = 
  EC (mkQuant nm desc symb Real (Just $ unitWrapper un) Nothing) eqn lbe

-- | Same as fromEqn, but has no units.
--FIXME: Space hack
fromEqn' :: String -> NP -> Sentence -> Symbol -> Expr -> Label -> QDefinition
fromEqn' nm desc _ symb eqn lbe = EC (mkQuant nm desc symb Real Nothing Nothing) eqn lbe

-- | Create a 'QDefinition' with an uid, noun phrase (term), symbol,
-- abbreviation, unit, and defining equation.
fromEqn'' :: (IsUnit u, ConceptDomain u) => String -> NP -> Sentence ->
 Symbol -> String -> Maybe u -> Expr -> Label -> QDefinition
fromEqn'' nm desc _ symb abbr u eqn lbe = 
  EC (mkQuant nm desc symb Real (fmap unitWrapper u) (Just abbr)) 
  eqn lbe


fromEqn''' :: (IsUnit u, ConceptDomain u) => 
  String -> NP -> Sentence -> Symbol -> u -> Expr -> String -> QDefinition
fromEqn''' nm desc _ symb un eqn sn = 
  EC (mkQuant nm desc symb Real (Just $ unitWrapper un) Nothing) 
  eqn (mkLabelSame sn (Def DD))

fromEqn'''' :: String -> NP -> Sentence -> Symbol -> Expr -> String -> QDefinition
fromEqn'''' nm desc _ symb eqn sn = EC (mkQuant nm desc symb Real Nothing Nothing)
  eqn (mkLabelSame sn (Def DD))

-- | Smart constructor for QDefinitions. Requires a quantity and its defining 
-- equation
ec :: (Quantity c) => c -> Expr -> Label -> QDefinition
ec c eqn lbe = EC (qw c) eqn lbe --hack?

-- | Returns a 'VarChunk' from a 'QDefinition'.
-- Currently only used in example /Modules/ which are being reworked.
getVC :: QDefinition -> VarChunk
getVC qd = vcSt (qd ^. uid) (qd ^. term) (symbol qd) (qd ^. typ)
