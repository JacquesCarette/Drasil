{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Language.Drasil.Chunk.Eq 
  (QDefinition, fromEqn, fromEqn', fromEqn'', equat, getVC
  , ec, qua, fromEqn''', fromEqn'''') where

import Control.Lens ((^.), makeLenses)
import Language.Drasil.Expr (Expr)
import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  HasSymbol(symbol), IsUnit, ExprRelat(relat), HasDerivation(derivations), 
  HasReference(getReferences), ConceptDomain, HasLabel(getLabel),
  HasAdditionalNotes(getNotes))
import Language.Drasil.Chunk.References (References)
import Language.Drasil.Chunk.Quantity (Quantity(getUnit), HasSpace(typ), QuantityDict,
  mkQuant, qw)
import Language.Drasil.Chunk.VarChunk (VarChunk, vcSt)
import Language.Drasil.Development.Unit (unitWrapper)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Space (Space(Real))
import Language.Drasil.Chunk.Derivation (Derivation)
import Language.Drasil.Chunk.ShortName (HasShortName(shortname))
import Language.Drasil.Label.Core (Label)
import Language.Drasil.Label (mkLabelRA'')

import Language.Drasil.NounPhrase (NP)
import Language.Drasil.Spec (Sentence)

-- | A QDefinition is a 'Quantity' with a defining equation.
data QDefinition = EC
          { _qua :: QuantityDict
          , _equat :: Expr
          , _ref :: References -- FIXME: to be removed
          , _deri :: Derivation -- FIXME: to be removed
          , _lb :: Label -- FIXME: to be removed
          , _notes :: Maybe [Sentence] -- FIXME: to be removed
          }
makeLenses ''QDefinition

-- this works because UnitalChunk is a Chunk
instance HasUID        QDefinition where uid = qua . uid
instance NamedIdea     QDefinition where term = qua . term
instance Idea          QDefinition where getA c = getA $ c ^. qua
instance HasSpace      QDefinition where typ = qua . typ
instance HasSymbol     QDefinition where symbol e st = symbol (e^.qua) st
instance Quantity      QDefinition where getUnit (EC a _ _ _ _ _)   = getUnit a
instance ExprRelat     QDefinition where relat = equat
instance HasReference  QDefinition where getReferences = ref
instance Eq            QDefinition where a == b = (a ^. uid) == (b ^. uid)
instance HasDerivation QDefinition where derivations = deri
instance HasLabel      QDefinition where getLabel = lb
instance HasAdditionalNotes QDefinition where getNotes = notes
instance HasShortName  QDefinition where -- FIXME: This could lead to trouble; need
                                         -- to ensure sanity checking when building
                                         -- Refs. Double-check QDef is a DD before allowing
  shortname = lb . shortname
 

-- | Create a 'QDefinition' with a uid, noun phrase (term), definition, symbol,
-- unit, and defining equation.  And it ignores the definition...
--FIXME: Space hack
fromEqn :: (IsUnit u, ConceptDomain u) => 
  String -> NP -> Sentence -> Symbol -> u -> Expr -> References -> Label -> QDefinition
fromEqn nm desc _ symb un eqn refs lbe = 
  EC (mkQuant nm desc symb Real (Just $ unitWrapper un) Nothing) eqn refs [] lbe Nothing

-- | Same as fromEqn, but has no units.
--FIXME: Space hack
fromEqn' :: String -> NP -> Sentence -> Symbol -> Expr -> References -> Label -> QDefinition
fromEqn' nm desc _ symb eqn refs lbe = EC (mkQuant nm desc symb Real Nothing Nothing) eqn refs [] lbe Nothing

-- | Create a 'QDefinition' with an uid, noun phrase (term), symbol,
-- abbreviation, unit, and defining equation.
fromEqn'' :: (IsUnit u, ConceptDomain u) => String -> NP -> Sentence ->
 Symbol -> String -> Maybe u -> Expr -> References -> Label -> QDefinition
fromEqn'' nm desc _ symb abbr u eqn refs lbe = 
  EC (mkQuant nm desc symb Real (fmap unitWrapper u) (Just abbr)) 
  eqn refs [] lbe Nothing


fromEqn''' :: (IsUnit u, ConceptDomain u) => 
  String -> NP -> Sentence -> Symbol -> u -> Expr -> References -> Derivation -> String -> QDefinition
fromEqn''' nm desc _ symb un eqn refs dv sn = 
  EC (mkQuant nm desc symb Real (Just $ unitWrapper un) Nothing) 
  eqn refs dv (mkLabelRA'' sn) Nothing

fromEqn'''' :: String -> NP -> Sentence -> Symbol -> Expr -> References -> Derivation -> String -> QDefinition
fromEqn'''' nm desc _ symb eqn refs dv sn = EC (mkQuant nm desc symb Real Nothing Nothing)
  eqn refs dv (mkLabelRA'' sn) Nothing

-- | Smart constructor for QDefinitions. Requires a quantity and its defining 
-- equation
ec :: (Quantity c) => c -> Expr -> Label -> QDefinition
ec c eqn lbe = EC (qw c) eqn [] [] lbe Nothing --hack?

-- | Returns a 'VarChunk' from a 'QDefinition'.
-- Currently only used in example /Modules/ which are being reworked.
getVC :: QDefinition -> VarChunk
getVC qd = vcSt (qd ^. uid) (qd ^. term) (symbol qd) (qd ^. typ)
