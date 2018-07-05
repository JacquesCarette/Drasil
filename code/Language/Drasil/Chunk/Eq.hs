{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Language.Drasil.Chunk.Eq 
  (QDefinition, fromEqn, fromEqn', fromEqn'', equat, getVC
  , ec) where

import Control.Lens ((^.), makeLenses, view)
import Language.Drasil.Expr (Expr)
import Language.Drasil.Classes (HasUID(uid),NamedIdea(term), Idea(getA),
  HasSymbol(symbol), IsUnit, ExprRelat(relat), HasDerivation(derivations), 
  HasReference(getReferences), ConceptDomain, HasAdditionalNotes(getNotes))
import Language.Drasil.Chunk.References (References)
import Language.Drasil.Chunk.Quantity (Quantity(getUnit), HasSpace(typ), QuantityDict,
  mkQuant, qw)
import Language.Drasil.Chunk.VarChunk (VarChunk, vcSt)
import Language.Drasil.Unit (unitWrapper)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Space (Space(Real))
import Language.Drasil.Chunk.Derivation (Derivation)
import Language.Drasil.Chunk.ShortName (ShortName, HasShortName(shortname), shortname')

import Language.Drasil.NounPhrase (NP)
import Language.Drasil.Spec (Sentence)

-- | A QDefinition is a 'Quantity' with a defining equation.
data QDefinition = EC
          { _qua :: QuantityDict
          , _equat :: Expr
          , _ref :: References
          , _deri :: Derivation
          , _refName :: ShortName
          , _notes :: Maybe [Sentence]
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
instance HasAdditionalNotes QDefinition where getNotes = notes
instance HasShortName  QDefinition where -- FIXME: This could lead to trouble; need
                                         -- to ensure sanity checking when building
                                         -- Refs. Double-check QDef is a DD before allowing
  shortname = view refName

-- | Create a 'QDefinition' with a uid, noun phrase (term), definition, symbol,
-- unit, and defining equation.  And it ignores the definition...
--FIXME: Space hack
fromEqn :: (IsUnit u, ConceptDomain u) => 
  String -> NP -> Sentence -> Symbol -> u -> Expr -> References -> String -> QDefinition
fromEqn nm desc _ symb un eqn refs sn = 
  EC (mkQuant nm desc symb Real (Just $ unitWrapper un) Nothing) eqn refs [] (shortname' sn) Nothing

-- | Same as fromEqn, but has no units.
--FIXME: Space hack
fromEqn' :: String -> NP -> Sentence -> Symbol -> Expr -> References -> String -> QDefinition
fromEqn' nm desc _ symb eqn refs sn = EC (mkQuant nm desc symb Real Nothing Nothing) eqn refs [] (shortname' sn) Nothing

-- | Create a 'QDefinition' with an uid, noun phrase (term), symbol,
-- abbreviation, unit, and defining equation.
fromEqn'' :: (IsUnit u, ConceptDomain u) => String -> NP -> Sentence ->
 Symbol -> String -> Maybe u -> Expr -> References -> String -> QDefinition
fromEqn'' nm desc _ symb abbr u eqn refs sn = 
  EC (mkQuant nm desc symb Real (fmap unitWrapper u) (Just abbr)) eqn refs [] (shortname' sn) Nothing

-- | Smart constructor for QDefinitions. Requires a quantity and its defining 
-- equation
ec :: (Quantity c) => c -> Expr -> ShortName -> QDefinition
ec c eqn sn = EC (qw c) eqn [] [] sn {-hack?-} Nothing

-- | Returns a 'VarChunk' from a 'QDefinition'.
-- Currently only used in example /Modules/ which are being reworked.
getVC :: QDefinition -> VarChunk
getVC qd = vcSt (qd ^. uid) (qd ^. term) (symbol qd) (qd ^. typ)
