{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.DataDefinition where

import Language.Drasil.Chunk.Eq (QDefinition, fromEqn, fromEqn')
import Language.Drasil.Spec (Sentence(EmptyS))
import Language.Drasil.Chunk.References (References)
import Language.Drasil.Chunk.Derivation (Derivation)
import Language.Drasil.Expr (Expr)
import Language.Drasil.Chunk.Quantity (Quantity(getUnit), HasSpace(typ))
import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  HasSymbol(symbol), ExprRelat(relat), HasDerivation(derivations), 
  HasReference(getReferences), HasAdditionalNotes(getNotes))
import Language.Drasil.Chunk.SymbolForm (eqSymb)
import Language.Drasil.Chunk.ShortName (ShortName, HasShortName(shortname), shortname')
import Language.Drasil.Label.Core (Label)
import Control.Lens(makeLenses, (^.), view)

import Language.Drasil.Chunk.Eq(fromEqn''', fromEqn'''')

data Scope = Scp { _spec :: Label {-indirect reference-}}

data ScopeType = Local Scope {-only visible within a limited scope-} | Global {-visible everywhere-}

-- A data definition is a QDefinition that may have additional notes. 
-- It also has attributes like derivation, source, etc.
data DataDefinition = DD { _qd :: QDefinition
                         , _scp :: ScopeType
                         , _ref :: References
                         , _deri :: Derivation
                         , _lbl :: ShortName {-FIXME: Upgrade to Label-}
                         , _notes :: Maybe [Sentence]
                         }
makeLenses ''DataDefinition

-- this works because UnitalChunk is a Chunk
instance HasUID             DataDefinition where uid = qd . uid
instance NamedIdea          DataDefinition where term = qd . term
instance Idea               DataDefinition where getA c = getA $ c ^. qd
instance HasSpace           DataDefinition where typ = qd . typ
instance HasSymbol          DataDefinition where symbol e st = symbol (e^.qd) st
instance Quantity           DataDefinition where getUnit (DD a _ _ _ _ _) = getUnit a
instance ExprRelat          DataDefinition where relat = qd . relat
instance HasReference       DataDefinition where getReferences = ref
instance Eq                 DataDefinition where a == b = (a ^. uid) == (b ^. uid)
instance HasDerivation      DataDefinition where derivations = deri
instance HasAdditionalNotes DataDefinition where getNotes = notes
instance HasShortName       DataDefinition where shortname = view lbl

-- Used to help make Qdefinitions when uid, term, and symbol come from the same source
mkDataDef :: (Quantity c) => c -> Expr -> QDefinition
mkDataDef cncpt equation = datadef $ getUnit cncpt --should references be passed in at this point?
  where datadef (Just a) = fromEqn  (cncpt ^. uid) (cncpt ^. term) EmptyS
                           (eqSymb cncpt) a equation [] (cncpt ^. uid) --shortname
        datadef Nothing  = fromEqn' (cncpt ^. uid) (cncpt ^. term) EmptyS
                           (eqSymb cncpt) equation [] (cncpt ^. uid) --shortname

mkDataDef' :: (Quantity c) => c -> Expr -> Derivation -> QDefinition
mkDataDef' cncpt equation dv = datadef $ getUnit cncpt --should references be passed in at this point?
  where datadef (Just a) = fromEqn'''  (cncpt ^. uid) (cncpt ^. term) EmptyS
                           (eqSymb cncpt) a equation [] dv (cncpt ^. uid) --shortname
        datadef Nothing  = fromEqn'''' (cncpt ^. uid) (cncpt ^. term) EmptyS
                           (eqSymb cncpt) equation [] dv (cncpt ^. uid) --shortname

-- | Smart constructor for data definitions 
mkDD :: QDefinition -> References -> Derivation -> String{-Label-} -> Maybe [Sentence] -> DataDefinition
mkDD a b c _ e = DD a Global b c (shortname' $ a ^. uid  {-shortname' d-}) e -- FIXME: should the shortname be passed in or derived?

qdFromDD :: DataDefinition -> QDefinition
qdFromDD (DD a _ _ _ _ _) = a
