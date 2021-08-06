{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Theory.Drasil.DataDefinition where

import Control.Lens (makeLenses, (^.), view)
import Language.Drasil
import Data.Drasil.TheoryConcepts (dataDefn)

-- | A scope is an indirect reference to a 'UID'.
newtype Scope = Scp { _spec :: UID }

-- | Determines the scope of data.
data ScopeType =
    Local Scope -- ^ Only visible within a limited scope.
  | Global      -- ^ Visible everywhere.

-- | A data definition is a 'QDefinition' that may have additional notes: 
-- the scope, any references (as 'DecRef's), maybe a derivation, a label ('ShortName'), a reference address, and other notes ('Sentence's).
data DataDefinition e = DatDef { _qd    :: Express e => QDefinition e
                               , _scp   :: ScopeType
                               , _rf    :: [DecRef]
                               , _deri  :: Maybe Derivation
                               , lbl    :: ShortName
                               , ra     :: String
                               , _notes :: [Sentence]
                               }
makeLenses ''DataDefinition

-- | Finds the 'UID' of a '(DataDefinition e) where'.
instance Express e => HasUID             (DataDefinition e) where uid = qd . uid
-- | Finds the term ('NP') of the 'QDefinition' used to make the '(DataDefinition e) where'.
instance Express e => NamedIdea          (DataDefinition e) where term = qd . term
-- | Finds the idea contained in the 'QDefinition' used to make the '(DataDefinition e) where'.
instance Express e => Idea               (DataDefinition e) where getA c = getA $ c ^. qd
-- | Finds the Space of the 'QDefinition' used to make the '(DataDefinition e) where'.
instance Express e => HasSpace           (DataDefinition e) where typ = qd . typ
-- | Finds the Symbol of the 'QDefinition' used to make the '(DataDefinition e) where'.
instance Express e => HasSymbol          (DataDefinition e) where symbol e = symbol (e ^. qd)
-- | '(DataDefinition e) where's have a 'Quantity'.
instance Express e => Quantity           (DataDefinition e) where 
-- | Finds the defining expression of the 'QDefinition' used to make the '(DataDefinition e) where'.
instance DefiningExpr       DataDefinition where defnExpr = qd . defnExpr
-- | Converts the defining expression of a '(DataDefinition e) where' into the model expression language.
instance Express e => Express            (DataDefinition e) where express d = defines (sy d) (d ^. defnExpr)
{-- Finds 'Reference's contained in the '(DataDefinition e) where'.
instance HasReference       (DataDefinition e) where getReferences = rf-}
-- | Finds 'DecRef's contained in the '(DataDefinition e) where'.
instance Express e => HasDecRef          (DataDefinition e) where getDecRefs = rf
-- | Equal if 'UID's are equal.
instance Express e => Eq                 (DataDefinition e) where a == b = (a ^. uid) == (b ^. uid)
-- | Finds the derivation of the '(DataDefinition e) where'. May contain Nothing.
instance Express e => HasDerivation      (DataDefinition e) where derivations = deri
-- | Finds any additional notes for the '(DataDefinition e) where'.
instance Express e => HasAdditionalNotes (DataDefinition e) where getNotes = notes
-- | Finds the units of the 'QDefinition' used to make the '(DataDefinition e) where'.
instance Express e => MayHaveUnit        (DataDefinition e) where getUnit = getUnit . view qd 
-- | Finds the 'ShortName' of the '(DataDefinition e) where'.
instance Express e => HasShortName       (DataDefinition e) where shortname = lbl
-- | Finds the reference address of a '(DataDefinition e) where'.
instance Express e => HasRefAddress      (DataDefinition e) where getRefAdd l = RP (prepend $ abrv l) (ra l)
-- | Finds the domain of the 'QDefinition' used to make the '(DataDefinition e) where'.
instance Express e => ConceptDomain      (DataDefinition e) where cdom _ = cdom dataDefn
-- | Finds the idea of a '(DataDefinition e) where' (abbreviation).
instance Express e => CommonIdea         (DataDefinition e) where abrv _ = abrv dataDefn
-- | Finds the reference address of a '(DataDefinition e) where'.
instance Express e => Referable          (DataDefinition e) where
  refAdd      = ra
  renderRef l = RP (prepend $ abrv l) (refAdd l)

-- | Smart constructor for data definitions.
dd :: Express e => QDefinition e -> [DecRef] -> Maybe Derivation -> String -> [Sentence] -> DataDefinition e
dd q []   _   _  = error $ "Source field of " ++ q ^. uid ++ " is empty"
dd q refs der sn = DatDef q Global refs der (shortname' $ S sn) (prependAbrv dataDefn sn)

-- | Smart constructor for data definitions with no references.
ddNoRefs :: Express e => QDefinition e -> Maybe Derivation -> String -> [Sentence] -> DataDefinition e
ddNoRefs q der sn = DatDef q Global [] der (shortname' $ S sn) (prependAbrv dataDefn sn)

-- | Extracts the 'QDefinition' from a '(DataDefinition e) where'.
qdFromDD :: Express e => DataDefinition e -> QDefinition e
qdFromDD = (^. qd)
