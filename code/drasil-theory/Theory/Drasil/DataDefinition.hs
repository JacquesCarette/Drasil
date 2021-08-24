{-# LANGUAGE RankNTypes, NoMonomorphismRestriction, GADTs #-}
-- | Defines types and functions for Data Definitions.
module Theory.Drasil.DataDefinition where

import Control.Lens ((^.), view, Lens', lens)
import Language.Drasil
import Language.Drasil.Development (showUID)
import Data.Drasil.TheoryConcepts (dataDefn)

-- * Types

-- | A scope is an indirect reference to a 'UID'.
newtype Scope = Scp { _spec :: UID }

-- | Determines the scope of data.
data ScopeType =
    Local Scope -- ^ Only visible within a limited scope.
  | Global      -- ^ Visible everywhere.

-- TODO: Alternative -- use ModelKinds and constrain via that?

-- | A data definition is a 'QDefinition' that may have additional notes: 
-- the scope, any references (as 'DecRef's), maybe a derivation, a label ('ShortName'), a reference address, and other notes ('Sentence's).
data DataDefinition e where
  DD :: Express e => QDefinition e -> ScopeType -> [DecRef] -> Maybe Derivation -> ShortName -> String -> [Sentence] -> DataDefinition e

ddQd :: Express e => Lens' (DataDefinition e) (QDefinition e)
ddQd = lens (\(DD qd _ _ _ _ _ _) -> qd) (\(DD _ st dr md sn s ss) qd -> DD qd st dr md sn s ss)

ddScopeType :: Express e => Lens' (DataDefinition e) ScopeType
ddScopeType = lens (\(DD _ st _ _ _ _ _) -> st) (\(DD qd _ dr md sn s ss) st' -> DD qd st' dr md sn s ss)

ddRFs :: Express e => Lens' (DataDefinition e) [DecRef]
ddRFs = lens (\(DD _ _ rfs _ _ _ _) -> rfs) (\(DD qd st _ md sn s ss) dr' -> DD qd st dr' md sn s ss)

ddMbDeriv :: Express e => Lens' (DataDefinition e) (Maybe Derivation)
ddMbDeriv = lens (\(DD _ _ _ mbD _ _ _) -> mbD) (\(DD qd st dr _ sn s ss) md' -> DD qd st dr md' sn s ss)

ddShrtNm :: Express e => Lens' (DataDefinition e) ShortName
ddShrtNm = lens (\(DD _ _ _ _ sn _ _) -> sn) (\(DD qd st dr md _ s ss) sn' -> DD qd st dr md sn' s ss)

ddRA :: Express e => Lens' (DataDefinition e) String
ddRA = lens (\(DD _ _ _ _ _ s _) -> s) (\(DD qd st dr md sn _ ss) s' -> DD qd st dr md sn s' ss)

ddNotes :: Express e => Lens' (DataDefinition e) [Sentence]
ddNotes = lens (\(DD _ _ _ _ _ _ ss) -> ss) (\(DD qd st dr md sn s _) ss' -> DD qd st dr md sn s ss')



-- | Finds the 'UID' of a '(DataDefinition e) where'.
instance Express e => HasUID             (DataDefinition e) where uid = ddQd . uid
-- | Finds the term ('NP') of the 'QDefinition' used to make the '(DataDefinition e) where'.
instance Express e => NamedIdea          (DataDefinition e) where term = ddQd . term
-- | Finds the idea contained in the 'QDefinition' used to make the '(DataDefinition e) where'.
instance Express e => Idea               (DataDefinition e) where getA c = getA $ c ^. ddQd
-- | Finds the Space of the 'QDefinition' used to make the '(DataDefinition e) where'.
instance Express e => HasSpace           (DataDefinition e) where typ = ddQd . typ
-- | Finds the Symbol of the 'QDefinition' used to make the '(DataDefinition e) where'.
instance Express e => HasSymbol          (DataDefinition e) where symbol e = symbol (e ^. ddQd)
-- | '(DataDefinition e) where's have a 'Quantity'.
instance Express e => Quantity           (DataDefinition e) where
-- | Finds the defining expression of the 'QDefinition' used to make the '(DataDefinition e) where'.
instance DefiningExpr       DataDefinition where defnExpr = ddQd . defnExpr
-- | Converts the defining expression of a '(DataDefinition e) where' into the model expression language.
instance Express e => Express            (DataDefinition e) where express d = defines (sy d) (d ^. defnExpr)
{-- Finds 'Reference's contained in the '(DataDefinition e) where'.
instance HasReference       (DataDefinition e) where getReferences = rf-}
-- | Finds 'DecRef's contained in the '(DataDefinition e) where'.
instance Express e => HasDecRef          (DataDefinition e) where getDecRefs = ddRFs
-- | Equal if 'UID's are equal.
instance Express e => Eq                 (DataDefinition e) where a == b = (a ^. uid) == (b ^. uid)
-- | Finds the derivation of the '(DataDefinition e) where'. May contain Nothing.
instance Express e => HasDerivation      (DataDefinition e) where derivations = ddMbDeriv
-- | Finds any additional notes for the '(DataDefinition e) where'.
instance Express e => HasAdditionalNotes (DataDefinition e) where getNotes = ddNotes
-- | Finds the units of the 'QDefinition' used to make the '(DataDefinition e) where'.
instance Express e => MayHaveUnit        (DataDefinition e) where getUnit = getUnit . view ddQd
-- | Finds the 'ShortName' of the '(DataDefinition e) where'.
instance Express e => HasShortName       (DataDefinition e) where shortname = (^. ddShrtNm)
-- | Finds the reference address of a '(DataDefinition e) where'.
instance Express e => HasRefAddress      (DataDefinition e) where getRefAdd l = RP (prepend $ abrv l) (l ^. ddRA)
-- | Finds the domain of the 'QDefinition' used to make the '(DataDefinition e) where'.
instance ConceptDomain      (DataDefinition e) where cdom _ = cdom dataDefn
-- | Finds the idea of a '(DataDefinition e) where' (abbreviation).
instance Express e => CommonIdea         (DataDefinition e) where abrv _ = abrv dataDefn
-- | Finds the reference address of a '(DataDefinition e) where'.
instance Express e => Referable          (DataDefinition e) where
  refAdd      = (^. ddRA)
  renderRef l = RP (prepend $ abrv l) (refAdd l)

-- * Constructors

-- | Smart constructor for data definitions.
dd :: Express e => QDefinition e -> [DecRef] -> Maybe Derivation -> String -> [Sentence] -> DataDefinition e
dd q []   _   _  = error $ "Source field of " ++ showUID q ++ " is empty"
dd q refs der sn = DD q Global refs der (shortname' $ S sn) (prependAbrv dataDefn sn)

-- | Smart constructor for data definitions with no references.
ddNoRefs :: Express e => QDefinition e -> Maybe Derivation -> String -> [Sentence] -> DataDefinition e
ddNoRefs q der sn = DD q Global [] der (shortname' $ S sn) (prependAbrv dataDefn sn)

-- | Extracts the 'QDefinition' from a '(DataDefinition e) where'.
qdFromDD :: Express e => DataDefinition e -> QDefinition e
qdFromDD = (^. ddQd)
