{-# LANGUAGE RankNTypes, NoMonomorphismRestriction, GADTs, TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Defines types and functions for Data Definitions.
module Theory.Drasil.DataDefinition where

import Control.Lens
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

data DDPkt = DDPkt {
  _pktST :: ScopeType,
  _pktDR :: [DecRef],
  _pktMD :: Maybe Derivation,
  _pktSN :: ShortName,
  _pktS  :: String,
  _pktSS :: [Sentence]
}
makeLenses ''DDPkt

-- | A data definition is a 'QDefinition' that may have additional notes: 
-- the scope, any references (as 'DecRef's), maybe a derivation, a label ('ShortName'), a reference address, and other notes ('Sentence's).
data DataDefinition where
  DDE  :: SimpleQDef -> DDPkt -> DataDefinition
  DDME :: ModelQDef -> DDPkt -> DataDefinition

ddQD :: Lens' SimpleQDef a -> Lens' ModelQDef a -> Lens' DataDefinition a
ddQD lqde lqdme = lens g s
  where
    g (DDE  qd _) = qd ^. lqde
    g (DDME qd _) = qd ^. lqdme
    s (DDE  qd pkt) u = DDE  (qd & lqde .~ u)  pkt
    s (DDME qd pkt) u = DDME (qd & lqdme .~ u) pkt

ddPkt :: Lens' DDPkt a -> Lens' DataDefinition a
ddPkt lpkt = lens g s
  where
    g (DDE  _ pkt) = pkt ^. lpkt
    g (DDME _ pkt) = pkt ^. lpkt
    s (DDE  qd pkt) a' = DDE  qd (pkt & lpkt .~ a')
    s (DDME qd pkt) a' = DDME qd (pkt & lpkt .~ a')

-- | Finds the 'UID' of a 'DataDefinition where'.
instance HasUID             DataDefinition where uid = ddQD uid uid
-- | Finds the term ('NP') of the 'QDefinition' used to make the 'DataDefinition where'.
instance NamedIdea          DataDefinition where term = ddQD term term
-- | Finds the idea contained in the 'QDefinition' used to make the 'DataDefinition where'.
instance Idea               DataDefinition where getA = either getA getA . qdFromDD
-- | Finds the Space of the 'QDefinition' used to make the 'DataDefinition where'.
instance HasSpace           DataDefinition where typ = ddQD typ typ
-- | Finds the Symbol of the 'QDefinition' used to make the 'DataDefinition where'.
instance HasSymbol          DataDefinition where symbol = either symbol symbol . qdFromDD
-- | 'DataDefinition where's have a 'Quantity'.
instance Quantity           DataDefinition where
-- | Converts the defining expression of a 'DataDefinition where' into the model expression language.
instance Express            DataDefinition where express = either express express . qdFromDD
{-- Finds 'Reference's contained in the 'DataDefinition where'.
instance HasReference       DataDefinition where getReferences = rf-}
-- | Finds 'DecRef's contained in the 'DataDefinition where'.
instance HasDecRef          DataDefinition where getDecRefs = ddPkt pktDR
-- | Equal if 'UID's are equal.
instance Eq                 DataDefinition where a == b = (a ^. uid) == (b ^. uid)
-- | Finds the derivation of the 'DataDefinition where'. May contain Nothing.
instance HasDerivation      DataDefinition where derivations = ddPkt pktMD
-- | Finds any additional notes for the 'DataDefinition where'.
instance HasAdditionalNotes DataDefinition where getNotes = ddPkt pktSS
-- | Finds the units of the 'QDefinition' used to make the 'DataDefinition where'.
instance MayHaveUnit        DataDefinition where getUnit = either getUnit getUnit . qdFromDD
-- | Finds the 'ShortName' of the 'DataDefinition where'.
instance HasShortName       DataDefinition where shortname = (^. ddPkt pktSN)
-- | Finds the reference address of a 'DataDefinition where'.
instance HasRefAddress      DataDefinition where getRefAdd l = RP (prepend $ abrv l) (l ^. ddPkt pktS)
-- | Finds the domain of the 'QDefinition' used to make the 'DataDefinition where'.
instance ConceptDomain      DataDefinition where cdom _ = cdom dataDefn
-- | Finds the idea of a 'DataDefinition where' (abbreviation).
instance CommonIdea         DataDefinition where abrv _ = abrv dataDefn
-- | Finds the reference address of a 'DataDefinition where'.
instance Referable          DataDefinition where
  refAdd      = (^. ddPkt pktS)
  renderRef l = RP (prepend $ abrv l) (refAdd l)
-- | Expose all expressions that need to be type-checked.
instance RequiresChecking DataDefinition Expr Space where
  requiredChecks (DDE  qd _) = requiredChecks qd
  requiredChecks  DDME {}    = []

-- * Constructors

-- | Smart constructor for data definitions.
ddE :: SimpleQDef -> [DecRef] -> Maybe Derivation -> String -> [Sentence] -> DataDefinition
ddE q []   _   _  = error $ "Source field of " ++ showUID q ++ " is empty"
ddE q refs der sn = DDE q . DDPkt Global refs der (shortname' $ S sn) (prependAbrv dataDefn sn)

-- | Smart constructor for data definitions with no references.
ddENoRefs :: SimpleQDef -> Maybe Derivation -> String -> [Sentence] -> DataDefinition
ddENoRefs q der sn = DDE q . DDPkt Global [] der (shortname' $ S sn) (prependAbrv dataDefn sn)

-- | Smart constructor for data definitions.
ddME :: ModelQDef -> [DecRef] -> Maybe Derivation -> String -> [Sentence] -> DataDefinition
ddME q []   _   _  = error $ "Source field of " ++ showUID q ++ " is empty"
ddME q refs der sn = DDME q . DDPkt Global refs der (shortname' $ S sn) (prependAbrv dataDefn sn)

-- | Smart constructor for data definitions with no references.
ddMENoRefs :: ModelQDef -> Maybe Derivation -> String -> [Sentence] -> DataDefinition
ddMENoRefs q der sn = DDME q . DDPkt Global [] der (shortname' $ S sn) (prependAbrv dataDefn sn)

-- | Extracts the 'QDefinition e' from a 'DataDefinition'.
qdFromDD :: DataDefinition -> Either SimpleQDef ModelQDef
qdFromDD (DDE  qd _) = Left qd
qdFromDD (DDME qd _) = Right qd

qdEFromDD :: DataDefinition -> Maybe SimpleQDef
qdEFromDD (DDE qd _) = Just qd
qdEFromDD _          = Nothing
