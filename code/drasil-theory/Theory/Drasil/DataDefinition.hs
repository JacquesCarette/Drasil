{-# LANGUAGE TemplateHaskell #-}
module Theory.Drasil.DataDefinition where

import Control.Lens (makeLenses, (^.), view)
import Language.Drasil
import Data.Drasil.IdeaDicts (dataDefn)

newtype Scope = Scp { _spec :: UID } {-indirect reference-}

data ScopeType =
    Local Scope {- only visible within a limited scope -}
  | Global      {- visible everywhere -}

-- A data definition is a QDefinition that may have additional notes. 
-- It also has attributes like derivation, source, etc.
data DataDefinition = DatDef { _qd :: QDefinition
                             , _scp :: ScopeType
                             , _ref :: [Reference]
                             , _deri :: Maybe Derivation
                             , lbl :: ShortName
                             , ra :: String
                             , _notes :: [Sentence]
                             }
makeLenses ''DataDefinition

instance HasUID             DataDefinition where uid = qd . uid
instance NamedIdea          DataDefinition where term = qd . term
instance Idea               DataDefinition where getA c = getA $ c ^. qd
instance HasSpace           DataDefinition where typ = qd . typ
instance HasSymbol          DataDefinition where symbol e = symbol (e^.qd)
instance Quantity           DataDefinition where 
instance DefiningExpr       DataDefinition where defnExpr = qd . defnExpr
instance HasReference       DataDefinition where getReferences = ref
instance Eq                 DataDefinition where a == b = (a ^. uid) == (b ^. uid)
instance HasDerivation      DataDefinition where derivations = deri
instance HasAdditionalNotes DataDefinition where getNotes = notes
instance MayHaveUnit        DataDefinition where getUnit = getUnit . view qd 
instance HasShortName       DataDefinition where shortname = lbl
instance HasRefAddress      DataDefinition where getRefAdd = ra
instance ConceptDomain      DataDefinition where cdom _ = cdom dataDefn
instance CommonIdea         DataDefinition where abrv _ = abrv dataDefn
instance Referable          DataDefinition where
  refAdd      = getRefAdd
  renderRef l = RP (prepend $ abrv l) (getRefAdd l)

-- | Smart constructor for data definitions 
dd :: QDefinition -> [Reference] -> Maybe Derivation -> String -> [Sentence] -> DataDefinition
dd q []   _   _  = error $ "Source field of " ++ q ^. uid ++ " is empty"
dd q refs der sn = DatDef q Global refs der (shortname' sn) (prependAbrv dataDefn sn)

-- | Smart constructor for data definitions with no references
ddNoRefs :: QDefinition -> Maybe Derivation -> String -> [Sentence] -> DataDefinition
ddNoRefs q der sn = DatDef q Global [] der (shortname' sn) (prependAbrv dataDefn sn)

qdFromDD :: DataDefinition -> QDefinition
qdFromDD d = d ^. qd

-- Used to help make Qdefinitions when uid, term, and symbol come from the same source
mkQuantDef :: (Quantity c, MayHaveUnit c) => c -> Expr -> QDefinition
mkQuantDef cncpt equation = datadef $ getUnit cncpt
  where datadef (Just a) = fromEqnSt  (cncpt ^. uid) (cncpt ^. term) EmptyS (symbol cncpt) Real a equation
        datadef Nothing  = fromEqnSt' (cncpt ^. uid) (cncpt ^. term) EmptyS (symbol cncpt) Real equation
