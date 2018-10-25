{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.DataDefinition where

import Control.Lens(makeLenses, (^.), view)

import Language.Drasil.Chunk.Eq (QDefinition, fromEqn, fromEqn',
  fromEqn''', fromEqn'''')
import Language.Drasil.Chunk.Quantity (Quantity, HasSpace(typ))
import Language.Drasil.Derivation (Derivation)
import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  HasSymbol(symbol), DefiningExpr(defnExpr), -- ExprRelat(relat), 
  HasDerivation(derivations), HasReference(getReferences), HasAdditionalNotes(getNotes),
  HasShortName(shortname), HasLabel(getLabel))
import Language.Drasil.Development.Unit(MayHaveUnit(getUnit))
import Language.Drasil.Expr (Expr)
import Language.Drasil.Label.Core (Label)
import Language.Drasil.Label (mkLabelSame)
import Language.Drasil.RefTypes(RefType(..), DType(..), Reference)
import Language.Drasil.Spec (Sentence(EmptyS))
import Language.Drasil.Symbol.Helpers (eqSymb)

data Scope = Scp { _spec :: Label {-indirect reference-}}

data ScopeType = Local Scope {-only visible within a limited scope-} | Global {-visible everywhere-}


-- A data definition is a QDefinition that may have additional notes. 
-- It also has attributes like derivation, source, etc.
data DataDefinition = DatDef { _qd :: QDefinition
                             , _scp :: ScopeType
                             , _ref :: [Reference]
                             , _deri :: Derivation
                             , _lbl :: Label
                             , _notes :: Maybe [Sentence]
                             }
makeLenses ''DataDefinition

-- this works because UnitalChunk is a Chunk
instance HasUID             DataDefinition where uid = qd . uid
instance NamedIdea          DataDefinition where term = qd . term
instance Idea               DataDefinition where getA c = getA $ c ^. qd
instance HasSpace           DataDefinition where typ = qd . typ
instance HasSymbol          DataDefinition where symbol e st = symbol (e^.qd) st
instance Quantity           DataDefinition where 
instance DefiningExpr       DataDefinition where defnExpr = qd . defnExpr
instance HasReference       DataDefinition where getReferences = ref
instance Eq                 DataDefinition where a == b = (a ^. uid) == (b ^. uid)
instance HasDerivation      DataDefinition where derivations = deri
instance HasAdditionalNotes DataDefinition where getNotes = notes
instance MayHaveUnit        DataDefinition where getUnit = getUnit . view qd 
instance HasLabel           DataDefinition where getLabel = lbl
instance HasShortName       DataDefinition where shortname = lbl . shortname

-- | Smart constructor for data definitions 
mkDD :: QDefinition -> [Reference] -> Derivation -> String -> Maybe [Sentence] -> DataDefinition
mkDD a b c d e = DatDef a Global b c (mkLabelSame d (Def DD)) e

mkDDL :: QDefinition -> [Reference] -> Derivation -> Label -> Maybe [Sentence] -> DataDefinition
mkDDL a b c label e = DatDef a Global b c label e

qdFromDD :: DataDefinition -> QDefinition
qdFromDD (DatDef a _ _ _ _ _) = a

-- Used to help make Qdefinitions when uid, term, and symbol come from the same source
mkQuantDef :: (Quantity c) => c -> Expr -> QDefinition
mkQuantDef cncpt equation = datadef $ getUnit cncpt --should [Reference] be passed in at this point?
  where datadef (Just a) = fromEqn  (cncpt ^. uid) (cncpt ^. term) EmptyS
                           (eqSymb cncpt) a equation
        datadef Nothing  = fromEqn' (cncpt ^. uid) (cncpt ^. term) EmptyS
                           (eqSymb cncpt) equation

mkQuantDef' :: (Quantity c) => c -> Expr -> QDefinition
mkQuantDef' cncpt equation = quantdef $ getUnit cncpt --should references be passed in at this point?
  where quantdef (Just a) = fromEqn'''  (cncpt ^. uid) (cncpt ^. term) EmptyS
                           (eqSymb cncpt) a equation
        quantdef Nothing  = fromEqn'''' (cncpt ^. uid) (cncpt ^. term) EmptyS
                           (eqSymb cncpt) equation

