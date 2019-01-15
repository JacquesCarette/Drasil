{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.DataDefinition where

import Control.Lens(makeLenses, (^.), view)
import Data.Drasil.IdeaDicts (dataDefn)
import Language.Drasil.Chunk.Citation (Citation)
import Language.Drasil.Chunk.CommonIdea (prependAbrv)
import Language.Drasil.Chunk.Eq (QDefinition, fromEqn, fromEqn')
import Language.Drasil.Classes.Core (HasUID(uid), HasShortName(shortname),
  HasRefAddress(getRefAdd), HasSymbol(symbol))
import Language.Drasil.Classes.Document (HasCitation(getCitations))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
  DefiningExpr(defnExpr), Quantity, HasSpace(typ), HasDerivation(derivations),
  HasAdditionalNotes(getNotes), ConceptDomain(cdom), CommonIdea(abrv))
import Language.Drasil.Derivation (Derivation)
import Language.Drasil.Development.Unit(MayHaveUnit(getUnit))
import Language.Drasil.Expr (Expr)
import Language.Drasil.Sentence (Sentence(EmptyS))
import Language.Drasil.ShortName (ShortName, shortname')
import Language.Drasil.Symbol.Helpers (eqSymb)
import Language.Drasil.UID (UID)

data Scope = Scp { _spec :: UID } {-indirect reference-}

data ScopeType =
    Local Scope {- only visible within a limited scope -}
  | Global      {- visible everywhere -}

-- A data definition is a QDefinition that may have additional notes. 
-- It also has attributes like derivation, source, etc.
data DataDefinition = DatDef { _qd :: QDefinition
                             , _scp :: ScopeType
                             , _cit :: [Citation]
                             , _deri :: Derivation
                             , lbl :: ShortName
                             , ra :: String
                             , _notes :: [Sentence]
                             }
makeLenses ''DataDefinition

instance HasUID             DataDefinition where uid = qd . uid
instance NamedIdea          DataDefinition where term = qd . term
instance Idea               DataDefinition where getA c = getA $ c ^. qd
instance HasSpace           DataDefinition where typ = qd . typ
instance HasSymbol          DataDefinition where symbol e st = symbol (e^.qd) st
instance Quantity           DataDefinition where 
instance DefiningExpr       DataDefinition where defnExpr = qd . defnExpr
instance HasCitation        DataDefinition where getCitations = cit
instance Eq                 DataDefinition where a == b = (a ^. uid) == (b ^. uid)
instance HasDerivation      DataDefinition where derivations = deri
instance HasAdditionalNotes DataDefinition where getNotes = notes
instance MayHaveUnit        DataDefinition where getUnit = getUnit . view qd 
instance HasShortName       DataDefinition where shortname = lbl
instance HasRefAddress      DataDefinition where getRefAdd = ra
instance ConceptDomain      DataDefinition where cdom _ = cdom dataDefn
instance CommonIdea         DataDefinition where abrv _ = abrv dataDefn

-- | Smart constructor for data definitions 
mkDD :: QDefinition -> [Citation] -> Derivation -> String -> [Sentence] -> DataDefinition
mkDD a b c d e = DatDef a Global b c (shortname' d) (prependAbrv dataDefn d) e

qdFromDD :: DataDefinition -> QDefinition
qdFromDD dd = dd ^. qd

-- Used to help make Qdefinitions when uid, term, and symbol come from the same source
mkQuantDef :: (Quantity c, MayHaveUnit c) => c -> Expr -> QDefinition
mkQuantDef cncpt equation = datadef $ getUnit cncpt
  where datadef (Just a) = fromEqn  (cncpt ^. uid) (cncpt ^. term) EmptyS (eqSymb cncpt) a equation
        datadef Nothing  = fromEqn' (cncpt ^. uid) (cncpt ^. term) EmptyS (eqSymb cncpt) equation
