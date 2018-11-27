{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.DataDefinition where

import Control.Lens(makeLenses, (^.), view)
import Data.Drasil.IdeaDicts (softEng)
import Language.Drasil.Chunk.Citation (Citation, HasCitation(getCitations))
import Language.Drasil.Chunk.Eq (QDefinition, fromEqn, fromEqn')
import Language.Drasil.Derivation (Derivation)
import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  HasSymbol(symbol), DefiningExpr(defnExpr), Quantity, HasSpace(typ),
  HasDerivation(derivations),  HasAdditionalNotes(getNotes),
  HasShortName(shortname), HasLabel(getLabel), ConceptDomain(cdom), CommonIdea(abrv))
import Language.Drasil.Development.Unit(MayHaveUnit(getUnit))
import Language.Drasil.Expr (Expr)
import Language.Drasil.Label.Core (Label)
import Language.Drasil.Label (mkLabelSame)
import Language.Drasil.RefTypes(RefType(..), DType(..))
import Language.Drasil.Sentence (Sentence(EmptyS))
import Language.Drasil.Symbol.Helpers (eqSymb)
import Language.Drasil.Chunk.CommonIdea (CI, commonIdeaWithDict)
import Language.Drasil.NounPhrase (cn')

data Scope = Scp { _spec :: Label {-indirect reference-}}

data ScopeType = Local Scope {-only visible within a limited scope-} | Global {-visible everywhere-}

-- A data definition is a QDefinition that may have additional notes. 
-- It also has attributes like derivation, source, etc.
data DataDefinition = DatDef { _qd :: QDefinition
                             , _scp :: ScopeType
                             , _cit :: [Citation]
                             , _deri :: Derivation
                             , _lbl :: Label
                             , _notes :: [Sentence]
                             , _ci :: CI
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
instance HasLabel           DataDefinition where getLabel = lbl
instance HasShortName       DataDefinition where shortname = lbl . shortname
instance ConceptDomain      DataDefinition where cdom = ci . cdom
instance CommonIdea         DataDefinition where abrv = abrv . view ci

dataDefn :: CI
dataDefn    = commonIdeaWithDict "dataDefn"    (cn' "data definition")                             "DD"        [softEng]

-- | Smart constructor for data definitions 
mkDD :: QDefinition -> [Citation] -> Derivation -> String -> [Sentence] -> DataDefinition
mkDD a b c d e = DatDef a Global b c (mkLabelSame d (Def DD)) e dataDefn

mkDDL :: QDefinition -> [Citation] -> Derivation -> Label -> [Sentence] -> DataDefinition
mkDDL a b c label e = DatDef a Global b c label e dataDefn

qdFromDD :: DataDefinition -> QDefinition
qdFromDD dd = dd ^. qd

-- Used to help make Qdefinitions when uid, term, and symbol come from the same source
mkQuantDef :: (Quantity c, MayHaveUnit c) => c -> Expr -> QDefinition
mkQuantDef cncpt equation = datadef $ getUnit cncpt --should [Reference] be passed in at this point?
  where datadef (Just a) = fromEqn  (cncpt ^. uid) (cncpt ^. term) EmptyS (eqSymb cncpt) a equation
        datadef Nothing  = fromEqn' (cncpt ^. uid) (cncpt ^. term) EmptyS (eqSymb cncpt) equation
