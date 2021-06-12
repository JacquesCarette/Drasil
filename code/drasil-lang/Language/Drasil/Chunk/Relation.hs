{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Relation (RelationConcept, makeRC, addRelToCC) where

import Control.Lens (makeLenses, (^.), view, set)

import Language.Drasil.Chunk.Concept (ConceptChunk, dccWDS, cw)
import Language.Drasil.Classes.Core (HasUID(uid))
import Language.Drasil.Classes (Display(..), ExprRelat(..), Concept,
  ConceptDomain(..), Definition(..), Idea(..), NamedIdea(..))
import Language.Drasil.Expr (Relation)
import Language.Drasil.NounPhrase (NP)
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.UID (UID)

-- | For a 'ConceptChunk' that also has a 'Relation' attached.
data RelationConcept = RC { _conc :: ConceptChunk
                          , _rel  :: Relation
                          }
makeLenses ''RelationConcept

-- | Finds the 'UID' of the 'ConceptChunk' used to make the 'RelationConcept'.
instance HasUID        RelationConcept where uid = conc . uid
-- | Equal if 'UID's are equal.
instance Eq            RelationConcept where a == b = (a ^. uid) == (b ^. uid)
-- | Finds the term ('NP') of the 'ConceptChunk' used to make the 'RelationConcept'.
instance NamedIdea     RelationConcept where term = conc . term
-- | Finds the idea contained in the 'ConceptChunk' used to make the 'RelationConcept'.
instance Idea          RelationConcept where getA = getA . view conc
-- | Finds the definition contained in the 'ConceptChunk' used to make the 'RelationConcept'.
instance Definition    RelationConcept where defn = conc . defn
-- | Finds the domain of the 'ConceptChunk' used to make the 'RelationConcept'.
instance ConceptDomain RelationConcept where cdom = cdom . view conc
-- | Finds the relation expression for a 'RelationConcept'.
instance ExprRelat     RelationConcept where relat = (^. rel)
-- | Convert the 'RelationConcept' into the display expression language.
instance Display       RelationConcept where toDispExpr = toDispExpr . (^. rel)

-- | Create a 'RelationConcept' from a given 'UID', term ('NP'), definition ('Sentence'), and 'Relation'.
makeRC :: UID -> NP -> Sentence -> Relation -> RelationConcept
makeRC rID rTerm rDefn = RC (dccWDS rID rTerm rDefn)

-- | Create a new 'RelationConcept' from an old 'Concept'. Takes a 'Concept', new 'UID' and relation.
addRelToCC :: Concept c => c -> UID -> Relation -> RelationConcept
addRelToCC c rID = RC (set uid rID (cw c))
