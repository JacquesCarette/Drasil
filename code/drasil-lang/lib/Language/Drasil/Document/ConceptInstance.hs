{-# Language TemplateHaskell #-}
module Language.Drasil.Document.ConceptInstance (
  ConceptInstance,
  cic
) where

import Control.Lens (makeLenses, (^.), view)

import Drasil.Database (UID, HasUID(..), declareHasChunkRefs, Generically(..), nsUid, mkUid)

import Language.Drasil.Chunk.Concept.Core (ConceptChunk, sDom)
import Language.Drasil.ShortName (HasShortName(..), ShortName, shortname')
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), Concept)
import Language.Drasil.Label.Type ((+::+), defer, name, raw,
  LblType(..), Referable(..), HasRefAddress(..))
import Language.Drasil.Sentence (Sentence (S))
import Language.Drasil.NaturalLanguage.English.NounPhrase (pn)
import Language.Drasil.Chunk.Concept (cncpt')

-- | Contains a 'ConceptChunk', reference address, and a 'ShortName'.
-- It is a concept that can be referred to, or rather, a instance of where a concept is applied.
-- Often used in Goal Statements, Assumptions, Requirements, etc.
--
-- Ex. Something like the assumption that gravity is 9.81 m/s. When we write our equations,
-- we can then link this assumption so that we do not have to explicitly define
-- that assumption when needed to verify our work.
data ConceptInstance = ConInst { _ciuid :: UID
                               , _cc :: ConceptChunk
                               , ra :: String
                               , shnm :: ShortName}
makeLenses ''ConceptInstance
declareHasChunkRefs ''ConceptInstance

-- | Equal if 'UID's are equal.
instance Eq            ConceptInstance where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
-- | Finds 'UID' of the 'ConceptChunk' used to make the 'ConceptInstance'.
instance HasUID        ConceptInstance where uid = ciuid
-- | Finds term ('NP') of the 'ConceptChunk' used to make the 'ConceptInstance'.
instance NamedIdea     ConceptInstance where term = cc . term
-- | Finds the idea contained in the 'ConceptChunk' used to make the 'ConceptInstance'.
instance Idea          ConceptInstance where getA = getA . view cc
-- | Finds the definition contained in the 'ConceptChunk' used to make the 'ConceptInstance'.
instance Definition    ConceptInstance where defn = cc . defn
-- | Finds the domain contained in the 'ConceptChunk' used to make the 'ConceptInstance'.
instance ConceptDomain ConceptInstance where cdom = cdom . view cc
-- | Finds the 'ShortName' contained in a 'ConceptInstance'.
instance HasShortName  ConceptInstance where shortname = shnm
-- | Finds the reference address contained in a 'ConceptInstance'.
instance HasRefAddress ConceptInstance where getRefAdd l = RP (defer (sDom $ cdom l) +::+ raw ":" +::+ name) (ra l)
-- | Finds the reference information contained in a 'ConceptInstance'.
instance Referable     ConceptInstance where
  refAdd      = ra        -- Finds the reference address contained in a ConceptInstance.
  renderRef   = getRefAdd -- Finds the reference address but in a diferent form.

-- | Constructor for a 'ConceptInstance'. Takes in the Reference Address
-- ('String'), a definition ('Sentence'), a short name ('String'), and a domain
-- (for explicit tagging).
cic :: Concept c => String -> Sentence -> String -> c -> ConceptInstance
cic u d sn dom = ConInst (nsUid "instance" $ icc ^. uid) icc u $ shortname' (S sn)
  where
    icc = cncpt' (mkUid u) (pn sn) d [dom]
