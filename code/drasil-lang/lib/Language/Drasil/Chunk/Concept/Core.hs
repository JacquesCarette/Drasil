{-# Language TemplateHaskell #-}
-- | Define concept-related chunks. A concept is usually something that has
-- a term, definition, and comes from some domain of knowledge.
module Language.Drasil.Chunk.Concept.Core(
  -- * Concept-related Datatypes
  Conception(ConDict)
  , ConceptInstance(ConInst)
  , sDom)
  where
import Language.Drasil.ShortName (HasShortName(..), ShortName)
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom))
import Language.Drasil.Chunk.NamedIdea (IdeaDict)
import Language.Drasil.Label.Type ((+::+), defer, name, raw,
  LblType(..), Referable(..), HasRefAddress(..))
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.UID (UID, HasUID(..))

import Control.Lens (makeLenses, (^.), view)

-- | Check if something has one domain. Throws an error if there is more than one.
sDom :: [UID] -> UID
sDom [d] = d
sDom d = error $ "Expected ConceptDomain to have a single domain, found " ++
  show (length d) ++ " instead."

-- | The Conception datatype records a concept that contains an idea ('IdeaDict'),
-- a definition ('Sentence'), and an associated domain of knowledge (['UID']).
--
-- Ex. The concept of "Accuracy" may be defined as the quality or state of being correct or precise.
data Conception = ConDict { _idea :: IdeaDict -- ^ Contains the idea of the concept.
                            , _defn' :: Sentence -- ^ The definition of the concept.
                            , cdom' :: [UID] -- ^ Domain of the concept.
                            }
makeLenses ''Conception

-- | Equal if 'UID's are equal.
instance Eq            Conception where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
-- | Finds 'UID' of the 'IdeaDict' used to make the 'Conception'.
instance HasUID        Conception where uid = idea . uid
-- | Finds term ('NP') of the 'IdeaDict' used to make the 'Conception'.
instance NamedIdea     Conception where term = idea . term
-- | Finds the idea contained in the 'IdeaDict' used to make the 'Conception'.
instance Idea          Conception where getA = getA . view idea
-- | Finds definition of a 'Conception'.
instance Definition    Conception where defn = defn'
-- | Finds the domain of 'UID's of a 'Conception'.
instance ConceptDomain Conception where cdom = cdom'

-- | Contains a 'Conception', reference address, and a 'ShortName'.
-- It is a concept that can be referred to, or rather, a instance of where a concept is applied.
-- Often used in Goal Statements, Assumptions, Requirements, etc.
--
-- Ex. Something like the assumption that gravity is 9.81 m/s. When we write our equations,
-- we can then link this assumption so that we do not have to explicitly define
-- that assumption when needed to verify our work.
data ConceptInstance = ConInst { _cc :: Conception , ra :: String, shnm :: ShortName}
makeLenses ''ConceptInstance

-- | Equal if 'UID's are equal.
instance Eq            ConceptInstance where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
-- | Finds 'UID' of the 'Conception' used to make the 'ConceptInstance'.
instance HasUID        ConceptInstance where uid = cc . idea . uid
-- | Finds term ('NP') of the 'Conception' used to make the 'ConceptInstance'.
instance NamedIdea     ConceptInstance where term = cc . idea . term
-- | Finds the idea contained in the 'Conception' used to make the 'ConceptInstance'.
instance Idea          ConceptInstance where getA = getA . view (cc . idea)
-- | Finds the definition contained in the 'Conception' used to make the 'ConceptInstance'.
instance Definition    ConceptInstance where defn = cc . defn'
-- | Finds the domain contained in the 'Conception' used to make the 'ConceptInstance'.
instance ConceptDomain ConceptInstance where cdom = cdom' . view cc
-- | Finds the 'ShortName' contained in a 'ConceptInstance'.
instance HasShortName  ConceptInstance where shortname = shnm
-- | Finds the reference address contained in a 'ConceptInstance'.
instance HasRefAddress ConceptInstance where getRefAdd l = RP (defer (sDom $ cdom l) +::+ raw ":" +::+ name) (ra l)
-- | Finds the reference information contained in a 'ConceptInstance'.
instance Referable     ConceptInstance where
  refAdd      = ra        -- Finds the reference address contained in a ConceptInstance.
  renderRef   = getRefAdd -- Finds the reference address but in a diferent form.

