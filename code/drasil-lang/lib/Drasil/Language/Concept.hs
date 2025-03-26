{-# Language TemplateHaskell #-}
-- | Define concept-related chunks. A concept is usually something that has
-- a term, definition, and comes from some domain of knowledge.
module Drasil.Language.Concept (
    ConceptChunk(ConDict), dcc, dccWDS, cc, cc', ccs, cw
  , ConceptInstance(ConInst), cic
  , sDom
  , Definition(..), ConceptDomain(..), Concept
  )
  where

import Control.Lens (makeLenses, (^.), view, Lens')

import Drasil.Database.UID (UID, HasUID(..))
import Drasil.Language.Idea (IdeaDict, NamedIdea(..), Idea(..), nw, nc, mkIdea)

import Language.Drasil.NounPhrase (NP, pn)
import Language.Drasil.ShortName (HasShortName(..), ShortName, shortname')
import Language.Drasil.Label.Type ((+::+), defer, name, raw,
  LblType(..), Referable(..), HasRefAddress(..))
import Language.Drasil.Sentence (Sentence(S))

-- | Check if something has one domain. Throws an error if there is more than one.
sDom :: [UID] -> UID -- FIXME: Why is this useful? It doesn't look like it should be!
sDom [d] = d
sDom d = error $ "Expected ConceptDomain to have a single domain, found " ++
  show (length d) ++ " instead."

-- TODO: conceptual typeclass?
-- TODO: I was thinking of splitting QDefinitions into Definitions with 2 type variables
--       Can we change this name from "Definition" to anything else? "NaturalDefinition"?
-- | Defines a chunk.
class Definition c where
  -- | Provides (a 'Lens' to) the definition for a chunk.
  defn :: Lens' c Sentence

-- TODO: `drasil-database`-related typeclass? UIDs should be  moved to `drasil-database` too.
-- | Some concepts have a domain (related information encoded in 'UID's to other chunks).
class ConceptDomain c where
  -- | Provides Getter for the concept domain tags for a chunk
  cdom :: c -> [UID]
  -- ^ /cdom/ should be exported for use by the
  -- Drasil framework, but should not be exported beyond that.

-- TODO: conceptual type synonym?
-- | Concepts are 'Idea's with definitions and domains.
type Concept c = (Idea c, Definition c, ConceptDomain c)
-- TODO: Would the below make this a bit better to work with?
--        type Concept = forall c. (Idea c, Definition c, ConceptDomain c) => c

-- | The ConceptChunk datatype records a concept that contains an idea ('IdeaDict'),
-- a definition ('Sentence'), and an associated domain of knowledge (['UID']).
--
-- Ex. The concept of "Accuracy" may be defined as the quality or state of being correct or precise.
data ConceptChunk = ConDict { _idea :: IdeaDict -- ^ Contains the idea of the concept.
                            , _defn' :: Sentence -- ^ The definition of the concept.
                            , cdom' :: [UID] -- ^ Domain of the concept.
                            }
makeLenses ''ConceptChunk

-- | Equal if 'UID's are equal.
instance Eq            ConceptChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
-- | Finds 'UID' of the 'IdeaDict' used to make the 'ConceptChunk'.
instance HasUID        ConceptChunk where uid = idea . uid
-- | Finds term ('NP') of the 'IdeaDict' used to make the 'ConceptChunk'.
instance NamedIdea     ConceptChunk where term = idea . term
-- | Finds the idea contained in the 'IdeaDict' used to make the 'ConceptChunk'.
instance Idea          ConceptChunk where getA = getA . view idea
-- | Finds definition of a 'ConceptChunk'.
instance Definition    ConceptChunk where defn = defn'
-- | Finds the domain of 'UID's of a 'ConceptChunk'.
instance ConceptDomain ConceptChunk where cdom = cdom'

-- | Contains a 'ConceptChunk', reference address, and a 'ShortName'.
-- It is a concept that can be referred to, or rather, a instance of where a concept is applied.
-- Often used in Goal Statements, Assumptions, Requirements, etc.
--
-- Ex. Something like the assumption that gravity is 9.81 m/s. When we write our equations,
-- we can then link this assumption so that we do not have to explicitly define
-- that assumption when needed to verify our work.
data ConceptInstance = ConInst { _icc :: ConceptChunk
                               , ra :: String
                               , shnm :: ShortName
                               }
makeLenses ''ConceptInstance

-- | Equal if 'UID's are equal.
instance Eq            ConceptInstance where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
-- | Finds 'UID' of the 'ConceptChunk' used to make the 'ConceptInstance'.
instance HasUID        ConceptInstance where uid = icc . idea . uid
-- | Finds term ('NP') of the 'ConceptChunk' used to make the 'ConceptInstance'.
instance NamedIdea     ConceptInstance where term = icc . idea . term
-- | Finds the idea contained in the 'ConceptChunk' used to make the 'ConceptInstance'.
instance Idea          ConceptInstance where getA = getA . view (icc . idea)
-- | Finds the definition contained in the 'ConceptChunk' used to make the 'ConceptInstance'.
instance Definition    ConceptInstance where defn = icc . defn'
-- | Finds the domain contained in the 'ConceptChunk' used to make the 'ConceptInstance'.
instance ConceptDomain ConceptInstance where cdom = cdom' . view icc
-- | Finds the 'ShortName' contained in a 'ConceptInstance'.
instance HasShortName  ConceptInstance where shortname = shnm
-- | Finds the reference address contained in a 'ConceptInstance'.
instance HasRefAddress ConceptInstance where getRefAdd l = RP (defer (sDom $ cdom l) +::+ raw ":" +::+ name) (ra l)
-- | Finds the reference information contained in a 'ConceptInstance'.
instance Referable     ConceptInstance where
  refAdd      = ra        -- Finds the reference address contained in a ConceptInstance.
  renderRef   = getRefAdd -- Finds the reference address but in a diferent form.

--FIXME: Temporary ConceptDomain tag hacking to not break everything. 
 
dcc :: String -> NP -> String -> ConceptChunk 
-- | Smart constructor for creating concept chunks given a 'UID', 'NounPhrase'
-- ('NP') and definition (as a 'String').
dcc i ter des = ConDict (mkIdea i ter Nothing) (S des) []
-- ^ Concept domain tagging is not yet implemented in this constructor.

-- | Similar to 'dcc', except the definition takes a 'Sentence'.
dccWDS :: String -> NP -> Sentence -> ConceptChunk
dccWDS i t d = ConDict (mkIdea i t Nothing) d []

-- | Constructor for projecting an idea into a 'ConceptChunk'. Takes the
-- definition of the 'ConceptChunk' as a 'String'. Does not allow concept domain
-- tagging.
cc :: Idea c => c -> String -> ConceptChunk
cc n d = ConDict (nw n) (S d) []

-- | Same as 'cc', except definition is a 'Sentence'.
cc' :: Idea c => c -> Sentence -> ConceptChunk
cc' n d = ConDict (nw n) d []

-- | Similar to 'cc'', but allows explicit domain tagging.
ccs :: (Idea c, Concept d) => c -> Sentence -> [d] -> ConceptChunk --Explicit tagging
ccs n d l = ConDict (nw n) d $ map (^. uid) l

-- | For projecting out to the 'ConceptChunk' data-type.
cw :: Concept c => c -> ConceptChunk
cw c = ConDict (nw c) (c ^. defn) (cdom c)

-- | Constructor for a 'ConceptInstance'. Takes in the Reference Address
-- ('String'), a definition ('Sentence'), a short name ('String'), and a domain
-- (for explicit tagging).
cic :: Concept c => String -> Sentence -> String -> c -> ConceptInstance
cic u d sn dom = ConInst (ccs (nc u $ pn sn) d [dom]) u $ shortname' (S sn)

