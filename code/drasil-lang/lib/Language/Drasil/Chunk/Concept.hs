-- | Contains functions to create the concept related chunk types found in
-- "Language.Drasil.Chunk.Concept.Core".
module Language.Drasil.Chunk.Concept (
  -- * Concept Chunks
  -- ** From an idea ('IdeaDict')
  ConceptChunk, cncpt, cncpt', cncpt'', cncpt''',
  dcc, dccA, dccAWDS, dccWDS, cw,
  -- ** From a 'ConceptChunk'
  ConceptInstance, cic
  ) where

import Control.Lens ((^.))

import Drasil.Database (HasUID(uid), nsUid, UID, mkUid)

import Language.Drasil.Classes (ConceptDomain(cdom), Concept)
import Language.Drasil.Chunk.Concept.Core (ConceptChunk(ConDict), ConceptInstance(ConInst))
import Language.Drasil.Sentence (Sentence(S))
import Language.Drasil.Chunk.NamedIdea (idea, idea', NamedIdea (..), Idea (..))
import Language.Drasil.NaturalLanguage.English.NounPhrase (NP, pn)
import Language.Drasil.ShortName (shortname')
import qualified Language.Drasil.Classes as D (defn)

-- FIXME: There should only be two smart constructors ultimately for
-- `ConceptChunk`s. One with an abbreviation, the other without. In other words,
-- only `cncpt` and `cncpt'` should exist. The other ones should not. The
-- problem here is that dealing with the other ones requires domain analysis.

-- | Construct a 'ConceptChunk'.
cncpt :: Concept dom =>
  -- | The 'UID'.
  UID ->
  -- The 'term' being defined.
  NP ->
  -- | The definition of the 'term'
  Sentence ->
  -- | The term's abbreviation.
  String ->
  -- | The domain the 'term' belongs to.
  [dom] -> ConceptChunk
cncpt u trm defn accAbbr = ConDict (idea u trm accAbbr) defn . map (^. uid)

-- | Construct a 'ConceptChunk'.
cncpt' :: Concept dom =>
  -- | The 'UID'.
  UID ->
  -- | The 'term' being defined.
  NP ->
  -- | The definition of the 'term'
  Sentence ->
  -- | The domain the 'term' belongs to.
  [dom] -> ConceptChunk
cncpt' u trm defn = ConDict (idea' u trm) defn . map (^. uid)

-- | Construct a 'ConceptChunk'.
cncpt'' ::
  -- | The 'UID'.
  UID ->
  -- | The 'term' being defined.
  NP ->
  -- | The definition of the 'term'
  Sentence ->
  -- | The term's abbreviation.
  String -> ConceptChunk
cncpt'' u trm defn accAbbr = cncpt u trm defn accAbbr ([] :: [ConceptChunk])

-- | Construct a 'ConceptChunk'.
cncpt''' ::
  -- | The 'UID'.
  UID ->
  -- | The 'term' being defined.
  NP ->
  -- | The definition of the 'term'
  Sentence -> ConceptChunk
cncpt''' u trm defn = ConDict (idea' u trm) defn []

{-# DEPRECATED dccA, dccAWDS, dcc, dccWDS
  "Old smart constructor; use one of `cncpt`, `cncpt'`, `cncpt''`, `cncpt'''` instead." #-}

-- | Smart constructor for creating a concept chunks with an abbreviation. Takes
-- a UID (String), a term (NounPhrase), a definition (String), and an
-- abbreviation (Maybe String).
dccA :: String -> NP -> String -> Maybe String -> ConceptChunk
dccA i ter def a = ConDict (go a) (S def) []
  where
    u = mkUid i
    go (Just accAbbr) = idea u ter accAbbr
    go Nothing        = idea' u ter

dccAWDS :: String -> NP -> Sentence -> Maybe String -> ConceptChunk
dccAWDS i ter def a = ConDict (go a) def []
  where
    u = mkUid i
    go (Just accAbbr) = idea u ter accAbbr
    go Nothing        = idea' u ter

-- | Smart constructor for creating concept chunks given a 'UID', 'NounPhrase'
-- ('NP') and definition (as a 'String').
dcc :: String -> NP -> String -> ConceptChunk
dcc i ter des = dccA i ter des Nothing

-- | Similar to 'dcc', except the definition takes a 'Sentence'.
dccWDS :: String -> NP -> Sentence -> ConceptChunk
dccWDS i t d = dccAWDS i t d Nothing

{-# DEPRECATED cw
  "Chunk down-casting is strongly discouraged. If you want to construct a `ConceptChunk`, use one of its normal constructors." #-}

-- | For projecting out to the 'ConceptChunk' data-type.
cw :: Concept c => c -> ConceptChunk
cw c = ConDict (go $ getA c) (c ^. D.defn) (cdom c)
  where go (Just accAbbr) = idea  (c ^. uid) (c ^. term) accAbbr
        go Nothing        = idea' (c ^. uid) (c ^. term)

-- | Constructor for a 'ConceptInstance'. Takes in the Reference Address
-- ('String'), a definition ('Sentence'), a short name ('String'), and a domain
-- (for explicit tagging).
cic :: Concept c => String -> Sentence -> String -> c -> ConceptInstance
cic u d sn dom = ConInst (nsUid "instance" $ icc ^. uid) icc u $ shortname' (S sn)
  where
    icc = cc (idea' (mkUid u) $ pn sn) d [dom]
    cc n d' l = ConDict n d' $ map (^. uid) l
