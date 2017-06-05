module Drasil.SpecificSystemDescription where

import Language.Drasil
import qualified Data.Drasil.Concepts.Documentation as D
import Control.Lens ((^.))

-- | Specific System description section builder. Takes the system and subsections.
specSysDescr :: NP -> [Section] -> Section
specSysDescr sys subs = section (D.fterm titleize D.specificsystemdescription) [intro sys] subs

-- FIXME: this all should be broken down and mostly generated.
-- Generates an introduction based on the system.
intro :: NamedIdea c => c -> Contents
intro sys = Paragraph $ S "This section first presents the problem" +:+
  S "description, which gives a high-level view of the problem to be" +:+
  S "solved. This is followed by the solution characteristics" +:+
  S "specification, which presents the assumptions" `sC`
  S "theories, and definitions that are used for the" +:+. (phrase $ sys ^. term)
