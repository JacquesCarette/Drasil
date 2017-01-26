module Drasil.SpecificSystemDescription where

import Language.Drasil
import Control.Lens ((^.))

specSysDescr :: NamedIdea c => c -> [Section] -> Section
specSysDescr sys subs = Section (S "Specific System Description") 
  ([Con $ intro sys] ++ map Sub subs)
  
intro :: NamedIdea c => c -> Contents
intro sys = Paragraph $ S "This section first presents the problem" +:+
  S "description, which gives a high-level view of the problem to be" +:+
  S "solved. This is followed by the solution characteristics" +:+
  S "specification, which presents the assumptions" `sC`
  S "theories, and definitions that are used for the" +:+. (sys ^. term)