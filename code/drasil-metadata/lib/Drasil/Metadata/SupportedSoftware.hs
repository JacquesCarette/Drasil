-- | Categories of software that Drasil can generate.
module Drasil.Metadata.SupportedSoftware (
    -- * Supported Software
    runnableSoftware, website
) where

import Drasil.Database (mkUid)
import Language.Drasil (IdeaDict, cn, mkIdea, idea')

runnableSoftware, website :: IdeaDict
runnableSoftware = mkIdea "runnable software" (cn "runnable software") (Just "software")
website = idea' (mkUid "website") (cn "website")
