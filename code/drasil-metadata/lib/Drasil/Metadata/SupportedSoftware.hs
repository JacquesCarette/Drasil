-- | Categories of software that Drasil can generate.
module Drasil.Metadata.SupportedSoftware (
    -- * Supported Software
    runnableSoftware, website
) where

import Language.Drasil (IdeaDict, cn, mkIdea)

runnableSoftware, website :: IdeaDict
runnableSoftware = mkIdea "runnable software" (cn "runnable software") (Just "software")
website = mkIdea "website" (cn "website") (Just "web")
