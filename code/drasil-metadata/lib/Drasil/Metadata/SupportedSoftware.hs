-- | Categories of software that Drasil can generate.
module Drasil.Metadata.SupportedSoftware (
    -- * Supported Software
    runnableSoftware, website
) where

import Drasil.Database (mkUid)
import Language.Drasil (IdeaDict, cn, idea, idea')

runnableSoftware, website :: IdeaDict
runnableSoftware = idea (mkUid "runnable software") (cn "runnable software") "software"
website = idea' (mkUid "website") (cn "website")
