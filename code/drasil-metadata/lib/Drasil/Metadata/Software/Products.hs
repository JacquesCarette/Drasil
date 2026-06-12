-- | Defines common software products for internal use
module Drasil.Metadata.Software.Products where

import Drasil.Database (mkUid)
import Language.Drasil

sciCompS :: IdeaDict
sciCompS   = idea' (mkUid "sciCompS") (cn' "scientific computing software")
