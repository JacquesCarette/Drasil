module Drasil.Sections.ReferenceMaterial (intro) where
import Language.Drasil

-- | Create a reference material section with a default introduction and given
-- subsections.
refSec :: [Section] -> Section
refSec secs = Section (S "Reference Material") (Con intro : map Sub secs) "RefMat" (shortname' "RefMat")

-- | Default reference section introduction
intro :: Contents
intro = Paragraph $ S "This section records information for easy reference."
