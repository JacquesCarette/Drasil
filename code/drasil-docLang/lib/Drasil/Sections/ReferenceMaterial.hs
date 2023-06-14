-- | Defines functions used in the Reference Material section.
module Drasil.Sections.ReferenceMaterial (intro, emptySectSentence) where
{-
Depending where this is used, it might be worth combining it with other module(s),
or simply moving it to Data.Drasil.Documentation since it's highly reusable
If anything, this could be called by the recipes that need it
-}
import Language.Drasil

-- | Default Reference Material section introduction.
intro :: Contents
intro = mkParagraph $ S "This section records information for easy reference."

-- | Helper to create default sentence for empty sections using NamedIdea
emptySectSentence :: NamedIdea n => n -> Contents
emptySectSentence var = foldlSP [S "There are no" +:+ plural var]
