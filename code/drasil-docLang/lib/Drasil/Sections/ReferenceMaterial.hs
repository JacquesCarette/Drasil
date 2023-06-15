-- | Defines functions used in the Reference Material section.
module Drasil.Sections.ReferenceMaterial (intro, emptySectSentPlu, emptySectSentSing) where
{-
Depending where this is used, it might be worth combining it with other module(s),
or simply moving it to Data.Drasil.Documentation since it's highly reusable
If anything, this could be called by the recipes that need it
-}
import Language.Drasil

-- | Default Reference Material section introduction.
intro :: Contents
intro = mkParagraph $ S "This section records information for easy reference."

-- | Helper to create default plural sentence for empty sections using `NamedIdea`s
emptySectSentPlu :: NamedIdea n => [n] -> Sentence
emptySectSentPlu var = S "There are no" +:+ foldlList Comma Options (map plural var)

-- | Helper to create default singular sentence for empty sections using `NamedIdea`s
emptySectSentSing :: NamedIdea n => [n] -> Sentence
emptySectSentSing var = S "There is no" +:+ foldlList Comma Options (map phrase var)
