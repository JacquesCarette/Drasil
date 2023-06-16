-- | Defines functions used in the Reference Material section.
module Drasil.Sections.ReferenceMaterial (intro, emptySectSentPlu, emptySectSentSing) where
{-
Depending where this is used, it might be worth combining it with other module(s),
or simply moving it to Data.Drasil.Documentation since it's highly reusable
If anything, this could be called by the recipes that need it
-}
import Language.Drasil
import Language.Drasil.Sentence.Combinators (are, is)

-- | Default Reference Material section introduction.
intro :: Contents
intro = mkParagraph $ S "This section records information for easy reference."

data Plurality = Sing | Plu

-- | Helper to create default `Sentence`s for empty sections using `NamedIdea`s
emptySectSent :: NamedIdea n => Plurality -> [n] -> Sentence
emptySectSent p n = foldlSent [verb p (S "There") (S "no"), foldlList Comma Options (map (f p) n)]
    where 
        verb Sing = is
        verb Plu  = are
        f Sing = phrase
        f Plu  = plural

-- | Helper for variants of `emptySectSent`
emptySectSentSing, emptySectSentPlu :: NamedIdea n => [n] -> Sentence
emptySectSentSing = emptySectSent Sing
emptySectSentPlu  = emptySectSent Plu
