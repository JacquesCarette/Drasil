{-# Language TupleSections #-}
-- | Standard code to make a table of contents.
module Drasil.SRS.Sections.TableOfContents (toToC) where

import Language.Drasil.Document

-- | Makes the table of contents entry for a section, with the entries for its
-- subsections nested under it.
toToC :: Section -> ItemType
toToC s = case subSecs s of
  [] -> Flat entry
  ss -> Nested entry $ Bullet $ (, Nothing) . toToC <$> ss
  where
    entry = namedRef s (tle s)
