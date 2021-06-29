{-# Language TypeFamilies #-}
-- | Defining the core classes which represent knowledge-about-knowledge
module Language.Drasil.Classes.Core2 (HasShortName(shortname)) where

import Language.Drasil.ShortName (ShortName)

-- | A 'ShortName' is the text to be displayed for a link.
-- Used for referencing within a document that can include symbols and whatnot if required.
-- Visible in the typeset documents (pdf).
class HasShortName  s where
  shortname :: s -> ShortName