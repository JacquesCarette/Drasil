-- | Short names are used for displaying references.
module Language.Drasil.Document.ShortName (
  ShortName, shortname', getSentSN, HasShortName(..)
) where

-- 'ShortName' is defined alongside 'Sentence' because 'Ref' needs
-- 'HasShortName', and 'ShortName' wraps a 'Sentence'.
import Language.Drasil.Sentence (ShortName, shortname', getSentSN, HasShortName(..))
