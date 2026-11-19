-- | Defines the documents and multi-file projects that organize 'LayoutObj's.
module Drasil.Document.Layout.Document (
  -- * Types
  Document(..), Project(..), File(..),
  -- * Type Aliases
  Author, Filename, RefMap,
  -- * Smart constructors
  mkProject
) where

import Data.Map (Map, fromList)

import Drasil.Document.Layout.LayoutObj (LayoutObj(..), Depth, extractRefs)
import Drasil.Document.Layout.Spec (Spec, Title)

-- | A document must contain a title, author, and contents (as 'LayoutObj's).
data Document = Document Title Author [LayoutObj]
-- | A Project must contain a title, author, RefMap, and Files.
data Project  = Project Title Author RefMap [File]
-- | A File must contain a title, filename, depth, and contents (as 'LayoutObj's).
data File     = File Title Filename Depth [LayoutObj]
-- | An author is just a sentence ('Spec').
type Author   = Spec
-- | Holds a file name.
type Filename = String
-- | A mapping of refs to the file that contains them.
type RefMap   = Map String Filename

-- | Builds a 'Project', recording which of its files defines each label.
mkProject :: Title -> Author -> [File] -> Project
mkProject t a fs = Project t a (fromList $ concatMap fileRefs fs) fs
  where
    fileRefs (File _ fn _ los) = (, fn) <$> concatMap extractRefs los
