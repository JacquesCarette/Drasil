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

import Drasil.Document.Layout.Citation (Citation(Cite))
import Drasil.Document.Layout.LayoutObj (LayoutObj(..), ListType(..), Depth)
import Drasil.Document.Layout.Spec (Spec(S), Title, Label)

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
mkProject t a fs = Project t a (fromList $ concatMap extractFileRefs fs) fs

-- | Helper function for creating a RefMap for a File.
extractFileRefs :: File -> [(String, Filename)]
extractFileRefs (File _ l _ c) = (, l) <$> concatMap extractRefs c

-- | Helper function for creating a RefMap for a LayoutObj
extractRefs :: LayoutObj -> [String]
extractRefs (Header _ _ l)     = extractStrFromLabel l
extractRefs (HDiv   _ _ l)     = extractStrFromLabel l
extractRefs (Table  _ _ l _ _) = extractStrFromLabel l
extractRefs (Definition _ l)   = extractStrFromLabel l
extractRefs (List t)           = pass t
  where
    pass (Ordered ls)     = process  ls
    pass (Unordered ls)   = process  ls
    pass (Simple ls)      = process' ls
    pass (Desc ls)        = process' ls
    pass (Definitions ls) = process' ls
    process  = concatMap (\(_, l)    -> foldMap extractStrFromLabel l)
    process' = concatMap (\(_, _, l) -> foldMap extractStrFromLabel l)
extractRefs (Figure l _ _ _)   = extractStrFromLabel l
extractRefs (Bib ls)           = bibRefs <$> ls
  where
    bibRefs (Cite l _ _) = l
extractRefs _                   = []

-- | Helper function for mapping a Label to a Filename
extractStrFromLabel :: Label -> [String]
extractStrFromLabel (S l) = [l]
extractStrFromLabel _     = []
