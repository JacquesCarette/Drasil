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
mkProject t a fs = Project t a (fromList $ concatMap createRefMap' fs) fs

-- | Helper function for creating a RefMap for a File.
createRefMap' :: File -> [(String, Filename)]
createRefMap' (File _ l _ c) = concatMap (createRefMap l) c

-- | Helper function for creating a RefMap for a LayoutObj
createRefMap :: Filename -> LayoutObj -> [(String, Filename)]
createRefMap fn (Header _ _ l)     = createRef fn l
createRefMap fn (HDiv   _ _ l)     = createRef fn l
createRefMap fn (Table  _ _ l _ _) = createRef fn l
createRefMap fn (Definition _ l)   = createRef fn l
createRefMap fn (List t)           = pass t
  where
    pass (Ordered ls)     = process  ls
    pass (Unordered ls)   = process  ls
    pass (Simple ls)      = process' ls
    pass (Desc ls)        = process' ls
    pass (Definitions ls) = process' ls
    process  = concatMap (\(_, l)    -> foldMap (createRef fn) l)
    process' = concatMap (\(_, _, l) -> foldMap (createRef fn) l)
createRefMap fn (Figure l _ _ _)   = createRef fn l
createRefMap fn (Bib ls)           = bibRefs <$> ls
  where
    bibRefs (Cite l _ _) = (l, fn)
createRefMap _ _                   = []

-- | Helper function for mapping a Label to a Filename
createRef :: Filename -> Label -> [(String, Filename)]
createRef fn (S l) = [(l, fn)]
createRef _  _     = []
