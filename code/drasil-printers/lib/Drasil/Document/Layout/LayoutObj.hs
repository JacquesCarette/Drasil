-- | Defines types similar to content types in "Language.Drasil" but better suited for printing.
module Drasil.Document.Layout.LayoutObj (
  -- * Types
  Document(..), Project(..), File(..), LayoutObj(..), ListType(..),
  ItemType(..),
  -- * Type Aliases
  Author, Contents, Items, Tags, Depth, Width, Height, Filepath, Filename,
  Caption, RefMap, MaxWidthPercent,
  -- * Smart constructors
  mkProject
) where

import Data.Map (Map, fromList)

import Drasil.Document.Layout.Citation (BibRef, Citation(Cite))
import Drasil.Document.Layout.Spec (Spec(S), Title, Label)

-- | A document must contain a title, author, and contents (as 'LayoutObj's).
data Document = Document Title Author [LayoutObj]
-- | A Project must contain a title, author, RefMap, and Files.
data Project  = Project Title Author RefMap [File]
-- | A File must contain a title, filename, depth, and contents (as 'LayoutObj's).
data File     = File Title Filename Depth [LayoutObj]
-- | An author is just a sentence ('Spec').
type Author   = Spec
-- | Contents are just a sentence ('Spec').
type Contents = Spec
-- | A group of layout objects.
type Items    = [LayoutObj]
-- | Tags.
type Tags     = [String]
-- | Depth of a header.
type Depth    = Int
-- | Horizontal dimension of a graph.
type Width    = Float
-- | Vertical dimension of a graph.
type Height   = Float
-- | Holds a file path.
type Filepath = String
-- | Holds a file name.
type Filename = String
-- | A caption is just a sentence ('Spec').
type Caption  = Spec
-- | A mapping of refs to the file that contains them.
type RefMap   = Map String Filename
-- | MaxWidthPercent should be kept in the range 1-100.
-- Values outside this range may have unexpected results.
-- Used for specifying max figure width as
-- @pagewidth*MaxWidthPercent/100@.
type MaxWidthPercent = Float

data LayoutObj
  = Table Tags [[Spec]] Label Bool Caption                          -- ^ Holds all information needed for a table.
  | Header Depth Title Label                                        -- ^ Holds all information needed for a header.
  | Paragraph Contents                                              -- ^ Paragraph.
  | EqnBlock Contents                                               -- ^ Equation block.
  | Definition [(String,[LayoutObj])] Label                         -- ^ Definition. Holds the type, contents, and a label.
  | List ListType                                                   -- ^ List.
  | Figure Label (Maybe Caption) Filepath MaxWidthPercent           -- ^ Holds all information needed for a figure.
  | Graph [(Spec, Spec)] (Maybe Width) (Maybe Height) Caption Label -- ^ Holds all information needed for a graph.
  | CodeBlock Contents                                              -- ^ Code block.
  | HDiv Tags [LayoutObj] Label                                     -- ^ Holds tags, more contents, and a label.
  | Cell [LayoutObj]
  -- this shouldn't be here, it should have been expanded.
  | Bib BibRef                                                      -- ^ Bibliography section.

-- | Different types of lists that contain an 'ItemType' and may contain a label and a title.
-- May be ordered, unordered, simple, descriptive, or for definitions. More suitable to printing.
data ListType
  = Ordered     [(ItemType, Maybe Label)]
  | Unordered   [(ItemType, Maybe Label)]
  | Simple      [(Title, ItemType, Maybe Label)]
  | Desc        [(Title, ItemType, Maybe Label)]
  | Definitions [(Title, ItemType, Maybe Label)]

-- | A list may contain an element or another list. More suitable to printing.
data ItemType
  = Flat Spec
  | Nested Spec ListType

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
