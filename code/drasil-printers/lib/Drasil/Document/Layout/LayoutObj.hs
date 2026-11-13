-- | Defines types similar to content types in "Language.Drasil" but better suited for printing.
module Drasil.Document.Layout.LayoutObj (
  -- * Types
  LayoutObj(..), ListType(..), ItemType(..),
  -- * Type Aliases
  Contents, Items, Tags, Depth, Width, Height, Filepath, Caption,
  MaxWidthPercent
) where

import Drasil.Document.Layout.Citation (BibRef)
import Drasil.Document.Layout.Spec (Spec, Title, Label)

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
-- | A caption is just a sentence ('Spec').
type Caption  = Spec
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
