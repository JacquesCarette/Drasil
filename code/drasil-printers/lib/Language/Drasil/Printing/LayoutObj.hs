-- | Defines types similar to content types in "Language.Drasil" but better suited for printing.
module Language.Drasil.Printing.LayoutObj where

import Data.Map (Map)

import Language.Drasil hiding (ListType, Contents, BibRef, Document)

import Language.Drasil.Printing.AST (ListType, Spec, Title, Label)
import Language.Drasil.Printing.Citation (BibRef)

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

data LayoutObj = 
     Table Tags [[Spec]] Label Bool Caption                          -- ^ Holds all information needed for a table.
   | Header Depth Title Label                                        -- ^ Holds all information needed for a header.
   | Paragraph Contents                                              -- ^ Paragraph.
   | EqnBlock Contents                                               -- ^ Equation block.
   | Definition DType [(String,[LayoutObj])] Label                   -- ^ Definition. Holds the type, contents, and a label.
   | List ListType                                                   -- ^ List.
   | Figure Label Caption Filepath MaxWidthPercent                   -- ^ Holds all information needed for a figure.
   | Graph [(Spec, Spec)] (Maybe Width) (Maybe Height) Caption Label -- ^ Holds all information needed for a graph.
   | CodeBlock Contents                                              -- ^ Code block.
   | HDiv Tags [LayoutObj] Label                                     -- ^ Holds tags, more contents, and a label.
   | Cell [LayoutObj] 
   -- this shouldn't be here, it should have been expanded.
   | Bib BibRef                                                      -- ^ Bibliography section.
