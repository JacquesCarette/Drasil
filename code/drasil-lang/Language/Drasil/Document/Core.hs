module Language.Drasil.Document.Core where

import Language.Drasil.Chunk.AssumpChunk (AssumpChunk)
import Language.Drasil.Chunk.Change (Change)
import Language.Drasil.Chunk.Citation (BibRef)
import Language.Drasil.Chunk.Eq (QDefinition)
import Language.Drasil.Chunk.DataDefinition (DataDefinition)
import Language.Drasil.Chunk.Relation (RelationConcept)
import Language.Drasil.Chunk.ReqChunk (ReqChunk)

import Language.Drasil.Expr (Expr)
import Language.Drasil.Label (Label)
import Language.Drasil.RefTypes (RefAdd)
import Language.Drasil.Spec (Sentence(..))


data ListType = Bullet [(ItemType,Maybe RefAdd)] -- ^ Bulleted list
              | Numeric [(ItemType,Maybe RefAdd)] -- ^ Enumerated List
              | Simple [ListTuple] -- ^ Simple list with items denoted by @-@
              | Desc [ListTuple] -- ^ Descriptive list, renders as "Title: Item" (see 'ListTuple')
              | Definitions [ListTuple] -- ^ Renders a list of "@Title@ is the @Item@"

data ItemType = Flat Sentence -- ^ Standard singular item
              | Nested Header ListType -- ^ Nest a list as an item

-- | MaxWidthPercent should be kept in the range 1-100.
-- Values outside this range may have unexpected results.
-- Used for specifying max figure width as
-- pagewidth*MaxWidthPercent/100.
type MaxWidthPercent = Float

type Title    = Sentence
type Author   = Sentence
type Header   = Sentence -- Used when creating sublists
type Depth    = Int
type Width    = Float
type Height   = Float
type ListTuple = (Title,ItemType,Maybe RefAdd) -- ^ Title: Item
type Filepath = String
type Lbl      = Sentence


--FIXME: Remove Data, Data', and Theory from below.
-- | Types of definitions
data DType = Data QDefinition -- ^ QDefinition is the chunk with the defining
                              -- equation used to generate the Data Definition
           | Data' DataDefinition
           | General
           | Theory RelationConcept -- ^ Theoretical models use a relation as
                                    -- their definition
           | Instance
           | TM
           | DD

-- | Types of layout objects we deal with explicitly
data RawContent = Table [Sentence] [[Sentence]] Title Bool RefAdd
  -- ^ table has: header-row data(rows) label/caption showlabel?
               | Paragraph Sentence -- ^ Paragraphs are just sentences.
               | EqnBlock Expr
     --        CodeBlock Code   -- GOOL complicates this.  Removed for now.
               | Definition DType
               | Enumeration ListType -- ^ Lists
               | Figure Lbl Filepath MaxWidthPercent RefAdd-- ^ Should use relative file path.
               | Requirement ReqChunk
               | Assumption AssumpChunk
               | Change Change
               | Bib BibRef
     --        UsesHierarchy [(ModuleChunk,[ModuleChunk])]
               | Graph [(Sentence, Sentence)] (Maybe Width) (Maybe Height) Lbl RefAdd
               -- ^ TODO: Fill this one in.
               ------NEW TMOD/DDEF/IM/GD BEGINS HERE------
               ---- FIXME: The above Definition will need to be removed ----
               --------------------------------------------
               | Defnt DType [(Identifier, [Contents])] RefAdd
type Identifier = String

data LabelledContent = LblC { _lbl :: Label
                            , _ctype :: RawContent
                            }

data UnlabelledContent = UnlblC { _cntnts :: RawContent }

data Contents = UlC UnlabelledContent
              | LlC LabelledContent
