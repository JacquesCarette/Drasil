-- | Document Description Language
module Language.Drasil.Document where

import Language.Drasil.Chunk.AssumpChunk
import Language.Drasil.Chunk.Change
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.Relation
import Language.Drasil.Chunk.ReqChunk
import Language.Drasil.Spec (Sentence(..))
import Language.Drasil.RefTypes (RefAdd)
import Language.Drasil.Expr
import Language.Drasil.Chunk.Citation (BibRef)
import Language.Drasil.Classes (HasUID(uid), HasShortName(shortname))
import Control.Lens ((^.))

type Title    = Sentence
type Author   = Sentence
type Header   = Sentence -- Used when creating sublists
type Depth    = Int
type Width    = Float
type Height   = Float
type ListPair = (Title,ItemType) -- ^ Title: Item
type Filepath = String
type Label    = Sentence

-- | A Document has a Title ('Sentence'), Author(s) ('Sentence'), and Sections
-- which hold the contents of the document
data Document = Document Title Author [Section]

-- | Section Contents are split into subsections or contents, where contents
-- are standard layout objects (see 'Contents')
data SecCons = Sub Section
             | Con Contents

-- | Sections have a title ('Sentence') and a list of contents ('SecCons')
-- and a String that will be its shortname
data Section = Section Title [SecCons] RefAdd String

instance HasShortName Section where shortname (Section t _ _) = t

-- | Types of layout objects we deal with explicitly
data Contents = Table [Sentence] [[Sentence]] Title Bool RefAdd
  -- ^ table has: header-row data(rows) label/caption showlabel?
               | Paragraph Sentence -- ^ Paragraphs are just sentences.
               | EqnBlock Expr RefAdd
     --        CodeBlock Code   -- GOOL complicates this.  Removed for now.
               | Definition DType
               | Enumeration ListType -- ^ Lists
               | Figure Label Filepath MaxWidthPercent RefAdd-- ^ Should use relative file path.
               | Requirement ReqChunk
               | Assumption AssumpChunk
               | Change Change
               | Bib BibRef
     --        UsesHierarchy [(ModuleChunk,[ModuleChunk])]
               | Graph [(Sentence, Sentence)] (Maybe Width) (Maybe Height) Label RefAdd
               -- ^ TODO: Fill this one in.
               ------NEW TMOD/DDEF/IM/GD BEGINS HERE------
               ---- FIXME: The above Definition will need to be removed ----
               --------------------------------------------
               | Defnt DType [(Identifier, [Contents])] RefAdd
type Identifier = String

instance HasShortName  Contents where
  shortname (Table _ _ _ _ r)     = S "Table:" :+: S r
  shortname (Figure _ _ _ r)      = S "Figure:" :+: S r
  shortname (Graph _ _ _ _ r)     = S "Figure:" :+: S r
  shortname (EqnBlock _ r)        = S "Equation:" :+: S r
  shortname (Definition d)        = S $ getDefName d
  shortname (Defnt _ _ r)         = S r
  shortname (Requirement rc)      = shortname rc
  shortname (Assumption ca)       = shortname ca
  shortname (Change lcc)          = shortname lcc
  shortname (Enumeration _)       = error "Can't reference lists"
  shortname (Paragraph _)         = error "Can't reference paragraphs"
  shortname (Bib _)               = error $
    "Bibliography list of references cannot be referenced. " ++
    "You must reference the Section or an individual citation."

-- | MaxWidthPercent should be kept in the range 1-100. 
-- Values outside this range may have unexpected results.
-- Used for specifying max figure width as
-- pagewidth*MaxWidthPercent/100.
type MaxWidthPercent = Float

data ListType = Bullet [ItemType] -- ^ Bulleted list
              | Numeric [ItemType] -- ^ Enumerated List
              | Simple [ListPair] -- ^ Simple list with items denoted by @-@
              | Desc [ListPair] -- ^ Descriptive list, renders as "Title: Item" (see 'ListPair')
              | Definitions [ListPair] -- ^ Renders a list of "@Title@ is the @Item@"
         
data ItemType = Flat Sentence -- ^ Standard singular item
              | Nested Header ListType -- ^ Nest a list as an item

--FIXME: Remove Data and Theory from below.
-- | Types of definitions
data DType = Data QDefinition -- ^ QDefinition is the chunk with the defining 
                              -- equation used to generate the Data Definition
           | General
           | Theory RelationConcept -- ^ Theoretical models use a relation as
                                    -- their definition
           | Instance
           | TM
           | DD

---------------------------------------------------------------------------
-- smart constructors and combinators for making instances of the above
-- data types.  Over time, the types should no longer be exported, and 
-- only these used

-- | Smart constructor for creating Sections with introductory contents
-- (ie. paragraphs, tables, etc.) and a list of subsections.
section :: Sentence -> [Contents] -> [Section] -> RefAdd -> String -> Section
section title intro secs sn = Section title (map Con intro ++ map Sub secs) sn

-- | Figure smart constructor. Assumes 100% of page width as max width.
fig :: Label -> Filepath -> RefAdd -> Contents
fig l f = Figure l f 100

-- | Figure smart constructor for customized max widths.
figWithWidth :: Label -> Filepath -> MaxWidthPercent -> RefAdd -> Contents
figWithWidth = Figure

datadefn :: QDefinition -> Contents
datadefn = Definition . Data

reldefn :: RelationConcept -> Contents
reldefn = Definition . Theory

-- Below were moved to avoid import cycles between this file and Reference.hs --

-- | Automatically create the label for a definition
getDefName :: DType -> String
getDefName (Data c)   = "DD:" ++ concatMap repUnd (c ^. uid) -- FIXME: To be removed
getDefName (Theory c) = "T:" ++ concatMap repUnd (c ^. uid) -- FIXME: To be removed
getDefName TM         = "T:"
getDefName DD         = "DD:"
getDefName Instance   = "IM:"
getDefName General    = "GD:"

repUnd :: Char -> String
repUnd '_' = "."
repUnd c = c : []
