-- | Document Description Language
module Language.Drasil.Document where
import Prelude hiding (id)
import Language.Drasil.Chunk (id)
import Language.Drasil.Chunk.AssumpChunk
import Language.Drasil.Chunk.Attribute (getShortName)
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.Relation
import Language.Drasil.Chunk.ReqChunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Wrapper
import Language.Drasil.Spec (Sentence(..), RefType(..), (+:+))
import Language.Drasil.RefHelpers
import Language.Drasil.Expr
import Language.Drasil.Citations
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
type Sections = [Section]

-- | A Document has a Title ('Sentence'), Author(s) ('Sentence'), and Sections
-- which hold the contents of the document
data Document = Document Title Author Sections

-- | Section Contents are split into subsections or contents, where contents
-- are standard layout objects (see 'Contents')
data SecCons = Sub Section
             | Con Contents

-- | Sections have a title ('Sentence') and a list of contents ('SecCons')
data Section = Section Title [SecCons]

-- | Types of layout objects we deal with explicitly
data Contents = Table [Sentence] [[Sentence]] Title Bool
  -- ^ table has: header-row data(rows) label/caption showlabel?
               | Paragraph Sentence -- ^ Paragraphs are just sentences.
               | EqnBlock Expr
     --        CodeBlock Code   -- GOOL complicates this.  Removed for now.
               | Definition DType
               -- ^ Data/General definition or theoretical model. SymbolMap for
               -- looking up variables (currently a hack).
               | Enumeration ListType -- ^ Lists
               | Figure Label Filepath MaxWidthPercent -- ^ Should use relative file path.
               | Requirement ReqChunk
               | Assumption AssumpChunk
               | LikelyChange NWrapper
               | UnlikelyChange NWrapper
               | Bib BibRef
     --        UsesHierarchy [(ModuleChunk,[ModuleChunk])]
               | Graph [(Sentence, Sentence)] (Maybe Width) (Maybe Height) Label
               -- ^ TODO: Fill this one in.
               ------NEW TMOD/DDEF/IM/GD BEGINS HERE------
               ---- FIXME: The above Definition will need to be removed ----
               ---- FIXME: The below TMod, GDef, IMod, and DDef will need to be
               --- consolidated into one type (similar to deprecated Definition)
               --------------------------------------------
               | Defnt DType [(Identifier, [Contents])] RefName
               | TMod [(Identifier,[Contents])] RefName RelationConcept -- Ex. (Label, Paragraph $ phrase thing) and Reference name
               | GDef
               | IMod
               | DDef [(Identifier,[Contents])] RefName QDefinition --Similar to TMod
               -------- END TMOD/DDEF/etc. ----------------
type Identifier = String
type RefName = Sentence

-- | MaxWidthPercent should be kept in the range 1-100. 
-- Values outside this range may have unexpected results.
-- Used for specifying max figure width as
-- pagewidth*MaxWidthPercent/100.
type MaxWidthPercent = Float

data ListType = Bullet [ItemType] -- ^ Bulleted list
              | Number [ItemType] -- ^ Enumerated List
              | Simple [ListPair] -- ^ Simple list with items denoted by @-@
              | Desc [ListPair] -- ^ Descriptive list, renders as "Title: Item" (see 'ListPair')
              | Definitions [ListPair] -- ^ Renders a list of "@Title@ is the @Item@"
         
data ItemType = Flat Sentence -- ^ Standard singular item
              | Nested Header ListType -- ^ Nest a list as an item
               
-- | Types of definitions
data DType = Data QDefinition -- ^ QDefinition is the chunk with the defining 
                              -- equation used to generate the Data Definition
           | General
           | Theory RelationConcept -- ^ Theoretical models use a relation as
                                    -- their definition
           | Instance
           | TM
           | DD

-- | Every layout object has a reference name (for intra-document referencing)
-- and a reference type (denoting what type of reference to create)
class LayoutObj l where
  refName :: l -> Sentence
  rType   :: l -> RefType

instance LayoutObj Section where
  refName (Section t _) = S "Sec:" :+: inferName t
  rType _ = Sect

instance LayoutObj Contents where
  refName (Table _ _ l _)         = S "Table:" :+: inferName l
  refName (Figure l _ _)          = S "Figure:" :+: inferName l
  refName (Paragraph _)           = error "Can't reference paragraphs" --yet
  refName (EqnBlock _)            = error "EqnBlock ref unimplemented"
--  refName (CodeBlock _)         = error "Codeblock ref unimplemented"
  refName (Definition d)          = getDefName d
  refName (Defnt dt _ r)          = getDefName dt +:+ r
  refName (Enumeration _)         = error "List refs unimplemented"
  refName (Requirement rc)        = S $ "R:" ++ alphanumOnly (rc ^. id)
  refName (Assumption ac)         = S $ "A:" ++ alphanumOnly (ac ^. id)
  refName (LikelyChange lcc)      = S $ "LC:" ++ alphanumOnly (lcc ^. id)
  refName (UnlikelyChange ucc)    = S $ "UC:" ++ alphanumOnly (ucc ^. id)
--  refName (UsesHierarchy _)     = S $ "Figure:UsesHierarchy"
  refName (Graph _ _ _ l)         = S "Figure:" :+: inferName l
  refName (TMod _ _ _)            = error "TMod referencing unimplemented"
  refName (IMod)                  = error "IMod referencing unimplemented"
  refName (GDef)                  = error "GDef referencing unimplemented"
  refName (DDef _ _ _)            = error "DDef referencing unimplemented"
  refName (Bib _)                 = error "Bib referencing unimplemented"
  rType (Table _ _ _ _)           = Tab
  rType (Figure _ _ _)              = Fig
  rType (Definition (Data qd))    = Def $ fmap S $ getA qd
  rType (Definition (Theory rc))  = Def $ fmap S $ getA rc
  rType (Definition _)            = Def Nothing
  rType (Defnt _ _ _)             = Def Nothing
  rType (Requirement r)           = Req $ getShortName r
  rType (Assumption a)            = Assump $ getShortName a
  rType (LikelyChange lc)         = LC $ fmap S $ getA lc
  rType (UnlikelyChange _)        = UC
 -- rType (UsesHierarchy _)       = Fig
  rType (Graph _ _ _ _)           = Fig
  rType (TMod _ _ _)              = Def Nothing
  rType (IMod)                    = Def Nothing
  rType (GDef)                    = Def Nothing
  rType (DDef _ _ _)              = Def Nothing
  rType _                         = error "Attempting to reference unimplemented reference type"
  
-- | Automatically create the label for a definition
getDefName :: DType -> Sentence
getDefName (Data c)   = S $ "DD:" ++ (repUnd (c ^. id))
getDefName (Theory c) = S $ "T:" ++ (repUnd (c ^. id))
getDefName TM         = S "T:"
getDefName DD         = S "DD:"
getDefName _          = error "Unimplemented definition type reference"

---------------------------------------------------------------------------
-- smart constructors and combinators for making instances of the above
-- data types.  Over time, the types should no longer be exported, and 
-- only these used

-- | Smart constructor for creating Sections with introductory contents
-- (ie. paragraphs, tables, etc.) and a list of subsections.
section :: Sentence -> [Contents] -> [Section] -> Section
section title intro secs = Section title (map Con intro ++ map Sub secs)

-- | Figure smart constructor. Assumes 100% of page width as max width.
fig :: Label -> Filepath -> Contents
fig l f = Figure l f 100

-- | Figure smart constructor for customized max widths.
figWithWidth :: Label -> Filepath -> MaxWidthPercent -> Contents
figWithWidth = Figure
