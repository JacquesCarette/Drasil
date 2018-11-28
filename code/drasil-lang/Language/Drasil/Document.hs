{-# Language TemplateHaskell #-}
-- | Document Description Language
module Language.Drasil.Document where

import Data.Drasil.IdeaDicts (documentc)
import Language.Drasil.Document.Core
import Language.Drasil.Classes (HasLabel(getLabel), HasShortName(shortname))
import Language.Drasil.UID (UID)
import Language.Drasil.Label (Label)
import Language.Drasil.Sentence (Sentence(..))
import Language.Drasil.Chunk.CommonIdea (CI, commonIdeaWithDict)
import Language.Drasil.NounPhrase (cn')

import Control.Lens (makeLenses)

-- | Section Contents are split into subsections or contents, where contents
-- are standard layout objects (see 'Contents')
data SecCons = Sub   Section
             | Con   Contents

-- | Sections have a title ('Sentence') and a list of contents ('SecCons')
-- and its shortname
data Section = Section 
             { tle :: Title 
             , cons :: [SecCons]
             , _lab :: Label
             }
makeLenses ''Section

instance HasLabel      Section where getLabel = lab
instance HasShortName  Section where shortname = lab . shortname

sectionci :: CI
sectionci    = commonIdeaWithDict "sectionci"    (cn' "section")                   "DD"        [documentc]

-- | A Document has a Title ('Sentence'), Author(s) ('Sentence'), and Sections
-- which hold the contents of the document
data Document = Document Title Author [Section]

{--data RawContent = Table UID [Sentence] [[Sentence]] Title Bool
  -- ^ table has: header-row data(rows) label/caption showlabel?
               | Paragraph Sentence -- ^ Paragraphs are just sentences.
               | EqnBlock UID Expr
               | Enumeration UID ListType -- ^ Lists
               | Defini UID DType [(Identifier, [Contents])]
               | Figure UID Lbl Filepath MaxWidthPercent -- ^ Should use relative file path.
               | Assumption UID Sentence Label -- FIXME: hack, remove
               | Bib BibRef
               | Graph UID [(Sentence, Sentence)] (Maybe Width) (Maybe Height) Lbl--}

helpUIDfrmRaw :: RawContent -> UID
helpUIDfrmRaw (Table u _ _ _ _) = u
helpUIDfrmRaw (Figure u _ _ _)  = u
helpUIDfrmRaw (Graph u _ _ _ _) = u
helpUIDfrmRaw (EqnBlock u _)    = u
helpUIDfrmRaw (Enumeration u _) = u
helpUIDfrmRaw (Defini u _ _)    = u
helpUIDfrmRaw (Assumption u _ _)= u
helpUIDfrmRaw (Bib _)           = error "This chunk doesn't have a UID."
helpUIDfrmRaw (Paragraph _)     = error "This chunk doesn't have a UID." 

-- | Smart constructor for labelled content chunks
llcc :: Label -> RawContent -> LabelledContent
llcc = \x y -> LblC (helpUIDfrmRaw y) x y

-- | Smart constructor for unlabelled content chunks
ulcc :: RawContent -> UnlabelledContent
ulcc = UnlblC

---------------------------------------------------------------------------
-- smart constructors needed for LabelledContent
mkParagraph :: Sentence -> Contents
mkParagraph x = UlC $ ulcc $ Paragraph x

mkFig :: Label -> RawContent -> Contents
mkFig x y = LlC $ llcc x y

--Fixme: use mkRawLc or llcc?
mkRawLC :: RawContent -> Label -> LabelledContent
mkRawLC x lb = llcc lb x

---------------------------------------------------------------------------
-- smart constructors and combinators for making instances of the above
-- data types.  Over time, the types should no longer be exported, and
-- only these used

-- | Smart constructor for creating Sections with introductory contents
-- (ie. paragraphs, tables, etc.) and a list of subsections.
section :: Sentence -> [Contents] -> [Section] -> Label -> Section
section title intro secs lbe = Section title (map Con intro ++ map Sub secs) lbe

section'' :: Sentence -> [Contents] -> [Section] -> Label -> Section
section'' title intro secs lbe = section title intro secs lbe

-- | Figure smart constructor. Assumes 100% of page width as max width.
fig :: Lbl -> Filepath -> RawContent
fig l f = Figure "fixme" l f 100

-- | Figure smart constructor for customized max widths.
figWithWidth :: Lbl -> Filepath -> MaxWidthPercent -> RawContent
figWithWidth = Figure "fixme"
