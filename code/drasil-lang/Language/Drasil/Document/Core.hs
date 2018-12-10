{-# Language TemplateHaskell #-}
module Language.Drasil.Document.Core where

import Language.Drasil.Chunk.Citation (BibRef)

import Language.Drasil.Classes (HasUID(uid), HasRefAddress(getRefAdd),
  MayHaveLabel(getMaybeLabel), HasLabel(getLabel), HasShortName(shortname))
import Language.Drasil.Expr (Expr)
import Language.Drasil.Label.Core (Label)
import Language.Drasil.Label () -- for instances
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.UID (UID)
import Language.Drasil.RefTypes (RefAdd, DType(..))

import Control.Lens ((^.), makeLenses, Lens', set)

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


data Contents = UlC UnlabelledContent
              | LlC LabelledContent

-- For 'Defini' below.  From DocumentLanguage.Definitions
--   tmodel, TM, mkTMField [ Para, EqnBlock, Enumeration]
--   ddefn, DD, mkDDField [Para, EqnBlock, Enumeration]
--   gdefn, General, mkGDField [Para, EqnBlock, Enumeration]
--   instanceModel, Instance, mkIMField [Para, EqnBlock, Enumeration]


-- | Types of layout objects we deal with explicitly
data RawContent = Table [Sentence] [[Sentence]] Title Bool
  -- ^ table has: header-row data(rows) label/caption showlabel?
               | Paragraph Sentence -- ^ Paragraphs are just sentences.
               | EqnBlock Expr
               | Enumeration ListType -- ^ Lists
               | Defini DType [(Identifier, [Contents])]
               | Figure Lbl Filepath MaxWidthPercent -- ^ Should use relative file path.
               | Assumption UID Sentence -- FIXME: hack, remove
               | Bib BibRef
               | Graph [(Sentence, Sentence)] (Maybe Width) (Maybe Height) Lbl
               -- ^ TODO: Fill this one in.
type Identifier = String

data LabelledContent = LblC { _lbl :: Label
                            , _ctype :: RawContent
                            }

data UnlabelledContent = UnlblC { _cntnts :: RawContent }

makeLenses ''LabelledContent
makeLenses ''UnlabelledContent

-- FIXME: this is here temporarily due to import cycles
class HasContents c where
  accessContents :: Lens' c RawContent

instance HasUID        LabelledContent where uid = lbl . uid  
instance HasRefAddress LabelledContent where getRefAdd = lbl . getRefAdd
instance HasLabel      LabelledContent where getLabel = lbl
instance MayHaveLabel  LabelledContent where getMaybeLabel x = Just (x ^. getLabel)
instance HasContents   LabelledContent where accessContents = ctype
instance HasShortName  LabelledContent where shortname = lbl . shortname

instance MayHaveLabel UnlabelledContent where getMaybeLabel _ = Nothing
instance HasContents  UnlabelledContent where accessContents = cntnts

instance HasContents Contents where
  accessContents f (UlC c) = fmap (UlC . (\x -> set cntnts x c)) (f $ c ^. cntnts)
  accessContents f (LlC c) = fmap (LlC . (\x -> set ctype x c)) (f $ c ^. ctype)
