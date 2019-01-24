{-# Language TemplateHaskell #-}
module Language.Drasil.Document.Core where

import Language.Drasil.Chunk.Citation (BibRef)

import Language.Drasil.Classes.Core (HasUID(uid), HasRefAddress(getRefAdd),
  HasShortName(shortname))
import Language.Drasil.Classes (Referable(refAdd, renderRef))
import Language.Drasil.Expr (Expr)
import Language.Drasil.Label.Type (LblType(RP), IRefProg, name, raw, (+::+))
import Language.Drasil.RefProg(Reference)
import Language.Drasil.Sentence (Sentence)

import Control.Lens ((^.), makeLenses, Lens', set, view)

data ListType = Bullet [(ItemType,Maybe String)] -- ^ Bulleted list
              | Numeric [(ItemType,Maybe String)] -- ^ Enumerated List
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
type ListTuple = (Title,ItemType,Maybe String) -- ^ Title: Item
type Filepath = String
type Lbl      = Sentence


data Contents = UlC UnlabelledContent
              | LlC LabelledContent

-- For 'Defini' below.  From DocumentLanguage.Definitions
--   tmodel, TM, mkTMField [ Para, EqnBlock, Enumeration]
--   ddefn, DD, mkDDField [Para, EqnBlock, Enumeration]
--   gdefn, General, mkGDField [Para, EqnBlock, Enumeration]
--   instanceModel, Instance, mkIMField [Para, EqnBlock, Enumeration]

-- | Types of definitions
data DType = General
           | Instance
           | TM
           | DD

-- | Types of layout objects we deal with explicitly
data RawContent = Table [Sentence] [[Sentence]] Title Bool
  -- ^ table has: header-row data(rows) label/caption showlabel?
               | Paragraph Sentence -- ^ Paragraphs are just sentences.
               | EqnBlock Expr
               | Enumeration ListType -- ^ Lists
               | Defini DType [(Identifier, [Contents])]
               | Figure Lbl Filepath MaxWidthPercent -- ^ Should use relative file path.
               | Bib BibRef
               | Graph [(Sentence, Sentence)] (Maybe Width) (Maybe Height) Lbl
               -- ^ TODO: Fill this one in.
type Identifier = String

data LabelledContent = LblC { _ref :: Reference
                            , _ctype :: RawContent
                            }

data UnlabelledContent = UnlblC { _cntnts :: RawContent }

makeLenses ''LabelledContent
makeLenses ''UnlabelledContent

-- FIXME: this is here temporarily due to import cycles
class HasContents c where
  accessContents :: Lens' c RawContent

instance HasUID        LabelledContent where uid = ref . uid  
instance HasRefAddress LabelledContent where getRefAdd = getRefAdd . view ref
instance HasContents   LabelledContent where accessContents = ctype
instance HasShortName  LabelledContent where shortname = shortname . view ref

instance HasContents  UnlabelledContent where accessContents = cntnts

instance HasContents Contents where
  accessContents f (UlC c) = fmap (UlC . (\x -> set cntnts x c)) (f $ c ^. cntnts)
  accessContents f (LlC c) = fmap (LlC . (\x -> set ctype x c)) (f $ c ^. ctype)

instance Referable LabelledContent where
  refAdd     (LblC lb _) = getRefAdd lb
  renderRef  (LblC lb c) = RP (refLabelledCon c) (getRefAdd lb)

refLabelledCon :: RawContent -> IRefProg
refLabelledCon (Table _ _ _ _)       = raw "Table:" +::+ name 
refLabelledCon (Figure _ _ _)        = raw "Fig:" +::+ name
refLabelledCon (Graph _ _ _ _)       = raw "Fig:" +::+ name
refLabelledCon (Defini _ _)          = raw "Def:" +::+ name
refLabelledCon (EqnBlock _)          = raw "EqnB:" +::+ name
refLabelledCon (Enumeration _)       = raw "Lst:" +::+ name 
refLabelledCon (Paragraph _)         = error "Shouldn't reference paragraphs"
refLabelledCon (Bib _)               = error $ 
    "Bibliography list of references cannot be referenced. " ++
    "You must reference the Section or an individual citation."
