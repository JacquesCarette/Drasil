{-# Language TemplateHaskell #-}
-- | Document Description Language
module Language.Drasil.Document where

import Language.Drasil.Document.Core
import Language.Drasil.Chunk.AssumpChunk (AssumpChunk)
import Language.Drasil.Chunk.Change (Change)
import Language.Drasil.Chunk.Citation (BibRef)
import Language.Drasil.Chunk.Eq (QDefinition)
import Language.Drasil.Chunk.DataDefinition (DataDefinition)
import Language.Drasil.Chunk.Relation (RelationConcept)
import Language.Drasil.Chunk.ReqChunk (ReqChunk)
import Language.Drasil.Chunk.ShortName (HasShortName(shortname))
import Language.Drasil.Classes (HasUID(uid), HasRefAddress(getRefAdd),
  MayHaveLabel(getMaybeLabel), HasLabel(getLabel))

import Language.Drasil.Label (Label, mkLabelRA, mkEmptyLabel)
import Language.Drasil.RefTypes (RefAdd)
import Language.Drasil.Spec (Sentence(..))

import Control.Lens ((^.), makeLenses, Lens', set)

makeLenses ''LabelledContent
makeLenses ''UnlabelledContent

instance HasRefAddress LabelledContent where getRefAdd = lbl . getRefAdd
instance HasLabel      LabelledContent where getLabel = lbl
instance MayHaveLabel  LabelledContent where getMaybeLabel x = Just (x ^. getLabel)
instance HasContents   LabelledContent where accessContents = ctype
instance HasShortName  LabelledContent where shortname = lbl . shortname

instance MayHaveLabel UnlabelledContent where getMaybeLabel _ = Nothing
instance HasContents  UnlabelledContent where accessContents = cntnts

-- FIXME: this is here temporarily due to import cycles
class HasContents c where
  accessContents :: Lens' c RawContent

repUnd :: Char -> String
repUnd '_' = "."
repUnd c = c : []

-- | Automatically create the label for a definition
getDefName :: DType -> String
getDefName (Data c)   = "DD:" ++ concatMap repUnd (c ^. uid) -- FIXME: To be removed
getDefName (Data' c)  = "DD:" ++ concatMap repUnd (c ^. uid) -- FIXME: To be removed
getDefName (Theory c) = "T:" ++ concatMap repUnd (c ^. uid) -- FIXME: To be removed
getDefName TM         = "T:"
getDefName DD         = "DD:"
getDefName Instance   = "IM:"
getDefName General    = "GD:"


instance HasContents Contents where
  accessContents f (UlC c) = fmap (UlC . (\x -> set cntnts x c)) (f $ c ^. cntnts)
  accessContents f (LlC c) = fmap (LlC . (\x -> set ctype x c)) (f $ c ^. ctype)

-- | Section Contents are split into subsections or contents, where contents
-- are standard layout objects (see 'Contents')
data SecCons = Sub   Section
             | Con   Contents

-- | Sections have a title ('Sentence') and a list of contents ('SecCons')
-- and its shortname
data Section = Section 
             { tle :: Title 
             , cons :: [SecCons]
             , _lb :: Label
             }
makeLenses ''Section

instance HasLabel      Section where getLabel = lb
instance HasShortName  Section where shortname = lb . shortname

-- | A Document has a Title ('Sentence'), Author(s) ('Sentence'), and Sections
-- which hold the contents of the document
data Document = Document Title Author [Section]

-- | Smart constructor for labelled content chunks
llcc :: Label -> RawContent -> LabelledContent
llcc = LblC

-- | Smart constructor for unlabelled content chunks
ulcc :: RawContent -> UnlabelledContent
ulcc = UnlblC

---------------------------------------------------------------------------
-- smart constructors needed for LabelledContent
-- nothing has a shortname right now
mkTableLC :: String -> String -> String -> RawContent -> LabelledContent
mkTableLC labelUID refAdd sn' tbl = llcc (mkLabelRA labelUID refAdd sn') tbl

mkParagraph :: Sentence -> Contents
mkParagraph x = UlC $ ulcc $ Paragraph x

mkFig :: Label -> RawContent -> Contents
mkFig x y = LlC $ llcc x y

mkDefinitionLC :: String -> String -> String -> RawContent -> LabelledContent
mkDefinitionLC labelUID refAdd sn dfn = llcc (mkLabelRA labelUID refAdd sn) dfn

{-mkEqnBlock
mkEnumeration
mkFigure
mkRequirement
mkAssumption
mkChange
mkBib
mkGraph
mkDefnt-}
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
fig :: Lbl -> Filepath -> RefAdd -> RawContent
fig l f = Figure l f 100

-- | Figure smart constructor for customized max widths.
figWithWidth :: Lbl -> Filepath -> MaxWidthPercent -> RefAdd -> RawContent
figWithWidth = Figure

reldefn :: RelationConcept -> LabelledContent
reldefn = (\x -> llcc mkEmptyLabel x) . Definition . Theory