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
import Language.Drasil.Chunk.ShortName (HasShortName(shortname), ShortName,
  shortname')
import Language.Drasil.Classes (HasUID(uid), HasRefAddress(getRefAdd), 
  MayHaveLabel(getMaybeLabel), HasLabel(getLabel))

import Language.Drasil.Expr (Expr)
import Language.Drasil.Label (Label, mkLabelRA)
import Language.Drasil.RefTypes (RefAdd)
import Language.Drasil.Spec (Sentence(..))

import Control.Lens ((^.), makeLenses, Lens')

makeLenses ''LabelledContent
makeLenses ''UnlabelledContent

instance HasRefAddress LabelledContent where getRefAdd = lbl . getRefAdd
instance HasLabel      LabelledContent where getLabel = lbl
instance MayHaveLabel  LabelledContent where getMaybeLabel x = Just (x ^. getLabel)
instance HasContents   LabelledContent where accessContents = ctype

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

--FIXME: needs to be removed!
instance HasShortName  RawContent where
  shortname (Table _ _ _ _ r)     = shortname' $ "Table:" ++ r
  shortname (Figure _ _ _ r)      = shortname' $ "Figure:" ++ r
  shortname (Graph _ _ _ _ r)     = shortname' $ "Figure:" ++ r
  shortname (EqnBlock _)          = shortname' $ "Equation:"
  shortname (Definition d)        = shortname' $ getDefName d
  shortname (Defnt _ _ r)         = shortname' r
  shortname (Requirement rc)      = shortname rc
  shortname (Assumption ca)       = shortname ca
  shortname (Change lcc)          = shortname lcc
  shortname (Enumeration _)       = error "Can't reference lists"
  shortname (Paragraph _)         = error "Can't reference paragraphs"
  shortname (Bib _)               = error $
    "Bibliography list of references cannot be referenced. " ++
    "You must reference the Section or an individual citation."

instance HasContents  Contents where 
  accessContents = accessContents

-- | Section Contents are split into subsections or contents, where contents
-- are standard layout objects (see 'Contents')
data SecCons = Sub Section
             | Con Contents

-- | Sections have a title ('Sentence') and a list of contents ('SecCons')
-- and its shortname
data Section = Section 
             { tle :: Title 
             , cons :: [SecCons] 
             , _ra :: RefAdd      --Hack to be fixed in later branch
             , _sn :: ShortName   --Hack to be fixed in later branch 
             }
makeLenses ''Section

instance HasShortName  Section where shortname (Section _ _ _ sn') = sn'

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

mkParagraph x = UlC $ ulcc $ Paragraph x

mkFig :: Label -> RawContent -> Contents
mkFig x y = LlC $ llcc x y

{-
mkEqnBlock
mkDefinition
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
section :: Sentence -> [Contents] -> [Section] -> String -> ShortName -> Section
section title intro secs r sn' = Section title (map Con intro ++ map Sub secs) r sn'

section'' :: Sentence -> [Contents] -> [Section] -> String  -> Section
section'' title intro secs r = section title intro secs r (shortname' r)

-- | Figure smart constructor. Assumes 100% of page width as max width.
fig :: Lbl -> Filepath -> RefAdd -> RawContent
fig l f = Figure l f 100

-- | Figure smart constructor for customized max widths.
figWithWidth :: Lbl -> Filepath -> MaxWidthPercent -> RefAdd -> RawContent
figWithWidth = Figure

datadefn :: QDefinition -> RawContent
datadefn = Definition . Data

reldefn :: RelationConcept -> RawContent
reldefn = Definition . Theory