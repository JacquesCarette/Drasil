{-# Language TemplateHaskell #-}
-- | Document Description Language
module Language.Drasil.Document where

import Language.Drasil.Document.Core
import Language.Drasil.Chunk.AssumpChunk (AssumpChunk)
import Language.Drasil.Chunk.Change (Change)
import Language.Drasil.Chunk.Citation (BibRef)
import Language.Drasil.Chunk.Change (Change(..), ChngType(..))
import Language.Drasil.Chunk.Eq (QDefinition)
import Language.Drasil.Chunk.Relation (RelationConcept)
import Language.Drasil.Chunk.ReqChunk (ReqChunk)
import Language.Drasil.Chunk.ShortName (HasShortName(shortname), getStringSN)
import Language.Drasil.Classes (HasUID(uid), HasRefAddress(getRefAdd),
  MayHaveLabel(getMaybeLabel), HasLabel(getLabel))

import Language.Drasil.Label (Label, getAdd, mkLabelRA, mkLabelRA', mkEmptyLabel)
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
getDefName (Data c)   = "DD:"
getDefName (Data' c)  = "DD:"
getDefName (Theory c) = "T:"
getDefName TM         = "T:"
getDefName DD         = "DD:"
getDefName Instance   = "IM:"
getDefName General    = "GD:"

getDefLabel :: DType -> Label
getDefLabel (Data c)   = c ^. getLabel
getDefLabel (Data' c)  = c ^. getLabel
getDefLabel (Theory c) = let u = getMaybeLabel c in
  case u of Just l  -> l
            Nothing -> error "No explicit label given for relation concept"
getDefLabel (_)        = mkEmptyLabel

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

-- FIXME: no pattern for Bib BibRef of RawContent
mkRawLC :: RawContent -> Label -> LabelledContent
mkRawLC x@(Table _ _ _ _)  lb = llcc (mkLabelRA' ("Table:" ++ (getAdd (lb ^. getRefAdd))) 
  (getStringSN (lb ^. shortname))) x
mkRawLC x@(Paragraph _)      lb = llcc (mkLabelRA' ("Paragraph:" ++ (getAdd (lb ^. getRefAdd)))  
  (getStringSN (lb ^. shortname))) x
mkRawLC x@(Definition d)     lb = let u = getAdd ((getDefLabel d) ^. getRefAdd) in
  case u of "empty" -> llcc (mkLabelRA' ("Definition:" ++ (getAdd (lb ^. getRefAdd))) 
                         (getStringSN (lb ^. shortname))) x
            _       -> llcc (mkLabelRA' ((getDefName d) ++ u)
                         (getStringSN ((getDefLabel d) ^. shortname))) x 
mkRawLC x@(Enumeration _)    lb = llcc (mkLabelRA' ("List:" ++ (getAdd (lb ^. getRefAdd)))
  (getStringSN (lb ^. shortname))) x
mkRawLC x@(Figure _ _ _)   lb = llcc (mkLabelRA' ("Figure:" ++ (getAdd (lb ^. getRefAdd)))
  (getStringSN (lb ^. shortname))) x
mkRawLC x@(Requirement rq)   _  = llcc (mkLabelRA' ("R:" ++ (getAdd ((rq ^. getLabel) ^. getRefAdd)))
  (getStringSN (rq ^. shortname))) x
mkRawLC x@(Assumption ac)    _  = llcc (mkLabelRA' ("A:" ++ (getAdd ((ac ^. getLabel) ^. getRefAdd)))
  (getStringSN (ac ^. shortname))) x
mkRawLC x@(Change (ChC _ Likely _ lb))   _  = llcc (mkLabelRA' 
  ("LC:" ++ (getAdd (lb ^. getRefAdd))) (getStringSN (lb ^. shortname))) x
mkRawLC x@(Change (ChC _ Unlikely _ lb)) _  = llcc (mkLabelRA'
  ("UC:" ++ (getAdd (lb ^. getRefAdd))) (getStringSN (lb ^. shortname))) x
mkRawLC x@(Graph _ _ _ _)   lb = llcc (mkLabelRA' ("Graph:" ++ (getAdd (lb ^. getRefAdd)))
  (getStringSN (lb ^. shortname))) x
mkRawLC x@(Defnt d _)       lb = let u = getAdd ((getDefLabel d) ^. getRefAdd) in
  case u of "empty" -> llcc (mkLabelRA' ("Definition:" ++ (getAdd (lb ^. getRefAdd)))
                         (getStringSN (lb ^. shortname))) x
            _       -> llcc (mkLabelRA' ((getDefName d) ++ u)
                         (getStringSN ((getDefLabel d) ^. shortname))) x
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
fig :: Lbl -> Filepath -> RawContent
fig l f = Figure l f 100

-- | Figure smart constructor for customized max widths.
figWithWidth :: Lbl -> Filepath -> MaxWidthPercent -> RawContent
figWithWidth = Figure

reldefn :: RelationConcept -> LabelledContent
reldefn = (\x -> reldefn' (getMaybeLabel x) x)
  where
  	reldefn' (Just x) y = llcc x ((Definition . Theory) y)
  	reldefn' Nothing _  = error "cannot make reference to a Nothing label"
