{-# Language TemplateHaskell #-}
-- | Document Description Language.
module Language.Drasil.Document where

import Language.Drasil.ShortName (HasShortName(..), ShortName, shortname')
import Language.Drasil.Document.Core (UnlabelledContent(UnlblC),
  LabelledContent(LblC), HasCaption(..), RawContent(Figure, Paragraph),
  Contents(..), Lbl, Filepath, Author, Title, MaxWidthPercent )
import Language.Drasil.Label.Type (getAdd, prepend, LblType(..),
  Referable(..), HasRefAddress(..) )
import Language.Drasil.Reference (Reference(Reference))
import Language.Drasil.Sentence (Sentence(..))
import Drasil.Database.UID (UID, HasUID(..), (+++.), mkUid, nsUid)

import Utils.Drasil (repUnd)

import Control.Lens ((^.), makeLenses, view)

-- * Section Types

-- | Section Contents are split into subsections or contents, where contents
-- are standard layout objects (see 'Contents').
data SecCons = Sub Section
             | Con Contents

data Partition = Sections
                | Part
                | Chapter

-- | Sections have a title ('Sentence'), a list of contents ('SecCons')
-- and a shortname ('Reference').
data Section = Section
             { tle  :: Title
             , cons :: [SecCons]
             , _lab :: Reference
             }
makeLenses ''Section

{-
data Section = Section
             { depth  :: Depth
             , header :: SecHeader 
             , cons   :: Content
             }

data SecHeader = SecHeader Title Reference
data Content   = Content   Contents
-}
-- | Finds the 'UID' of a 'Section'.
instance HasUID        Section where uid = lab . uid
-- | 'Section's are equal if 'UID's are equal.
instance Eq Section where a == b = (a ^. uid) == (b ^. uid)
-- | Finds the short name of a 'Section'.
instance HasShortName  Section where shortname = shortname . view lab
-- | Finds the reference information of a 'Section'.
instance Referable Section where
  refAdd     = getAdd . getRefAdd . view lab
  renderRef (Section _ _ lb)  = RP (prepend "Sec") (getAdd $ getRefAdd lb)
-- | Finds the reference address of a 'Section'.
instance HasRefAddress Section where getRefAdd (Section _ _ lb) = RP (prepend "Sec") (getAdd $ getRefAdd lb)

-- | A Document has a Title ('Sentence'), Author(s) ('Sentence'), and 'Section's
-- which hold the contents of the document.
data Document = Document Title Author ShowTableOfContents [Section]
              | Notebook Title Author [Section]

-- Temporarily data type for 'notebook' document, might be extended or use 'Document' instead
--data Notebook = Notebook Title Author [Section]

-- | Determines whether or not the table of contents appears on the generated artifacts.
data ShowTableOfContents = ToC | NoToC

-- Medium hack for now. This function is unable to tell if the section
-- is for a table of contents, as that doesn't appear until docLang.
-- This function is needed by the TeX printer, as TeX carries its own form of creating
-- a table of contents. However, the printer package is compiled before the docLang one.
-- | Manually removes the first section of a document (table of contents section).
-- temp fix for Notebook (see if we need this in notebook later)
checkToC :: Document -> Document
checkToC (Document t a toC sc) =
  case toC of
    ToC -> Document t a toC $ drop 1 sc
    _   -> Document t a toC sc
checkToC (Notebook t a sc) = Notebook t a sc

-- * Content Constructors

-- | Smart constructor for labelled content chunks.
llcc :: Reference -> RawContent -> LabelledContent
llcc = LblC

-- | Smart constructor for unlabelled content chunks (no 'Reference').
ulcc :: RawContent -> UnlabelledContent
ulcc = UnlblC

---------------------------------------------------------------------------
-- | Smart constructor that wraps 'UnlabelledContent' into 'Contents'.
mkParagraph :: Sentence -> Contents
mkParagraph x = UlC $ ulcc $ Paragraph x

-- | Smart constructor that wraps 'LabelledContent' into 'Contents'.
mkFig :: Reference -> RawContent -> Contents
mkFig x y = LlC $ llcc x y

--Fixme: use mkRawLc or llcc?
-- | Smart constructor similar to 'llcc', but takes in 'RawContent' first.
mkRawLC :: RawContent -> Reference -> LabelledContent
mkRawLC x lb = llcc lb x

---------------------------------------------------------------------------
-- * Section Constructors

-- smart constructors and combinators for making instances of the above
-- data types. Over time, the types should no longer be exported, and
-- only these used.

-- | Smart constructor for creating 'Section's with a title ('Sentence'), introductory contents
-- (ie. paragraphs, tables, etc.), a list of subsections, and a shortname ('Reference').
section :: Sentence -> [Contents] -> [Section] -> Reference -> Section
section title intro secs = Section title (map Con intro ++ map Sub secs)

-- | 'Figure' smart constructor with a 'Lbl' and a 'Filepath'. Assumes 100% of page width as max width. Defaults to 'WithCaption'.
fig :: Lbl -> Filepath -> RawContent
fig l f = Figure l f 100 WithCaption

-- | 'Figure' smart constructor without a caption.
figNoCap :: Lbl -> Filepath -> RawContent
figNoCap l f = Figure l f 100 NoCaption

-- | 'Figure' smart constructor that allows for customized max widths. Defaults to 'WithCaption'.
figWithWidth :: Lbl -> Filepath -> MaxWidthPercent -> RawContent
figWithWidth l f wp = Figure l f wp WithCaption

-- | 'Figure' smart constructor with customized max widths and no caption.
figNoCapWithWidth :: Lbl -> Filepath -> MaxWidthPercent -> RawContent
figNoCapWithWidth l f wp = Figure l f wp NoCaption

---------------------------------------------------------------------------
-- * Reference Constructors

docNs :: UID -> UID
docNs = nsUid "doc"

docUid :: String -> UID
docUid = docNs . mkUid

-- FIXME: horrible hacks.
-- FIXME: May need UID checker function here.
-- These should eventually either disappear, or at least move out to docLang
-- | Create a reference for a table. Takes in the name of a table (which will also be used for its shortname).
makeTabRef :: String -> Reference
makeTabRef rs = Reference (docUid rs) (RP (prepend "Tab") ("Table:" ++ repUnd rs)) (shortname' (S rs))

-- | Create a reference for a figure. Takes in the name of a figure (which will also be used for its shortname).
makeFigRef :: String -> Reference
makeFigRef rs = Reference (docUid rs) (RP (prepend "Fig") ("Figure:" ++ repUnd rs)) (shortname' (S rs))

-- | Create a reference for a section. Takes in the name of a section and a shortname for the section.
makeSecRef :: String -> Sentence -> Reference
makeSecRef r s = Reference (mkUid $ r ++ "Label") (RP (prepend "Sec") ("Sec:" ++ repUnd r))
  (shortname' s)

-- | Create a reference for a equation. Takes in the name of the equation (which will also be used for its shortname).
makeEqnRef :: String -> Reference
makeEqnRef rs = Reference (docUid rs) (RP (prepend "Eqn") ("Equation:" ++ repUnd rs)) (shortname' (S rs))

-- | Create a reference for a 'URI'. Takes in a 'UID' (as a 'String'), a reference address, and a shortname.
makeURI :: String -> String -> ShortName -> Reference
makeURI u r = Reference (mkUid u) (URI r)

-- | Variants of 'makeTabRef' that takes a 'UID' instead of a 'String'.
makeTabRef' :: UID -> Reference
makeTabRef' rs = Reference (docNs rs) (RP (prepend "Tab") ("Table:" ++ repUnd (show rs))) (shortname' (S $ show rs))

-- | Variants of 'makeFigRef' that takes a 'UID' instead of a 'String'.
makeFigRef' :: UID -> Reference
makeFigRef' rs = Reference (docNs rs) (RP (prepend "Fig") ("Figure:" ++ repUnd (show rs))) (shortname' (S $ show rs))

-- | Variants of 'makeSecRef' that takes a 'UID' instead of a 'String'.
makeSecRef' :: UID -> Sentence -> Reference
makeSecRef' r s = Reference (r +++. "Label") (RP (prepend "Sec") ("Sec:" ++ repUnd (show r)))
  (shortname' s)

-- | Variants of 'makeEqnRef' that takes a 'UID' instead of a 'String'.
makeEqnRef' :: UID -> Reference
makeEqnRef' rs = Reference (docNs rs) (RP (prepend "Eqn") ("Equation:" ++ repUnd (show rs))) (shortname' (S $ show rs))

-- | Variants of 'makeURI' that takes a 'UID' instead of a 'String'.
makeURI' :: UID -> String -> ShortName -> Reference
makeURI' u r = Reference u (URI r)
