{-# Language TemplateHaskell #-}
-- | Document Description Language
module Language.Drasil.Document where
import Data.Drasil.IdeaDicts (documentc)
import Language.Drasil.Chunk.CommonIdea (CI, commonIdeaWithDict)
import Language.Drasil.Classes.Core (HasUID(uid), HasShortName(shortname), getRefAdd)
import Language.Drasil.Classes (Referable(refAdd, renderRef))
import Language.Drasil.Document.Core
import Language.Drasil.Label.Type (prepend, LblType(RP, URI),raw, (+::+), name)
import Language.Drasil.Misc (repUnd)
import Language.Drasil.NounPhrase (cn')
import Language.Drasil.RefProg (Reference(Reference))
import Language.Drasil.Sentence (Sentence(..))
import Language.Drasil.ShortName (ShortName, shortname')
import Language.Drasil.UID (UID)

import Control.Lens (makeLenses, view)

-- | Section Contents are split into subsections or contents, where contents
-- are standard layout objects (see 'Contents')
data SecCons = Sub   Section
             | Con   Contents

-- | Sections have a title ('Sentence') and a list of contents ('SecCons')
-- and its shortname
data Section = Section 
             { tle  :: Title 
             , cons :: [SecCons]
             , _lab :: Reference
             }
makeLenses ''Section

instance HasUID        Section where uid = lab . uid
instance HasShortName  Section where shortname = shortname . view lab
instance Referable Section where
  refAdd    (Section _ _ lb ) = getRefAdd lb
  renderRef (Section _ _ lb)  = RP (raw "Section: " +::+ name) (getRefAdd lb)

sectionci :: CI
sectionci    = commonIdeaWithDict "sectionci"    (cn' "section")                   "DD"        [documentc]

-- | A Document has a Title ('Sentence'), Author(s) ('Sentence'), and Sections
-- which hold the contents of the document
data Document = Document Title Author [Section]

-- | Smart constructor for labelled content chunks
llcc :: Reference -> RawContent -> LabelledContent
llcc = LblC

-- | Smart constructor for unlabelled content chunks
ulcc :: RawContent -> UnlabelledContent
ulcc = UnlblC

---------------------------------------------------------------------------
-- smart constructors needed for LabelledContent
mkParagraph :: Sentence -> Contents
mkParagraph x = UlC $ ulcc $ Paragraph x

mkFig :: Reference -> RawContent -> Contents
mkFig x y = LlC $ llcc x y

--Fixme: use mkRawLc or llcc?
mkRawLC :: RawContent -> Reference -> LabelledContent
mkRawLC x lb = llcc lb x

---------------------------------------------------------------------------
-- smart constructors and combinators for making instances of the above
-- data types.  Over time, the types should no longer be exported, and
-- only these used

-- | Smart constructor for creating Sections with introductory contents
-- (ie. paragraphs, tables, etc.) and a list of subsections.
section :: Sentence -> [Contents] -> [Section] -> Reference -> Section
section title intro secs lbe = Section title (map Con intro ++ map Sub secs) lbe

extractSection :: Document -> [Section]
extractSection (Document _ _ sec) = concatMap getSec sec

getSec :: Section -> [Section]
getSec t@(Section _ sc _) = t : concatMap getSecCons sc

getSecCons :: SecCons -> [Section]
getSecCons (Sub sec) = getSec sec
getSecCons (Con _)   = []

-- | Figure smart constructor. Assumes 100% of page width as max width.
fig :: Lbl -> Filepath -> RawContent
fig l f = Figure l f 100

-- | Figure smart constructor for customized max widths.
figWithWidth :: Lbl -> Filepath -> MaxWidthPercent -> RawContent
figWithWidth = Figure

---------------------------------------------------------------------------
-- FIXME: horrible hacks.
-- These should eventually either disappear, or at least move out to docLang
makeTabRef :: String -> Reference
makeTabRef rs = Reference rs (RP (prepend "Tab") ("Table:" ++ repUnd rs)) (shortname' rs)

makeFigRef :: String -> Reference
makeFigRef rs = Reference rs (RP (prepend "Fig") ("Figure:" ++ repUnd rs)) (shortname' rs)

makeSecRef :: String -> String -> Reference
makeSecRef r s = Reference (r ++ "Label") (RP (prepend "Section") ("Sec:" ++ repUnd r))
  (shortname' s)

makeLstRef :: String -> String -> Reference
makeLstRef r s = Reference (r ++ "Label") (RP (prepend "Lst") ("Lst:" ++ repUnd r))
  (shortname' s)

-- | Create a reference for a URI
makeURI :: UID -> String -> ShortName -> Reference
makeURI u r s = Reference u (URI r) s
