{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.Citation
  ( -- Types
    Citation, BibRef, EntryID
    -- Accessors
  , citeID, citeKind
    -- Citation smart constructors
  , cArticle, cBookA, cBookE, cBooklet
  , cInBookACP, cInBookECP, cInBookAC, cInBookEC, cInBookAP, cInBookEP
  , cInCollection, cInProceedings, cManual, cMThesis, cMisc, cPhDThesis
  , cProceedings, cTechReport, cUnpublished
  -- FIXME move out of here later
  , HasCitation(getCitations)
  ) where

import Language.Drasil.People (People)
import Language.Drasil.Sentence (Sentence)

import Language.Drasil.Classes.Core (HasUID(uid), HasShortName(shortname))
import Language.Drasil.Classes (HasLabel(getLabel), HasFields(getFields))
-- import Language.Drasil.Chunk.CommonIdea (CI, commonIdeaWithDict)
import Language.Drasil.Data.Citation (author, chapter, pages, editor, bookTitle, title, 
  year, school, journal, institution, note, publisher, CitationKind(..), CiteField)
import Language.Drasil.Misc (noSpaces)
import Language.Drasil.Label.Core (Label)

import Control.Lens (makeLenses, (^.), Lens', over)

type BibRef = [Citation]
type EntryID = String -- Should contain no spaces

-- | All citations require a unique identifier (Label) used by the Drasil chunk.
-- We will re-use the UID part as an EntryID (String) used for creating reference links.
-- Finally we will have the reference information (type and fields).
data Citation = Cite
  { _citeKind :: CitationKind
  , _fields :: [CiteField]
  , _lb :: Label
  }
makeLenses ''Citation

instance HasUID       Citation where uid       = lb . uid
instance HasLabel     Citation where getLabel  = lb
instance HasShortName Citation where shortname = lb . shortname
instance HasFields    Citation where getFields = fields


-- | Smart constructor which implicitly uses EntryID as chunk i.
cite :: CitationKind -> [CiteField] -> Label -> Citation
cite x y z = Cite x y (over uid noSpaces z)

-- | We don't let anyone know that the EntryID is in fact the UID
citeID :: Citation -> EntryID
citeID c = c ^. uid

-- | Article citation requires author(s), title, journal, year.
-- Optional fields can be: volume, number, pages, month, and note.
-- Implicitly uses the EntryID as the chunk i.
cArticle :: People -> Sentence -> Sentence -> Int -> [CiteField] -> Label -> Citation
cArticle aut t journ yr opt lbe = cite Article 
  ((author aut) : (title t) : (journal journ) : (year yr) : opt) lbe

-- | Book citation requires author or editor, title, publisher, year.
-- Optional fields can be volume or number, series, address, edition, month, and note.
-- Implicitly uses the EntryID as the chunk i.
cBookA, cBookE :: People -> Sentence -> Sentence -> Int ->
  [CiteField] -> Label -> Citation
-- | Book citation by author
cBookA aut t pub yr opt lbe = cite Book (author aut : stdFields t pub yr opt) lbe
-- | Book citation by editor
cBookE ed t pub yr opt lbe = cite Book (editor ed : stdFields t pub yr opt) lbe

-- | Booklet citation requires title.
-- Optional fields can be author, how published, address, month, year, note.
-- Implicitly uses the EntryID as the chunk i.
cBooklet :: Sentence -> [CiteField] -> Label -> Citation
cBooklet t opt lbe = cite Booklet (title t : opt) lbe

-- | InBook citation requires author or editor, title, chapter and/or pages,
-- publisher, year. Optional fields can be volume or number, series, type,
-- address, edition, month, and note.
-- Implicitly uses the EntryID as the chunk i.
-- This smart constructor includes both chapter and page numbers.
cInBookACP, cInBookECP :: People -> Sentence -> Int -> [Int] ->
  Sentence -> Int -> [CiteField] -> Label -> Citation
-- | InBook citation by author.
cInBookACP auth t chap pgs pub yr opt lbe = cite InBook 
  (author auth : chapter chap : pages pgs : stdFields t pub yr opt) lbe
-- | InBook citation by editor.
cInBookECP ed t chap pgs pub yr opt lbe = cite InBook 
  (editor ed : chapter chap : pages pgs : stdFields t pub yr opt) lbe

-- | InBook citation excluding page numbers.
cInBookAC, cInBookEC :: People -> Sentence -> Int ->
  Sentence -> Int -> [CiteField] -> Label -> Citation

-- | Otherwise ientical to 'cInBookACP'
cInBookAC auth t chap pub yr opt lbe = cite InBook 
  (author auth : chapter chap : stdFields t pub yr opt) lbe
-- | Otherwise ientical to 'cInBookECP'
cInBookEC ed t chap pub yr opt lbe = cite InBook 
  (editor ed : chapter chap : stdFields t pub yr opt) lbe

-- | InBook citation excluding chapter.
cInBookAP, cInBookEP :: People -> Sentence -> [Int] ->
  Sentence -> Int -> [CiteField] -> Label -> Citation

-- | Otherwise ientical to 'cInBookACP'
cInBookAP auth t pgs pub yr opt lbe = cite InBook 
  (author auth : pages pgs : stdFields t pub yr opt) lbe
-- | Otherwise ientical to 'cInBookECP'
cInBookEP ed t pgs pub yr opt lbe = cite InBook 
  (editor ed : pages pgs : stdFields t pub yr opt) lbe

-- | InCollection citation requires author, title, bookTitle, publisher, year.
-- Optional fields can be editor, volume or number, series, type, chapter,
-- pages, address, edition, month, and note.
-- Implicitly uses the EntryID as the chunk i.
cInCollection :: People -> Sentence -> Sentence -> Sentence -> Int ->
  [CiteField] -> Label -> Citation
cInCollection auth t bt pub yr opt lbe = cite InCollection 
  (author auth : bookTitle bt : stdFields t pub yr opt) lbe

-- | InProceedings citation requires author, title, bookTitle, year.
-- Optional fields can be editor, volume or number, series, pages,
-- address, month, organization, publisher, and note.
-- Implicitly uses the EntryID as the chunk i.
cInProceedings :: People -> Sentence -> Sentence -> Int ->
  [CiteField] -> Label -> Citation
cInProceedings auth t bt yr opt lbe = cite InProceedings 
  (author auth : title t : bookTitle bt : year yr : opt) lbe

-- | Manual (technical documentation) citation requires title.
-- Optional fields can be author, organization, address, edition, month, year, and note.
-- Implicitly uses the EntryID as the chunk i.
cManual :: Sentence -> [CiteField] -> Label -> Citation
cManual t opt lbe = cite Manual (title t : opt) lbe

-- | Master's Thesis citation requires author, title, school, and year.
-- Optional fields can be type, address, month, and note.
-- Implicitly uses the EntryID as the chunk i.
cMThesis :: People -> Sentence -> Sentence -> Int -> [CiteField] -> Label -> Citation
cMThesis auth t sch yr opt lbe = cite MThesis (thesis auth t sch yr opt) lbe

-- | Misc citation requires nothing.
-- Optional fields can be author, title, howpublished, month, year, and note.
-- Implicitly uses the EntryID as the chunk i.
cMisc :: [CiteField] -> Label -> Citation
cMisc opt lbe = cite Misc opt lbe

-- | PhD Thesis citation requires author, title, school, and year.
-- Optional fields can be type, address, month, and note.
-- Implicitly uses the EntryID as the chunk i.
cPhDThesis :: People -> Sentence -> Sentence -> Int -> [CiteField] -> Label -> Citation
cPhDThesis auth t sch yr opt lbe = cite PhDThesis (thesis auth t sch yr opt) lbe

-- | Proceedings citation requires title and year.
-- Optional fields can be editor, volume or number, series, address,
-- publisher, note, month, and organization.
-- Implicitly uses the EntryID as the chunk i.
cProceedings :: Sentence -> Int -> [CiteField] -> Label -> Citation
cProceedings t yr opt lbe = cite Proceedings (title t : year yr : opt) lbe

-- | Technical Report citation requires author, title, institution, and year.
-- Optional fields can be type, number, address, month, and note.
-- Implicitly uses the EntryID as the chunk i.
cTechReport :: People -> Sentence -> Sentence -> Int -> [CiteField] -> Label -> Citation
cTechReport auth t inst yr opt lbe = cite TechReport 
  (author auth : title t : institution inst : year yr : opt) lbe

-- | Unpublished citation requires author, title, and note.
-- Optional fields can be month and year.
-- Implicitly uses the EntryID as the chunk i.
cUnpublished :: People -> Sentence -> Sentence -> [CiteField] -> Label -> Citation
cUnpublished auth t n opt lbe = cite Unpublished 
  (author auth : title t : note n : opt) lbe

-- Helper function (do not export) for creating book reference.
stdFields :: Sentence -> Sentence -> Int -> [CiteField] -> [CiteField]
stdFields t pub yr opt = title t : publisher pub : year yr : opt

-- Helper function (do not export) for creating thesis reference.
thesis :: People -> Sentence -> Sentence -> Int -> [CiteField] -> [CiteField]
thesis auth t sch yr opt = author auth : title t : school sch : year yr : opt

------------
-- This does not belong here, should be moved later
class HasCitation c where
  getCitations :: Lens' c [Citation]
