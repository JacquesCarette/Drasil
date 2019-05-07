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
  ) where

import Language.Drasil.People (People)

import Language.Drasil.Classes.Core (HasUID(uid), HasShortName(shortname))
import Language.Drasil.Classes.Citations (HasFields(getFields))
import Language.Drasil.Classes (Referable(refAdd, renderRef))
import Language.Drasil.Data.Citation (author, chapter, pages, editor, bookTitle, title, 
  year, school, journal, institution, note, publisher, CitationKind(..), CiteField)
import Language.Drasil.Label.Type (LblType(Citation))
import Language.Drasil.Misc (noSpaces)
import Language.Drasil.ShortName (ShortName, shortname')
import Language.Drasil.UID (UID)

import Control.Lens (makeLenses, (^.))

type BibRef = [Citation]
type EntryID = String -- Should contain no spaces

-- | All citations require a unique identifier used by the Drasil chunk.
-- We will re-use the UID part as an EntryID (String) used for creating reference links.
-- Finally we will have the reference information (type and fields).
data Citation = Cite
  { _citeKind :: CitationKind
  , _fields :: [CiteField]
  , _citeID :: UID
  ,  sn :: ShortName
  }
makeLenses ''Citation

instance HasUID       Citation where uid       = citeID
instance HasShortName Citation where shortname = sn
instance HasFields    Citation where getFields = fields
instance Referable    Citation where
  refAdd    c = c ^. citeID -- citeID should be unique.
  renderRef c = Citation $ refAdd c

-- | Smart constructor which implicitly uses EntryID as chunk i.
cite :: CitationKind -> [CiteField] -> String -> Citation
cite x y z = let s = noSpaces z in Cite x y s (shortname' s)

-- | Article citation requires author(s), title, journal, year.
-- Optional fields can be: volume, number, pages, month, and note.
-- Implicitly uses the EntryID as the chunk i.
cArticle :: People -> String -> String -> Int -> [CiteField] -> String -> Citation
cArticle aut t journ yr opt = cite Article 
  ((author aut) : (title t) : (journal journ) : (year yr) : opt)

-- | Book citation requires author or editor, title, publisher, year.
-- Optional fields can be volume or number, series, address, edition, month, and note.
-- Implicitly uses the EntryID as the chunk i.
cBookA, cBookE :: People -> String -> String -> Int ->
  [CiteField] -> String -> Citation
-- | Book citation by author
cBookA aut t pub yr opt = cite Book (author aut : stdFields t pub yr opt)
-- | Book citation by editor
cBookE ed t pub yr opt = cite Book (editor ed : stdFields t pub yr opt)

-- | Booklet citation requires title.
-- Optional fields can be author, how published, address, month, year, note.
-- Implicitly uses the EntryID as the chunk i.
cBooklet :: String -> [CiteField] -> String -> Citation
cBooklet t opt = cite Booklet (title t : opt)

-- | InBook citation requires author or editor, title, chapter and/or pages,
-- publisher, year. Optional fields can be volume or number, series, type,
-- address, edition, month, and note.
-- Implicitly uses the EntryID as the chunk i.
-- This smart constructor includes both chapter and page numbers.
cInBookACP, cInBookECP :: People -> String -> Int -> [Int] ->
  String -> Int -> [CiteField] -> String -> Citation
-- | InBook citation by author.
cInBookACP auth t chap pgs pub yr opt = cite InBook 
  (author auth : chapter chap : pages pgs : stdFields t pub yr opt)
-- | InBook citation by editor.
cInBookECP ed t chap pgs pub yr opt = cite InBook 
  (editor ed : chapter chap : pages pgs : stdFields t pub yr opt)

-- | InBook citation excluding page numbers.
cInBookAC, cInBookEC :: People -> String -> Int ->
  String -> Int -> [CiteField] -> String -> Citation

-- | Otherwise ientical to 'cInBookACP'
cInBookAC auth t chap pub yr opt = cite InBook 
  (author auth : chapter chap : stdFields t pub yr opt)
-- | Otherwise ientical to 'cInBookECP'
cInBookEC ed t chap pub yr opt = cite InBook 
  (editor ed : chapter chap : stdFields t pub yr opt)

-- | InBook citation excluding chapter.
cInBookAP, cInBookEP :: People -> String -> [Int] ->
  String -> Int -> [CiteField] -> String -> Citation

-- | Otherwise ientical to 'cInBookACP'
cInBookAP auth t pgs pub yr opt = cite InBook 
  (author auth : pages pgs : stdFields t pub yr opt)
-- | Otherwise ientical to 'cInBookECP'
cInBookEP ed t pgs pub yr opt = cite InBook 
  (editor ed : pages pgs : stdFields t pub yr opt)

-- | InCollection citation requires author, title, bookTitle, publisher, year.
-- Optional fields can be editor, volume or number, series, type, chapter,
-- pages, address, edition, month, and note.
-- Implicitly uses the EntryID as the chunk i.
cInCollection :: People -> String -> String -> String -> Int ->
  [CiteField] -> String -> Citation
cInCollection auth t bt pub yr opt = cite InCollection 
  (author auth : bookTitle bt : stdFields t pub yr opt)

-- | InProceedings citation requires author, title, bookTitle, year.
-- Optional fields can be editor, volume or number, series, pages,
-- address, month, organization, publisher, and note.
-- Implicitly uses the EntryID as the chunk i.
cInProceedings :: People -> String -> String -> Int ->
  [CiteField] -> String -> Citation
cInProceedings auth t bt yr opt = cite InProceedings 
  (author auth : title t : bookTitle bt : year yr : opt)

-- | Manual (technical documentation) citation requires title.
-- Optional fields can be author, organization, address, edition, month, year, and note.
-- Implicitly uses the EntryID as the chunk i.
cManual :: String -> [CiteField] -> String -> Citation
cManual t opt = cite Manual (title t : opt)

-- | Master's Thesis citation requires author, title, school, and year.
-- Optional fields can be type, address, month, and note.
-- Implicitly uses the EntryID as the chunk i.
cMThesis :: People -> String -> String -> Int -> [CiteField] -> String -> Citation
cMThesis auth t sch yr opt = cite MThesis (thesis auth t sch yr opt)

-- | Misc citation requires nothing.
-- Optional fields can be author, title, howpublished, month, year, and note.
-- Implicitly uses the EntryID as the chunk i.
cMisc :: [CiteField] -> String -> Citation
cMisc = cite Misc

-- | PhD Thesis citation requires author, title, school, and year.
-- Optional fields can be type, address, month, and note.
-- Implicitly uses the EntryID as the chunk i.
cPhDThesis :: People -> String -> String -> Int -> [CiteField] -> String -> Citation
cPhDThesis auth t sch yr opt = cite PhDThesis (thesis auth t sch yr opt)

-- | Proceedings citation requires title and year.
-- Optional fields can be editor, volume or number, series, address,
-- publisher, note, month, and organization.
-- Implicitly uses the EntryID as the chunk i.
cProceedings :: String -> Int -> [CiteField] -> String -> Citation
cProceedings t yr opt = cite Proceedings (title t : year yr : opt)

-- | Technical Report citation requires author, title, institution, and year.
-- Optional fields can be type, number, address, month, and note.
-- Implicitly uses the EntryID as the chunk i.
cTechReport :: People -> String -> String -> Int -> [CiteField] -> String -> Citation
cTechReport auth t inst yr opt = cite TechReport 
  (author auth : title t : institution inst : year yr : opt)

-- | Unpublished citation requires author, title, and note.
-- Optional fields can be month and year.
-- Implicitly uses the EntryID as the chunk i.
cUnpublished :: People -> String -> String -> [CiteField] -> String -> Citation
cUnpublished auth t n opt = cite Unpublished 
  (author auth : title t : note n : opt)

-- Helper function (do not export) for creating book reference.
stdFields :: String -> String -> Int -> [CiteField] -> [CiteField]
stdFields t pub yr opt = title t : publisher pub : year yr : opt

-- Helper function (do not export) for creating thesis reference.
thesis :: People -> String -> String -> Int -> [CiteField] -> [CiteField]
thesis auth t sch yr opt = author auth : title t : school sch : year yr : opt

