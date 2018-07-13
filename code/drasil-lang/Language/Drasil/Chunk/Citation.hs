{-# Language TemplateHaskell, TypeFamilies #-}
module Language.Drasil.Chunk.Citation
  ( -- Types
    Citation, BibRef, CiteField(..), Month(..), HP(..), CitationKind(..), EntryID
    -- Class for Reference.hs
  , HasFields(getFields)
    -- Accessors
  , citeID, externRefT
    -- CiteFields smart constructors
      -- People -> CiteField
  , author, editor
      -- Sentence -> CiteField
  , address, bookTitle, howPublished, howPublishedU, institution, journal, note
  , organization, publisher, school, series, title, typeField
      -- Int -> CiteField
  , chapter, edition, number, volume, year
      -- [Int] -> CiteField
  , pages
      -- Month -> CiteField
  , month
    -- Citation smart constructors
  , cArticle, cBookA, cBookE, cBooklet
  , cInBookACP, cInBookECP, cInBookAC, cInBookEC, cInBookAP, cInBookEP
  , cInCollection, cInProceedings, cManual, cMThesis, cMisc, cPhDThesis
  , cProceedings, cTechReport, cUnpublished
  ) where

import Language.Drasil.People (People)
import Language.Drasil.Spec (Sentence)
import Language.Drasil.UID (UID)

import Control.Lens (makeLenses)

import Language.Drasil.Classes (HasUID(uid), HasLabel(getLabel))
import Language.Drasil.Misc (noSpaces)
import Language.Drasil.Chunk.ShortName (HasShortName(shortname), shortname')
import Language.Drasil.Label.Core (Label)

import Control.Lens ((^.), Lens', makeLenses)
import Data.List (find)

type BibRef = [Citation]
type EntryID = String -- Should contain no spaces

-- | Fields used in citations.
data CiteField = Address      Sentence
               | Author       People
               | BookTitle    Sentence -- Used for 'InCollection' references only.
               | Chapter      Int
               | Edition      Int
               | Editor       People
               | HowPublished HP
               | Institution  Sentence
               | Journal      Sentence
               | Month        Month
               | Note         Sentence
               | Number       Int
               | Organization Sentence
               | Pages        [Int] -- Range of pages (ex1. 1-32; ex2. 7,31,52-55)
               | Publisher    Sentence
               | School       Sentence
               | Series       Sentence
               | Title        Sentence
               | Type         Sentence -- BibTeX "type" field
               | Volume       Int
               | Year         Int

-- | How Published. Necessary for URLs to work properly.
data HP = URL Sentence
        | Verb Sentence

-- | Month must be of this format.
data Month = Jan
           | Feb
           | Mar
           | Apr
           | May
           | Jun
           | Jul
           | Aug
           | Sep
           | Oct
           | Nov
           | Dec deriving (Eq, Ord)

instance Show Month where
  show Jan = "January"
  show Feb = "February"
  show Mar = "March"
  show Apr = "April"
  show May = "May"
  show Jun = "June"
  show Jul = "July"
  show Aug = "August"
  show Sep = "September"
  show Oct = "October"
  show Nov = "November"
  show Dec = "December"

-- | External references come in many flavours. Articles, Books, etc.
-- (we are using the types available in Bibtex)
data CitationKind = Article
                  | Book
                  | Booklet
                  | InBook
                  | InCollection
                  | InProceedings
                  | Manual
                  | MThesis
                  | Misc
                  | PhDThesis
                  | Proceedings
                  | TechReport
                  | Unpublished

-- | All citations require a unique identifier (String) used by the Drasil chunk.
-- We will also have an EntryID (String) used for creating reference links.
-- Finally we will have the reference information (type and fields).
data Citation = Cite
  { _id :: UID
  , citeID :: EntryID
  , externRefT :: CitationKind
  , fields :: [CiteField]
  , _lb :: Label
  }
makeLenses ''Citation

-- | Smart constructor which implicitly uses EntryID as chunk i.
cite :: EntryID -> CitationKind -> [CiteField] -> Label -> Citation
cite i = Cite i (noSpaces i)

-- | Citations are chunks.
instance HasUID Citation where uid f (Cite a b c d l) = fmap (\x -> Cite x b c d l) (f a)
instance HasLabel Citation where getLabel = lb
instance HasShortName  Citation where shortname = lb . shortname
instance HasFields    Citation where getFields = fields
=======

class HasFields c where
  getFields :: Lens' c [CiteField]

-- | Article citation requires author(s), title, journal, year.
-- Optional fields can be: volume, number, pages, month, and note.
-- Implicitly uses the EntryID as the chunk i.
cArticle :: String -> People -> Sentence -> Sentence -> Int -> [CiteField] -> Label -> Citation
cArticle i aut t journ yr opt lbe = cite i Article 
  ((author aut) : (title t) : (journal journ) : (year yr) : opt) lbe

-- | Book citation requires author or editor, title, publisher, year.
-- Optional fields can be volume or number, series, address, edition, month, and note.
-- Implicitly uses the EntryID as the chunk i.
cBookA, cBookE :: String -> People -> Sentence -> Sentence -> Int ->
  [CiteField] -> Label -> Citation
-- | Book citation by author
cBookA i aut t pub yr opt lbe = cite i Book (author aut : stdFields t pub yr opt) lbe
-- | Book citation by editor
cBookE i ed t pub yr opt lbe = cite i Book (editor ed : stdFields t pub yr opt) lbe

-- | Booklet citation requires title.
-- Optional fields can be author, how published, address, month, year, note.
-- Implicitly uses the EntryID as the chunk i.
cBooklet :: String -> Sentence -> [CiteField] -> Label -> Citation
cBooklet i t opt lbe = cite i Booklet (title t : opt) lbe

-- | InBook citation requires author or editor, title, chapter and/or pages,
-- publisher, year. Optional fields can be volume or number, series, type,
-- address, edition, month, and note.
-- Implicitly uses the EntryID as the chunk i.
-- This smart constructor includes both chapter and page numbers.
cInBookACP, cInBookECP :: String -> People -> Sentence -> Int -> [Int] ->
  Sentence -> Int -> [CiteField] -> Label -> Citation
-- | InBook citation by author.
cInBookACP i auth t chap pgs pub yr opt lbe = cite i InBook 
  (author auth : chapter chap : pages pgs : stdFields t pub yr opt) lbe
-- | InBook citation by editor.
cInBookECP i ed t chap pgs pub yr opt lbe = cite i InBook 
  (editor ed : chapter chap : pages pgs : stdFields t pub yr opt) lbe

-- | InBook citation excluding page numbers.
cInBookAC, cInBookEC :: String -> People -> Sentence -> Int ->
  Sentence -> Int -> [CiteField] -> Label -> Citation

-- | Otherwise ientical to 'cInBookACP'
cInBookAC i auth t chap pub yr opt lbe = cite i InBook 
  (author auth : chapter chap : stdFields t pub yr opt) lbe
-- | Otherwise ientical to 'cInBookECP'
cInBookEC i ed t chap pub yr opt lbe = cite i InBook 
  (editor ed : chapter chap : stdFields t pub yr opt) lbe

-- | InBook citation excluding chapter.
cInBookAP, cInBookEP :: String -> People -> Sentence -> [Int] ->
  Sentence -> Int -> [CiteField] -> Label -> Citation

-- | Otherwise ientical to 'cInBookACP'
cInBookAP i auth t pgs pub yr opt lbe = cite i InBook 
  (author auth : pages pgs : stdFields t pub yr opt) lbe
-- | Otherwise ientical to 'cInBookECP'
cInBookEP i ed t pgs pub yr opt lbe = cite i InBook 
  (editor ed : pages pgs : stdFields t pub yr opt) lbe

-- | InCollection citation requires author, title, bookTitle, publisher, year.
-- Optional fields can be editor, volume or number, series, type, chapter,
-- pages, address, edition, month, and note.
-- Implicitly uses the EntryID as the chunk i.
cInCollection :: String -> People -> Sentence -> Sentence -> Sentence -> Int ->
  [CiteField] -> Label -> Citation
cInCollection i auth t bt pub yr opt lbe = cite i InCollection 
  (author auth : bookTitle bt : stdFields t pub yr opt) lbe

-- | InProceedings citation requires author, title, bookTitle, year.
-- Optional fields can be editor, volume or number, series, pages,
-- address, month, organization, publisher, and note.
-- Implicitly uses the EntryID as the chunk i.
cInProceedings :: String -> People -> Sentence -> Sentence -> Int ->
  [CiteField] -> Label -> Citation
cInProceedings i auth t bt yr opt lbe = cite i InProceedings 
  (author auth : title t : bookTitle bt : year yr : opt) lbe

-- | Manual (technical documentation) citation requires title.
-- Optional fields can be author, organization, address, edition, month, year, and note.
-- Implicitly uses the EntryID as the chunk i.
cManual :: String -> Sentence -> [CiteField] -> Label -> Citation
cManual i t opt lbe = cite i Manual (title t : opt) lbe

-- | Master's Thesis citation requires author, title, school, and year.
-- Optional fields can be type, address, month, and note.
-- Implicitly uses the EntryID as the chunk i.
cMThesis :: String -> People -> Sentence -> Sentence -> Int -> [CiteField] -> Label -> Citation
cMThesis i auth t sch yr opt lbe = cite i MThesis (thesis auth t sch yr opt) lbe

-- | Misc citation requires nothing.
-- Optional fields can be author, title, howpublished, month, year, and note.
-- Implicitly uses the EntryID as the chunk i.
cMisc :: String -> [CiteField] -> Label -> Citation
cMisc i opt lbe = cite i Misc opt lbe

-- | PhD Thesis citation requires author, title, school, and year.
-- Optional fields can be type, address, month, and note.
-- Implicitly uses the EntryID as the chunk i.
cPhDThesis :: String -> People -> Sentence -> Sentence -> Int -> [CiteField] -> Label -> Citation
cPhDThesis i auth t sch yr opt lbe = cite i PhDThesis (thesis auth t sch yr opt) lbe

-- | Proceedings citation requires title and year.
-- Optional fields can be editor, volume or number, series, address,
-- publisher, note, month, and organization.
-- Implicitly uses the EntryID as the chunk i.
cProceedings :: String -> Sentence -> Int -> [CiteField] -> Label -> Citation
cProceedings i t yr opt lbe = cite i Proceedings (title t : year yr : opt) lbe

-- | Technical Report citation requires author, title, institution, and year.
-- Optional fields can be type, number, address, month, and note.
-- Implicitly uses the EntryID as the chunk i.
cTechReport :: String -> People -> Sentence -> Sentence -> Int -> [CiteField] -> Label -> Citation
cTechReport i auth t inst yr opt lbe = cite i TechReport 
  (author auth : title t : institution inst : year yr : opt) lbe

-- | Unpublished citation requires author, title, and note.
-- Optional fields can be month and year.
-- Implicitly uses the EntryID as the chunk i.
cUnpublished :: String -> People -> Sentence -> Sentence -> [CiteField] -> Label -> Citation
cUnpublished i auth t n opt lbe = cite i Unpublished 
  (author auth : title t : note n : opt) lbe

-- Helper function (do not export) for creating book reference.
stdFields :: Sentence -> Sentence -> Int -> [CiteField] -> [CiteField]
stdFields t pub yr opt = title t : publisher pub : year yr : opt

-- Helper function (do not export) for creating thesis reference.
thesis :: People -> Sentence -> Sentence -> Int -> [CiteField] -> [CiteField]
thesis auth t sch yr opt = author auth : title t : school sch : year yr : opt

-- | Smart field constructor
author, editor :: People -> CiteField
author = Author
editor = Editor

-- | Smart field constructor
address, bookTitle, howPublished, howPublishedU, institution, journal, note,
  organization, publisher, school, series, title,
  typeField :: Sentence -> CiteField

address       = Address
bookTitle     = BookTitle
howPublished  = HowPublished . Verb
howPublishedU = HowPublished . URL
institution   = Institution
journal       = Journal
note          = Note
organization  = Organization
publisher     = Publisher
school        = School
series        = Series
title         = Title
typeField     = Type

-- | Smart field constructor
chapter, edition, number, volume, year :: Int -> CiteField

chapter = Chapter
edition = Edition
number = Number
volume = Volume
year = Year

-- | Smart field constructor
pages :: [Int] -> CiteField
pages = Pages

-- | Smart field constructor
month :: Month -> CiteField
month = Month
