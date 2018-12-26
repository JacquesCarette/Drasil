module Language.Drasil.Data.Citation 
  ( -- Types
    CiteField(..), HP(..), CitationKind(..)
    -- "smart" constructors
  , author, editor
      -- Sentence -> CiteField
  , address, bookTitle, howPublished, howPublishedU, institution, journal, note
  , organization, publisher, school, series, title, typeField
      -- FIXME: these should be checked for bounds
      -- Int -> CiteField
  , chapter, edition, number, volume, year
      -- [Int] -> CiteField
  , pages
      -- Month -> CiteField
  , month

  ) where

import Language.Drasil.People (People)
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.Data.Date (Month(..))

-- | Fields used in citations.
data CiteField = Address      String
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
-- | Smart field constructor
author, editor :: People -> CiteField
author = Author
editor = Editor

-- | Smart field constructor
address :: String -> CiteField

bookTitle, howPublished, howPublishedU, institution, journal, note,
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
