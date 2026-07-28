{-# Language TemplateHaskell #-}
-- | Contains all necessary types and constructors for citing sources in Drasil.
module Language.Drasil.Data.Citation (
  -- * Types
  CiteField(..), HP(..), CitationKind(..),
  -- * Class
  HasFields(..),
  -- * 'CiteField' Constructors
  -- ** 'People' -> 'CiteField'
  author, editor,
  -- ** 'String' -> 'CiteField'
  address, bookTitle, howPublished, howPublishedU, institution, journal, note,
  organization, publisher, school, series, title, typeField,
  -- FIXME: these should be checked for bounds
  -- ** 'Int' -> 'CiteField'
  chapter, edition, number, volume, year,
  -- ** ['Int'] -> 'CiteField'
  pages,
  -- ** 'Month' -> 'CiteField'
  month,
  -- * Comparators
  compareAuthYearTitle
) where

import Control.Lens (Lens', (^.))

import Drasil.Database (declareHasChunkRefs, Generically(..))

import Language.Drasil.People (People, comparePeople)
import Language.Drasil.Data.Date (Month(..))

-- | Fields used in citations.
data CiteField = Address      String
               | Author       People
               | BookTitle    String -- ^ Used for 'InCollection' references only.
               | Chapter      Int
               | Edition      Int
               | Editor       People
               | HowPublished HP     -- ^ Can be published via URL or something else.
               | Institution  String
               | Journal      String
               | Month        Month
               | Note         String
               | Number       Int
               | Organization String
               | Pages        [Int] -- ^ Range of pages (ex1. 1-32; ex2. 7,31,52-55).
               | Publisher    String
               | School       String
               | Series       String
               | Title        String
               | Type         String -- ^ BibTeX "type" field.
               | Volume       Int
               | Year         Int

-- | 'Citation's should have a fields ('CiteField').
class HasFields c where
  -- | Provides a 'Lens' to 'CiteField's.
  getFields :: Lens' c [CiteField]

-- | How something is published. Necessary for URLs to work properly.
data HP = URL String
        | Verb String

-- | External references come in many flavours. Articles, Books, etc.
-- (we are using the types available in Bibtex).
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

declareHasChunkRefs ''HP
declareHasChunkRefs ''CiteField
declareHasChunkRefs ''CitationKind

-- | Smart field constructor for a 'CiteField'.
author, editor :: People -> CiteField
author = Author
editor = Editor

-- | Smart field constructor for a 'CiteField'.
address, bookTitle, institution, journal,
  howPublished, howPublishedU, note, organization, publisher, school, series, title,
  typeField :: String -> CiteField

address       = Address
bookTitle     = BookTitle
howPublished  = HowPublished . Verb
-- | URL version of 'howPublished'.
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

-- | Smart field constructor for a 'CiteField'.
chapter, edition, number, volume, year :: Int -> CiteField

chapter = Chapter
edition = Edition
number = Number
volume = Volume
year = Year

-- | Smart field constructor for a 'CiteField'.
pages :: [Int] -> CiteField
pages = Pages

-- | Smart field constructor for a 'CiteField'.
month :: Month -> CiteField
month = Month

-- | Orders two authors. If given two of the exact same authors, year, and
-- title, returns an error.
compareAuthYearTitle :: HasFields c => c -> c -> Ordering
compareAuthYearTitle c1 c2
  | cp /= EQ  = cp
  | y1 /= y2  = y1 `compare` y2
  | otherwise = t1 `compare` t2
  where
    (a1, y1, t1) = getAuthorYearTitle c1
    (a2, y2, t2) = getAuthorYearTitle c2

    cp = comparePeople a1 a2

-- | Search for the Author, Year, and Title of a Citation-like data type, and
-- error out if it doesn't have them.
getAuthorYearTitle :: HasFields c => c -> (People, Int, String)
getAuthorYearTitle c = (a, y, t)
  where
    fs :: [CiteField]
    fs = c ^. getFields

    findFirst :: (CiteField -> Maybe a) -> String -> [CiteField] -> a
    findFirst _     err  [] = error err
    findFirst xtract err (x : xs) =
       case xtract x of
         Just z    -> z
         Nothing -> findFirst xtract err xs

    justAuthor (Author x) = Just x
    justAuthor _          = Nothing

    a = findFirst justAuthor "No author found" fs

    justYear (Year x) = Just x
    justYear _        = Nothing

    y = findFirst justYear "No year found" fs

    justTitle (Title x) = Just x
    justTitle _         = Nothing

    t = findFirst justTitle "No title found" fs
