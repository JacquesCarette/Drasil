-- | Defines types used for printing citations.
module Drasil.Document.Layout.Citation (
  -- * Types
  Citation(..), CiteField(..), HP(..), CitationKind(..), Month(..), Person(..),
  -- * Type Aliases
  BibRef, EntryID, People
) where

import Drasil.Document.Layout.Spec (Spec)

-- | A collection of citations.
type BibRef = [Citation]

-- | A 'String' that should contain no spaces.
type EntryID = String

-- | A citation contains an entry id, the kind of citation, and the appropriate citation fields.
data Citation = Cite EntryID CitationKind [CiteField]

-- | Fields used in citations. More suitable to printing
data CiteField = Address      Spec
               | Author       People
               | BookTitle    Spec -- ^ Used for 'InCollection' references only.
               | Chapter      Int
               | Edition      Int
               | Editor       People
               | HowPublished HP
               | Institution  Spec
               | Journal      Spec
               | Month        Month
               | Note         Spec
               | Number       Int
               | Organization Spec
               | Pages        [Int] -- ^ Range of pages (ex1. 1-32; ex2. 7,31,52-55)
               | Publisher    Spec
               | School       Spec
               | Series       Spec
               | Title        Spec
               | Type         Spec -- ^ BibTeX "type" field
               | Volume       Int
               | Year         Int

-- | How something is published. Necessary for URLs to work properly.
data HP = URL Spec
        | Verb Spec

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

-- | Months of the year.
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

-- | A person, by the forms of their name that citations use.
data Person = Person {
  -- | Their full name, with a period after each initial.
    fullName      :: String
  -- | Their name in the form: Last, First Middle.
  , rendPersLFM   :: String
  -- | Their name in the form: Last, F. M.
  , rendPersLFM'  :: String
  -- | Their name in the form: Last, First M.
  , rendPersLFM'' :: String
  }

-- | People is a synonym for many 'Person's.
type People = [Person]
