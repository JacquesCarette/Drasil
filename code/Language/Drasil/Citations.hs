module Language.Drasil.Citations where

import Language.Drasil.People
import Language.Drasil.Spec (Sentence(..))

type BibRef = [Citation]
type City   = Sentence
type State  = Sentence

data Citation =
    Book      [CiteField]
  | Article   [CiteField]
  | MThesis   [CiteField]
  | PhDThesis [CiteField]
  | Misc      [CiteField]
  | Online    [CiteField]

--FIXME: use a 3-tuple for dates?
data CiteField = Author     People
               | Title      Sentence
               | Series     Sentence
               | Collection Sentence
               | Volume     Integer
               | Edition    Integer
               | Place    (City, State) --State can also mean country
               | Publisher  Sentence
               | Journal    Sentence
               | Year       Integer
               | Date Integer Month Integer --date of published
               | Page       Integer
               | Pages    (Integer, Integer)
               | Note       Sentence --extra text at the end of a citation
               | Issue      Integer
               | School     Sentence
               | URL        Sentence
               | HowPub     Sentence --how it was published, when using Misc
               | URLdate Integer Month Integer --date accessed/viewed
               | Editor     People

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
  show Jan = "Janurary"
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

{-
{-EXAMPLE-}
ref1 :: Citation
ref1 = Book [
  Author [person "John" "Smith"],
  Title $ S "This is a Title",
  Place (S "Toronto", S "Canada"),
  Date 28 July 2017,
  Publisher $ S "McMaster",
  Volume 3] 
-}

-------------
-- Helpers --
-------------
getAuthors :: Citation -> People
getAuthors (Book      fields) = getP fields
getAuthors (Article   fields) = getP fields
getAuthors (MThesis   fields) = getP fields
getAuthors (PhDThesis fields) = getP fields
getAuthors (Misc      fields) = getP fields
getAuthors (Online    fields) = getP fields

getYear :: Citation -> Integer
getYear (Book      fields) = getY fields
getYear (Article   fields) = getY fields
getYear (MThesis   fields) = getY fields
getYear (PhDThesis fields) = getY fields
getYear (Misc      fields) = getY fields
getYear (Online    fields) = getY fields

getP :: [CiteField] -> People
getP [] = error "No authors found"
getP ((Author people):_) = people
getP (_:xs) = getP xs

getY :: [CiteField] -> Integer
getY [] = error "No year found"
getY ((Year year):_) = year
getY ((Date _ _ year):_) = year
getY (_:xs) = getY xs
