{-# LANGUAGE GADTs #-}
module Language.Drasil.Citations where

import Language.Drasil.People
import Language.Drasil.Spec (Sentence(..))

type BibRef = [Citation]
type City   = Sentence
type State  = Sentence

data Citation where --add artical, website
  Book :: [CiteField] -> Citation
  Article :: [CiteField] -> Citation
  
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
               | Date Integer Month Integer
               | Page       Integer
               | Pages    (Integer, Integer)
               | Note       Sentence --extra text at the end of a citation
               | Issue      Integer

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
getAuthors (Book    fields) = getP fields
getAuthors (Article fields) = getP fields

getYear :: Citation -> Integer
getYear (Book    fields) = getY fields
getYear (Article fields) = getY fields

getP :: [CiteField] -> People
getP [] = error "No authors found"
getP ((Author people):_) = people
getP (_:xs) = getP xs

getY :: [CiteField] -> Integer
getY [] = error "No year found"
getY ((Year year):_) = year
getY ((Date _ _ year):_) = year
getY (_:xs) = getY xs