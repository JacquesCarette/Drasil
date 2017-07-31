type BibRef = [Citation]
type City   = Sentence
type State  = Sentence
-- type JournalName = Sentence
-- type Vol    = Integer
-- type Issue  = Integer

data Citation where --add artical, website
  Book :: [CiteField] -> Citation
  
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
               | Date Integer Month Integer deriving (Eq, Ord)
               --ordering is different for different styles.

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

data StyleGuide = MLA | APA | Chicago

useStyleHTML :: StyleGuide -> (CiteField -> String)
useStyleHTML MLA = showMLA
useStyleHTML APA = showAPA
useStyleHTML Chicago = showChicago

useStyleTeX :: StyleGuide -> String
useStyleTeX MLA = "ieeetr"
useStyleTeX APA = "apalike"
useStyleTeX Chicago = "plain"

styleSetting :: StyleGuide
styleSetting = MLA --This will be an input for the user eventually

-------------
-- Helpers --
-------------
getAuthors :: Citation -> People
getAuthors (Book fields) = getP fields
  where getP [] = error "No authors found"
        getP ((Author people):xs) = people
        getP (_:xs) = getP xs

getYear :: Citation -> Integer
getYear (Book fields) = getY fields
  where getY [] = error "No year found"
        getY ((Year year):xs) = year
        getY ((Date _ _ year):xs) = year
        getY (_:xs) = getP xs

-- Used only on single digit Int
sufx :: Integer -> String
sufx 1 = "st."
sufx 2 = "nd."
sufx 3 = "rd."
sufx _ = "th."

-- Use on any sized Int
sufxer :: Integer -> String
sufxer = sufx . read . last . show

-- LFM is Last, First Middle
rendPersLFM :: Person -> String
rendPersLFM (Person _ n _ Mono) = n
rendPersLFM (Person f l ms _) = l ++ ", " ++ unwords (f:ms)

-- LFM' is Last, F. M.
rendPersLFM' :: Person -> String
rendPersLFM' (Person _ n _ Mono) = n
rendPersLFM' (Person f l ms _) = l ++ ", " ++ (unwords . map initial) (f:ms)

-- LFM'' is Last, First M.
rendPersLFM'' :: Person -> String
rendPersLFM'' (Person _ n _ Mono) = n
rendPersLFM'' (Person f l ms _) = l ++ ", " ++ unwords (f:(map initial ms))

initial :: String -> String
initial = (\xs -> head xs ++ ".")

-----------------------------
-- Rendering Unique to TeX --
-----------------------------
showBibTeX :: CiteField -> String
showBibTeX (Place (city, state)) = showField "place" (city :+: S ", " :+: state)
showBibTeX (Edition    s) = showField "edition" (S $ show s :+: sufxer s)
showBibTeX (Series     s) = showField "series" s
showBibTeX (Title      s) = showField "title" s
showBibTeX (Volume     s) = showField "volume" (S $ show s)
showBibTeX (Publisher  s) = showField "publisher" s
showBibTeX (Author     p) = showField "author" (rendPeople p)
showBibTeX (Year       y) = showField "year" (S $ show y)
showBibTeX (Date   d m y) = showField "year" (S $ unwords [show d, show m, show y])
showBibTeX (Collection s) = showField "collection" s
showBibTeX (Journal    s) = showField "journal" s

showField :: String -> Sentence -> String
showField f s = f ++ "={" ++ rend s ++ "}"
  where rend :: Sentence -> String
        rend = p_spec . spec

rendPeople :: People -> Sentence
rendPeople []  = error "No authors given"
rendPeople people = foldl1 (\x y -> S x :+: S " and " :+: S y) $ map rendPersLFM people

--------------------
--Tex bibliography--
--------------------
bibLines :: String -> String
bibLines fname = foldl1 (++"\\n"++) $ [
  "\\newline",
  "\\bibliography{" ++ fname ++ "}",
  "\\bibstyle{" ++ useStyleTeX sg ++ "}"]

mkBibRef :: BibRef -> String
mkBibRef = foldl1 (++"\\n\\n"++) . sort . map renderCite

--for when we add other things to reference like website, newspaper, articals
renderCite :: Citation -> String
renderCite b@(Book _) = renderBook b

--Rendering a book--
renderBook :: Citation -> String
renderBook b@(Book fields) = "@book{" ++ cite b ++ ",\\n" ++
  (concat . intersperse ",\\n" . map showBibTeX) fields ++ "}"
renderBook _ = error "Tried to render a non-book using renderBook." 

cite :: Citation -> String
cite book = concat $ intersperse "_" $
  map lstName (getAuthors b) ++ [show $ getYear book]
  where lstName (Person _ l _ _) = l

{-
It may be best to use https://github.com/backtracking/bibtex2html or
nxg.me.uk/dist/bibhtml/ or www.spinellis.gr/sw/textproc/bib2xhtml/ 
which reads .bib files and converts them to an .html file. This will ensure
consistency and maintainability.
-}
------------------------------
-- Rendering Unique to HTML --
------------------------------
{-these apply to books, rendering is slightly different for articals, web, ...-}
showMLA :: CiteField -> String
showMLA (Place (city, state)) = rend (city :+: S ", " :+: state) ++ ":"
showMLA (Edition    s) = rend (S $ show s :+: sufxer s) ++ " ed.,"
showMLA (Series     s) = "<em>" ++ rend s ++ "</em>."
showMLA (Title      s) = "<em>" ++ rend s ++ "</em>." --If there is a series or collection, this should be in quotes, not italics
showMLA (Volume     s) = "vol." ++ show s ++ ","
showMLA (Publisher  s) = rend s ++ ","
showMLA (Author     p) = rend (rendPeople rendPersLFM p) ++ "."
showMLA (Year       y) = rend (S $ show y) ++ "."
showMLA (Date   d m y) = rend (S $ unwords [show d, show m, show y]) ++ "."
showMLA (Collection s) = "<em>" ++ rend s ++ "</em>."
showMLA (Journal    s) = "<em>" ++ rend s ++ "</em>.,"

showAPA :: CiteField -> String
showAPA (Author   p) = rend (rendPeople rendPersLFM' p) ++ "." --APA uses initals rather than full name
showAPA (Year     y) = "(" ++ rend (S $ show y) ++ ")." --APA puts "()" around the year
showAPA (Date _ _ y) = showAPA (Year y) --APA doesn't care about the day or month
showAPA i = showMLA i --Most items are rendered the same as MLA

showChicago :: CiteField -> String
showChicago (Author   p) = rend (rendPeople rendPersLFM'' p) ++ "." --APA uses middle initals rather than full name
showChicago (Date _ _ y) = showChicago (Year y) --APA doesn't care about the day or month
showChicago i = showMLA i --Most items are rendered the same as MLA

rend :: Sentence -> String
rend = p_spec . spec

rendPeople :: (Person -> String) -> People -> Sentence
rendPeople f []  = error "No authors given"
rendPeople f people = foldlList $ map f people --name is found in People.hs, foldlList is in SentenceStructures.hs

foldlList :: [Sentence] -> Sentence
foldlList []    = EmptyS
foldlList [a,b] = a :+: S " and " :+: b
foldlList lst   = foldle1 (\a b -> a :+: S ", " :+: b) (\a b -> a :+: S ", and " :+: b) lst

foldle1 :: (a -> a -> a) -> (a -> a -> a) -> [a] -> a
foldle1 _ _ []       = error "foldle1 cannot be used with empty list"
foldle1 _ _ [x]      = x
foldle1 _ g [x,y]    = g x y
foldle1 f g (x:y:xs) = foldle1 f g ((f x y):xs)

---------------------
--HTML bibliography--
---------------------
mkBibRef :: BibRef -> Contents
mkBibRef = listRef . sort . map renderCite
  where listRef = Enumeration . Simple . zip [S $ "[" ++ show x ++ "]:" | x <- [1..]] . map (Flat . S)
  --some function to get a numbered list, idealy it wouldn't go from string to sentence
  
--for when we add other things to reference like website, newspaper, articals
renderCite :: Citation -> String
renderCite b@(Book _) = renderBook b

--Rendering a book--
renderBook :: Citation -> String
renderBook c@(Book fields) = unwords $
  map (useStyleHTML styleSetting) (sort fields) ++ endingField c styleSetting
renderBook _ = error "Tried to render a non-book using renderBook."

endingField :: Citation -> StyleGuide -> [String]
endingField (Book _) MLA = ["Print."]
endingField _ _ = []

{-example of use-}
ref1 :: Citation
ref1 = Book [
  Author [person "John" "Smith"],
  Title $ S "This is a Title",
  Place (S "Toronto", S "Canada"),
  Date 28 July 2017,
  Publisher $ S "McMaster",
  Volume 3]

{-

  {-if we want to change BibTeX to more database style-}
  _place      :: (City, State),
  _edition    :: Sentence,
  _series     :: Sentence,
  _title      :: Sentence,
  _volume     :: Sentence,
  _publisher  :: Sentence,
  _author     :: People,
  _year       :: Integer,
  _collection :: Sentence
  
  {-example of a BibTeX citation-}
  @book{last_0ADAD,
  place={city, state},
  edition={ed},
  series={series},
  title={title},
  volume={vol},
  publisher={pub},
  author={last, first middle},
  year={0ADAD},
  collection={series}}
  
  {-if there are multiple authors-}
  l1_l2_l3_1ADAD
  author={l1, f1 mi1 and l2, f2 m2 and l3, f3 m3}
  
  @book{Smith_1993,
  author    = {John Smith}, 
  title     = {The title of the work},
  publisher = {The name of the publisher},
  year      = 1993,
  volume    = 4,
  series    = 10,
  address   = {The address},
  edition   = 3,
  month     = 7,
  note      = {An optional note},
  isbn      = {3257227892}
  }
  
-}