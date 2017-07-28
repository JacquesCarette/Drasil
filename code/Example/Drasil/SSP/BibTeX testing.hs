type BibRef = [Citation]
type City   = Sentence
type State  = Sentence

data Citation where
  Book :: [CiteFieldB] -> Citation
  
data CiteFieldB =
               | Author     People
               | Title      Sentence
               | Series     Sentence
               | Collection Sentence
               | Volume     Sentence
               | Edition    Sentence
               | Publisher  Sentence
               | Journal    Sentence
               | Year       Integer
               | Date Integer Month Integer
               | Place    (City, State) deriving (Ord)--State can also mean country

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

-----------------------------
-- Rendering Unique to TeX --
-----------------------------
instance Show CiteField where
  show Place (city, state) = showField "place" (city :+: S ", " :+: state)
  show Edition    s = showField "edition" s
  show Series     s = showField "series" s
  show Title      s = showField "title" s
  show Volume     s = showField "volume" s
  show Publisher  s = showField "publisher" s
  show Author     p = showField "author" (rendPeople p)
  show Year       y = showField "year" (S $ show y)
  show Date   d m y = showField "year" (S $ unwords [show d, show m, show y])
  show Collection s = showField "collection" s
  show Journal    s = showField "journal" s

showField :: String -> Sentence -> String
showField f s = f ++ "={" ++ rend s ++ "}"
  where rend :: Sentence -> String
        rend = p_spec . spec

rendPeople :: People -> Sentence
rendPeople []  = error "No authors given"
rendPeople people = foldl1 (\x y -> S x :+: S " and " :+: S y) $ map rendPerson people

rendPerson :: Person -> String
rendPerson (Person _ n _ Mono) = n
rendPerson (Person f l ms _) = l ++ ", " ++ unwords (f:ms)

--------------------
--Tex bibliography--
--------------------
mkBibRef :: BibRef -> String
mkBibRef = foldl1 (++"\\n\\n"++) . map renderCite

--for when we add other things to reference like website, newspaper, articals
renderCite :: Citation -> String
renderCite b@(Book _) = renderBook b

--Rendering a book--
renderBook :: Citation -> String
renderBook b@(Book fields) = "@book{" ++ cite b ++ ",\\n" ++
  (concat . intersperse ",\\n" . map show) fields ++ "}"  

cite :: Citation -> String
cite book = concat $ intersperse "_" $
  map lstName (getAuthors b) ++ [show $ getYear book]
  where lstName (Person _ l _ _) = l

{-below is unfinished and mostly still TeX-}
------------------------------
-- Rendering Unique to HTML --
------------------------------
instance Show CiteField where
  show Place (city, state) = rend (city :+: S ", " :+: state) ++ ":"
  show Edition    s = rend s ++ " ed.,"
  show Series     s = "<em>" ++ rend s ++ "</em>."
  show Title      s = "<em>" ++ rend s ++ "</em>." --If there is a series or collection, this should be in quotes, not italics
  show Volume     s = "vol." ++ rend s ","
  show Publisher  s = rend s ++ ","
  show Author     p = rend rendPeople ++ "."
  show Year       y = rend (S $ show y) ++ "."
  show Date   d m y = rend (S $ unwords [show d, show m, show y]) ++ "."
  show Collection s = "<em>" ++ rend s ++ "</em>."
  show Journal    s = "<em>" ++ rend s ++ "</em>.,"

rend :: Sentence -> String
rend = p_spec . spec

rendPeople :: People -> Sentence
rendPeople []  = error "No authors given"
rendPeople people = foldlList $ map name people --name is found in People.hs, foldlList is in SentenceStructures.hs

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
mkBibRef :: BibRef -> String
mkBibRef = listRef . map renderCite
  where listRef = --some function to get a numbered list

--for when we add other things to reference like website, newspaper, articals
renderCite :: Citation -> String
renderCite b@(Book _) = renderBook b

--Rendering a book--
renderBook :: Citation -> String
renderBook (Book fields) = unwords $ map show (sort fields) ++ ["Print."]


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
  -}