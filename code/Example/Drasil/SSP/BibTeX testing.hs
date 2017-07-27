type BibRef = [Citation]
type City   = Sentence
type State  = Sentence

data Citation where
  Book :: [CiteFieldB] -> Citation
  
data CiteFieldB = Place    (City, State)
               | Edition    Sentence
               | Series     Sentence
               | Title      Sentence
               | Volume     Sentence
               | Publisher  Sentence
               | Author     People
               | Year       Integer
               | Collection Sentence

-----------------------------
-- Rendering Unique to TeX --
-----------------------------
instance Show CiteField where
  show Place (city, state) = showField "place" (city :+: ", " :+: state)
  show Edition    s = showField "edition" s
  show Series     s = showField "series" s
  show Title      s = showField "title" s
  show Volume     s = showField "volume" s
  show Publisher  s = showField "publisher" s
  show Author     p = showField "author" (rendPeople p)
  show Year       n = showField "year" (S $ show n)
  show Collection s = showField "collection" s

showField :: String -> Sentence -> String
showField f s = f ++ "={" ++ rend s ++ "}"
  where rend :: Sentence -> String
        rend = p_spec . spec

rendPeople :: People -> Sentence
rendPeople []  = error "No authors given"
rendPeople people = foldl1 (\x y -> S x :+: S " and " :+: S y) $ rendPerson people

rendPerson :: Person -> String
rendPerson (Person _ n _ Mono) = n
rendPerson (Person f l ms _) = l ++ ", " ++ (concat . intersperse " ") (f:ms)

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
        getY (_:xs) = getP xs

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