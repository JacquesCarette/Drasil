module Language.Drasil.HTML.Print(
  -- * Citation Renderer
  renderCite,
  -- * HTML Bib Formatter
  htmlBibFormatter,
  -- * HTML Spec Printing
  pSpec,
  -- * Term Fencing Helpers
  OpenClose(Open, Close),
  fence) where

import Prelude hiding ((<>))
import Data.List (sortBy)
import Text.PrettyPrint hiding (Str)

import Language.Drasil (People, Person, fullName, rendPersLFM, rendPersLFM',
  rendPersLFM'', special, checkValidStr, numList)
import Language.Drasil.Document (CitationKind(..))

import Language.Drasil.HTML.Monad (unPH)
import Language.Drasil.HTML.Helpers (em, reflinkInfo, reflinkURI, BibFormatter(..))

import Language.Drasil.Config (StyleGuide(APA, MLA, Chicago), bibStyleH)
import Language.Drasil.Printing.AST (Fence(Curly, Paren, Abs, Norm),
  Spec(Quote, EmptyS, Ref, Sp, S, (:+:)), LinkType(External))
import Language.Drasil.Printing.Citation (CiteField(Year, Number, Volume, Title, Author,
  Editor, Pages, Type, Month, Organization, Institution, Chapter, HowPublished, School, Note,
  Journal, BookTitle, Publisher, Series, Address, Edition), HP(URL, Verb),
  Citation(Cite))
import Language.Drasil.Printing.Helpers (comm, dot, paren, sufxer, sufxPrint)

-- | Referring to 'fence' (for parenthesis and brackeds). Either opened or closed.
data OpenClose = Open | Close

-----------------------------------------------------------------
--------------------BEGIN SPEC PRINTING--------------------------
-----------------------------------------------------------------

-- | Renders the Sentences ('Spec's) in the HTML body.
pSpec :: Spec -> Doc
pSpec (a :+: b) = pSpec a <> pSpec b
pSpec (S s)     = either error (text . concatMap escapeChars) $ checkValidStr s invalid
  where
    invalid = ['<', '>']
    escapeChars '&' = "\\&"
    escapeChars c = [c]
pSpec (Sp s)             = text $ unPH $ special s
pSpec (Ref External r a) = reflinkURI r $ pSpec a
pSpec EmptyS             = text "" -- Expected in the output
pSpec (Quote q)          = doubleQuotes $ pSpec q



-- | Allows for open/closed variants of parenthesis, curly brackets, absolute value symbols, and normal symbols.
fence :: OpenClose -> Fence -> String
fence Open  Paren = "("
fence Close Paren = ")"
fence Open  Curly = "{"
fence Close Curly = "}"
fence _     Abs   = "|"
fence _     Norm  = "||"

---------------------
--HTML bibliography--
---------------------

-- | HTML specific bib rendering functions
htmlBibFormatter :: BibFormatter
htmlBibFormatter = BibFormatter {
  emph = em,
  spec = pSpec
}

-- | For when we add other things to reference like website, newspaper
renderCite :: BibFormatter -> Citation -> (Doc, Doc)
renderCite f (Cite e Book cfs)      = (text e, renderF cfs (useStyleBk    f)  <> text (sufxPrint cfs))
renderCite f (Cite e Article cfs)   = (text e, renderF cfs (useStyleArtcl f)  <> text (sufxPrint cfs))
renderCite f (Cite e MThesis cfs)   = (text e, renderF cfs (useStyleBk    f)  <> text (sufxPrint cfs))
renderCite f (Cite e PhDThesis cfs) = (text e, renderF cfs (useStyleBk    f)  <> text (sufxPrint cfs))
renderCite f (Cite e Misc cfs)      = (text e, renderF cfs (useStyleBk    f))
renderCite f (Cite e _ cfs)         = (text e, renderF cfs (useStyleArtcl f)) --FIXME: Properly render these later.

-- | Render fields to be used in the document.
renderF :: [CiteField] -> (StyleGuide -> (CiteField -> Doc)) -> Doc
renderF fields styl = hsep $ map (styl bibStyleH) (sortBy compCiteField fields)

-- | Compares two cite fields.
compCiteField :: CiteField -> CiteField -> Ordering
compCiteField (Institution _) _ = LT
compCiteField _ (Institution _) = GT
compCiteField (Organization _) _ = LT
compCiteField _ (Organization _) = GT
compCiteField (Author     _) _ = LT
compCiteField _ (Author     _) = GT
compCiteField (Title      _) _ = LT
compCiteField _ (Title      _) = GT
compCiteField (Series     _) _ = LT
compCiteField _ (Series     _) = GT
compCiteField (BookTitle _) _  = LT
compCiteField _ (BookTitle _)  = GT
compCiteField (Editor     _) _ = LT
compCiteField _ (Editor     _) = GT
compCiteField (Journal    _) _ = LT
compCiteField _ (Journal    _) = GT
compCiteField (Volume     _) _ = LT
compCiteField _ (Volume     _) = GT
compCiteField (Number     _) _ = LT
compCiteField _ (Number     _) = GT
compCiteField (Edition    _) _ = LT
compCiteField _ (Edition    _) = GT
compCiteField (HowPublished (Verb _)) _ = LT
compCiteField _ (HowPublished (Verb _)) = GT
compCiteField (School     _) _ = LT
compCiteField _ (School     _) = GT
compCiteField (Address      _) _ = LT
compCiteField _ (Address      _) = GT
compCiteField (Publisher  _) _ = LT
compCiteField _ (Publisher  _) = GT
compCiteField (Month      _) _ = LT
compCiteField _ (Month      _) = GT
compCiteField (Year       _) _ = LT
compCiteField _ (Year       _) = GT
compCiteField (HowPublished (URL _)) _ = LT
compCiteField _ (HowPublished (URL _)) = GT
compCiteField (Chapter    _) _ = LT
compCiteField _ (Chapter    _) = GT
compCiteField (Pages      _) _ = LT
compCiteField _ (Pages      _) = GT
compCiteField (Note       _) _ = LT
compCiteField _ (Note       _) = GT
compCiteField (Type       _) _ = LT

-- Config helpers --
-- | Renders citation as a book style.
useStyleBk :: BibFormatter -> StyleGuide -> (CiteField -> Doc)
useStyleBk f MLA     = bookMLA f
useStyleBk f APA     = bookAPA f
useStyleBk f Chicago = bookChicago f

-- | Renders citation as an article style.
useStyleArtcl :: BibFormatter -> StyleGuide -> (CiteField -> Doc)
useStyleArtcl f MLA     = artclMLA f
useStyleArtcl f APA     = artclAPA f
useStyleArtcl f Chicago = artclChicago f

-- FIXME: move these show functions and use tags, combinators
-- | Cite books in MLA format.
bookMLA :: BibFormatter -> CiteField -> Doc
bookMLA f (Address   s) = spec f s <> text ":"
bookMLA _ (Edition   s) = comm $ text $ show s ++ sufxer s ++ " ed."
bookMLA f (Series    s) = dot $ emph f $ spec f s
bookMLA f (Title     s) = dot $ emph f $ spec f s --If there is a series or collection, this should be in quotes, not italics
bookMLA _ (Volume    s) = comm $ text $ "vol. " ++ show s
bookMLA f (Publisher s) = comm $ spec f s
bookMLA f (Author    p) = dot $ spec f (rendPeople' p)
bookMLA _ (Year      y) = dot $ text $ show y
--bookMLA _ (Date    d m y) = dot $ unwords [show d, show m, show y]
--bookMLA f (URLdate d m y) = "Web. " ++ bookMLA f (Date d m y) sm
bookMLA f (BookTitle s) = dot $ emph f $ spec f s
bookMLA f (Journal   s) = comm $ emph f $ spec f s
bookMLA _ (Pages   [p]) = dot $ text $ "pg. " ++ show p
bookMLA _ (Pages     p) = dot $ text "pp. " <> foldPages p
bookMLA f (Note      s) = spec f s
bookMLA _ (Number    n) = comm $ text ("no. " ++ show n)
bookMLA f (School    s) = comm $ spec f s
--bookMLA _ (Thesis     t)  = comm $ show t
--bookMLA f (URL        s)  = dot $ spec f s
bookMLA f (HowPublished (Verb s))      = comm $ spec f s
bookMLA f (HowPublished (URL l@(S s))) = dot  $ spec f $ Ref External s l
bookMLA f (HowPublished (URL s))       = dot  $ spec f s
bookMLA _ (Editor       p) = comm $ text "Edited by " <> foldPeople p
bookMLA _ (Chapter      _) = text ""
bookMLA f (Institution  i) = comm $ spec f i
bookMLA f (Organization i) = comm $ spec f i
bookMLA _ (Month        m) = comm $ text $ show m
bookMLA f (Type         t) = comm $ spec f t

-- | Cite books in APA format.
bookAPA :: BibFormatter -> CiteField -> Doc --FIXME: year needs to come after author in APA
bookAPA f (Author   p) = spec f (rendPeople rendPersLFM' p) --L.APA uses initals rather than full name
bookAPA _ (Year     y) = dot $ text $ paren $ show y --APA puts "()" around the year
--bookAPA _ (Date _ _ y) = bookAPA (Year y) --LAPA doesn't care about the day or month
--bookAPA _ (URLdate d m y) = "Retrieved, " ++ (comm $ unwords [show d, show m, show y])
bookAPA _ (Pages    p) = dot $ foldPages p
bookAPA _ (Editor   p) = dot $ foldPeople p <> text " (Ed.)"
bookAPA f i = bookMLA f i --Most items are rendered the same as MLA

-- | Cite books in Chicago format.
bookChicago :: BibFormatter -> CiteField -> Doc
bookChicago f (Author   p) = spec f (rendPeople rendPersLFM'' p) -- APA uses middle initals rather than full name
bookChicago _ (Pages    p) = dot $ foldPages p
bookChicago _ (Editor   p) = dot $ foldPeople p <> text (toPlural p " ed")
bookChicago f i = bookMLA f i --Most items are rendered the same as MLA

-- for article renderings
-- | Cite articles in MLA format.
artclMLA :: BibFormatter -> CiteField -> Doc
artclMLA f (Title s) = doubleQuotes $ dot $ spec f s
artclMLA f i         = bookMLA f i

-- | Cite articles in APA format.
artclAPA :: BibFormatter -> CiteField -> Doc
artclAPA f (Title  s)  = dot $ spec f s
artclAPA _ (Volume n)  = em $ text $ show n
artclAPA _ (Number  n) = comm $ text $ paren $ show n
artclAPA f i           = bookAPA f i

-- | Cite articles in Chicago format.
artclChicago :: BibFormatter -> CiteField -> Doc
artclChicago f i@(Title    _) = artclMLA f i
artclChicago _ (Volume     n) = comm $ text $ show n
artclChicago _ (Number      n) = text $ "no. " ++ show n
artclChicago f i@(Year     _) = bookAPA f i
--artclChicago f i@(Date _ _ _) = bookAPA f i
artclChicago f i = bookChicago f i

-- PEOPLE RENDERING --
-- | Render a list of people (after applying a given function).
rendPeople :: (Person -> String) -> People -> Spec
rendPeople _ []  = S "N.a." -- "No authors given"
rendPeople f people = S . foldlList $ map f people --foldlList is in drasil-utils

-- | Render a list of people (of form FirstName LastName).
rendPeople' :: People -> Spec
rendPeople' []  = S "N.a." -- "No authors given"
rendPeople' people = S . foldlList $ map rendPersLFM (init people) ++  [rendPersL (last people)]

-- | Organize a list of pages.
foldPages :: [Int] -> Doc
foldPages = text . foldlList . numList "&ndash;"

-- | Organize a list of people.
foldPeople :: People -> Doc
foldPeople p = text . foldlList $ map fullName p

-- | Organize a list of Strings, separated by commas and inserting "and" before the last item.
foldlList :: [String] -> String
foldlList []    = ""
foldlList [a,b] = a ++ " and " ++ b
foldlList lst   = foldle1 (\a b -> a ++ ", " ++ b) (\a b -> a ++ ", and " ++ b) lst

-- | Similar to foldl, but applies a function to two arguments at a time.
foldle1 :: (a -> a -> a) -> (a -> a -> a) -> [a] -> a
foldle1 _ _ []       = error "foldle1 cannot be used with empty list"
foldle1 _ _ [x]      = x
foldle1 _ g [x,y]    = g x y
foldle1 f g (x:y:xs) = foldle1 f g (f x y : xs)

-- | Renders a person's last name.
rendPersL :: Person -> String
rendPersL =
  (\n -> (if not (null n) && last n == '.' then init else id) n) . rendPersLFM

-- | adds an 's' if there is more than one person in a list.
toPlural :: People -> String -> String
toPlural (_:_) str = str ++ "s"
toPlural _     str = str
