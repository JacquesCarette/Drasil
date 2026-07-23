{-# LANGUAGE OverloadedStrings #-}

module Language.Drasil.HTML2.Citation (
  printBib, htmlBibFormatter
) where

import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sortBy)

import Language.Drasil.HTML2.Helpers (
  HTMLGenOptions (..), BibFormatter (..), BibFormatter(..),
  colon, period, comma, vol, pg, pp, no, ed, editedBy)
import Language.Drasil.HTML2.Spec (specToHTML, printSpec)

import Language.Drasil (People, Person, fullName, rendPersLFM, rendPersLFM',
  rendPersLFM'', CitationKind(..), numList)
import Language.Drasil.Config (StyleGuide(APA, MLA, Chicago), bibStyleH)

import Language.Drasil.Printing.AST (Spec(S))
import Language.Drasil.Printing.Citation (CiteField(Year, Number, Volume, Title, Author,
  Editor, Pages, Type, Month, Organization, Institution, Chapter, HowPublished, School, Note,
  Journal, BookTitle, Publisher, Series, Address, Edition), HP(URL, Verb),
  Citation(Cite), BibRef)
import Language.Drasil.Printing.Helpers (paren, sufxer, sufxPrint)

import Drasil.Data.Formats.HTML (HTMLBody(..), DItem(..), Attr(..), emphasis, Format(Emphasis))
import qualified Drasil.Data.Formats.HTML as HTML (Format(..))

-- | HTML specific bib rendering functions
htmlBibFormatter :: BibFormatter
htmlBibFormatter =
  BibFormatter {
    emph = \x -> [TextFormat Emphasis [] x],
    spec = specToHTML
  }

-- | Makes a bilbliography for the document.
printBib :: HTMLGenOptions -> BibRef -> HTMLBody
printBib rOpts bib =
  DescriptionList [Attr "class" "reference-list"] (concatMap renderCitation bib)
  where
    renderCitation :: Citation -> [DItem]
    renderCitation cite@(Cite e _ _) =
      let (termDoc, detailsDoc) = renderCite (bibFmt rOpts) cite
          termHTML = [RawText "[", TextFormat HTML.Bold [] termDoc, RawText "]"]
       in [DTerm [Attr "id" (T.pack e)] termHTML, DDetails [] detailsDoc]

-- | For when we add other things to reference like website, newspaper
renderCite :: BibFormatter -> Citation -> ([HTMLBody], [HTMLBody])
renderCite f (Cite e Book cfs)      = ([RawText $ T.pack e],
  renderF cfs (useStyleBk    f)  ++ [RawText (T.pack $ sufxPrint cfs)])
renderCite f (Cite e Article cfs)   = ([RawText $ T.pack e],
  renderF cfs (useStyleArtcl f)  ++ [RawText (T.pack $ sufxPrint cfs)])
renderCite f (Cite e MThesis cfs)   = ([RawText $ T.pack e],
  renderF cfs (useStyleBk    f)  ++ [RawText (T.pack $ sufxPrint cfs)])
renderCite f (Cite e PhDThesis cfs) = ([RawText $ T.pack e],
  renderF cfs (useStyleBk    f)  ++ [RawText (T.pack $ sufxPrint cfs)])
renderCite f (Cite e Misc cfs)      = ([RawText $ T.pack e],
  renderF cfs (useStyleBk    f))
renderCite f (Cite e _ cfs)         = ([RawText $ T.pack e],
  renderF cfs (useStyleArtcl f)) --FIXME: Properly render these later.

-- | Render fields to be used in the document.
renderF :: [CiteField] -> (StyleGuide -> (CiteField -> [HTMLBody])) -> [HTMLBody]
renderF fields styl = concatMap (styl bibStyleH) (sortBy compCiteField fields)

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
useStyleBk :: BibFormatter -> StyleGuide -> (CiteField -> [HTMLBody])
useStyleBk f MLA     = bookMLA f
useStyleBk f APA     = bookAPA f
useStyleBk f Chicago = bookChicago f

-- | Renders citation as an article style.
useStyleArtcl :: BibFormatter -> StyleGuide -> (CiteField -> [HTMLBody])
useStyleArtcl f MLA     = artclMLA f
useStyleArtcl f APA     = artclAPA f
useStyleArtcl f Chicago = artclChicago f

-- | Cite books in MLA format.
bookMLA :: BibFormatter -> CiteField -> [HTMLBody]
bookMLA f (Address   s) = spec f s ++ [colon]
bookMLA _ (Edition   s) = [RawText (T.pack (show s ++ sufxer s)), ed]
bookMLA f (Series    s) = emph f (spec f s) ++ [period]
bookMLA f (Title     s) = emph f (spec f s) ++ [period] --If there is a series or collection, this should be in quotes, not italics
bookMLA _ (Volume    s) = [vol, RawText (T.pack (show s)), comma]
bookMLA f (Publisher s) = spec f s ++ [comma]
bookMLA f (Author    p) = spec f (rendPeople' p) ++ [period]
bookMLA _ (Year      y) = [RawText (T.pack (show y)), period]
bookMLA f (BookTitle s) = emph f (spec f s) ++ [period]
bookMLA f (Journal   s) = emph f (spec f s) ++ [comma]
bookMLA _ (Pages   [p]) = [pg, RawText (T.pack (show p)), period]
bookMLA _ (Pages     p) = [pp, foldPages p, period]
bookMLA f (Note      s) = spec f s
bookMLA _ (Number    n) = [no, RawText (T.pack (show n)), comma]
bookMLA f (School    s) = spec f s ++ [comma]
bookMLA f (HowPublished (Verb s)) = spec f s ++ [comma]
bookMLA f (HowPublished (URL s)) = [Anchor (printSpec s) [] (spec f s), period]
bookMLA _ (Editor       p) = [editedBy, foldPeople p, comma]
bookMLA _ (Chapter      _) = []
bookMLA f (Institution  i) = spec f i ++ [comma]
bookMLA f (Organization i) = spec f i ++ [comma]
bookMLA _ (Month        m) = [RawText (T.pack (show m)), comma]
bookMLA f (Type         t) = spec f t ++ [comma]

-- | Cite books in APA format.
bookAPA :: BibFormatter -> CiteField -> [HTMLBody] --FIXME: year needs to come after author in APA
bookAPA f (Author   p) = spec f (rendPeople rendPersLFM' p) --L.APA uses initals rather than full name
bookAPA _ (Year     y) = [RawText (T.pack (paren $ show y)), period] --APA puts "()" around the year
bookAPA _ (Pages    p) = [foldPages p, period]
bookAPA _ (Editor   p) = [foldPeople p, RawText " (Ed.)", period]
bookAPA f i = bookMLA f i --Most items are rendered the same as MLA

-- | Cite books in Chicago format.
bookChicago :: BibFormatter -> CiteField -> [HTMLBody]
bookChicago f (Author   p) = spec f (rendPeople rendPersLFM'' p) -- APA uses middle initals rather than full name
bookChicago _ (Pages    p) = [foldPages p, period]
bookChicago _ (Editor   p) = [foldPeople p, RawText (toPlural p " ed"), period]
bookChicago f i = bookMLA f i --Most items are rendered the same as MLA

-- for article renderings

-- | Cite articles in MLA format.
artclMLA :: BibFormatter -> CiteField -> [HTMLBody]
artclMLA f (Title s) = [RawText "\""] <> spec f s <> [RawText ".\" "]
artclMLA f i         = bookMLA f i

-- | Cite articles in APA format.
artclAPA :: BibFormatter -> CiteField -> [HTMLBody]
artclAPA f (Title  s)  = spec f s <> [RawText ". "]
artclAPA _ (Volume n)  = [emphasis [] (T.pack (show n))]
artclAPA _ (Number  n) = [RawText (", (" <> T.pack (show n) <> ") ")]
artclAPA f i           = bookAPA f i

-- | Cite articles in Chicago format.
artclChicago :: BibFormatter -> CiteField -> [HTMLBody]
artclChicago f i@(Title    _)  = artclMLA f i
artclChicago _ (Volume     n)  = [RawText (T.pack (show n)), comma]
artclChicago _ (Number      n) = [RawText ("no. " <> T.pack (show n))]
artclChicago f i@(Year     _)  = bookAPA f i
artclChicago f i = bookChicago f i

-- PEOPLE RENDERING --

-- | Render a list of people (after applying a given function).
rendPeople :: (Person -> String) -> People -> Spec
rendPeople _ []     = S "N.a." -- "No authors given"
rendPeople f people = S . foldlList $ map f people --foldlList is in drasil-utils

-- | Render a list of people (of form FirstName LastName).
rendPeople' :: People -> Spec
rendPeople' []     = S "N.a." -- "No authors given"
rendPeople' people = S . foldlList $ map rendPersLFM (init people) ++  [rendPersL (last people)]

-- | Organize a list of pages.
foldPages :: [Int] -> HTMLBody
foldPages = RawText . T.pack . foldlList . numList "–"

-- | Organize a list of people.
foldPeople :: People -> HTMLBody
foldPeople p = RawText . foldlList $ map (T.pack . fullName) p

-- | Organize a list of Strings, separated by commas and inserting "and" before the last item.
foldlList :: (IsString a, Semigroup a) => [a] -> a
foldlList []    = ""
foldlList [a,b] = a <> " and " <> b
foldlList lst   = foldle1 (\a b -> a <> ", " <> b) (\a b -> a <> ", and " <> b) lst

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
toPlural :: People -> Text -> Text
toPlural (_:_) str = str <> "s"
toPlural _     str = str
