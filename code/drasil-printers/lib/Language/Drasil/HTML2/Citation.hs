{-# LANGUAGE OverloadedStrings #-}

module Language.Drasil.HTML2.Citation (
  printBib
) where

import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sortBy)

import Language.Drasil (People, Person, fullName, rendPersLFM, rendPersLFM',
  rendPersLFM'', numList)
import Language.Drasil.Config (StyleGuide(APA, MLA, Chicago), bibStyleH)
import Language.Drasil.Document (CitationKind(..))
import Drasil.Data.Formats.HTML (HTMLBody(..), DItem(..), emphasis,
  Format(..), rawText', id_, class_)

import Language.Drasil.HTML2.Spec (specToHTML, printSpec, colon, period, comma,
  vol, pg, pp, no, ed, editedBy)
import Language.Drasil.Printing.AST (Spec(S))
import Language.Drasil.Printing.Citation (CiteField(..), HP(..), Citation(..), BibRef)
import Language.Drasil.Printing.Helpers (paren, sufxer, sufxPrint)

-- | Makes a bilbliography for the document.
printBib :: BibRef -> HTMLBody
printBib bib =
  DescriptionList [class_ "reference-list"] (concatMap renderCitation bib)
  where
    renderCitation :: Citation -> [DItem]
    renderCitation cite@(Cite e _ _) =
      let (termDoc, detailsDoc) = renderCite cite
          termHTML = ["[", TextFormat Bold [] termDoc, "]"]
       in [DTerm [id_ $ T.pack e] termHTML, DDetails [] detailsDoc]

-- | For when we add other things to reference like website, newspaper
renderCite :: Citation -> ([HTMLBody], [HTMLBody])
renderCite (Cite e Book cfs)      = ([rawText' e], renderF cfs useStyleBk    ++ [rawText' $ sufxPrint cfs])
renderCite (Cite e Article cfs)   = ([rawText' e], renderF cfs useStyleArtcl ++ [rawText' $ sufxPrint cfs])
renderCite (Cite e MThesis cfs)   = ([rawText' e], renderF cfs useStyleBk    ++ [rawText' $ sufxPrint cfs])
renderCite (Cite e PhDThesis cfs) = ([rawText' e], renderF cfs useStyleBk    ++ [rawText' $ sufxPrint cfs])
renderCite (Cite e Misc cfs)      = ([rawText' e], renderF cfs useStyleBk)
renderCite (Cite e _ cfs)         = ([rawText' e], renderF cfs useStyleArtcl)

-- | Generates fields to be used in the document.
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

-- | Generates citation as a book style.
useStyleBk :: StyleGuide -> (CiteField -> [HTMLBody])
useStyleBk MLA     = bookMLA
useStyleBk APA     = bookAPA
useStyleBk Chicago = bookChicago

-- | Generates citation as an article style.
useStyleArtcl :: StyleGuide -> (CiteField -> [HTMLBody])
useStyleArtcl MLA     = artclMLA
useStyleArtcl APA     = artclAPA
useStyleArtcl Chicago = artclChicago

-- | Cite books in MLA format.
bookMLA :: CiteField -> [HTMLBody]
bookMLA (Address   s) = specToHTML s ++ [colon]
bookMLA (Edition   s) = [rawText' (show s ++ sufxer s), ed]
bookMLA (Series    s) = TextFormat Emphasis [] (specToHTML s) : [period]
bookMLA (Title     s) = TextFormat Emphasis [] (specToHTML s) : [period] --If there is a series or collection, this should be in quotes, not italics
bookMLA (Volume    s) = [vol, rawText' (show s), comma]
bookMLA (Publisher s) = specToHTML s ++ [comma]
bookMLA (Author    p) = specToHTML (rendPeople' p) ++ [period]
bookMLA (Year      y) = [rawText' (show y), period]
bookMLA (BookTitle s) = TextFormat Emphasis [] (specToHTML s) : [period]
bookMLA (Journal   s) = TextFormat Emphasis [] (specToHTML s) : [comma]
bookMLA (Pages   [p]) = [pg, rawText' (show p), period]
bookMLA (Pages     p) = [pp, foldPages p, period]
bookMLA (Note      s) = specToHTML s
bookMLA (Number    n) = [no, rawText' (show n), comma]
bookMLA (School    s) = specToHTML s ++ [comma]
bookMLA (HowPublished (Verb s)) = specToHTML s ++ [comma]
bookMLA (HowPublished (URL s)) = [Anchor (printSpec s) [] (specToHTML s), period]
bookMLA (Editor       p) = [editedBy, foldPeople p, comma]
bookMLA (Chapter      _) = []
bookMLA (Institution  i) = specToHTML i ++ [comma]
bookMLA (Organization i) = specToHTML i ++ [comma]
bookMLA (Month        m) = [rawText' (show m), comma]
bookMLA (Type         t) = specToHTML t ++ [comma]

-- | Cite books in APA format.
bookAPA :: CiteField -> [HTMLBody] -- FIXME: year needs to come after author in APA
bookAPA (Author   p) = specToHTML (rendPeople rendPersLFM' p) --L.APA uses initials rather than full name
bookAPA (Year     y) = [rawText' (paren $ show y), period] --APA puts "()" around the year
bookAPA (Pages    p) = [foldPages p, period]
bookAPA (Editor   p) = [foldPeople p, " (Ed.)", period]
bookAPA i = bookMLA i --Most items are rendered the same as MLA

-- | Cite books in Chicago format.
bookChicago :: CiteField -> [HTMLBody]
bookChicago (Author   p) = specToHTML (rendPeople rendPersLFM'' p) -- APA uses middle initials rather than full name
bookChicago (Pages    p) = [foldPages p, period]
bookChicago (Editor   p) = [foldPeople p, RawText (toPlural p " ed"), period]
bookChicago i = bookMLA i --Most items are rendered the same as MLA

-- for article renderings

-- | Cite articles in MLA format.
artclMLA :: CiteField -> [HTMLBody]
artclMLA (Title s) = ["\""] <> specToHTML s <> [".\" "]
artclMLA i         = bookMLA i

-- | Cite articles in APA format.
artclAPA :: CiteField -> [HTMLBody]
artclAPA (Title  s)  = specToHTML s <> [". "]
artclAPA (Volume n)  = [emphasis [] (T.pack (show n))]
artclAPA (Number  n) = [RawText (", (" <> T.pack (show n) <> ") ")]
artclAPA i           = bookAPA i

-- | Cite articles in Chicago format.
artclChicago :: CiteField -> [HTMLBody]
artclChicago i@(Title    _)  = artclMLA i
artclChicago (Volume     n)  = [rawText' (show n), comma]
artclChicago (Number      n) = [RawText ("no. " <> T.pack (show n))]
artclChicago i@(Year     _)  = bookAPA i
artclChicago i = bookChicago i

-- PEOPLE RENDERING --

-- | Generate a list of people (after applying a given function).
rendPeople :: (Person -> String) -> People -> Spec
rendPeople _ []     = S "N.a." -- "No authors given"
rendPeople f people = S . foldlList $ map f people --foldlList is in drasil-utils

-- | Generate a list of people (of form FirstName LastName).
rendPeople' :: People -> Spec
rendPeople' []     = S "N.a." -- "No authors given"
rendPeople' people = S . foldlList $ map rendPersLFM (init people) ++  [rendPersL (last people)]

-- | Organize a list of pages.
foldPages :: [Int] -> HTMLBody
foldPages = rawText' . foldlList . numList "–"

-- | Organize a list of people.
foldPeople :: People -> HTMLBody
foldPeople p = rawText' . foldlList $ map fullName p

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

-- | Generate a person's last name.
rendPersL :: Person -> String
rendPersL =
  (\n -> (if not (null n) && last n == '.' then init else id) n) . rendPersLFM

-- | Adds an 's' if there is more than one person in a list.
toPlural :: People -> Text -> Text
toPlural (_:_) str = str <> "s"
toPlural _     str = str
