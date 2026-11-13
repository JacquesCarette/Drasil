module Language.Drasil.Printing.Import.Citation (layCite) where

import Control.Lens ((^.))

import Drasil.Database (showUID)
import qualified Language.Drasil as L (Month(..), Person, fullName,
  rendPersLFM, rendPersLFM', rendPersLFM'')
import Language.Drasil.Document (Citation, CiteField(..), HP(..), citeKind, HasFields(..))
import qualified Language.Drasil.Document as L (CitationKind(..))

import qualified Drasil.Document.Layout.Development as P

-- | For importing a bibliography.
layCite :: Citation -> P.Citation
layCite c = P.Cite (showUID c) (layKind $ c ^. citeKind) (layField <$> (c ^. getFields))

-- | Helper for translating 'Citefield's into a printable representation of 'P.CiteField's
layField :: CiteField -> P.CiteField
layField (Address      s)        = P.Address      $ P.S s
layField (Author       p)        = P.Author       $ layPerson <$> p
layField (BookTitle    b)        = P.BookTitle    $ P.S b
layField (Chapter      c)        = P.Chapter      c
layField (Edition      e)        = P.Edition      e
layField (Editor       e)        = P.Editor       $ layPerson <$> e
layField (Institution  i)        = P.Institution  $ P.S i
layField (Journal      j)        = P.Journal      $ P.S j
layField (Month        m)        = P.Month        $ layMonth m
layField (Note         n)        = P.Note         $ P.S n
layField (Number       n)        = P.Number       n
layField (Organization o)        = P.Organization $ P.S o
layField (Pages        p)        = P.Pages        p
layField (Publisher    p)        = P.Publisher    $ P.S p
layField (School       s)        = P.School       $ P.S s
layField (Series       s)        = P.Series       $ P.S s
layField (Title        t)        = P.Title        $ P.S t
layField (Type         t)        = P.Type         $ P.S t
layField (Volume       v)        = P.Volume       v
layField (Year         y)        = P.Year         y
layField (HowPublished (URL  u)) = P.HowPublished (P.URL  $ P.S u)
layField (HowPublished (Verb v)) = P.HowPublished (P.Verb $ P.S v)

-- | Helper for translating a 'L.CitationKind' into the layout language's.
layKind :: L.CitationKind -> P.CitationKind
layKind L.Article       = P.Article
layKind L.Book          = P.Book
layKind L.Booklet       = P.Booklet
layKind L.InBook        = P.InBook
layKind L.InCollection  = P.InCollection
layKind L.InProceedings = P.InProceedings
layKind L.Manual        = P.Manual
layKind L.MThesis       = P.MThesis
layKind L.Misc          = P.Misc
layKind L.PhDThesis     = P.PhDThesis
layKind L.Proceedings   = P.Proceedings
layKind L.TechReport    = P.TechReport
layKind L.Unpublished   = P.Unpublished

-- | Helper for translating a 'L.Month' into the layout language's.
layMonth :: L.Month -> P.Month
layMonth L.Jan = P.Jan
layMonth L.Feb = P.Feb
layMonth L.Mar = P.Mar
layMonth L.Apr = P.Apr
layMonth L.May = P.May
layMonth L.Jun = P.Jun
layMonth L.Jul = P.Jul
layMonth L.Aug = P.Aug
layMonth L.Sep = P.Sep
layMonth L.Oct = P.Oct
layMonth L.Nov = P.Nov
layMonth L.Dec = P.Dec

-- | Helper for translating a 'L.Person' into the layout language's, which
-- holds their name in each form that citations use.
layPerson :: L.Person -> P.Person
layPerson p = P.Person {
    P.fullName      = L.fullName p
  , P.rendPersLFM   = L.rendPersLFM p
  , P.rendPersLFM'  = L.rendPersLFM' p
  , P.rendPersLFM'' = L.rendPersLFM'' p
  }
