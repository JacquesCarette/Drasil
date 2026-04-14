module Language.Drasil.Printing.Import.Citation where

import Control.Lens ((^.))

import Drasil.Database (showUID)
import Language.Drasil (Citation, CiteField(..), HP (..), citeKind, HasFields (..))

import qualified Language.Drasil.Printing.AST as P
import qualified Language.Drasil.Printing.Citation as P

-- | For importing a bibliography.
layCite :: Citation -> P.Citation
layCite c = P.Cite (showUID c) (c ^. citeKind) (map layField (c ^. getFields))

-- | Helper for translating 'Citefield's into a printable representation of 'P.CiteField's
layField :: CiteField -> P.CiteField
layField (Address      s)        = P.Address      $ P.S s
layField (Author       p)        = P.Author       p
layField (BookTitle    b)        = P.BookTitle    $ P.S b
layField (Chapter      c)        = P.Chapter      c
layField (Edition      e)        = P.Edition      e
layField (Editor       e)        = P.Editor       e
layField (Institution  i)        = P.Institution  $ P.S i
layField (Journal      j)        = P.Journal      $ P.S j
layField (Month        m)        = P.Month        m
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
