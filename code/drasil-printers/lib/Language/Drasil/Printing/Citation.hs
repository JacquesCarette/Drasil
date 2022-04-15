-- | Defines types used for printing citations.
module Language.Drasil.Printing.Citation where

import Language.Drasil hiding (CiteField, HP, Citation)

import Language.Drasil.Printing.AST (Spec)

-- | A collection of citations.
type BibRef = [Citation]

-- | A citation contains an entry id, the kind of citation, and the appropriate citation fields.
data Citation = Cite EntryID CitationKind [CiteField]

-- | Fields used in citations. More suitable to printing
data CiteField = Address      Spec
               | Author       People
               | BookTitle    Spec -- ^ Used for 'InCollection' references only.
               | Chapter      Int
               | Edition      Int
               | Editor       People
               | HowPublished HP
               | Institution  Spec
               | Journal      Spec
               | Month        Month
               | Note         Spec
               | Number       Int
               | Organization Spec
               | Pages        [Int] -- ^ Range of pages (ex1. 1-32; ex2. 7,31,52-55)
               | Publisher    Spec
               | School       Spec
               | Series       Spec
               | Title        Spec
               | Type         Spec -- ^ BibTeX "type" field
               | Volume       Int
               | Year         Int

-- | How something is published. Necessary for URLs to work properly.
data HP = URL Spec
        | Verb Spec
