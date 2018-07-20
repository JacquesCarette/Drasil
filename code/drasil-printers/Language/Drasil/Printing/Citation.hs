module Language.Drasil.Printing.Citation where

import Language.Drasil hiding (CiteField, HP, Citation)

import Language.Drasil.Printing.AST (Spec)

--import Language.Drasil.People (People)
--import Language.Drasil.Chunk.Citation (Month, EntryID, CitationKind)

type BibRef = [Citation]

data Citation = Cite EntryID CitationKind [CiteField]

-- | Fields used in citations.
data CiteField = Address      Spec
               | Author       People
               | BookTitle    Spec -- Used for 'InCollection' references only.
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
               | Pages        [Int] -- Range of pages (ex1. 1-32; ex2. 7,31,52-55)
               | Publisher    Spec
               | School       Spec
               | Series       Spec
               | Title        Spec
               | Type         Spec -- BibTeX "type" field
               | Volume       Int
               | Year         Int

-- | How Published. Necessary for URLs to work properly.
data HP = URL Spec
        | Verb Spec
