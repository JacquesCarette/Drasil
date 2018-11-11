module Language.Drasil.Data.Citation (CiteField(..), HP(..), CitationKind(..)) where

import Language.Drasil.People (People)
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.Data.Date (Month(..))

-- | Fields used in citations.
data CiteField = Address      Sentence
               | Author       People
               | BookTitle    Sentence -- Used for 'InCollection' references only.
               | Chapter      Int
               | Edition      Int
               | Editor       People
               | HowPublished HP
               | Institution  Sentence
               | Journal      Sentence
               | Month        Month
               | Note         Sentence
               | Number       Int
               | Organization Sentence
               | Pages        [Int] -- Range of pages (ex1. 1-32; ex2. 7,31,52-55)
               | Publisher    Sentence
               | School       Sentence
               | Series       Sentence
               | Title        Sentence
               | Type         Sentence -- BibTeX "type" field
               | Volume       Int
               | Year         Int

-- | How Published. Necessary for URLs to work properly.
data HP = URL Sentence
        | Verb Sentence

-- | External references come in many flavours. Articles, Books, etc.
-- (we are using the types available in Bibtex)
data CitationKind = Article
                  | Book
                  | Booklet
                  | InBook
                  | InCollection
                  | InProceedings
                  | Manual
                  | MThesis
                  | Misc
                  | PhDThesis
                  | Proceedings
                  | TechReport
                  | Unpublished
