{-# Language GADTs #-}
module Language.Drasil.HTML.AST where

import Language.Drasil.Document (DType (..), MaxWidthPercent)
import Language.Drasil.Citations (Month(..))
import Language.Drasil.People (People)
import Language.Drasil.Printing.AST

-- | Internal HTML version of Document
-- (for converting 'Language.Drasil.Document.Document')
data Document = Document Title Author [LayoutObj]
type Title    = Spec
type Author   = Spec
type Contents = Spec
type Items    = [LayoutObj]
type Tags     = [String]
type Label    = Spec
type Filepath = String
type Caption  = Spec

-- | Internal HTML version of LayoutObj 
-- (for converting 'Language.Drasil.LayoutObj.LayoutObj')
data ALUR = Assumption | LikelyChange | UnlikelyChange | Requirement
data LayoutObj = Table Tags [[Spec]] Label Bool Caption
               | Header Int Contents
               | Paragraph Contents
               | HDiv Tags [LayoutObj] Label
               | Tagless Contents
             --  CodeBlock Code
               | Definition DType [(String,[LayoutObj])] Label
               | List ListType
               | Figure Label Caption Filepath MaxWidthPercent
               | ALUR ALUR Contents Label Label
               | Bib BibRef
               -- Span Tags Contents
               
data ListType = Ordered [ItemType] | Unordered [ItemType]
              | Simple      [(Title,ItemType)]
              | Desc        [(Title,ItemType)]
              | Definitions  [(Title,ItemType)]

data ItemType = Flat Spec | Nested Spec ListType

type BibRef = [Citation]
type City   = Spec
type State  = Spec

data Citation = Book [CiteField] | Article [CiteField]
              | MThesis [CiteField] | PhDThesis [CiteField]
              | Misc [CiteField] | Online [CiteField]

data CiteField = Author     People
               | Title      Spec
               | Series     Spec
               | Collection Spec
               | Volume     Integer
               | Edition    Integer
               | Place    (City, State) --State can also mean country
               | Publisher  Spec
               | Journal    Spec
               | Year       Integer
               | Date Integer Month Integer
               | Page       Integer
               | Pages    (Integer, Integer)
               | Note       Spec
               | Issue      Integer
               | School     Spec
               | Thesis     Thesis
               | URL        Spec
               | HowPub     Spec
               | URLdate Integer Month Integer
               | Editor     People

data Thesis = M | PhD
