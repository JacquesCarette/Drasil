{-# Language GADTs #-}
module Language.Drasil.HTML.AST where

import Language.Drasil.Document (DType (..), MaxWidthPercent)
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

data Citation = Book [CiteField] | Article [CiteField]
              | MThesis [CiteField] | PhDThesis [CiteField]
              | Misc [CiteField] | Online [CiteField]
