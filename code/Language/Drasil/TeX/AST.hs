module Language.Drasil.TeX.AST where

import Language.Drasil.Document (MaxWidthPercent, DType)
import Language.Drasil.Printing.AST

data Document = Document Title Author [LayoutObj]
type Title    = Spec
type Author   = Spec
type Contents = Spec
type Items    = [LayoutObj]
type Depth    = Int
type Width    = Float
type Height   = Float
type Label    = Spec
type Filepath = String
type Caption  = Spec

data LayoutObj = Table [[Spec]] Label Bool Title
               | Section Depth Title [LayoutObj] Label
               | Paragraph Contents
               | EqnBlock Contents
             --  | CodeBlock Code
               | Definition [(String,[LayoutObj])] Label
               | List ListType
               | Figure Label Caption Filepath MaxWidthPercent
               | Requirement Contents Label
               | Assumption Contents Label
               | LikelyChange Contents Label
               | UnlikelyChange Contents Label
               | Graph [(Spec, Spec)] (Maybe Width) (Maybe Height) Caption Label
               | Bib BibRef
               | Defnt DType [(String,[LayoutObj])] Label -- To replace Definition eventually
               
data ListType = Item [ItemType]
              | Enum [ItemType]
              | Simple [(Spec,ItemType)]
              | Desc [(Spec,ItemType)]
              | Definitions [(Spec, ItemType)]

data ItemType = Flat Spec
              | Nested Spec ListType

type BibRef = [Citation]
type City   = Spec
type State  = Spec

data Citation = Book [CiteField] | Article [CiteField]
              | MThesis [CiteField] | PhDThesis [CiteField]
              | Misc [CiteField] | Online [CiteField]


