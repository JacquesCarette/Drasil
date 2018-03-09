module Language.Drasil.TeX.AST where

import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Unicode (Greek,Special)
import Language.Drasil.Spec (USymb, RefType)
import Language.Drasil.Chunk.Citation (Month(..), CitationKind, EntryID)
import Language.Drasil.People (People)
import Language.Drasil.Document (MaxWidthPercent, DType)
import Language.Drasil.Printing.AST

infixr 5 :+:
data Spec = E Expr
          | S String
          | Spec :+: Spec -- concat
          | Spec :^: Spec -- superscript
          | Spec :-: Spec -- subscript
          | Spec :/: Spec -- frac
          | Sy USymb
          | N Symbol
          | G Greek
          | Sp Special
          | Ref RefType Spec Spec
          | EmptyS
          | HARDNL        -- newline. Temp fix for multi-line descriptions;
                          -- May move to a new LayoutObj, but only exists in TeX
                          -- so it's not really a big deal ATM.

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