{-# Language GADTs #-}
module Language.Drasil.HTML.AST where

import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Spec (USymb, RefAdd)
import Language.Drasil.Unicode (Greek, Special)
import Language.Drasil.Document (DType (..), MaxWidthPercent)
import Language.Drasil.Chunk.Citation (Month(..), CitationKind(..), EntryID)
import Language.Drasil.People (People)
import Language.Drasil.Printing.AST (Expr(..))

-- | Internal HTML version of Sentence
-- (for converting 'Language.Drasil.Spec.Sentence')
infixr 5 :+:
data Spec where
  E :: Expr -> Spec
  S :: String -> Spec
  (:+:) :: Spec -> Spec -> Spec -- concat
  (:^:) :: Spec -> Spec -> Spec -- superscript
  (:-:) :: Spec -> Spec -> Spec -- subscript
  (:/:) :: Spec -> Spec -> Spec -- frac
  Sy :: USymb -> Spec
  N :: Symbol -> Spec
  G :: Greek -> Spec
  Sp :: Special -> Spec
  HARDNL :: Spec
  Ref :: RefAdd -> Spec -> Spec
  EmptyS :: Spec

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

instance Show ListType where
  show (Ordered _)   = "o"
  show (Unordered _) = "u"
  show (Desc _)      = error "Printing descriptive list failed"
  show (Simple _)    = error "Printing Simple list failed, see ASTHTML/PrintHTML"
  show (Definitions _)  = error "Printing list of definitions failed"

type BibRef = [Citation]
type City   = Spec
type State  = Spec

data Citation = Cite EntryID CitationKind [CiteField]

-- | Fields used in citations.
data CiteField = Address      Spec
               | Author       People
               | BookTitle    Spec -- Used for 'InBookTitle' references only.
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

instance Eq CiteField where
  (Address      _) == (Address      _) = True --
  (Author       _) == (Author       _) = True --
  (BookTitle    _) == (BookTitle    _) = True --
  (Chapter      _) == (Chapter      _) = True --
  (Edition      _) == (Edition      _) = True --
  (Editor       _) == (Editor       _) = True --
  (HowPublished _) == (HowPublished _) = True --
  (Institution  _) == (Institution  _) = True --
  (Journal      _) == (Journal      _) = True --
  (Month        _) == (Month        _) = True --
  (Note         _) == (Note         _) = True --
  (Number       _) == (Number       _) = True --
  (Organization _) == (Organization _) = True --
  (Pages        _) == (Pages        _) = True --
  (Publisher    _) == (Publisher    _) = True --
  (School       _) == (School       _) = True --
  (Series       _) == (Series       _) = True --
  (Title        _) == (Title        _) = True --
  (Type         _) == (Type         _) = True --
  (Volume       _) == (Volume       _) = True --
  (Year         _) == (Year         _) = True --
  _                == _                = False

instance Ord CiteField where
  compare (Institution _) _ = LT
  compare _ (Institution _) = GT
  compare (Organization _) _ = LT
  compare _ (Organization _) = GT
  compare (Author _) _ = LT
  compare _ (Author _) = GT
  compare (Title _) _ = LT
  compare _ (Title _) = GT
  compare (Series _) _ = LT
  compare _ (Series _) = GT
  compare (BookTitle _) _ = LT
  compare _ (BookTitle _) = GT
  compare (Editor _) _ = LT
  compare _ (Editor _) = GT
  compare (Journal _) _ = LT
  compare _ (Journal _) = GT
  compare (Volume _) _ = LT
  compare _ (Volume _) = GT
  compare (Edition _) _ = LT
  compare _ (Edition _) = GT
  compare (School _) _ = LT
  compare _ (School _) = GT
  compare (Address _) _ = LT
  compare _ (Address _) = GT
  compare (Publisher _) _ = LT
  compare _ (Publisher _) = GT
  compare (HowPublished (Verb _)) _ = LT
  compare _ (HowPublished (Verb _)) = GT
  compare (Number _) _ = LT
  compare _ (Number _) = GT
  compare (Month _) _ = LT
  compare _ (Month _) = GT
  compare (Year _) _ = LT
  compare _ (Year _) = GT
  compare (HowPublished _) _ = LT
  compare _ (HowPublished _) = GT
  compare (Chapter _) _ = LT
  compare _ (Chapter _) = GT
  compare (Pages _) _ = LT
  compare _ (Pages _) = GT
  compare (Note _) _ = LT
  compare _ (Note _) = GT
  compare (Type _) _ = LT
