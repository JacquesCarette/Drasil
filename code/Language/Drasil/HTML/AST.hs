{-# Language GADTs #-}
module Language.Drasil.HTML.AST where

import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Spec (USymb, RefAdd)
import Language.Drasil.Unicode (Greek, Special)
import Language.Drasil.Document (DType (..), MaxWidthPercent)
import Language.Drasil.Citations (Month(..))
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

data Thesis = M | PhD deriving Eq

instance Show Thesis where
  show M   = "Master's thesis"
  show PhD = "PhD thesis"

instance Show Citation where
  show (Book      _) = "Print."
  show (Article   _) = "Print."
  show (MThesis   _) = "Print."
  show (PhDThesis _) = "Print."
  show (Misc      _) = ""
  show (Online    _) = ""

instance Eq CiteField where
  (==) (Author _)     (Author _)     = True
  (==) (Title _)      (Title _)      = True
  (==) (Series _)     (Series _)     = True
  (==) (Collection _) (Collection _) = True
  (==) (Volume _)     (Volume _)     = True
  (==) (Edition _)    (Edition _)    = True
  (==) (Place _)      (Place _)      = True
  (==) (Publisher _)  (Publisher _)  = True
  (==) (Journal _)    (Journal _)    = True
  (==) (Year _)       (Year _)       = True
  (==) (Date _ _ _)   (Date _ _ _)   = True
  (==) (Page _)       (Page _)       = True
  (==) (Pages _)      (Pages _)      = True
  (==) (Note _)       (Note _)       = True
  (==) (Issue _)      (Issue _)      = True
  (==) (School _)     (School _)     = True
  (==) (Thesis _)     (Thesis _)     = True
  (==) (URL _)        (URL _)        = True
  (==) (HowPub _)     (HowPub _)     = True
  (==) (URLdate _ _ _) (URLdate _ _ _) = True
  (==) (Editor _)     (Editor _)     = True
  (==) _ _ = False

instance Ord CiteField where --FIXME: APA has year come directly after Author
  compare (Author     _) _ = LT
  compare _ (Author     _) = GT
  compare (Title      _) _ = LT
  compare _ (Title      _) = GT
  compare (Series     _) _ = LT
  compare _ (Series     _) = GT
  compare (Collection _) _ = LT
  compare _ (Collection _) = GT
  compare (Editor     _) _ = LT
  compare _ (Editor     _) = GT
  compare (Journal    _) _ = LT
  compare _ (Journal    _) = GT
  compare (Volume     _) _ = LT
  compare _ (Volume     _) = GT
  compare (Edition    _) _ = LT
  compare _ (Edition    _) = GT
  compare (Thesis     _) _ = LT
  compare _ (Thesis     _) = GT
  compare (School     _) _ = LT
  compare _ (School     _) = GT
  compare (Place      _) _ = LT
  compare _ (Place      _) = GT
  compare (Publisher  _) _ = LT
  compare _ (Publisher  _) = GT
  compare (HowPub     _) _ = LT
  compare _ (HowPub     _) = GT
  compare (Issue      _) _ = LT
  compare _ (Issue      _) = GT
  compare (Date   _ _ _) _ = LT
  compare _ (Date   _ _ _) = GT
  compare (Year       _) _ = LT
  compare _ (Year       _) = GT
  compare (URL       _) _  = LT
  compare _ (URL       _)  = GT
  compare (Page       _) _ = LT
  compare _ (Page       _) = GT
  compare (Pages      _) _ = LT
  compare _ (Pages      _) = GT
  compare (URLdate _ _ _) _ = LT
  compare _ (URLdate _ _ _) = GT
  compare (Note       _) _ = LT
  --compare _ (Note       _) = GT
