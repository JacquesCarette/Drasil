-- | Defines functions to transform Drasil-based documents into a printable version.
module Language.Drasil.Printing.Import.Document where

import Data.Map (fromList)

import Language.Drasil hiding (neg, sec, symbol, isIn, codeExpr)
import Language.Drasil.Development (showUID)

import qualified Language.Drasil.Printing.AST as P
import qualified Language.Drasil.Printing.Citation as P
import qualified Language.Drasil.Printing.LayoutObj as T
import Language.Drasil.Printing.PrintingInformation
  (PrintingInformation)

import Language.Drasil.Printing.Import.ModelExpr (modelExpr)
import Language.Drasil.Printing.Import.CodeExpr (codeExpr)
import Language.Drasil.Printing.Import.Sentence (spec)

import Control.Lens ((^.))
import Data.Bifunctor (bimap, second)

-- * Main Function

-- | Translates from 'Document' to a printable representation of 'T.Document'.
makeDocument :: PrintingInformation -> Document -> T.Document
makeDocument sm (Document titleLb authorName _ sections) =
  T.Document (spec sm titleLb) (spec sm authorName) (createLayout sm sections)
makeDocument sm (Notebook titleLb authorName sections) =
  T.Document (spec sm titleLb) (spec sm authorName) (createLayout' sm sections)

-- | Translates from 'Document' to a printable representation of 'T.Project'.
makeProject :: PrintingInformation -> Document -> T.Project
makeProject _ Notebook {} = error "Unsupported format: Notebook"
makeProject sm (Document titleLb authorName _ sections) =
  T.Project (spec sm titleLb) (spec sm authorName) refMap files
  where
    files   = createFiles sm sections
    refMap = fromList $ concatMap createRefMap' files

-- * Helpers

-- | Helper function for creating sections as Files.
createFiles :: PrintingInformation -> [Section] -> [T.File]
createFiles sm secs = map (file sm) secs'
  where
    secs' = concatMap (extractSubS 0) secs

-- | Helper function for creating a RefMap for a Document.
createRefMap' :: T.File -> [(String, T.Filename)]
createRefMap' (T.File _ l _ c) = concatMap (createRefMap l) c

-- | Helper function for creating a RefMap for a LayoutObj
createRefMap :: T.Filename -> T.LayoutObj -> [(String, T.Filename)]
createRefMap fn (T.Header _ _ l)     = createRef fn l
createRefMap fn (T.HDiv   _ _ l)     = createRef fn l
createRefMap fn (T.Table  _ _ l _ _) = createRef fn l
createRefMap fn (T.Definition _ _ l) = createRef fn l
createRefMap fn (T.List t)           = pass t
  where
    pass (P.Ordered ls)     = process  ls
    pass (P.Unordered ls)   = process  ls
    pass (P.Simple ls)      = process' ls
    pass (P.Desc ls)        = process' ls
    pass (P.Definitions ls) = process' ls
    process  = concatMap (\(_, l)    -> maybe [] (createRef fn) l)
    process' = concatMap (\(_, _, l) -> maybe [] (createRef fn) l)
createRefMap fn (T.Figure l _ _ _)   = createRef fn l
createRefMap fn (T.Bib ls)           = map bibRefs ls
  where 
    bibRefs (P.Cite l _ _) = (l, fn)
createRefMap _ _                     = []

-- | Helper function for mapping a Label to a Filename
createRef :: T.Filename -> P.Label -> [(String, T.Filename)]
createRef fn (P.S l) = [(l, fn)]
createRef _   _      = []

-- | Helper function for creating sections as layout objects.
createLayout :: PrintingInformation -> [Section] -> [T.LayoutObj]
createLayout sm = map (sec sm 0)

createLayout' :: PrintingInformation -> [Section] -> [T.LayoutObj]
createLayout' sm = map (cel sm 0)

-- | Helper for extracting subsections into their own sections.
extractSubS :: Int -> Section -> [(T.Depth, Section)]
extractSubS d x@(Section tl c r)
  | d > 1 = [(d, x)]
  | otherwise = (d, Section tl (filter isCon c) r) : 
      concatMap (sepSub (d + 1)) c
  where 
    isCon (Con _)        = True
    isCon  _             = False
    sepSub _   (Con _)   = []
    sepSub dep (Sub s) = extractSubS dep s

-- | Helper for converting a Section to a File
file :: PrintingInformation -> (T.Depth, Section) -> T.File
file sm (d, x@(Section titleLb contents _)) = 
  T.File (spec sm titleLb) fn d los
  where
    refr = refAdd x
    fn = filter (/= ':') refr
    los = T.Header d (spec sm titleLb) (P.S refr) :
      map (layout sm d) contents

-- | Helper function for creating sections at the appropriate depth.
sec :: PrintingInformation -> Int -> Section -> T.LayoutObj
sec sm depth x@(Section titleLb contents _) = --FIXME: should ShortName be used somewhere?
  let refr = P.S (refAdd x) in
  T.HDiv [concat (replicate depth "sub") ++ "section"]
  (T.Header depth (spec sm titleLb) refr :
   map (layout sm depth) contents) refr

cel :: PrintingInformation -> Int -> Section -> T.LayoutObj
cel sm depth x@(Section titleLb contents _) = 
  let refr = P.S (refAdd x) in
  T.Cell (T.Header depth (spec sm titleLb) refr :
   map (layout sm depth) contents) 

-- | Helper for translating sections into a printable representation of layout objects ('T.LayoutObj').
layout :: PrintingInformation -> Int -> SecCons -> T.LayoutObj
layout sm currDepth (Sub s) = sec sm (currDepth+1) s
layout sm _         (Con c) = lay sm c

-- | Helper that translates 'Contents' to a printable representation of 'T.LayoutObj'.
-- Called internally by 'layout'.
lay :: PrintingInformation -> Contents -> T.LayoutObj
lay sm (LlC x) = layLabelled sm x
lay sm (UlC x) = layUnlabelled sm (x ^. accessContents)

-- | Helper that translates 'LabelledContent's to a printable representation of 'T.LayoutObj'.
-- Called internally by 'lay'.
layLabelled :: PrintingInformation -> LabelledContent -> T.LayoutObj
layLabelled sm x@(LblC _ (Table hdr lls t b)) = T.Table ["table"]
  (map (spec sm) hdr : map (map (spec sm)) lls)
  (P.S $ getAdd $ getRefAdd x)
  b (spec sm t)
layLabelled sm x@(LblC _ (EqnBlock c))          = T.HDiv ["equation"]
  [T.EqnBlock (P.E (modelExpr c sm))]
  (P.S $ getAdd $ getRefAdd x)
layLabelled sm x@(LblC _ (Figure c f wp))     = T.Figure
  (P.S $ getAdd $ getRefAdd x)
  (Just $ spec sm c) f wp
layLabelled sm x@(LblC _ (Graph ps w h t))    = T.Graph
  (map (bimap (spec sm) (spec sm)) ps) w h (spec sm t)
  (P.S $ getAdd $ getRefAdd x)
layLabelled sm x@(LblC _ (Defini dtyp pairs)) = T.Definition
  dtyp (layPairs pairs)
  (P.S $ getAdd $ getRefAdd x)
  where layPairs = map (second (map (lay sm)))
layLabelled sm (LblC _ (Paragraph c))    = T.Paragraph (spec sm c)
layLabelled sm x@(LblC _ (DerivBlock h d)) = T.HDiv ["subsubsubsection"]
  (T.Header 3 (spec sm h) refr : map (layUnlabelled sm) d) refr
  where refr = P.S $ refAdd x ++ "Deriv"
layLabelled sm (LblC _ (Enumeration cs)) = T.List $ makeL sm cs
layLabelled  _ (LblC _ (Bib bib))        = T.Bib $ map layCite bib
layLabelled sm (LblC _ (CodeBlock c))  = T.CodeBlock (P.E (codeExpr c sm))

-- | Helper that translates 'RawContent's to a printable representation of 'T.LayoutObj'.
-- Called internally by 'lay'.
layUnlabelled :: PrintingInformation -> RawContent -> T.LayoutObj
layUnlabelled sm (Table hdr lls t b) = T.Table ["table"]
  (map (spec sm) hdr : map (map (spec sm)) lls) (P.S "nolabel0") b (spec sm t)
layUnlabelled sm (Paragraph c)    = T.Paragraph (spec sm c)
layUnlabelled sm (EqnBlock c)     = T.HDiv ["equation"] [T.EqnBlock (P.E (modelExpr c sm))] P.EmptyS
layUnlabelled sm (DerivBlock h d) = T.HDiv ["subsubsubsection"]
  (T.Header 3 (spec sm h) refr : map (layUnlabelled sm) d) refr
  where refr = P.S "nolabel1"
layUnlabelled sm (Enumeration cs) = T.List $ makeL sm cs
layUnlabelled sm (Figure c f wp)  = T.Figure (P.S "nolabel2") (Just $ spec sm c) f wp
layUnlabelled sm (Graph ps w h t) = T.Graph (map (bimap (spec sm) (spec sm)) ps)
                               w h (spec sm t) (P.S "nolabel6")
layUnlabelled sm (Defini dtyp pairs)  = T.Definition dtyp (layPairs pairs) (P.S "nolabel7")
  where layPairs = map (second (map temp))
        temp  y   = layUnlabelled sm (y ^. accessContents)
layUnlabelled  _ (Bib bib)              = T.Bib $ map layCite bib
layUnlabelled sm (CodeBlock c)     = T.CodeBlock (P.E (codeExpr c sm))

-- | For importing a bibliography.
layCite :: Citation -> P.Citation
layCite c = P.Cite (showUID c) (c ^. citeKind) (map layField (c ^. getFields))

-- | Helper for translating 'Citefield's into a printable representation of 'P.CiteField's
layField :: CiteField -> P.CiteField
layField (Address      s) = P.Address      $ P.S s
layField (Author       p) = P.Author       p
layField (BookTitle    b) = P.BookTitle    $ P.S b
layField (Chapter      c) = P.Chapter      c
layField (Edition      e) = P.Edition      e
layField (Editor       e) = P.Editor       e
layField (Institution  i) = P.Institution  $ P.S i
layField (Journal      j) = P.Journal      $ P.S j
layField (Month        m) = P.Month        m
layField (Note         n) = P.Note         $ P.S n
layField (Number       n) = P.Number       n
layField (Organization o) = P.Organization $ P.S o
layField (Pages        p) = P.Pages        p
layField (Publisher    p) = P.Publisher    $ P.S p
layField (School       s) = P.School       $ P.S s
layField (Series       s) = P.Series       $ P.S s
layField (Title        t) = P.Title        $ P.S t
layField (Type         t) = P.Type         $ P.S t
layField (Volume       v) = P.Volume       v
layField (Year         y) = P.Year         y
layField (HowPublished (URL  u)) = P.HowPublished (P.URL  $ P.S u)
layField (HowPublished (Verb v)) = P.HowPublished (P.Verb $ P.S v)

-- | Translates lists to be printable.
makeL :: PrintingInformation -> ListType -> P.ListType
makeL sm (Bullet bs)      = P.Unordered   $ map (bimap (item sm) (fmap P.S)) bs
makeL sm (Numeric ns)     = P.Ordered     $ map (bimap (item sm) (fmap P.S)) ns
makeL sm (Simple ps)      = P.Simple      $ map (\(x,y,z) -> (spec sm x, item sm y, fmap P.S z)) ps
makeL sm (Desc ps)        = P.Desc        $ map (\(x,y,z) -> (spec sm x, item sm y, fmap P.S z)) ps
makeL sm (Definitions ps) = P.Definitions $ map (\(x,y,z) -> (spec sm x, item sm y, fmap P.S z)) ps

-- | Helper for translating list items to be printable.
item :: PrintingInformation -> ItemType -> P.ItemType
item sm (Flat i)     = P.Flat $ spec sm i
item sm (Nested t s) = P.Nested (spec sm t) (makeL sm s)
