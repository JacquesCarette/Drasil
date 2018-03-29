module Language.Drasil.TeX.Import(makeDocument,spec) where

import Control.Lens ((^.))

import Language.Drasil.Expr (sy, ($=))
import Language.Drasil.Chunk.AssumpChunk
import Language.Drasil.Expr.Extract
import Language.Drasil.Chunk.Change (chng, chngType, ChngType(Likely))
import Language.Drasil.Chunk.Concept (defn)
import Language.Drasil.Spec
import qualified Language.Drasil.TeX.AST as T
import qualified Language.Drasil.Printing.AST as P
import qualified Language.Drasil.Printing.Citation as P
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.ExprRelat (relat)
import Language.Drasil.Chunk.NamedIdea (term)
import Language.Drasil.Chunk.Quantity (Quantity(..))
import Language.Drasil.Chunk.SymbolForm (eqSymb)
import Language.Drasil.ChunkDB (getUnitLup, HasSymbolTable(..))
import Language.Drasil.Chunk.ReqChunk (requires)
import Language.Drasil.Chunk.Citation ( Citation, CiteField(..), HP(..)
                                      , citeID, externRefT, fields)
import Language.Drasil.Config (verboseDDDescription, numberedDDEquations, numberedTMEquations)
import Language.Drasil.Document
import Language.Drasil.Misc (unit'2Contents)
import Language.Drasil.NounPhrase (phrase, titleize)
import Language.Drasil.Reference
import Language.Drasil.Unit (usymb)
import Language.Drasil.Printing.Import (symbol,expr)

spec :: HasSymbolTable ctx => ctx -> Sentence -> P.Spec
spec _  (S s)          = P.S s
spec _  (Sy s)         = P.Sy s
spec sm (EmptyS :+: b) = spec sm b
spec sm (a :+: EmptyS) = spec sm a
spec sm (a :+: b)      = spec sm a P.:+: spec sm b
spec _  (Sp s)         = P.Sp s
spec sm (F f s)        = spec sm (accent f s)
spec _  (P s)          = P.E $ symbol s
spec _  (Ref t r _)    = P.Ref t r (P.S r)
spec sm (Quote q)      = P.S "``" P.:+: spec sm q P.:+: P.S "\""
spec _  EmptyS         = P.EmptyS
spec sm (E e)          = P.E $ expr e sm

-- decorate :: Decoration -> Sentence -> Sentence
-- decorate Hat    s = S "\\hat{" :+: s :+: S "}"
-- decorate Vector s = S "\\bf{" :+: s :+: S "}"
-- decorate Prime  s = s :+: S "'"

accent :: Accent -> Char -> Sentence
accent Grave  s = S $ "\\`{" ++ (s : "}")
accent Acute  s = S $ "\\'{" ++ (s : "}")

makeDocument :: HasSymbolTable ctx => ctx -> Document -> T.Document
makeDocument sm (Document title author sections) =
  T.Document (spec sm title) (spec sm author) (createLayout sm sections)

layout :: HasSymbolTable ctx => ctx -> Int -> SecCons -> T.LayoutObj
layout sm currDepth (Sub s) = sec sm (currDepth+1) s
layout sm _         (Con c) = lay sm c

createLayout :: HasSymbolTable ctx => ctx -> Sections -> [T.LayoutObj]
createLayout sm = map (sec sm 0)

sec :: HasSymbolTable ctx => ctx -> Int -> Section -> T.LayoutObj
sec sm depth x@(Section title contents _) =
  T.Section depth (spec sm title) (map (layout sm depth) contents) (P.S (refAdd x))

lay :: HasSymbolTable ctx => ctx -> Contents -> T.LayoutObj
lay sm x@(Table hdr lls t b _)
  | null lls || length hdr == length (head lls) = T.Table [] ((map (spec sm) hdr) :
      (map (map (spec sm)) lls)) (P.S (refAdd x)) b (spec sm t)
  | otherwise = error $ "Attempting to make table with " ++ show (length hdr) ++
                        " headers, but data contains " ++
                        show (length (head lls)) ++ " columns."
lay sm (Paragraph c)         = T.Paragraph (spec sm c)
lay sm (EqnBlock c _)        = T.EqnBlock (P.E (expr c sm))
--lay (CodeBlock c)         = T.CodeBlock c
lay sm x@(Definition c)       = T.Definition c (makePairs sm c) (P.S (refAdd x))
lay sm (Enumeration cs)       = T.List $ makeL sm cs
lay sm x@(Figure c f wp _)    = T.Figure (P.S (refAdd x)) (spec sm c) f wp
lay sm x@(Requirement r)      = T.ALUR T.Requirement (spec sm (requires r)) (P.S (refAdd x))
lay sm x@(Assumption a)       = T.ALUR T.Assumption (spec sm (assuming a)) (P.S (refAdd x))
lay sm x@(Change ct)          = 
  if chngType ct == Likely then
    T.ALUR T.LikelyChange (spec sm (chng ct)) (P.S (refAdd x))
  else
    T.ALUR T.UnlikelyChange (spec sm (chng ct)) (P.S (refAdd x))
lay sm x@(Graph ps w h t _)   = T.Graph (map (\(y,z) -> (spec sm y, spec sm z)) ps)
                               w h (spec sm t) (P.S (refAdd x))
lay sm (Defnt dtyp pairs rn)  = T.Definition dtyp (layPairs pairs) (P.S rn)
  where layPairs = map (\(x,y) -> (x, map (lay sm) y))
lay sm (Bib bib)          = T.Bib $ map (layCite sm) bib

-- | For importing bibliography
layCite :: HasSymbolTable ctx => ctx -> Citation -> P.Citation
layCite sm c = P.Cite (citeID c) (externRefT c) (map (layField sm) (fields c))

layField :: HasSymbolTable ctx => ctx -> CiteField -> P.CiteField
layField sm (Address      s) = P.Address      $ spec sm s
layField  _ (Author       p) = P.Author       p
layField sm (BookTitle    b) = P.BookTitle    $ spec sm b
layField  _ (Chapter      c) = P.Chapter      c
layField  _ (Edition      e) = P.Edition      e
layField  _ (Editor       e) = P.Editor       e
layField sm (Institution  i) = P.Institution  $ spec sm i
layField sm (Journal      j) = P.Journal      $ spec sm j
layField  _ (Month        m) = P.Month        m
layField sm (Note         n) = P.Note         $ spec sm n
layField  _ (Number       n) = P.Number       n
layField sm (Organization o) = P.Organization $ spec sm o
layField  _ (Pages        p) = P.Pages        p
layField sm (Publisher    p) = P.Publisher    $ spec sm p
layField sm (School       s) = P.School       $ spec sm s
layField sm (Series       s) = P.Series       $ spec sm s
layField sm (Title        t) = P.Title        $ spec sm t
layField sm (Type         t) = P.Type         $ spec sm t
layField  _ (Volume       v) = P.Volume       v
layField  _ (Year         y) = P.Year         y
layField sm (HowPublished (URL  u)) = P.HowPublished (P.URL  $ spec sm u)
layField sm (HowPublished (Verb v)) = P.HowPublished (P.Verb $ spec sm v)

makeL :: HasSymbolTable ctx => ctx -> ListType -> P.ListType
makeL sm (Bullet bs)      = P.Ordered     $ map (item sm) bs
makeL sm (Numeric ns)     = P.Unordered   $ map (item sm) ns
makeL sm (Simple ps)      = P.Simple      $ map (\(x,y) -> (spec sm x, item sm y)) ps
makeL sm (Desc ps)        = P.Desc        $ map (\(x,y) -> (spec sm x, item sm y)) ps
makeL sm (Definitions ps) = P.Definitions $ map (\(x,y) -> (spec sm x, item sm y)) ps

item :: HasSymbolTable ctx => ctx -> ItemType -> P.ItemType
item sm (Flat i)     = P.Flat   (spec sm i)
item sm (Nested t s) = P.Nested (spec sm t) (makeL sm s)

makePairs :: HasSymbolTable ctx => ctx -> DType -> [(String,[T.LayoutObj])]
makePairs m (Data c) = [
  ("Label",       [T.Paragraph $ spec m (titleize $ c ^. term)]),
  ("Units",       [T.Paragraph $ spec m (unit'2Contents c)]),
  ("Equation",    [eqnStyleDD  $ buildEqn m c]),
  ("Description", [T.Paragraph $ buildDDDescription m c])
  ]
makePairs m (Theory c) = [
  ("Label",       [T.Paragraph $ spec m (titleize $ c ^. term)]),
  ("Equation",    [eqnStyleTM $ P.E (expr (c ^. relat) m)]),
  ("Description", [T.Paragraph (spec m (c ^. defn))])
  ]
makePairs _ General  = error "Not yet implemented"
makePairs _ Instance = error "Not yet implemented"
makePairs _ TM       = error "Not yet implemented"
makePairs _ DD       = error "Not yet implemented"


-- Toggle equation style
eqnStyleDD :: T.Contents -> T.LayoutObj
eqnStyleDD = if numberedDDEquations then T.EqnBlock else T.Paragraph

eqnStyleTM :: T.Contents -> T.LayoutObj
eqnStyleTM = if numberedTMEquations then T.EqnBlock else T.Paragraph

buildEqn :: HasSymbolTable ctx => ctx -> QDefinition -> P.Spec
buildEqn sm c = P.E (symbol $ eqSymb c) P.:+: P.S " = " P.:+:
  P.E (expr (c^.equat) sm)

-- Build descriptions in data defs based on required verbosity
buildDDDescription :: HasSymbolTable ctx => ctx -> QDefinition -> P.Spec
buildDDDescription m c = descLines m
  (if verboseDDDescription then vars (sy c $= c^.equat) m else [])

descLines :: (HasSymbolTable ctx, Quantity q) => ctx -> [q] -> P.Spec
descLines _ []      = error "No chunks to describe"
descLines m (vc:[]) = (P.E (symbol $ eqSymb vc) P.:+:
  (P.S " is the " P.:+: (spec m (phrase $ vc ^. term)) P.:+:
   maybe (P.S "") (\a -> P.S " (" P.:+: P.Sy (a ^. usymb) P.:+: P.S ")") (getUnitLup vc m)))
descLines m (vc:vcs) = descLines m (vc:[]) P.:+: P.HARDNL P.:+: descLines m vcs
