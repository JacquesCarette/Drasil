module Language.Drasil.HTML.Import(makeDocument) where

import Control.Lens ((^.))
import Data.Maybe (fromJust)

import Language.Drasil.Expr (sy, ($=))
import qualified Language.Drasil.Printing.AST as P
import qualified Language.Drasil.Printing.Citation as P
import qualified Language.Drasil.Printing.LayoutObj as T
import Language.Drasil.Printing.Import (spec)

import Language.Drasil.Chunk.AssumpChunk
import Language.Drasil.Chunk.Attribute (getShortName)
import Language.Drasil.Chunk.Change (chng, chngType, ChngType(Likely))
import Language.Drasil.Chunk.Concept (defn)
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.ExprRelat (relat)
import Language.Drasil.Chunk.NamedIdea (term, getA)
import Language.Drasil.Chunk.Quantity (Quantity(..))
import Language.Drasil.Chunk.SymbolForm (eqSymb)
import Language.Drasil.ChunkDB (getUnitLup, HasSymbolTable(..))
import Language.Drasil.Chunk.ReqChunk (requires)
import Language.Drasil.Chunk.Citation ( Citation, CiteField(..), HP(..)
                                      , citeID, externRefT, fields)
import Language.Drasil.Config (verboseDDDescription, numberedDDEquations, numberedTMEquations)
import Language.Drasil.Document
import Language.Drasil.Expr.Extract
import Language.Drasil.Misc (unit'2Contents)
import Language.Drasil.NounPhrase (phrase, titleize)
import Language.Drasil.Reference
import Language.Drasil.Unit (usymb)
import Language.Drasil.Spec (Sentence(S,(:+:)))
import Language.Drasil.Printing.Import (symbol,expr)

-- | Translates from Document to the Printing representation of Document
makeDocument :: HasSymbolTable ctx => ctx -> Document -> T.Document
makeDocument sm (Document title author sections) =
  T.Document (spec sm title) (spec sm author) (createLayout sm sections)

-- | Translates from LayoutObj to the HTML representation of LayoutObj
layout :: HasSymbolTable ctx => ctx -> Int -> SecCons -> T.LayoutObj
layout sm currDepth (Sub s) = sec sm (currDepth+1) s
layout sm _         (Con c) = lay sm c

-- | Helper function for creating sections as layout objects
createLayout :: HasSymbolTable ctx => ctx -> [Section] -> [T.LayoutObj]
createLayout sm secs = map (sec sm 0) secs

-- | Helper function for creating sections at the appropriate depth
sec :: HasSymbolTable s => s -> Int -> Section -> T.LayoutObj
sec sm depth x@(Section title contents _) =
  T.HDiv [(concat $ replicate depth "sub") ++ "section"]
  ((T.Header (depth+2) (spec sm title) P.EmptyS):(map (layout sm depth) contents))
  (P.S (refAdd x))

-- | Translates from Contents to the HTML Representation of LayoutObj.
-- Called internally by layout.
lay :: HasSymbolTable ctx => ctx -> Contents -> T.LayoutObj
lay sm x@(Table hdr lls t b _) = T.Table ["table"]
  ((map (spec sm) hdr) : (map (map (spec sm)) lls)) (P.S (refAdd x)) b (spec sm t)
lay sm (Paragraph c)       = T.Paragraph (spec sm c)
lay sm (EqnBlock c _)      = T.HDiv ["equation"] [T.EqnBlock (P.E (P.Font P.Emph $ expr c sm))] (P.EmptyS)
                              -- FIXME: Make equations referable
--lay (CodeBlock c)        = T.CodeBlock c
lay sm x@(Definition c)    = T.Definition c (makePairs c sm) (P.S (refAdd x))
lay sm (Enumeration cs)    = T.List $ makeL cs sm
lay sm x@(Figure c f wp _) = T.Figure (P.S (refAdd x)) (spec sm c) f wp
lay sm x@(Graph ps w h t _) = T.Graph (map (\(y,z) -> (spec sm y, spec sm z)) ps)
                               w h (spec sm t) (P.S (refAdd x))
lay sm x@(Requirement r)   = T.ALUR T.Requirement
  (spec sm $ requires r) (P.S (refAdd x)) (spec sm (fromJust $ getShortName r))
lay sm x@(Assumption a)    = T.ALUR T.Assumption
  (spec sm (assuming a)) (P.S (refAdd x)) (spec sm (fromJust $ getShortName a))
lay sm x@(Change lc)       = T.ALUR
  (if (chngType lc) == Likely then T.LikelyChange else T.UnlikelyChange)
  (spec sm (chng lc)) (P.S (refAdd x)) (spec sm (fromJust $ getShortName lc))
lay sm (Defnt dtyp pairs rn) = T.Definition dtyp (layPairs pairs) (P.S rn)
  where layPairs = map (\(x,y) -> (x, map (lay sm) y))
lay sm (Bib bib)           = T.Bib $ map (layCite sm) bib

-- | For importing bibliography
layCite :: HasSymbolTable s => s -> Citation -> P.Citation
layCite sm c = P.Cite (citeID c) (externRefT c) (map (layField sm) (fields c))

layField :: HasSymbolTable s => s -> CiteField -> P.CiteField
layField sm (Address      s) = P.Address      $ spec sm s
layField  _ (Author       p) = P.Author       p
layField sm (BookTitle    s) = P.BookTitle    $ spec sm s
layField  _ (Chapter      i) = P.Chapter      i
layField  _ (Edition      n) = P.Edition      n
layField  _ (Editor       p) = P.Editor       p
layField sm (Institution  i) = P.Institution  $ spec sm i
layField sm (Journal      s) = P.Journal      $ spec sm s
layField  _ (Month        m) = P.Month        m
layField sm (Note         s) = P.Note         $ spec sm s
layField  _ (Number       n) = P.Number       n
layField sm (Organization i) = P.Organization $ spec sm i
layField  _ (Pages        n) = P.Pages        n
layField sm (Publisher    s) = P.Publisher    $ spec sm s
layField sm (School       s) = P.School       $ spec sm s
layField sm (Series       s) = P.Series       $ spec sm s
layField sm (Title        s) = P.Title        $ spec sm s
layField sm (Type         t) = P.Type         $ spec sm t
layField  _ (Volume       n) = P.Volume       n
layField  _ (Year         n) = P.Year         n
layField sm (HowPublished (URL  s)) = P.HowPublished (P.URL  $ spec sm s)
layField sm (HowPublished (Verb s)) = P.HowPublished (P.Verb $ spec sm s)

-- | Translates lists
makeL :: HasSymbolTable s => ListType -> s -> P.ListType
makeL (Bullet bs)      sm = P.Unordered   $ map (flip item sm) bs
makeL (Numeric ns)     sm = P.Ordered     $ map (flip item sm) ns
makeL (Simple ps)      sm = P.Simple      $ map (\(x,y) -> (spec sm x, item y sm)) ps
makeL (Desc ps)        sm = P.Desc        $ map (\(x,y) -> (spec sm x, item y sm)) ps
makeL (Definitions ps) sm = P.Definitions $ map (\(x,y) -> (spec sm x, item y sm)) ps

-- | Helper for translating list items
item :: HasSymbolTable s => ItemType -> s -> P.ItemType
item (Flat i)     sm = P.Flat $ spec sm i
item (Nested t s) sm = P.Nested (spec sm t) (makeL s sm)

-- | Translates definitions
-- (Data defs, General defs, Theoretical models, etc.)
makePairs :: HasSymbolTable s => DType -> s -> [(String,[T.LayoutObj])]
makePairs (Data c) m = [
  ("Number",      [T.Paragraph $ spec m (missingAcro (S "DD") $ fmap S $ getA c)]),
  ("Label",       [T.Paragraph $ spec m (titleize $ c ^. term)]),
  ("Units",       [T.Paragraph $ spec m (unit'2Contents c)]),
  ("Equation",    [T.HDiv ["equation"] [T.EqnBlock (buildEqn c m)] (P.EmptyS)]),
  ("Description", [T.Paragraph (buildDDDescription c m)])
  ]
makePairs (Theory c) m = [
  ("Number",      [T.Paragraph $ spec m (missingAcro (S "T") $ fmap S $ getA c)]),
  ("Label",       [T.Paragraph $ spec m (titleize $ c ^. term)]),
  ("Equation",    [T.HDiv ["equation"] [T.EqnBlock $ P.E $ expr (c ^. relat) m]
                  (P.EmptyS)]),
  ("Description", [T.Paragraph (spec m (c ^. defn))])
  ]
makePairs General  _ = error "Not yet implemented"
makePairs Instance _ = error "Not yet implemented"
makePairs TM _       = error "Not yet implemented"
makePairs DD _       = error "Not yet implemented"

missingAcro :: Sentence -> Maybe Sentence -> Sentence
missingAcro dflt Nothing = S "<b>":+: dflt :+: S "</b>"
missingAcro _ (Just a) = S "<b>":+: a :+: S "</b>"

-- | Translates the defining equation from a QDefinition to
-- HTML's version of Sentence
buildEqn :: HasSymbolTable s => QDefinition -> s -> P.Spec
buildEqn c sm = P.E (symbol $ eqSymb c) P.:+: P.S " = " P.:+:
  P.E (expr (c^.equat) sm)

-- | Build descriptions in data defs based on required verbosity
buildDDDescription :: HasSymbolTable s => QDefinition -> s -> P.Spec
buildDDDescription c m = descLines
  (if verboseDDDescription then (vars (sy c $= c^.equat) m) else []) m

-- | Helper for building each line of the description of a data def
descLines :: (HasSymbolTable s, Quantity q) => [q] -> s -> P.Spec
descLines []    _   = error "No chunks to describe"
descLines (vc:[]) m = (P.E $ P.Font P.Emph $ symbol (eqSymb vc)) P.:+:
  (P.S " is the " P.:+: (spec m (phrase $ vc ^. term)) P.:+:
   unWrp (getUnitLup vc m))
  where unWrp (Just a) = P.S " (" P.:+: P.Sy (a ^. usymb) P.:+: P.S ")"
        unWrp Nothing  = P.S ""
descLines (vc:vcs) m = descLines (vc:[]) m P.:+: P.HARDNL P.:+: descLines vcs m
