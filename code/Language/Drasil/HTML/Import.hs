module Language.Drasil.HTML.Import(makeDocument,spec) where

import Language.Drasil.Expr (sy, ($=))
import Language.Drasil.Spec
import qualified Language.Drasil.Printing.AST as P
import qualified Language.Drasil.Printing.Citation as PC
import qualified Language.Drasil.Printing.LayoutObj as H

import Language.Drasil.Chunk.AssumpChunk
import Language.Drasil.Chunk.Attribute
import Language.Drasil.Chunk.Change (chng, chngType, ChngType(..))
import Language.Drasil.Chunk.Concept (defn)
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.ExprRelat (relat)
import Language.Drasil.Chunk.NamedIdea (term, getA)
import Language.Drasil.Chunk.Quantity (Quantity(..))
import Language.Drasil.Chunk.SymbolForm (eqSymb)
import Language.Drasil.ChunkDB (HasSymbolTable(..), getUnitLup)
import Language.Drasil.Chunk.ReqChunk (requires)
import Language.Drasil.Chunk.Citation ( CiteField(..), HP(..), Citation
                                      , externRefT, citeID, fields)
import Language.Drasil.Config (verboseDDDescription)
import Language.Drasil.Document
import Language.Drasil.Expr.Extract
import Language.Drasil.Misc (unit'2Contents)
import Language.Drasil.NounPhrase (phrase, titleize)
import Language.Drasil.Reference
import Language.Drasil.Unit (usymb)
import Language.Drasil.Printing.Import (expr,symbol)

import Control.Lens ((^.))
import Data.Maybe (fromJust)

-- | Translates Sentence to the HTML representation of Sentence ('Spec')
spec :: HasSymbolTable s => Sentence -> s -> P.Spec
spec (S s)        _ = P.S s
spec (Sy s)       _ = P.Sy s
spec (EmptyS :+: b) sm = spec b sm
spec (a :+: EmptyS) sm = spec a sm
spec (a :+: b) sm   = spec a sm P.:+: spec b sm
spec (Sp s)       _ = P.Sp s
spec (P s)        _ = P.E $ P.Font P.Emph $ symbol s
spec (F f c)      _ = P.Acc f c
spec (Ref t r n) sm = P.Ref t r (spec n sm)
spec (Quote q) sm = P.Quote $ spec q sm -- P.S "&quot;" P.:+: spec q sm P.:+: P.S "&quot;"
spec EmptyS     _ = P.EmptyS
spec (E e)     sm = P.E $ P.Font P.Emph $ expr e sm

-- | Translates from Document to the HTML representation of Document
makeDocument :: HasSymbolTable s => Document -> s -> H.Document
makeDocument (Document title author sections) sm =
  H.Document (spec title sm) (spec author sm) (createLayout sections sm)

-- | Translates from LayoutObj to the HTML representation of LayoutObj
layout :: HasSymbolTable s => Int -> SecCons -> s -> H.LayoutObj
layout currDepth (Sub s) sm = sec (currDepth+1) s sm
layout _         (Con c) sm = lay c sm

-- | Helper function for creating sections as layout objects
createLayout :: HasSymbolTable s => [Section] -> s -> [H.LayoutObj]
createLayout secs sm = map (flip (sec 0) sm) secs

-- | Helper function for creating sections at the appropriate depth
sec :: HasSymbolTable s => Int -> Section -> s -> H.LayoutObj
sec depth x@(Section title contents _) sm =
  H.HDiv [(concat $ replicate depth "sub") ++ "section"]
  ((H.Header (depth+2) (spec title sm) P.EmptyS):(map (flip (layout depth) sm) contents))
  (P.S (refAdd x))

-- | Translates from Contents to the HTML Representation of LayoutObj.
-- Called internally by layout.
lay :: HasSymbolTable s => Contents -> s -> H.LayoutObj
lay x@(Table hdr lls t b _) sm = H.Table ["table"]
  ((map (flip spec sm) hdr) : (map (map (flip spec sm)) lls)) (P.S (refAdd x)) b (spec t sm)
lay (Paragraph c)       sm = H.Paragraph (spec c sm)
lay (EqnBlock c _)      sm = H.HDiv ["equation"] [H.EqnBlock (P.E (P.Font P.Emph $ expr c sm))] (P.EmptyS)
                              -- FIXME: Make equations referable
--lay (CodeBlock c)        = H.CodeBlock c
lay x@(Definition c)    sm = H.Definition c (makePairs c sm) (P.S (refAdd x))
lay (Enumeration cs)    sm = H.List $ makeL cs sm
lay x@(Figure c f wp _) sm = H.Figure (P.S (refAdd x)) (spec c sm) f wp
lay x@(Graph ps w h t _) sm = H.Graph (map (\(y,z) -> (spec y sm, spec z sm)) ps)
                               w h (spec t sm) (P.S (refAdd x))
lay x@(Requirement r)   sm = H.ALUR H.Requirement
  (spec (requires r) sm) (P.S (refAdd x)) (spec (fromJust $ getShortName r) sm)
lay x@(Assumption a)    sm = H.ALUR H.Assumption
  (spec (assuming a) sm) (P.S (refAdd x)) (spec (fromJust $ getShortName a) sm)
lay x@(Change lc) sm = H.ALUR
  (if (chngType lc) == Likely then H.LikelyChange else H.UnlikelyChange)
  (spec (chng lc) sm) (P.S (refAdd x)) (spec (fromJust $ getShortName lc) sm)
lay (Defnt dtyp pairs rn) sm = H.Definition dtyp (layPairs pairs) (P.S rn)
  where layPairs = map (\(x,y) -> (x, (map (\z -> lay z sm) y)))
lay (Bib bib)           sm = H.Bib $ map (layCite sm) bib

-- | For importing bibliography
layCite :: HasSymbolTable s => s -> Citation -> PC.Citation
layCite sm c = PC.Cite (citeID c) (externRefT c) (map (layField sm) (fields c))

layField :: HasSymbolTable s => s -> CiteField -> PC.CiteField
layField sm (Address      s) = PC.Address      $ spec s sm
layField  _ (Author       p) = PC.Author       p
layField sm (BookTitle    s) = PC.BookTitle    $ spec s sm
layField  _ (Chapter      i) = PC.Chapter      i
layField  _ (Edition      n) = PC.Edition      n
layField  _ (Editor       p) = PC.Editor       p
layField sm (Institution  i) = PC.Institution  $ spec i sm
layField sm (Journal      s) = PC.Journal      $ spec s sm
layField  _ (Month        m) = PC.Month        m
layField sm (Note         s) = PC.Note         $ spec s sm
layField  _ (Number       n) = PC.Number       n
layField sm (Organization i) = PC.Organization $ spec i sm
layField  _ (Pages        n) = PC.Pages        n
layField sm (Publisher    s) = PC.Publisher    $ spec s sm
layField sm (School       s) = PC.School       $ spec s sm
layField sm (Series       s) = PC.Series       $ spec s sm
layField sm (Title        s) = PC.Title        $ spec s sm
layField sm (Type         t) = PC.Type         $ spec t sm
layField  _ (Volume       n) = PC.Volume       n
layField  _ (Year         n) = PC.Year         n
layField sm (HowPublished (URL  s)) = PC.HowPublished (PC.URL  $ spec s sm)
layField sm (HowPublished (Verb s)) = PC.HowPublished (PC.Verb $ spec s sm)

-- | Translates lists
makeL :: HasSymbolTable s => ListType -> s -> P.ListType
makeL (Bullet bs)      sm = P.Unordered   $ map (flip item sm) bs
makeL (Numeric ns)     sm = P.Ordered     $ map (flip item sm) ns
makeL (Simple ps)      sm = P.Simple      $ map (\(x,y) -> (spec x sm, item y sm)) ps
makeL (Desc ps)        sm = P.Desc        $ map (\(x,y) -> (spec x sm, item y sm)) ps
makeL (Definitions ps) sm = P.Definitions $ map (\(x,y) -> (spec x sm, item y sm)) ps

-- | Helper for translating list items
item :: HasSymbolTable s => ItemType -> s -> P.ItemType
item (Flat i)     sm = P.Flat (spec i sm)
item (Nested t s) sm = P.Nested (spec t sm) (makeL s sm)

-- | Translates definitions
-- (Data defs, General defs, Theoretical models, etc.)
makePairs :: HasSymbolTable s => DType -> s -> [(String,[H.LayoutObj])]
makePairs (Data c) m = [
  ("Number",      [H.Paragraph $ spec (missingAcro (S "DD") $ fmap S $ getA c) m]),
  ("Label",       [H.Paragraph $ spec (titleize $ c ^. term) m]),
  ("Units",       [H.Paragraph $ spec (unit'2Contents c) m]),
  ("Equation",    [H.HDiv ["equation"] [H.EqnBlock (buildEqn c m)] (P.EmptyS)]),
  ("Description", [H.Paragraph (buildDDDescription c m)])
  ]
makePairs (Theory c) m = [
  ("Number",      [H.Paragraph $ spec (missingAcro (S "T") $ fmap S $ getA c) m]),
  ("Label",       [H.Paragraph $ spec (titleize $ c ^. term) m]),
  ("Equation",    [H.HDiv ["equation"] [H.EqnBlock (P.E (P.Font P.Emph $ expr (c ^. relat) m))]
                  (P.EmptyS)]),
  ("Description", [H.Paragraph (spec (c ^. defn) m)])
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
buildEqn c sm = P.E (P.Font P.Emph $ symbol (eqSymb c)) P.:+: P.S " = " P.:+:
  P.E (P.Font P.Emph $ expr (c^.equat) sm)

-- | Build descriptions in data defs based on required verbosity
buildDDDescription :: HasSymbolTable s => QDefinition -> s -> P.Spec
buildDDDescription c m = descLines
  (if verboseDDDescription then (vars (getQ c $= c^.equat) m) else []) m
  where getQ (EC a _ _) = sy a

-- | Helper for building each line of the description of a data def
descLines :: (HasSymbolTable s, Quantity q) => [q] -> s -> P.Spec
descLines []    _   = error "No chunks to describe"
descLines (vc:[]) m = (P.E $ P.Font P.Emph $ symbol (eqSymb vc)) P.:+:
  (P.S " is the " P.:+: (spec (phrase $ vc ^. term) m) P.:+:
   unWrp (getUnitLup vc m))
  where unWrp (Just a) = P.S " (" P.:+: P.Sy (a ^. usymb) P.:+: P.S ")"
        unWrp Nothing  = P.S ""
descLines (vc:vcs) m = descLines (vc:[]) m P.:+: P.HARDNL P.:+: descLines vcs m
