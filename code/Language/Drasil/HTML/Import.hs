module Language.Drasil.HTML.Import where
import Prelude hiding (id)
import Language.Drasil.Expr (Expr(..), Relation, UFunc(..), BiFunc(..),
                             Bound(..),DerivType(..))
import Language.Drasil.Spec
import qualified Language.Drasil.HTML.AST as H
import Language.Drasil.Unicode (Special(Partial))
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.Relation
import Language.Drasil.Chunk.Module
import Language.Drasil.Chunk.NamedIdea (NamedIdea, term)
import Language.Drasil.Chunk.Concept (defn)
import Language.Drasil.Chunk.SymbolForm (SymbolForm, symbol)
import Language.Drasil.Chunk.VarChunk (VarChunk)
import Control.Lens hiding ((:>),(:<))
import Language.Drasil.Expr.Extract
import Language.Drasil.Config (verboseDDDescription)
import Language.Drasil.Document
import Language.Drasil.Symbol
import Language.Drasil.Misc (unit'2Contents)
import Language.Drasil.SymbolAlphabet (lD)
import Language.Drasil.NounPhrase (phrase)

expr :: Expr -> H.Expr
expr (V v)            = H.Var   v
expr (Dbl d)          = H.Dbl   d
expr (Int i)          = H.Int   i
expr (a :* b)         = H.Mul   (expr a) (expr b)
expr (a :+ b)         = H.Add   (expr a) (expr b)
expr (a :/ b)         = H.Frac  (replace_divs a) (replace_divs b)
expr (a :^ b)         = H.Pow   (expr a) (expr b)
expr (a :- b)         = H.Sub   (expr a) (expr b)
expr (a :. b)         = H.Dot   (expr a) (expr b)
expr (Neg a)          = H.Neg   (expr a)
expr (Deriv Part a 1) = H.Mul (H.Sym (Special Partial)) (expr a)
expr (Deriv Total a 1)= H.Mul (H.Sym lD) (expr a)
expr (Deriv Part a b) = H.Frac (H.Mul (H.Sym (Special Partial)) (expr a)) 
                          (H.Mul (H.Sym (Special Partial)) (expr b))
expr (Deriv Total a b)= H.Frac (H.Mul (H.Sym lD) (expr a)) 
                          (H.Mul (H.Sym lD) (expr b))
expr (C c)            = H.Sym   (c ^. symbol)
expr (FCall f x)      = H.Call (expr f) (map expr x)
expr (Case ps)        = if length ps < 2 then 
                    error "Attempting to use multi-case expr incorrectly"
                    else H.Case (zip (map (expr . fst) ps) (map (rel . snd) ps))
expr e@(_ := _)       = rel e
expr e@(_ :> _)       = rel e
expr e@(_ :< _)       = rel e
expr (UnaryOp u)      = (\(x,y) -> H.Op x [y]) (ufunc u)
expr (Grouping e)     = H.Grouping (expr e)
expr (BinaryOp b)     = (\(x,y) -> H.Op x y) (bfunc b)

ufunc :: UFunc -> (H.Function, H.Expr)
ufunc (Log e) = (H.Log, expr e)
ufunc (Summation (Just (s, Low v, High h)) e) = 
  (H.Summation (Just ((s, expr v), expr h)), (expr e))
ufunc (Summation Nothing e) = (H.Summation Nothing,(expr e))
ufunc (Summation _ _) = error "HTML/Import.hs Incorrect use of Summation"
ufunc (Abs e) = (H.Abs, expr e)
ufunc i@(Integral _ _ _) = integral i
ufunc (Sin e) = (H.Sin, expr e)
ufunc (Cos e) = (H.Cos, expr e)
ufunc (Tan e) = (H.Tan, expr e)
ufunc (Sec e) = (H.Sec, expr e)
ufunc (Csc e) = (H.Csc, expr e)
ufunc (Cot e) = (H.Cot, expr e)

bfunc :: BiFunc -> (H.Function, [H.Expr])
bfunc (Cross e1 e2) = (H.Cross, map expr [e1,e2])

rel :: Relation -> H.Expr
rel (a := b) = H.Eq (expr a) (expr b)
rel (a :< b) = H.Lt (expr a) (expr b)
rel (a :> b) = H.Gt (expr a) (expr b)
rel _ = error "Attempting to use non-Relation Expr in relation context."

integral :: UFunc -> (H.Function, H.Expr)
integral (Integral (Just (Low v), Just (High h)) e wrtc) = 
  (H.Integral (Just (expr v), Just (expr h)) (int_wrt wrtc), expr e)
integral (Integral (Just (High h), Just (Low v)) e wrtc) = 
  (H.Integral (Just (expr v), Just (expr h)) (int_wrt wrtc), expr e)
integral (Integral (Just (Low v), Nothing) e wrtc) = 
  (H.Integral (Just (expr v), Nothing) (int_wrt wrtc), expr e)
integral (Integral (Nothing, Just (Low v)) e wrtc) = 
  (H.Integral (Just (expr v), Nothing) (int_wrt wrtc), expr e)
integral (Integral (Just (High h), Nothing) e wrtc) = 
  (H.Integral (Nothing, Just (expr h)) (int_wrt wrtc), expr e)
integral (Integral (Nothing, Just (High h)) e wrtc) = 
  (H.Integral (Nothing, Just (expr h)) (int_wrt wrtc), expr e)
integral (Integral (Nothing, Nothing) e wrtc) = 
  (H.Integral (Nothing, Nothing) (int_wrt wrtc), expr e)
integral _ = error "TeX/Import.hs Incorrect use of Integral"

int_wrt :: (NamedIdea c, SymbolForm c) => c -> H.Expr
int_wrt wrtc = (expr (Deriv Total (C wrtc) 1))

replace_divs :: Expr -> H.Expr
replace_divs (a :/ b) = H.Div (replace_divs a) (replace_divs b)
replace_divs (a :+ b) = H.Add (replace_divs a) (replace_divs b)
replace_divs (a :* b) = H.Mul (replace_divs a) (replace_divs b)
replace_divs (a :^ b) = H.Pow (replace_divs a) (replace_divs b)
replace_divs (a :- b) = H.Sub (replace_divs a) (replace_divs b)
replace_divs a        = expr a

spec :: Sentence -> H.Spec
spec (S s)     = H.S s
spec (Sy s)    = H.Sy s
spec (a :+: b) = spec a H.:+: spec b
spec (G g)     = H.G g
spec (Sp s)    = H.Sp s
spec (P s)     = H.N s
spec (F f s)   = spec $ accent f s
spec (Ref t r) = H.Ref t (spec r)
spec (Quote q) = H.S "&quot;" H.:+: spec q H.:+: H.S "&quot;"

accent :: Accent -> Char -> Sentence
accent Grave  s = S $ '&' : s : "grave;" --Only works on vowels.
accent Acute  s = S $ '&' : s : "acute;" --Only works on vowels.

decorate :: Decoration -> Sentence -> Sentence
decorate Hat    s = s :+: S "&#770;" 
decorate Vector s = S "<b>" :+: s :+: S "</b>"

makeDocument :: Document -> H.Document
makeDocument (Document title author sections) = 
  H.Document (spec title) (spec author) (createLayout sections)

layout :: Int -> SecCons -> H.LayoutObj
layout currDepth (Sub s) = sec (currDepth+1) s
layout _         (Con c) = lay c
  
createLayout :: [Section] -> [H.LayoutObj]
createLayout = map (sec 0)

sec :: Int -> Section -> H.LayoutObj
sec depth x@(Section title contents) = 
  H.HDiv [(concat $ replicate depth "sub") ++ "section"] 
  ((H.Header (depth+2) (spec title)):(map (layout depth) contents)) 
  (spec $ refName x)

lay :: Contents -> H.LayoutObj
lay x@(Table hdr lls t b)     = H.Table ["table"] 
  ((map spec hdr) : (map (map spec) lls)) (spec (refName x)) b (spec t)
lay (Paragraph c)     = H.Paragraph (spec c)
lay (EqnBlock c)      = H.HDiv ["equation"] [H.Tagless (H.E (expr c))] (H.S "")
--lay (CodeBlock c)     = H.CodeBlock c
lay x@(Definition c)  = H.Definition c (makePairs c) (spec $ refName x)
lay (Enumeration cs)  = H.List $ makeL cs
lay x@(Figure c f)    = H.Figure (spec (refName x)) (spec c) f
lay x@(Module m)      = H.Module (formatName m) (spec $ refName x)
lay (Graph _ _ _ _)   = H.Paragraph (H.S "")  -- need to implement!
lay (Requirement _)   = H.Paragraph (H.S "")  -- need to implement!
lay (Assumption _)    = H.Paragraph (H.S "")  -- need to implement!
lay (LikelyChange _)  = H.Paragraph (H.S "")  -- need to implement!
lay (UnlikelyChange _)= H.Paragraph (H.S "")  -- need to implement!

makeL :: ListType -> H.ListType
makeL (Bullet bs) = H.Unordered $ map item bs
makeL (Number ns) = H.Ordered $ map item ns
makeL (Simple ps) = H.Simple $ zip (map (spec . fst) ps) (map (item . snd) ps)
makeL (Desc ps)   = H.Desc $ zip (map (spec . fst) ps) (map (item . snd) ps)

item :: ItemType -> H.ItemType
item (Flat i) = H.Flat (spec i)
item (Nested t s) = H.Nested (spec t) (makeL s)

makePairs :: DType -> [(String,H.LayoutObj)]
makePairs (Data c) = [
  ("Label",       H.Paragraph $ H.N $ c ^. symbol),
  ("Units",       H.Paragraph $ spec $ unit'2Contents c),
  ("Equation",    H.HDiv ["equation"] [H.Tagless (buildEqn c)] (H.S "")),
  ("Description", H.Paragraph (buildDDDescription c))
  ]
makePairs (Theory c) = [
  ("Label",       H.Paragraph $ spec (phrase $ c ^. term)),
  ("Equation",    H.HDiv ["equation"] [H.Tagless (H.E (rel (relat c)))] 
                  (H.S "")),
  ("Description", H.Paragraph (spec (c ^. defn)))
  ]
makePairs General = error "Not yet implemented"
  
buildEqn :: QDefinition -> H.Spec  
buildEqn c = H.N (c ^. symbol) H.:+: H.S " = " H.:+: H.E (expr (equat c))

-- Build descriptions in data defs based on required verbosity
buildDDDescription :: QDefinition -> H.Spec
buildDDDescription c = descLines (
  (toVC c):(if verboseDDDescription then (vars (equat c)) else []))

descLines :: [VarChunk] -> H.Spec  
descLines []       = error "No chunks to describe"
descLines (vc:[])  = (H.N (vc ^. symbol) H.:+: 
  (H.S " is the " H.:+: (spec (phrase $ vc ^. term))))
descLines (vc:vcs) = descLines (vc:[]) H.:+: H.HARDNL H.:+: descLines vcs

--buildModuleDesc :: ModuleChunk -> [H.LayoutObj]
--buildModuleDesc m = [
--  H.List H.Simple
--    [ H.S (bold "Secrets: ") H.:+: (spec $ secret m),
--      H.S (bold "Services: ") H.:+: (spec $ m ^. term),
--      H.S (bold "Implemented By: ") H.:+: (H.S $ getImp $ imp m)
--    ]
--  ]
--  where bold = \x -> "<b>" ++ x ++ "</b>"
--        getImp (Just x) = x
--        getImp Nothing  = "--"
