module Language.Drasil.HTML.Import where

import Language.Drasil.Expr (Expr(..), Relation(..))
import Language.Drasil.Spec
import qualified Language.Drasil.HTML.AST as H
import Language.Drasil.Unicode (render, Partial(..))
import Language.Drasil.Format (Format(HTML))
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.Relation
import Language.Drasil.Unit
import Language.Drasil.Chunk
import Control.Lens
import Language.Drasil.Expr.Extract
import Language.Drasil.Config (verboseDDDescription)
import Language.Drasil.Document
import Language.Drasil.Symbol


expr :: Expr -> H.Expr
expr (V v)       = H.Var   v
expr (Dbl d)     = H.Dbl   d
expr (Int i)     = H.Int   i
expr (a :* b)    = H.Mul   (expr a) (expr b)
expr (a :+ b)    = H.Add   (expr a) (expr b)
expr (a :/ b)    = H.Frac  (replace_divs a) (replace_divs b)
expr (a :^ b)    = H.Pow   (expr a) (expr b)
expr (a :- b)    = H.Sub   (expr a) (expr b)
expr (a :. b)    = H.Dot   (expr a) (expr b)
expr (Neg a)     = H.Neg   (expr a)
expr (Deriv a b) = H.Frac (H.Mul (H.Sym (Special Partial)) (expr a)) 
                          (H.Mul (H.Sym (Special Partial)) (expr b))
expr (C c)       = H.Sym   (c ^. symbol)
expr (FCall f x) = H.Call (expr f) (map expr x)
expr (Case ps)   = if length ps < 2 then 
                    error "Attempting to use multi-case expr incorrectly"
                    else H.Case (zip (map (expr . fst) ps) (map (rel . snd) ps))


rel :: Relation -> H.Expr
rel (a := b) = H.Eq (expr a) (expr b)
rel (a :< b) = H.Lt (expr a) (expr b)
rel (a :> b) = H.Gt (expr a) (expr b)
--rel _ = error "unimplemented relation, see Language.Drasil.HTML.Import"

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
spec (U u)     = H.S $ render HTML u
spec (F f s)   = spec $ accent f s
-- spec (N s)     = H.N s
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

layout :: SecCons -> H.LayoutObj
layout (Sub s) = sec s
layout (Con c) = lay c
  
createLayout :: [Section] -> [H.LayoutObj]
createLayout = map sec

sec :: Section -> H.LayoutObj
sec x@(Section depth title contents) = 
  H.HDiv [(concat $ replicate depth "sub") ++ "section"] 
  ((H.Header (depth+2) (spec title)):(map layout contents)) 
  (spec $ refName x)

lay :: Contents -> H.LayoutObj
lay x@(Table hdr lls t b)     = H.Table ["table"] 
  ((map spec hdr) : (map (map spec) lls)) (spec (refName x)) b (spec t)
lay (Paragraph c)     = H.Paragraph (spec c)
lay (EqnBlock c)      = H.HDiv ["equation"] [H.Tagless (spec c)] (H.S "")
lay (CodeBlock c)     = H.CodeBlock c
lay x@(Definition c)  = H.Definition c (makePairs c) (spec $ refName x)
lay (BulletList cs)   = H.List $ H.Unordered (map lay cs)
lay (NumberedList cs) = H.List $ H.Ordered (map lay cs)
lay (SimpleList cs)   = H.List $ H.Simple (map (\(f,s) -> (spec f, lay s)) cs)
lay x@(Figure c f)    = H.Figure (spec (refName x)) (spec c) f

makePairs :: DType -> [(String,H.LayoutObj)]
makePairs (Data c) = [
  ("Label",       H.Paragraph $ H.N $ c ^. symbol),
  ("Units",       H.Paragraph $ H.Sy $ c ^. unit),
  ("Equation",    H.HDiv ["equation"] [H.Tagless (buildEqn c)] (H.S "")),
  ("Description", H.Paragraph (buildDDDescription c))
  ]
makePairs (Theory c) = [
  ("Label",       H.Paragraph $ H.S $ c ^. name),
  ("Equation",    H.HDiv ["equation"] [H.Tagless (H.E (rel (relat c)))] 
                  (H.S "")),
  ("Description", H.Paragraph (spec (c ^. descr)))
  ]
makePairs General = error "Not yet implemented"
  
buildEqn :: EqChunk -> H.Spec  
buildEqn c = H.N (c ^. symbol) H.:+: H.S " = " H.:+: H.E (expr (equat c))

-- Build descriptions in data defs based on required verbosity
buildDDDescription :: EqChunk -> H.Spec
buildDDDescription c = descLines (
  (toVC c):(if verboseDDDescription then (vars (equat c)) else []))

descLines :: [VarChunk] -> H.Spec  
descLines []       = error "No chunks to describe"
descLines (vc:[])  = (H.N (vc ^. symbol) H.:+: 
  (H.S " is the " H.:+: (spec (vc ^. descr))))
descLines (vc:vcs) = descLines (vc:[]) H.:+: H.HARDNL H.:+: descLines vcs
