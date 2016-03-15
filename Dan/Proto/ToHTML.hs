{-# OPTIONS -Wall #-} 
module ToHTML where

import ASTInternal (Expr(..))
import Spec
import qualified ASTHTML as H
import Unicode (render, Delta(..))
import Format (Format(HTML), FormatC(..))
import EqChunk
import Unit
import Chunk
import Control.Lens
import ExprTools
import Config (verboseDDDescription)
import LayoutObjs
import Symbol


expr :: Expr -> H.Expr
expr (V v)    = H.Var   v
expr (Dbl d)  = H.Dbl   d
expr (Int i)  = H.Int   i
expr (a :* b) = H.Mul   (expr a) (expr b)
expr (a :+ b) = H.Add   (expr a) (expr b)
expr (a :/ b) = H.Frac  (replace_divs a) (replace_divs b)
expr (a :^ b) = H.Pow   (expr a) (expr b)
expr (a :- b) = H.Sub   (expr a) (expr b)
expr (a := b) = H.Eq    (expr a) (expr b)
expr (a :. b) = H.Dot   (expr a) (expr b)
expr (Neg a)  = H.Neg   (expr a)
expr (Deriv a b) = H.Frac (H.Mul (H.Sym (Special Delta_L)) (expr a)) 
                          (H.Mul (H.Sym (Special Delta_L)) (expr b))
expr (C c)    = H.Sym   (c ^. symbol)
--expr _ = error "Unimplemented expression transformation in ToTeX."

replace_divs :: Expr -> H.Expr
replace_divs (a :/ b) = H.Div (replace_divs a) (replace_divs b)
replace_divs (a :+ b) = H.Add (replace_divs a) (replace_divs b)
replace_divs (a :* b) = H.Mul (replace_divs a) (replace_divs b)
replace_divs (a :^ b) = H.Pow (replace_divs a) (replace_divs b)
replace_divs (a :- b) = H.Sub (replace_divs a) (replace_divs b)
replace_divs a        = expr a

spec :: Spec -> H.Spec
spec (S s)     = H.S s
spec (Sy s)    = H.Sy s
spec (a :+: b) = spec a H.:+: spec b
spec (a :-: b) = spec a H.:-: spec b
spec (a :^: b) = spec a H.:^: spec b
spec (a :/: b) = spec a H.:/: spec b
spec Empty     = H.S ""
spec (U u)     = H.S $ render HTML u
spec (F f s)   = spec $ format f s
spec (N s)     = H.N s

format :: FormatC -> Spec -> Spec
format Hat    s = s :+: S "&#770;" 
format Vector s = S "<b>" :+: s :+: S "</b>"
format Grave  s = S "&" :+: s :+: S "grave;" --Only works on vowels.
format Acute  s = S "&" :+: s :+: S "acute;" --Only works on vowels.

makeDocument :: Document -> H.Document
makeDocument (Document title author layout) = 
  H.Document (spec title) (spec author) (createLayout layout)

createLayout :: [LayoutObj] -> [H.LayoutObj]
createLayout []     = []
createLayout (l:[]) = [lay l]
createLayout (l:ls) = lay l : createLayout ls

lay :: LayoutObj -> H.LayoutObj
lay (Table hdr lls)       = 
  H.Table ["table"] $ (map spec hdr) : (map (map spec) lls)
lay (Section depth title contents) = 
  H.HDiv [(concat $ replicate depth "sub") ++ "section"] 
  ((H.Header (depth+2) (spec title)):(createLayout contents))
lay (Paragraph c)         = H.Paragraph (spec c)
lay (EqnBlock c)          = H.HDiv ["equation"] [H.Tagless (spec c)]
lay (CodeBlock c)         = H.CodeBlock c
lay (Definition dt c)     = H.Definition dt $ makePairs dt c
lay (BulletList cs)       = H.List H.Unordered $ map spec cs
lay (NumberedList cs)     = H.List H.Ordered $ map spec cs

makePairs :: DType -> EqChunk -> [(String,H.LayoutObj)]
makePairs Data c = [
  ("Label",       H.Paragraph $ H.S "DD: " H.:+: (H.N $ c ^. symbol)),
  ("Units",       H.Paragraph $ H.Sy $ c ^. unit),
  ("Equation",    H.HDiv ["equation"] [H.Tagless (buildEqn c)]),
  ("Description", H.Paragraph (buildDDDescription c))
  ]
makePairs Theory c = [
  ("Label",       H.Paragraph $ H.S "T: " H.:+: (H.N $ c ^. symbol)),
  ("Equation",    H.HDiv ["equation"] [H.Tagless (H.E (expr (equat c)))]),
  ("Description", H.Paragraph (spec (c ^. descr)))
  ]
makePairs General _ = error "Not yet implemented"
  
buildEqn :: EqChunk -> H.Spec  
buildEqn c = H.N (c ^. symbol) H.:+: H.S " = " H.:+: H.E (expr (equat c))

-- Build descriptions in data defs based on required verbosity
buildDDDescription :: EqChunk -> H.Spec
buildDDDescription c = descLines (
  (toVC c):(if verboseDDDescription then (get_VCs (equat c)) else []))

descLines :: [VarChunk] -> H.Spec  
descLines []       = error "No chunks to describe"
descLines (vc:[])  = (H.N (vc ^. symbol) H.:+: 
  (H.S " is the " H.:+: (spec (vc ^. descr))))
descLines (vc:vcs) = descLines (vc:[]) H.:+: H.HARDNL H.:+: descLines vcs
