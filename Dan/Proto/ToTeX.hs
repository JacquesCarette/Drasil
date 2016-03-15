{-# OPTIONS -Wall #-} 
module ToTeX where

import ASTInternal (Expr(..))
import Spec
import qualified ASTTeX as T
import Unicode (render, Delta(..))
import Format (Format(TeX), FormatC(..))
import EqChunk
import Unit
import Chunk
import Control.Lens
import ExprTools
import Config (verboseDDDescription, numberedDDEquations, numberedTMEquations)
import LayoutObjs
import Symbol


expr :: Expr -> T.Expr
expr (V v)        = T.Var  v
expr (Dbl d)      = T.Dbl  d
expr (Int i)      = T.Int  i
expr (a :* b)     = T.Mul  (expr a) (expr b)
expr (a :+ b)     = T.Add  (expr a) (expr b)
expr (a :/ b)     = T.Frac (replace_divs a) (replace_divs b)
expr (a :^ b)     = T.Pow  (expr a) (expr b)
expr (a :- b)     = T.Sub  (expr a) (expr b)
expr (a := b)     = T.Eq   (expr a) (expr b)
expr (a :. b)     = T.Dot  (expr a) (expr b)
expr (Neg a)      = T.Neg  (expr a)
expr (C c)        = T.Sym  (c ^. symbol)
expr (Deriv a b)  = T.Frac (T.Mul (T.Sym (Special Delta_L)) (expr a))
                           (T.Mul (T.Sym (Special Delta_L)) (expr b))
--expr _ = error "Unimplemented expression transformation in ToTeX."

replace_divs :: Expr -> T.Expr
replace_divs (a :/ b) = T.Div (replace_divs a) (replace_divs b)
replace_divs (a :+ b) = T.Add (replace_divs a) (replace_divs b)
replace_divs (a :* b) = T.Mul (replace_divs a) (replace_divs b)
replace_divs (a :^ b) = T.Pow (replace_divs a) (replace_divs b)
replace_divs (a :- b) = T.Sub (replace_divs a) (replace_divs b)
replace_divs a        = expr a

spec :: Spec -> T.Spec
spec (S s)     = T.S s
spec (Sy s)    = T.Sy s
spec (a :+: b) = spec a T.:+: spec b
spec (a :-: b) = spec a T.:-: spec b
spec (a :^: b) = spec a T.:^: spec b
spec (a :/: b) = spec a T.:/: spec b
spec Empty     = T.S ""
spec (U u)     = T.S $ render TeX u
spec (F f s)   = spec $ format f s
spec (N s)     = T.N s

format :: FormatC -> Spec -> Spec
format Hat    s = S "\\hat{" :+: s :+: S "}"
format Vector s = S "\\bf{" :+: s :+: S "}"
format Grave  s = S "\\`{" :+: s :+: S "}"
format Acute  s = S "\\'{" :+: s :+: S "}"

makeDocument :: Document -> T.Document
makeDocument (Document title author layout) = 
  T.Document (spec title) (spec author) (createLayout layout)

createLayout :: [LayoutObj] -> [T.LayoutObj]
createLayout []     = []
createLayout (l:[]) = [lay l]
createLayout (l:ls) = lay l : createLayout ls

lay :: LayoutObj -> T.LayoutObj
lay (Table hdr lls) 
  | length hdr == length (head lls) = T.Table $ 
                                      (map spec hdr) : (map (map spec) lls)
  | otherwise = error $ "Attempting to make table with " ++ show (length hdr) ++
                        " headers, but data contains " ++ 
                        show (length (head lls)) ++ " columns."
lay (Section depth title layoutComponents) = 
  T.Section depth (spec title) (createLayout layoutComponents)
lay (Paragraph c)       = T.Paragraph (spec c)
lay (EqnBlock c)        = T.EqnBlock (spec c)
lay (CodeBlock c)       = T.CodeBlock c
lay (Definition dt c)   = T.Definition dt $ makePairs dt c
lay (BulletList cs)     = T.List T.Item $ map spec cs
lay (NumberedList cs)   = T.List T.Enum $ map spec cs

makePairs :: DType -> EqChunk -> [(String,T.LayoutObj)]
makePairs Data c = [
  ("Label",       T.Paragraph $ T.N $ c ^. symbol),
  ("Units",       T.Paragraph $ T.Sy $ c ^. unit),
  ("Equation",    eqnStyleDD $ buildEqn c),
  ("Description", T.Paragraph (buildDDDescription c))
  ]
makePairs Theory c = [
  ("Label",       T.Paragraph $ T.N $ c ^. symbol),
  ("Equation",    eqnStyleTM $ T.E (expr (equat c))),
  ("Description", T.Paragraph (spec (c ^. descr)))
  ]
makePairs General _ = error "Not yet implemented"

-- Toggle equation style
eqnStyleDD :: T.Contents -> T.LayoutObj
eqnStyleDD = if numberedDDEquations then T.EqnBlock else T.Paragraph

eqnStyleTM :: T.Contents -> T.LayoutObj
eqnStyleTM = if numberedTMEquations then T.EqnBlock else T.Paragraph
  
buildEqn :: EqChunk -> T.Spec  
buildEqn c = T.N (c ^. symbol) T.:+: T.S " = " T.:+: T.E (expr (equat c))

-- Build descriptions in data defs based on required verbosity
buildDDDescription :: EqChunk -> T.Spec
buildDDDescription c = descLines (
  (toVC c):(if verboseDDDescription then (get_VCs (equat c)) else []))

descLines :: [VarChunk] -> T.Spec  
descLines []       = error "No chunks to describe"
descLines (vc:[])  = (T.N (vc ^. symbol) T.:+: (T.S " is the " T.:+: 
                      (spec (vc ^. descr))))
descLines (vc:vcs) = descLines (vc:[]) T.:+: T.HARDNL T.:+: descLines vcs
