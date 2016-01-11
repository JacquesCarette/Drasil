module ToTeX where

import ASTInternal (Expr(..))
import Spec
import qualified ASTTeX as T
import Unicode (render)
import Format (Format(TeX), FormatC(..))


expr :: Expr -> T.Expr
expr (V v)    = T.Var v
expr (Dbl d)  = T.Dbl d
expr (Int i)  = T.Int i
expr (a :* b) = T.Mul (expr a) (expr b)
expr (a :+ b) = T.Add (expr a) (expr b)
expr (a :/ b) = T.Frac (replace_divs a) (replace_divs b)
expr (a :^ b) = T.Pow (expr a) (expr b)
expr (a :- b) = T.Sub (expr a) (expr b)
-- expr (C c)    = T.Var (c ^. symbol) -- Need to figure out how to convert a 
                    -- Symbol to a variable within expressions.
expr _ = error "Unimplemented expression transformation in ToTeX."

replace_divs :: Expr -> T.Expr
replace_divs (a :/ b) = T.Div (replace_divs a) (replace_divs b)
replace_divs (a :+ b) = T.Add (replace_divs a) (replace_divs b)
replace_divs (a :* b) = T.Mul (replace_divs a) (replace_divs b)
replace_divs (a :^ b) = T.Pow (replace_divs a) (replace_divs b)
replace_divs (a :- b) = T.Sub (replace_divs a) (replace_divs b)
replace_divs a = expr a

spec :: Spec -> T.Spec
-- spec (E e) = T.E (expr e)
spec (S s) = T.S s
spec (Sy s) = T.Sy s
spec (a :+: b) = spec a T.:+: spec b
spec (a :-: b) = spec a T.:-: spec b
spec (a :^: b) = spec a T.:^: spec b
spec (a :/: b) = spec a T.:/: spec b
spec Empty = T.S ""
spec (U u) = T.S $ render TeX u
-- spec (M m) = T.M m
-- spec (CS c) = T.CS c
spec (F f s) = spec $ format f s
spec (N s) = T.N s
-- spec (D cs) = T.D cs
{-
symbol :: Symbol -> T.Spec
symbol (Atomic s) = T.S s
symbol (Special s) = T.S $ render TeX s
symbol (Catenate s1 s2) = (symbol s1) T.:+: (symbol s2)
--
-- handle the special cases first, then general case
symbol (Corners [] [] [x] [] s) = (symbol s) T.:^: (symbol x)
symbol (Corners [] [] [] [x] s) = (symbol s) T.:-: (symbol x)
symbol (Corners [_] [] [] [] _) = error "rendering of ul prescript"
symbol (Corners [] [_] [] [] _) = error "rendering of ll prescript"
symbol (Corners _ _ _ _ _) = error "rendering of Corners (general)"

unit :: USymb -> T.Spec
unit (UName n) = symbol n
unit (UProd l) = foldr1 (T.:+:) (map unit l)
unit (UPow n p) = (unit n) T.:^: (T.E $ T.Int p)
-}
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
--For printing, will need to use "find" function from Chunk.hs
lay (Table hdr lls) = T.Table $ (map spec hdr) : (map (map spec) lls)
-- lay (Table [a]) = T.Table [map spec a]
lay (Section title layoutComponents) = 
  T.Section (spec title) (createLayout layoutComponents)
lay (Paragraph c) = T.Paragraph (spec c)
lay (EqnBlock c) = T.EqnBlock (spec c)
-- lay (Definition Data c) = T.Definition Data c datadefnFields 
  --Temp removal while propagating chunk changes.
-- lay (Definition Literate _) = error "missing case in lay"
