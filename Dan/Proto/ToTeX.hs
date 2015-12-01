{-# OPTIONS -Wall #-} 
module ToTeX where

import ASTInternal (Expr(..))
import Spec
import qualified ASTTeX as T
-- import Config (datadefnFields)
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
spec (S s) = T.S (s)
-- spec (a@(U Circle) :+: b) = spec a T.:+: T.S " " T.:+: spec b
spec (a :+: b) = spec a T.:+: spec b
spec (a :-: b) = spec a T.:-: spec b
spec (a :^: b) = spec a T.:^: spec b
spec (a :/: b) = spec a T.:/: spec b
spec Empty = T.S ""
-- spec (U u) = convertUnicode u
spec (U u) = T.S $ render TeX u
-- spec (M m) = T.M m
-- spec (CS c) = T.CS c
spec (F f s) = spec $ format f s
-- spec (D cs) = T.D cs

{-
convertUnicode :: Unicode -> T.Spec
convertUnicode Tau_L = T.S $ "\\tau"
convertUnicode Tau_U = T.S $ "\\Tau"
convertUnicode Alpha_L = T.S $ "\\alpha"
convertUnicode Alpha_U = T.S $ "\\Alpha"
convertUnicode Circle = T.S $ "\\circ"
convertUnicode Delta_U = T.S $ "\\Delta"
convertUnicode Delta_L = T.S $ "\\delta"
convertUnicode Rho_U = T.S $ "\\Rho"
convertUnicode Rho_L = T.S $ "\\rho"
convertUnicode Phi_U = T.S $ "\\Phi"
convertUnicode Phi_L = T.S $ "\\phi"
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
