module Language.Drasil.TeX.Import where

import Control.Lens

import Language.Drasil.Expr (Expr(..), Relation(..))
import Language.Drasil.Expr.Extract
import Language.Drasil.Spec
import qualified Language.Drasil.TeX.AST as T
import Language.Drasil.Unicode (render, Partial(..))
import Language.Drasil.Format (Format(TeX))
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.Relation
import Language.Drasil.Unit
import Language.Drasil.Chunk
import Language.Drasil.Config (verboseDDDescription, numberedDDEquations, numberedTMEquations)
import Language.Drasil.Document
import Language.Drasil.Symbol

expr :: Expr -> T.Expr
expr (V v)        = T.Var  v
expr (Dbl d)      = T.Dbl  d
expr (Int i)      = T.Int  i
expr (a :* b)     = T.Mul  (expr a) (expr b)
expr (a :+ b)     = T.Add  (expr a) (expr b)
expr (a :/ b)     = T.Frac (replace_divs a) (replace_divs b)
expr (a :^ b)     = T.Pow  (expr a) (expr b)
expr (a :- b)     = T.Sub  (expr a) (expr b)
expr (a :. b)     = T.Dot  (expr a) (expr b)
expr (Neg a)      = T.Neg  (expr a)
expr (C c)        = T.Sym  (c ^. symbol)
expr (Deriv a b)  = T.Frac (T.Mul (T.Sym (Special Partial)) (expr a))
                           (T.Mul (T.Sym (Special Partial)) (expr b))
expr (FCall f x)  = T.Call (expr f) (map expr x)
expr (Case ps)    = if length ps < 2 then 
                    error "Attempting to use multi-case expr incorrectly"
                    else T.Case (zip (map (expr . fst) ps) (map (rel . snd) ps))

rel :: Relation -> T.Expr
rel (a := b) = T.Eq (expr a) (expr b)
rel (a :< b) = T.Lt (expr a) (expr b)
rel (a :> b) = T.Gt (expr a) (expr b)
--rel _ = error "unimplemented relation, see Language.Drasil.TeX.Import"

replace_divs :: Expr -> T.Expr
replace_divs (a :/ b) = T.Div (replace_divs a) (replace_divs b)
replace_divs (a :+ b) = T.Add (replace_divs a) (replace_divs b)
replace_divs (a :* b) = T.Mul (replace_divs a) (replace_divs b)
replace_divs (a :^ b) = T.Pow (replace_divs a) (replace_divs b)
replace_divs (a :- b) = T.Sub (replace_divs a) (replace_divs b)
replace_divs a        = expr a

spec :: Sentence -> T.Spec
spec (S s)     = T.S s
spec (Sy s)    = T.Sy s
spec (a :+: b) = spec a T.:+: spec b
spec (U u)     = T.S $ render TeX u --TODO: Need to know context before printing.
spec (F f s)   = spec $ accent f s
-- spec (N s)     = T.N s
spec (Ref t r)   = T.Ref t (spec r)
spec (Quote q) = T.S "``" T.:+: spec q T.:+: T.S "\""

decorate :: Decoration -> Sentence -> Sentence
decorate Hat    s = S "\\hat{" :+: s :+: S "}"
decorate Vector s = S "\\bf{" :+: s :+: S "}"

accent :: Accent -> Char -> Sentence
accent Grave  s = S $ "\\`{" ++ (s : "}")
accent Acute  s = S $ "\\'{" ++ (s : "}")

makeDocument :: Document -> T.Document
makeDocument (Document title author sections) = 
  T.Document (spec title) (spec author) (createLayout sections)

layout :: SecCons -> T.LayoutObj
layout (Sub s) = sec s
layout (Con c) = lay c

createLayout :: Sections -> [T.LayoutObj]
createLayout = map sec

sec :: Section -> T.LayoutObj
sec x@(Section depth title contents) = 
  T.Section depth (spec title) (map layout contents) (spec $ refName x)

lay :: Contents -> T.LayoutObj
lay x@(Table hdr lls t b) 
  | length hdr == length (head lls) = T.Table ((map spec hdr) : 
      (map (map spec) lls)) (spec (refName x)) b (spec t)
  | otherwise = error $ "Attempting to make table with " ++ show (length hdr) ++
                        " headers, but data contains " ++ 
                        show (length (head lls)) ++ " columns."
lay (Paragraph c)     = T.Paragraph (spec c)
lay (EqnBlock c)      = T.EqnBlock (spec c)
lay (CodeBlock c)     = T.CodeBlock c
lay x@(Definition c)  = T.Definition (makePairs c) (spec $ refName x)
lay (BulletList cs)   = T.List $ T.Item (map lay cs)
lay (NumberedList cs) = T.List $ T.Enum (map lay cs)
lay (SimpleList cs)   = T.List $ T.Simple (concat $ 
                          map (\(f,s) -> [(spec f, lay s)]) cs)
lay x@(Figure c f)    = T.Figure (spec (refName x)) (spec c) f
  
makePairs :: DType -> [(String,T.LayoutObj)]
makePairs (Data c) = [
  ("Label",       T.Paragraph $ T.N $ c ^. symbol),
  ("Units",       T.Paragraph $ T.Sy $ c ^. unit),
  ("Equation",    eqnStyleDD $ buildEqn c),
  ("Description", T.Paragraph (buildDDDescription c))
  ]
makePairs (Theory c) = [
  ("Label",       T.Paragraph $ T.S $ c ^. name),
  ("Equation",    eqnStyleTM $ T.E (rel (relat c))),
  ("Description", T.Paragraph (spec (c ^. descr)))
  ]
makePairs General = error "Not yet implemented"

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
  (toVC c):(if verboseDDDescription then vars $ equat c else []))

descLines :: [VarChunk] -> T.Spec  
descLines []       = error "No chunks to describe"
descLines (vc:[])  = (T.N (vc ^. symbol) T.:+: (T.S " is the " T.:+: 
                      (spec (vc ^. descr))))
descLines (vc:vcs) = descLines (vc:[]) T.:+: T.HARDNL T.:+: descLines vcs
