module Language.Drasil.TeX.Import where

import Control.Lens hiding ((:>),(:<))

import Language.Drasil.Expr (Expr(..), Relation, UFunc(..))
import Language.Drasil.Expr.Extract
import Language.Drasil.Spec
import qualified Language.Drasil.TeX.AST as T
import Language.Drasil.Unicode (Special(Partial))
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.Relation
import Language.Drasil.Chunk.Module
import Language.Drasil.Unit
import Language.Drasil.Chunk
import Language.Drasil.Config (verboseDDDescription, numberedDDEquations, numberedTMEquations)
import Language.Drasil.Document
import Language.Drasil.Symbol
import Language.Drasil.Reference
import Language.Drasil.Printing.Helpers
import Data.List (intersperse)

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
expr x@(_ := _)   = rel x
expr x@(_ :> _)   = rel x
expr x@(_ :< _)   = rel x
expr (UnaryOp u e) = T.Op (ufunc u) [expr e]
expr (Grouping e) = T.Grouping (expr e)

ufunc :: UFunc -> T.Function
ufunc Log = T.Log
ufunc (Summation (i,n)) = T.Summation (fmap expr i, fmap expr n)
ufunc Abs = T.Abs
ufunc (Integral (i,n)) = T.Integral (fmap expr i, fmap expr n)
ufunc Sin = T.Sin
ufunc Cos = T.Cos
ufunc Tan = T.Tan
ufunc Sec = T.Sec
ufunc Csc = T.Csc
ufunc Cot = T.Cot

rel :: Relation -> T.Expr
rel (a := b) = T.Eq (expr a) (expr b)
rel (a :< b) = T.Lt (expr a) (expr b)
rel (a :> b) = T.Gt (expr a) (expr b)
rel _ = error "Attempting to use non-Relation Expr in relation context."

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
spec (G g)     = T.G g
spec (Sp s)    = T.Sp s
spec (F f s)   = spec $ accent f s
spec (P s)     = T.N s
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
lay (EqnBlock c)      = T.EqnBlock (T.E (expr c))
lay (CodeBlock c)     = T.CodeBlock c
lay x@(Definition c)  = T.Definition (makePairs c) (spec $ refName x)
lay (Enumeration cs)  = T.List $ makeL cs
lay x@(Figure c f)    = T.Figure (spec (refName x)) (spec c) f
lay x@(Module m)      = T.Module (T.S "") (spec $ refName x)
--  T.Section depth (T.S $ (concat $ intersperse " " $ map capitalize $
  --  words (m ^. name)) ++ " Module") (buildModuleDesc m) (spec $ refName x)

makeL :: ListType -> T.ListType  
makeL (Bullet bs) = T.Enum $ (map item bs)
makeL (Number ns) = T.Item $ (map item ns)
makeL (Simple ps) = T.Simple $ zip (map (spec . fst) ps) (map (item . snd) ps)
makeL (Desc ps)   = T.Desc $ zip (map (spec . fst) ps) (map (item . snd) ps)

item :: ItemType -> T.ItemType
item (Flat i) = T.Flat (spec i)
item (Nested t s) = T.Nested (spec t) (makeL s) 
  
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

--buildModuleDesc :: ModuleChunk -> [T.LayoutObj]
--buildModuleDesc m = [
--  T.List T.Desc
--    [ T.S "Secrets", spec $ secret m,
--      T.S "Services", spec $ m ^. descr,
--      T.S "Implemented By", T.S $ getImp $ imp m
--    ]
--  ]
--  where
--    getImp (Just x) = x
--    getImp Nothing  = "--"
