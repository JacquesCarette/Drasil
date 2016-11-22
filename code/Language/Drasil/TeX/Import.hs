module Language.Drasil.TeX.Import where

import Control.Lens hiding ((:>),(:<))

import Language.Drasil.Expr (Expr(..), Relation, UFunc(..), BiFunc(..),
                             Bound(..),DerivType(..))
import Language.Drasil.Expr.Extract
import Language.Drasil.Spec
import qualified Language.Drasil.TeX.AST as T
import Language.Drasil.Unicode (Special(Partial))
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.Relation
import Language.Drasil.Chunk.Module
import Language.Drasil.Chunk
import Language.Drasil.Config (verboseDDDescription, numberedDDEquations, numberedTMEquations)
import Language.Drasil.Document
import Language.Drasil.Symbol
import Language.Drasil.Misc (unit'2Contents)
import Language.Drasil.SymbolAlphabet

expr :: Expr -> T.Expr
expr (V v)             = T.Var  v
expr (Dbl d)           = T.Dbl  d
expr (Int i)           = T.Int  i
expr (a :* b)          = T.Mul  (expr a) (expr b)
expr (a :+ b)          = T.Add  (expr a) (expr b)
expr (a :/ b)          = T.Frac (replace_divs a) (replace_divs b)
expr (a :^ b)          = T.Pow  (expr a) (expr b)
expr (a :- b)          = T.Sub  (expr a) (expr b)
expr (a :. b)          = T.Dot  (expr a) (expr b)
expr (Neg a)           = T.Neg  (expr a)
expr (C c)             = T.Sym  (c ^. symbol)
expr (Deriv Part a 1)  = T.Mul (T.Sym (Special Partial)) (expr a)
expr (Deriv Total a 1) = T.Mul (T.Sym lD) (expr a)
expr (Deriv Part a b)  = T.Frac (T.Mul (T.Sym (Special Partial)) (expr a))
                           (T.Mul (T.Sym (Special Partial)) (expr b))
expr (Deriv Total a b) = T.Frac (T.Mul (T.Sym lD) (expr a))
                           (T.Mul (T.Sym lD) (expr b))
expr (FCall f x)       = T.Call (expr f) (map expr x)
expr (Case ps)         = if length ps < 2 then 
                    error "Attempting to use multi-case expr incorrectly"
                    else T.Case (zip (map (expr . fst) ps) (map (rel . snd) ps))
expr x@(_ := _)        = rel x
expr x@(_ :> _)        = rel x
expr x@(_ :< _)        = rel x
expr (UnaryOp u)       = (\(x,y) -> T.Op x [y]) (ufunc u)
expr (Grouping e)      = T.Grouping (expr e)
expr (BinaryOp b)      = (\(x,y) -> T.Op x y) (bfunc b)

ufunc :: UFunc -> (T.Function, T.Expr)
ufunc (Log e) = (T.Log, expr e)
ufunc (Summation (Just (s, Low v, High h)) e) = 
  (T.Summation (Just ((s, expr v), expr h)), expr e)
ufunc (Summation Nothing e) = (T.Summation Nothing, expr e)
ufunc (Summation _ _) = error "HTML/Import.hs Incorrect use of Summation"
ufunc (Abs e) = (T.Abs, expr e)
ufunc i@(Integral _ _ _) = integral i
ufunc (Sin e) = (T.Sin, expr e)
ufunc (Cos e) = (T.Cos, expr e)
ufunc (Tan e) = (T.Tan, expr e)
ufunc (Sec e) = (T.Sec, expr e)
ufunc (Csc e) = (T.Csc, expr e)
ufunc (Cot e) = (T.Cot, expr e)

bfunc :: BiFunc -> (T.Function, [T.Expr])
bfunc (Cross e1 e2) = (T.Cross, map expr [e1,e2])

rel :: Relation -> T.Expr
rel (a := b) = T.Eq (expr a) (expr b)
rel (a :< b) = T.Lt (expr a) (expr b)
rel (a :> b) = T.Gt (expr a) (expr b)
rel _ = error "Attempting to use non-Relation Expr in relation context."

integral :: UFunc -> (T.Function, T.Expr)
integral (Integral (Just (Low v), Just (High h)) e wrtc) = 
  (T.Integral (Just (expr v), Just (expr h)) (int_wrt wrtc), expr e)
integral (Integral (Just (High h), Just (Low v)) e wrtc) = 
  (T.Integral (Just (expr v), Just (expr h)) (int_wrt wrtc), expr e)
integral (Integral (Just (Low v), Nothing) e wrtc) = 
  (T.Integral (Just (expr v), Nothing) (int_wrt wrtc), expr e)
integral (Integral (Nothing, Just (Low v)) e wrtc) = 
  (T.Integral (Just (expr v), Nothing) (int_wrt wrtc), expr e)
integral (Integral (Just (High h), Nothing) e wrtc) = 
  (T.Integral (Nothing, Just (expr h)) (int_wrt wrtc), expr e)
integral (Integral (Nothing, Just (High h)) e wrtc) = 
  (T.Integral (Nothing, Just (expr h)) (int_wrt wrtc), expr e)
integral (Integral (Nothing, Nothing) e wrtc) = 
  (T.Integral (Nothing, Nothing) (int_wrt wrtc), expr e)
integral _ = error "TeX/Import.hs Incorrect use of Integral"

int_wrt :: Quantity c => c -> T.Expr
int_wrt wrtc = (expr (Deriv Total (C wrtc) 1))

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
spec (Ref t r) = T.Ref t (spec r)
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

layout :: Int -> SecCons -> T.LayoutObj
layout currDepth (Sub s) = sec (currDepth+1) s
layout _         (Con c) = lay c

createLayout :: Sections -> [T.LayoutObj]
createLayout = map (sec 0)

sec :: Int -> Section -> T.LayoutObj
sec depth x@(Section title contents) = 
  T.Section depth (spec title) (map (layout depth) contents) (spec $ refName x)

lay :: Contents -> T.LayoutObj
lay x@(Table hdr lls t b) 
  | null lls || length hdr == length (head lls) = T.Table ((map spec hdr) :
      (map (map spec) lls)) (spec (refName x)) b (spec t)
  | otherwise = error $ "Attempting to make table with " ++ show (length hdr) ++
                        " headers, but data contains " ++ 
                        show (length (head lls)) ++ " columns."
lay (Paragraph c)         = T.Paragraph (spec c)
lay (EqnBlock c)          = T.EqnBlock (T.E (expr c))
lay (CodeBlock c)         = T.CodeBlock c
lay x@(Definition c)      = T.Definition (makePairs c) (spec $ refName x)
lay (Enumeration cs)      = T.List $ makeL cs
lay x@(Figure c f)        = T.Figure (spec (refName x)) (spec c) f
lay x@(Module m)          = T.Module (formatName m) (spec $ refName x)
lay x@(Requirement r)     = T.Requirement (spec (r ^. descr)) (spec $ refName x)
lay x@(Assumption a)      = T.Assumption (spec (a ^. descr)) (spec $ refName x)
lay x@(LikelyChange lc)   = T.LikelyChange (spec (lc ^. descr))
  (spec $ refName x)
lay x@(UnlikelyChange ucc)= T.UnlikelyChange (spec (ucc ^. descr))
  (spec $ refName x)
lay (UsesHierarchy c)   = T.UsesHierarchy (makeUHPairs c)

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
  ("Units",       T.Paragraph $ spec $ unit'2Contents c),
  ("Equation",    eqnStyleDD $ buildEqn c),
  ("Description", T.Paragraph (buildDDDescription c))
  ]
makePairs (Theory c) = [
  ("Label",       T.Paragraph $ T.S $ c ^. name),
  ("Equation",    eqnStyleTM $ T.E (rel (relat c))),
  ("Description", T.Paragraph (spec (c ^. descr)))
  ]
makePairs General = error "Not yet implemented"

makeUHPairs :: [(ModuleChunk,[ModuleChunk])] -> [(T.Spec,T.Spec)]
makeUHPairs []          = []
makeUHPairs ((m,ms):xs) = (buildPairs m ms) ++ makeUHPairs xs
  where  buildPairs _ []        = []
         buildPairs m1 (m2:ms') = (makeEntry m1, makeEntry m2):buildPairs m1 ms'
           where  makeEntry m' = (spec $ refName $ Module m') T.:+:
                                  (T.S "/") T.:+: (T.S $ formatName m')

-- Toggle equation style
eqnStyleDD :: T.Contents -> T.LayoutObj
eqnStyleDD = if numberedDDEquations then T.EqnBlock else T.Paragraph

eqnStyleTM :: T.Contents -> T.LayoutObj
eqnStyleTM = if numberedTMEquations then T.EqnBlock else T.Paragraph
  
buildEqn :: QDefinition -> T.Spec  
buildEqn c = T.N (c ^. symbol) T.:+: T.S " = " T.:+: T.E (expr (equat c))

-- Build descriptions in data defs based on required verbosity
buildDDDescription :: QDefinition -> T.Spec
buildDDDescription c = descLines (
  (toVC c):(if verboseDDDescription then vars $ equat c else []))

descLines :: [VarChunk] -> T.Spec  
descLines []       = error "No chunks to describe"
descLines (vc:[])  = (T.N (vc ^. symbol) T.:+: (T.S " is the " T.:+: 
                      (spec (vc ^. descr))))
descLines (vc:vcs) = descLines (vc:[]) T.:+: T.HARDNL T.:+: descLines vcs


