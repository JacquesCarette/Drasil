{-# Language FlexibleInstances #-}
module Language.Drasil.TeX.Print where

import Prelude hiding (print)
import Data.List (intersperse)
import Text.PrettyPrint hiding (render,empty,($+$),vcat,(<>))
import qualified Text.PrettyPrint as TP

import Control.Monad.Reader 
import Control.Applicative hiding (empty)

import Language.Drasil.TeX.AST
import Language.Drasil.TeX.Import hiding (sec)
import qualified Language.Drasil.Output.Formats as A
import Language.Drasil.Spec (USymb(..), RefType(..))
import Language.Drasil.Config (srsTeXParams, lpmTeXParams, colAwidth, colBwidth,
              numberedSections, SRSParams(..), LPMParams(..))
import Language.Drasil.Printing.Helpers
import Language.Drasil.TeX.Helpers
import Language.Drasil.TeX.Monad
import Language.Drasil.Unicode
import Language.Drasil.Format (Format(TeX))
import Language.Drasil.Symbol (Symbol(..),Decoration(..))
import Language.Drasil.CCode.Print (printCode)
import qualified Language.Drasil.Document as L

genTeX :: A.DocType -> L.Document -> Doc
genTeX typ doc = runPrint (build typ $ makeDocument doc) Text

build :: A.DocType -> Document -> D
build (A.SRS _) doc   = buildSRS srsTeXParams doc
build (A.LPM _) doc   = buildLPM lpmTeXParams doc
build (A.Code _) _    = error "Unimplemented (See PrintTeX)"
build (A.Website _) _ = error "Cannot use TeX to typeset Website" --Can't happen

buildSRS :: SRSParams -> Document -> D
buildSRS (SRSParams (A.DocClass sb b1) (A.UsePackages ps)) 
         (Document t a c) =
  docclass sb b1 %%
  listpackages ps %% 
  title (pCon Plain t) %% 
  author (p_spec a) %% 
  document (maketitle %% print c)

buildLPM :: LPMParams -> Document -> D
buildLPM  (LPMParams (A.DocClass sb b1) (A.UsePackages ps) (A.ExDoc f n)) 
          (Document t a c) =
  docclass sb b1 %%
  listpackages ps %%
  exdoc f n %%
  title (p_spec t) %%
  author (p_spec a) %%
  document (maketitle %% print c)

listpackages :: [String] -> D
listpackages lp = foldr (%%) empty $ map usepackage lp

lo :: LayoutObj -> D
lo (Section d t con l)     = sec d (pCon Plain t) %% label (pCon Plain l) 
                                  %% print con
lo (Paragraph contents)    = pure $ text (pCon Plain contents)
lo (EqnBlock contents)     = pure $ text $ makeEquation contents
lo (Table rows r bl t)     = makeTable rows (pCon Plain r) bl (pCon Plain t)
lo (CodeBlock c)           = code $ pure $ printCode c
lo (Definition ssPs l)     = makeDefn ssPs (pCon Plain l)
lo (List lt)               = makeList lt
lo (Figure r c f)          = makeFigure (pCon Plain r) (pCon Plain c) f

print :: [LayoutObj] -> D
print l = foldr ($+$) empty $ map lo l

-----------------------------------------------------------------
------------------ SPEC PRINTING----------------------------
-----------------------------------------------------------------

p_spec :: Spec -> String
p_spec (E ex)      = p_expr ex
p_spec (a :+: s)   = p_spec a ++ p_spec s
p_spec (a :-: s)   = p_spec a ++ "_" ++ brace (p_spec s)
p_spec (a :^: s)   = p_spec a ++ "^" ++ brace (p_spec s)
p_spec (a :/: s)   = "\\frac" ++ brace (p_spec a) ++ brace (p_spec s)
p_spec (S s)       = s
p_spec (N s)       = symbol s
p_spec (Sy s)      = runReader (uSymbPrint s) Plain
p_spec HARDNL      = "\\newline"
p_spec (Ref t@Sect r) = if numberedSections 
                       then show t ++ "~\\ref" ++ brace (p_spec r) 
                       else "\\hyperref" ++ sqbrac (p_spec r) ++ 
                        brace (show t ++ "~" ++ p_spec r)
p_spec (Ref t@Def r) = "\\hyperref" ++ sqbrac (p_spec r) ++ 
                        brace (show t ++ "~" ++ p_spec r)
p_spec (Ref t r)   = show t ++ "~\\ref" ++ brace (p_spec r)

symbol :: Symbol -> String
symbol (Atomic s)       = s
symbol (Special s)      = render TeX s
symbol (Concat sl) = foldr (++) "" $ map symbol sl
--
-- handle the special cases first, then general case
symbol (Corners [] [] [x] [] s) = (symbol s) ++"^"++ brace (symbol x)
symbol (Corners [] [] [] [x] s) = (symbol s) ++"_"++ brace (symbol x)
symbol (Corners [_] [] [] [] _) = error "rendering of ul prescript"
symbol (Corners [] [_] [] [] _) = error "rendering of ll prescript"
symbol (Corners _ _ _ _ _)      = error "rendering of Corners (general)"
symbol (Atop f s) = sFormat f s

sFormat :: Decoration -> Symbol -> String
sFormat Hat    s = "\\hat{" ++ symbol s ++ "}"
sFormat Vector s = "\\mathbf{" ++ symbol s ++ "}"

-----------------------------------------------------------------
------------------ EXPRESSION PRINTING----------------------
-----------------------------------------------------------------
p_expr :: Expr -> String
p_expr (Var v)    = v
p_expr (Dbl d)    = show d
p_expr (Int i)    = show i
p_expr (Add x y)  = p_expr x ++ "+" ++ p_expr y
p_expr (Sub x y)  = p_expr x ++ "-" ++ p_expr y
p_expr (Mul x y)  = mul x y
p_expr (Frac n d) = fraction (p_expr n) (p_expr d) --Found in Helpers
p_expr (Div n d)  = divide n d
p_expr (Pow x y)  = p_expr x ++ "^" ++ brace (p_expr y)
p_expr (Sym s)    = symbol s
p_expr (Eq x y)   = p_expr x ++ "=" ++ p_expr y
p_expr (Lt x y)   = p_expr x ++ "<" ++ p_expr y
p_expr (Gt x y)   = p_expr x ++ ">" ++ p_expr y
p_expr (Dot x y)  = p_expr x ++ "\\cdot{}" ++ p_expr y
p_expr (Neg x)    = neg x
p_expr (Call f x) = p_expr f ++ paren (concat $ intersperse "," $ map p_expr x)
p_expr (Case ps)  = "\\begin{cases}\n" ++ cases ps ++ "\n\\end{cases}"
p_expr (Op f es)  = p_op f es
p_expr (Grouping x) = paren (p_expr x)

mul :: Expr -> Expr -> String
mul x@(Add _ _) y = paren (p_expr x) ++ p_expr y
mul x@(Sub _ _) y = paren (p_expr x) ++ p_expr y
mul x y@(Dbl _)   = p_expr x ++ "*" ++ p_expr y
mul x y@(Int _)   = p_expr x ++ "*" ++ p_expr y
mul x y@(Add _ _) = p_expr x ++ paren (p_expr y)
mul x y@(Sub _ _) = p_expr x ++ paren (p_expr y)
mul x@(Sym (Concat _)) y = p_expr x ++ "*" ++ p_expr y
mul x y@(Sym (Concat _)) = p_expr x ++ "*" ++ p_expr y
mul x@(Sym (Atomic s)) y = if length s > 1 then p_expr x ++ "*" ++ p_expr y else
                            p_expr x ++ p_expr y
mul x y@(Sym (Atomic s)) = if length s > 1 then p_expr x ++ "*" ++ p_expr y else
                            p_expr x ++ p_expr y
mul x y           = p_expr x ++ p_expr y

divide :: Expr -> Expr -> String
divide n d@(Add _ _) = p_expr n ++ "/" ++ paren (p_expr d)
divide n d@(Sub _ _) = p_expr n ++ "/" ++ paren (p_expr d)
divide n@(Add _ _) d = p_expr n ++ "/" ++ paren (p_expr d)
divide n@(Sub _ _) d = p_expr n ++ "/" ++ paren (p_expr d)
divide n d = p_expr n ++ "/" ++ p_expr d

neg :: Expr -> String
neg x@(Var _) = "-" ++ p_expr x
neg x@(Dbl _) = "-" ++ p_expr x
neg x@(Int _) = "-" ++ p_expr x
neg x@(Sym _) = "-" ++ p_expr x
neg   (Neg n) = p_expr n
neg x         = paren ("-" ++ p_expr x)

cases :: [(Expr,Expr)] -> String
cases []     = error "Attempt to create case expression without cases"
cases (p:[]) = p_expr (fst p) ++ ", & " ++ p_expr (snd p)
cases (p:ps) = p_expr (fst p) ++ ", & " ++ p_expr (snd p) ++ "\\\\\n" ++ cases ps
-----------------------------------------------------------------
------------------ TABLE PRINTING---------------------------
-----------------------------------------------------------------
  
makeTable :: [[Spec]] -> String -> Bool -> String -> D
makeTable lls r bool t = 
  pure (text (("\\begin{" ++ lt ++ "}") ++ brace (header lls)))
  %% makeRows lls %% (if bool then caption t else empty) %%
  label r %% (pure $ text ("\\end{" ++ lt ++ "}"))
  where header l = concat (replicate ((length (head l))-1) "l ") ++ "l"
--                    ++ "p" ++ brace (show tableWidth ++ "cm")
        lt = "longtable" ++ (if not bool then "*" else "")
        
makeRows :: [[Spec]] -> D
makeRows []     = empty
makeRows (c:cs) = pure (text (makeColumns c) $$ dbs) %% makeRows cs

makeColumns :: [Spec] -> String
makeColumns ls = (concat $ intersperse " & " $ map (pCon Plain) ls)

-----------------------------------------------------------------
------------------ READER-----------------------------------
-----------------------------------------------------------------

data Context = Equation | EqnB | Plain deriving (Show, Eq)

getCon :: Spec -> Context
getCon (a :+: _) = getCon a
getCon (S _)     = Plain
getCon (E _)     = Equation
getCon (_ :-: _) = Equation --Sub/superscripts must be in Equation ctxt.
getCon (_ :^: _) = Equation
getCon (_ :/: _) = Equation -- Fractions are always equations.
getCon (Sy _)    = Plain
getCon (N _)     = Equation
getCon HARDNL    = Plain
getCon (Ref _ _)   = Plain


lPrint :: Spec -> Reader Context String
lPrint t@(s1 :+: s2) = do
  c <- ask
  let ca = getCon s1
  let cb = getCon s2
  case c of
    EqnB -> return $ makeEquation t
    _    -> return $ pCon ca s1 ++ pCon cb s2
    
lPrint t = do
  c <- ask
  let ct = getCon t
  case c of
    EqnB -> return $ makeEquation t
    _    ->
      case ct of
        Equation -> return $ dollar (p_spec t)
        Plain    -> return $ p_spec t
        EqnB     -> return $ makeEquation t 
          --This will never run right now, but maybe eventually.

pCon :: Context -> Spec -> String
pCon = \c t -> runReader (lPrint t) c

uSymbPrint :: USymb -> Reader Context String --To fix unit printing will need this.
uSymbPrint (UName n) = do
  c <- ask
  let cn = getSyCon n
  if c == cn then
    return $ symbol n
  else
    case cn of
      Equation -> return $ dollar $ symbol n 
      _        -> return $ symbol n
uSymbPrint (UProd l) = do
  c <- ask
  return $ foldr1 (\x -> (x++)) 
    (map ((\ctxt t -> runReader t ctxt) c) (map uSymbPrint l))
uSymbPrint (UPow n p) = do
  c <- ask
  case c of
    Plain -> return $ runReader (uSymbPrint n) c ++ dollar ("^" ++ brace (show p))
    _     -> return $ runReader (uSymbPrint n) c ++ "^" ++ brace (show p)
uSymbPrint (UDiv n d) = do
  c <- ask
  case d of -- 4 possible cases, 2 need parentheses, 2 don't
    UProd _ -> return $ 
      runReader (uSymbPrint n) c ++ "/" ++ paren (runReader (uSymbPrint d) c)
    UDiv _ _ -> return $
      runReader (uSymbPrint n) c ++ "/" ++ paren (runReader (uSymbPrint d) c)
    _ -> return $ runReader (uSymbPrint n) c ++ "/" ++ runReader (uSymbPrint d) c
    
getSyCon :: Symbol -> Context
getSyCon (Atomic _)          = Plain
--getSyCon (Special Circle)  = Equation
  -- TODO: Need to figure this out, or figure out how to print catenations in a 
  --       better way.
getSyCon (Special _)         = Plain
getSyCon (Concat [])         = Plain
getSyCon (Concat (s:_))      = getSyCon s
getSyCon (Corners _ _ _ _ s) = getSyCon s
getSyCon (Atop _ s)          = getSyCon s

-----------------------------------------------------------------
------------------ DATA DEFINITION PRINTING-----------------
-----------------------------------------------------------------

makeDefn :: [(String,LayoutObj)] -> String -> D
makeDefn [] _ = error "Empty definition"
makeDefn ps l = beginDefn %% makeDefTable ps l %% endDefn

beginDefn :: D
beginDefn = (pure $ text "~") <> newline
  %% (pure $ text "\\noindent \\begin{minipage}{\\textwidth}")

endDefn :: D
endDefn = pure $ text "\\end{minipage}" TP.<> dbs

makeDefTable :: [(String,LayoutObj)] -> String -> D
makeDefTable [] _ = error "Trying to make empty Data Defn"
makeDefTable ps l = vcat [
  pure $ text $ "\\begin{tabular}{p{"++show colAwidth++"\\textwidth} p{"++show colBwidth++"\\textwidth}}",
  pure $ text "\\toprule \\textbf{Refname} & \\textbf{" TP.<> text l TP.<> text "}",
  label l,
  makeDRows ps, 
  pure $ dbs <+> text ("\\bottomrule \\end{tabular}")
  ]

makeDRows :: [(String,LayoutObj)] -> D
makeDRows []         = error "No fields to create Defn table"
makeDRows ((f,d):[]) = dBoilerplate %% (pure $ text (f ++ " & ")) <> lo d
makeDRows ((f,d):ps) = dBoilerplate %% (pure $ text (f ++ " & ")) <> lo d 
                       %% makeDRows ps
dBoilerplate :: D
dBoilerplate = pure $ dbs <+> text "\\midrule" <+> dbs 

-----------------------------------------------------------------
------------------ EQUATION PRINTING------------------------
-----------------------------------------------------------------

makeEquation :: Spec -> String
makeEquation contents = 
  ("\\begin{equation}" ++ p_spec contents ++ "\\end{equation}")
  --TODO: Add auto-generated labels -> Need to be able to ensure labeling based
  --  on chunk (i.e. "eq:h_g" for h_g = ...
  
-----------------------------------------------------------------
------------------ LIST PRINTING----------------------------
-----------------------------------------------------------------

makeList :: ListType -> D
makeList (Simple items) = itemize   $ vcat (sim_item items)
makeList (Item items)   = itemize   $ vcat (map p_item items)
makeList (Enum items)   = enumerate $ vcat (map p_item items)

p_item :: ItemType -> D
p_item (Flat s) = pure $ text ("\\item ") TP.<> text (pCon Plain s)
p_item (Nested t s) = vcat [pure $ text ("\\item ") TP.<> text (pCon Plain t), makeList s]

sim_item :: [(Spec,ItemType)] -> [D]
sim_item [] = [empty]
sim_item ((x,y):zs) = (pure $ text ("\\item[" ++ pCon Plain x ++ ":] ")) <> sp_item y :
  sim_item zs
    where sp_item (Flat s) = pure $ text (pCon Plain s)
          sp_item (Nested t s) = vcat [pure $ text (pCon Plain t), makeList s]
  
-----------------------------------------------------------------
------------------ FIGURE PRINTING--------------------------
-----------------------------------------------------------------

makeFigure :: String -> String -> String -> D
makeFigure r c f = 
  figure (center (
  vcat [
    includegraphics f,
    caption c,
    label r
  ] ) )

-----------------------------------------------------------------
------------------ EXPR OP PRINTING-------------------------
-----------------------------------------------------------------
p_op :: Function -> [Expr] -> String
p_op f@(Summation (i,n)) (x:[]) = show f ++ makeBounds (i,n) ++ brace (p_expr x)
p_op (Summation _) _ = error "Something went wrong with a summation"
p_op f@(Integral (i,n)) (x:[]) = show f ++ makeBounds (i,n) ++ brace (p_expr x)
p_op (Integral _) _  = error "Something went wrong with an integral" 
p_op Abs (x:[]) = "|" ++ p_expr x ++ "|"
p_op Abs _ = error "Abs should only take one expr."
p_op f (x:[]) = show f ++ paren (p_expr x) --Unary ops, this will change once more complicated functions appear.
p_op _ _ = error "Something went wrong with an operation"

makeBounds :: (Maybe Expr, Maybe Expr) -> String
makeBounds (Nothing,Nothing) = ""
makeBounds (Nothing,Just n) = "^" ++ brace (p_expr n)
makeBounds (Just i, Nothing) = "_" ++ brace (p_expr i)
makeBounds (Just i, Just n) = "_" ++ brace (p_expr i) ++ "^" ++ brace (p_expr n)
