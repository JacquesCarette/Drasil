module Language.Drasil.Plain.Print (
  Linearity(..), exprDoc, sentenceDoc, symbolDoc, unitDoc
) where

import Database.Drasil(ChunkDB)
import Language.Drasil (Sentence, Special(..), Symbol, USymb(..))
import qualified Language.Drasil as L (Expr)
import Language.Drasil.Plain.Helpers (toPlainName)
import Language.Drasil.Printing.AST (Expr(..), Spec(..), Ops(..), Fence(..), 
  OverSymb(..), Fonts(..), Spacing(..))
import Language.Drasil.Printing.Import (expr, spec, symbol)
import Language.Drasil.Printing.PrintingInformation (PrintingInformation(..), 
  defaultConfiguration)

import Prelude hiding ((<>))
import Data.List (partition)
import Text.PrettyPrint.HughesPJ (Doc, (<>), (<+>), brackets, comma, double, 
  doubleQuotes, empty, hcat, hsep, integer, parens, punctuate, space, text, 
  vcat)

data Linearity = Linear | Nonlinear

exprDoc :: ChunkDB -> Linearity -> L.Expr -> Doc
exprDoc db f e = pExprDoc f (expr e (PI db defaultConfiguration))

sentenceDoc :: ChunkDB -> Linearity -> Sentence -> Doc
sentenceDoc db f s = specDoc f (spec (PI db defaultConfiguration) s)

symbolDoc :: Symbol -> Doc
symbolDoc s = pExprDoc Linear (symbol s)

pExprDoc :: Linearity -> Expr -> Doc
pExprDoc _ (Dbl d) = double d
pExprDoc _ (Int i) = integer i
pExprDoc _ (Str s) = text s
pExprDoc f (Case cs) = caseDoc f cs
pExprDoc f (Mtx rs) = mtxDoc f rs
pExprDoc f (Row es) = hcat $ map (pExprDoc f) es
pExprDoc _ (Ident s) = text $ toPlainName s
pExprDoc _ (Label s) = text $ toPlainName s
pExprDoc _ (Spec s) = specialDoc s
pExprDoc f (Sub e) = text "_" <> pExprDoc f e
pExprDoc f (Sup e) = text "^" <> pExprDoc f e
pExprDoc _ (MO o) = opsDoc o
pExprDoc f (Over Hat e) = pExprDoc f e <> text "_hat"
pExprDoc f (Fenced l r e) = fenceDocL l <> pExprDoc f e <> fenceDocR r 
pExprDoc f (Font Bold e) = pExprDoc f e <> text "_vect"
pExprDoc f (Font Emph e) = text "_" <> pExprDoc f e <> text "_"
pExprDoc f (Div n d) = parens (pExprDoc f n) <> text "/" <> parens (pExprDoc f d)
pExprDoc f (Sqrt e) = text "sqrt" <> parens (pExprDoc f e)
pExprDoc _ (Spc Thin) = space

specDoc :: Linearity -> Spec -> Doc
specDoc f (E e) = pExprDoc f e
specDoc _ (S s) = text s
specDoc f (Sy u) = unitDoc f u
specDoc _ (Sp s) = specialDoc s
specDoc f (Ref _ r s) = specDoc f s <+> parens (text r)
specDoc f (s1 :+: s2) = specDoc f s1 <> specDoc f s2
specDoc _ EmptyS = empty
specDoc f (Quote s) = doubleQuotes $ specDoc f s
specDoc Nonlinear HARDNL = text "\n"
specDoc Linear HARDNL = error "HARDNL encountered in attempt to format linearly"

unitDoc :: Linearity -> USymb -> Doc
unitDoc f (US us) = formatu t b
  where
  (t,b) = partition ((> 0) . snd) us
  formatu :: [(Symbol,Integer)] -> [(Symbol,Integer)] -> Doc
  formatu [] l = line l
  formatu l [] = hsep $ map pow l
  formatu nu de = line nu <> text "/" <> line (map (\(s,i) -> (s,-i)) de)
  line :: [(Symbol,Integer)] -> Doc
  line []  = empty
  line [x] = pow x
  line l   = parens $ hsep $ map pow l
  pow :: (Symbol,Integer) -> Doc
  pow (x,1) = pExprDoc f $ symbol x
  pow (x,p) = pExprDoc f (symbol x) <> text "^" <> integer p

caseDoc :: Linearity -> [(Expr, Expr)] -> Doc
caseDoc Linear cs = hsep $ punctuate comma $ map (\(e,c) -> pExprDoc Linear c
  <+> text "=>" <+> pExprDoc Linear e) cs
caseDoc Nonlinear cs = vcat $ map (\(e,c) -> pExprDoc Nonlinear e <> comma <+> 
  pExprDoc Nonlinear c) cs

mtxDoc :: Linearity -> [[Expr]] -> Doc
mtxDoc Linear rs = brackets $ hsep $ map (brackets . hsep . map (pExprDoc 
  Linear)) rs
mtxDoc Nonlinear rs = brackets $ vcat $ map (hsep . map (pExprDoc Nonlinear)) rs

-- TODO: Double check that this is valid in all output languages
specialDoc :: Special -> Doc
specialDoc Circle  = text "degree"
specialDoc Partial = text "partial"

opsDoc :: Ops -> Doc
opsDoc IsIn = text "is in"
opsDoc Integer = text "integers"
opsDoc Real = text "real numbers"
opsDoc Rational = text "rational numbers"
opsDoc Natural = text "natural numbers"
opsDoc Boolean = text "booleans"
opsDoc Comma = comma
opsDoc Prime = text "'"
opsDoc Log = text "log"
opsDoc Ln = text "ln"
opsDoc Sin = text "sin"
opsDoc Cos = text "cos"
opsDoc Tan = text "tan"
opsDoc Sec = text "sec"
opsDoc Csc = text "csc"
opsDoc Cot = text "cot"
opsDoc Arcsin = text "arcsin"
opsDoc Arccos = text "arccos"
opsDoc Arctan = text "arctan"
opsDoc Not = text "!"
opsDoc Dim = text "dim"
opsDoc Exp = text "exp"
opsDoc Neg = text "-"
opsDoc Cross = text "cross"
opsDoc Dot = text "dot"
opsDoc Eq = text "=="
opsDoc NEq = text "!="
opsDoc Lt = text "<"
opsDoc Gt = text ">"
opsDoc LEq = text "<="
opsDoc GEq = text ">="
opsDoc Impl = text "=>"
opsDoc Iff = text "iff"
opsDoc Subt = text "-"
opsDoc And = text "&&"
opsDoc Or = text "||"
opsDoc Add = text "+"
opsDoc Mul = text "*"
opsDoc Summ = text "sum"
opsDoc Inte = text "integral"
opsDoc Prod = text "product"
opsDoc Point = text "."
opsDoc Perc = text "%"

fenceDocL :: Fence -> Doc
fenceDocL Paren = text "("
fenceDocL Curly = text "{"
fenceDocL Norm = text "||"
fenceDocL Abs = text "|"

fenceDocR :: Fence -> Doc
fenceDocR Paren = text ")"
fenceDocR Curly = text "}"
fenceDocR Norm = text "||"
fenceDocR Abs = text "|"