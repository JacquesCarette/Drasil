-- | Defines functions to print on plain files (for .txt, .log, etc.).
module Language.Drasil.Plain.Print (
  -- * Types
  SingleLine(..),
  -- * Functions
  exprDoc, codeExprDoc, sentenceDoc, symbolDoc, unitDoc, showSymb,
  showHasSymbImpl
) where

import Prelude hiding ((<>))
import Data.List (partition)
import Text.PrettyPrint.HughesPJ (Doc, (<>), (<+>), brackets, comma, double,
  doubleQuotes, empty, hcat, hsep, integer, parens, punctuate, space, text,
  vcat, render)

import qualified Drasil.Code.CodeExpr.Development as C (CodeExpr)
import Language.Drasil (Sentence, Special(..), Stage(..), Symbol, USymb(..))
import qualified Language.Drasil as L (Expr, HasSymbol(..))
import Utils.Drasil (toPlainName)

import Language.Drasil.Printing.AST (Expr(..), Spec(..), Ops(..), Fence(..),
  OverSymb(..), Fonts(..), Spacing(..), LinkType(..))
import Language.Drasil.Printing.Import (expr, codeExpr, spec, symbol)
import Language.Drasil.Printing.PrintingInformation (PrintingInformation)

-- | Data is either linear or not.
data SingleLine = OneLine | MultiLine

-- | Create expressions for a document in 'Doc' format.
exprDoc :: PrintingInformation -> SingleLine -> L.Expr -> Doc
exprDoc pinfo sl e = pExprDoc sl (expr e pinfo)

-- | Create code expressions for a document in 'Doc' format.
codeExprDoc :: PrintingInformation -> SingleLine -> C.CodeExpr -> Doc
codeExprDoc pinfo sl e = pExprDoc sl (codeExpr e pinfo)

-- | Create sentences for a document in 'Doc' format.
sentenceDoc :: PrintingInformation -> SingleLine -> Sentence -> Doc
sentenceDoc pinfo sl s = specDoc sl (spec pinfo s)

-- | Create symbols for a document in 'Doc' format.
symbolDoc :: Symbol -> Doc
symbolDoc s = pExprDoc OneLine (symbol s)

-- | Helper for printing expressions in 'Doc' format. Display format of an expression may change regarding the 'SingleLine'.
pExprDoc :: SingleLine -> Expr -> Doc
pExprDoc _ (Dbl d) = double d
pExprDoc _ (Int i) = integer i
pExprDoc _ (Str s) = text s
pExprDoc f (Case cs) = caseDoc f cs
pExprDoc f (Mtx rs) = mtxDoc f rs
pExprDoc f (Row es) = hcat $ map (pExprDoc f) es
pExprDoc f (Set es) = hcat $ map (pExprDoc f) es
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
pExprDoc _ (Clif _ _) = error "Plain text printer cannot render Clifford algebra expressions"

-- | Helper for printing sentences ('Spec's) in 'Doc' format.
specDoc :: SingleLine -> Spec -> Doc
specDoc f (E e) = pExprDoc f e
specDoc _ (S s) = text s
specDoc f (Tooltip _ s) = specDoc f s
specDoc _ (Sp s) = specialDoc s
specDoc f (Ref (Cite2 n) r _) = specDoc f n <+> text ("Ref: " ++ r)
specDoc f (Ref _ r s) = specDoc f s <+> text ("Ref: " ++ r) --may need to change?
specDoc f (s1 :+: s2) = specDoc f s1 <> specDoc f s2
specDoc _ EmptyS = empty
specDoc f (Quote s) = doubleQuotes $ specDoc f s
specDoc MultiLine HARDNL = text "\n"
specDoc OneLine HARDNL = error "HARDNL encountered in attempt to format linearly"

-- | Helper for printing units in 'Doc' format.
unitDoc :: SingleLine -> USymb -> Doc
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

-- | Helper for printing multicase expressions differently based on linearity (SingleLine).
caseDoc :: SingleLine -> [(Expr, Expr)] -> Doc
caseDoc OneLine cs = hsep $ punctuate comma $ map (\(e,c) -> pExprDoc OneLine c
  <+> text "=>" <+> pExprDoc OneLine e) cs
caseDoc MultiLine cs = vcat $ map (\(e,c) -> pExprDoc MultiLine e <> comma <+>
  pExprDoc MultiLine c) cs

-- | Helper for printing matrices.
mtxDoc :: SingleLine -> [[Expr]] -> Doc
mtxDoc OneLine rs = brackets $ hsep $ map (brackets . hsep . map (pExprDoc
  OneLine)) rs
mtxDoc MultiLine rs = brackets $ vcat $ map (hsep . map (pExprDoc MultiLine)) rs

-- TODO: Double check that this is valid in all output languages
-- | Helper for printing special characters (for degrees and partial derivatives).
specialDoc :: Special -> Doc
specialDoc Circle  = text "degree"

-- | Helper for printing operators.
opsDoc :: Ops -> Doc
opsDoc IsIn = text " is in "
opsDoc Integer = text "integers"
opsDoc Real = text "real numbers"
opsDoc Rational = text "rational numbers"
opsDoc Natural = text "natural numbers"
opsDoc Boolean = text "booleans"
opsDoc Comma = comma <> space
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
opsDoc Cross = text " cross "
opsDoc CAdd = text " + "
opsDoc CSub = text " - "
opsDoc Dot = text " dot "
opsDoc Scale = text " * "
opsDoc Eq = text " == "
opsDoc NEq = text " != "
opsDoc Lt = text " < "
opsDoc Gt = text " > "
opsDoc LEq = text " <= "
opsDoc GEq = text " >= "
opsDoc Impl = text " => "
opsDoc Iff = text "iff "
opsDoc Subt = text " - "
opsDoc And = text " && "
opsDoc Or = text " || "
opsDoc Add = text " + "
opsDoc SAdd = text " + "
opsDoc SRemove = text " - "
opsDoc SContains = text " in "
opsDoc SUnion = text "+"
opsDoc Mul = text " * "
opsDoc Summ = text "sum "
opsDoc Inte = text "integral "
opsDoc Prod = text "product "
opsDoc Point = text "."
opsDoc Perc = text "%"
opsDoc LArrow = text " <- "
opsDoc RArrow = text " -> "
opsDoc ForAll = text " ForAll "
opsDoc Partial = text "partial"
opsDoc WedgeProd = text " ^ "
opsDoc GeometricProd = text " * "
opsDoc Grade = text "grade"

-- | Helper for printing the left side of some characters "(, {, \\|, |".
fenceDocL :: Fence -> Doc
fenceDocL Paren = text "("
fenceDocL Curly = text "{"
fenceDocL Norm = text "\\|"
fenceDocL Abs = text "|"

-- | Helper for printing the right side of some characters "), }, \\|, |".
fenceDocR :: Fence -> Doc
fenceDocR Paren = text ")"
fenceDocR Curly = text "}"
fenceDocR Norm = text "\\|"
fenceDocR Abs = text "|"

-- | Helper for printing Symbols
showSymb :: Symbol -> String
showSymb a = render $ symbolDoc a

-- | Helper for printing a HasSymbol in Implementation Stage
showHasSymbImpl :: L.HasSymbol x => x -> String
showHasSymbImpl x = showSymb (L.symbol x Implementation)