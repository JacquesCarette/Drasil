-- | Defines functions to print on plain files (for .txt, .log, etc.).
module Language.Drasil.Plain.Print (
  -- * Types
  Linearity(..),
  -- * Functions
  exprDoc, codeExprDoc, sentenceDoc, symbolDoc, unitDoc, showSymb,
  showHasSymbImpl
) where

import Database.Drasil (ChunkDB)
import Language.Drasil (Sentence, Special(..), Stage(..), Symbol, USymb(..))
import qualified Language.Drasil as L (Expr, HasSymbol(..))
import qualified Language.Drasil.CodeExpr.Development as C (CodeExpr)
import Language.Drasil.Printing.AST (Expr(..), Spec(..), Ops(..), Fence(..), 
  OverSymb(..), Fonts(..), Spacing(..), LinkType(..))
import Language.Drasil.Printing.Import (expr, codeExpr, spec, symbol)
import Language.Drasil.Printing.PrintingInformation (PrintingConfiguration(..), 
  PrintingInformation(..), Notation(Scientific))

import Utils.Drasil (toPlainName)

import Prelude hiding ((<>))
import Data.List (partition)
import Text.PrettyPrint.HughesPJ (Doc, (<>), (<+>), brackets, comma, double, 
  doubleQuotes, empty, hcat, hsep, integer, parens, punctuate, space, text, 
  vcat, render)

-- | Data is either linear or not.
data Linearity = Linear | Nonlinear

-- | Simple printing configuration is scientific.
plainConfiguration :: PrintingConfiguration
plainConfiguration = PC Scientific

-- | Create expressions for a document in 'Doc' format.
exprDoc :: ChunkDB -> Stage -> Linearity -> L.Expr -> Doc
exprDoc db st f e = pExprDoc f (expr e (PI db st plainConfiguration))

-- | Create code expressions for a document in 'Doc' format.
codeExprDoc :: ChunkDB -> Stage -> Linearity -> C.CodeExpr -> Doc
codeExprDoc db st f e = pExprDoc f (codeExpr e (PI db st plainConfiguration))

-- | Create sentences for a document in 'Doc' format.
sentenceDoc :: ChunkDB -> Stage -> Linearity -> Sentence -> Doc
sentenceDoc db st f s = specDoc f (spec (PI db st plainConfiguration) s)

-- | Create symbols for a document in 'Doc' format.
symbolDoc :: Symbol -> Doc
symbolDoc s = pExprDoc Linear (symbol s)

-- | Helper for printing expressions in 'Doc' format. Display format of an expression may change regarding the 'Linearity'.
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

-- | Helper for printing sentences ('Spec's) in 'Doc' format.
specDoc :: Linearity -> Spec -> Doc
specDoc f (E e) = pExprDoc f e
specDoc _ (S s) = text s
specDoc _ (Sp s) = specialDoc s
specDoc f (Ref (Cite2 n) r _) = specDoc f n <+> text ("Ref: " ++ r)
specDoc f (Ref _ r s) = specDoc f s <+> text ("Ref: " ++ r) --may need to change?
specDoc f (s1 :+: s2) = specDoc f s1 <> specDoc f s2
specDoc _ EmptyS = empty
specDoc f (Quote s) = doubleQuotes $ specDoc f s
specDoc Nonlinear HARDNL = text "\n"
specDoc Linear HARDNL = error "HARDNL encountered in attempt to format linearly"

-- | Helper for printing units in 'Doc' format.
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

-- | Helper for printing multicase expressions differently based on linearity.
caseDoc :: Linearity -> [(Expr, Expr)] -> Doc
caseDoc Linear cs = hsep $ punctuate comma $ map (\(e,c) -> pExprDoc Linear c
  <+> text "=>" <+> pExprDoc Linear e) cs
caseDoc Nonlinear cs = vcat $ map (\(e,c) -> pExprDoc Nonlinear e <> comma <+> 
  pExprDoc Nonlinear c) cs

-- | Helper for printing matrices.
mtxDoc :: Linearity -> [[Expr]] -> Doc
mtxDoc Linear rs = brackets $ hsep $ map (brackets . hsep . map (pExprDoc 
  Linear)) rs
mtxDoc Nonlinear rs = brackets $ vcat $ map (hsep . map (pExprDoc Nonlinear)) rs

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
opsDoc VAdd = text " + "
opsDoc VSub = text " - "
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
