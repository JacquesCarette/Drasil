module Language.Drasil.Plain.Print (
  Linearity(..), exprDoc, sentenceDoc, symbolDoc, unitDoc
) where

import Database.Drasil(ChunkDB)
import Language.Drasil hiding (Expr(..), Sentence(..), symbol)
import qualified Language.Drasil as L (Expr, Sentence)
import Language.Drasil.Plain.Helpers (toPlainName)
import Language.Drasil.Printing.AST (Expr(..), Spec(..))
import Language.Drasil.Printing.Import (expr, spec, symbol)
import Language.Drasil.Printing.PrintingInformation (PrintingInformation(..), 
  defaultConfiguration)

import Prelude hiding ((<>))
import Data.List (partition)
import Text.PrettyPrint.HughesPJ (Doc, (<>), double, doubleQuotes, empty, hsep, 
  integer, parens, text)

data Linearity = Linear | Nonlinear

exprDoc :: ChunkDB -> Linearity -> L.Expr -> Doc
exprDoc db f e = pExprDoc f (expr e (PI db defaultConfiguration))

sentenceDoc :: ChunkDB -> Linearity -> L.Sentence -> Doc
sentenceDoc db f s = specDoc f (spec (PI db defaultConfiguration) s)

symbolDoc :: Symbol -> Doc
symbolDoc s = pExprDoc Linear (symbol s)

pExprDoc :: Linearity -> Expr -> Doc
pExprDoc _ (Dbl d) = double d
pExprDoc _ (Int i) = integer i
pExprDoc _ (Str s) = text s
pExprDoc _ (Ident s) = text $ toPlainName s
pExprDoc _ _ = error "Expr to Doc unimplemented"

specDoc :: Linearity -> Spec -> Doc
specDoc f (E e) = pExprDoc f e
specDoc _ (S s) = text s
specDoc f (Sy u) = unitDoc f u
specDoc _ (Sp s) = specialDoc s
specDoc f (s1 :+: s2) = specDoc f s1 <> specDoc f s2
specDoc _ EmptyS = empty
specDoc f (Quote s) = doubleQuotes $ specDoc f s
specDoc _ _ = error "Term is not a string" 

unitDoc :: Linearity -> USymb -> Doc
unitDoc f (US us) = formatu t b
  where
  (t,b) = partition ((> 0) . snd) us
  formatu :: [(Symbol,Integer)] -> [(Symbol,Integer)] -> Doc
  formatu [] l = line l
  formatu l [] = hsep $ map pow l
  formatu nu de = line nu <> text "/" <> line de
  line :: [(Symbol,Integer)] -> Doc
  line []  = empty
  line [x] = pow x
  line l   = parens $ hsep $ map pow l
  pow :: (Symbol,Integer) -> Doc
  pow (x,1) = pExprDoc f $ symbol x
  pow (x,p) = pExprDoc f (symbol x) <> text "^" <> integer p

decorate :: Doc -> Decoration -> Doc
decorate s Hat = s <> text "_hat"
decorate s Vector = s <> text "_vect"
decorate s Prime = s <> text "'"

-- TODO: Double check that this is valid in all output languages
specialDoc :: Special -> Doc
specialDoc Circle  = text "circ"
specialDoc Partial = text "partial"