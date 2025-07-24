-- | Defines functions to render 'Space's into printable 'P.Expr's.
module Language.Drasil.Printing.Import.Space where

import Language.Drasil (Space(..))

import qualified Language.Drasil.Printing.AST as P
import Language.Drasil.Printing.PrintingInformation (PrintingInformation)

import Data.List (intersperse)
import Data.List.NonEmpty (toList)

-- | Render a 'Space'.
space :: PrintingInformation -> Space -> P.Expr
space _  Integer        = P.MO P.Integer
space _  Rational       = P.MO P.Rational
space _  Real           = P.MO P.Real
space _  Natural        = P.MO P.Natural
space _  Boolean        = P.MO P.Boolean
space _  Char           = P.Ident "Char"
space _  String         = P.Ident "String"
space _ (ClifS {}) = error "Clif space not translated"
space _  Matrix {}      = error "Matrix space not translated"
space _  (Array _)      = error "Array space not translated"
space _  (Actor s)      = P.Ident s
space _ (Set _ )        = error "Set space not translated"
space _  Void           = error "Void not translated"
space sm (Function i t) = P.Row $
  intersperse (P.MO P.Cross) (map (space sm) $ toList i) ++  -- AxBxC...xY
  [P.MO P.RArrow, space sm t]                                -- -> Z
