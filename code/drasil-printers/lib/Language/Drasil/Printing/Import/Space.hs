-- | Defines functions to render 'Space's into printable 'P.Expr's.
module Language.Drasil.Printing.Import.Space (space) where

import Data.List (intersperse)
import Data.List.NonEmpty (toList)

import Language.Drasil (Space(..))

import qualified Drasil.Document.Layout.Internal as P

-- | Render a 'Space'.
space :: Space -> P.Expr
space Integer        = P.MO P.Integer
space Rational       = P.MO P.Rational
space Real           = P.MO P.Real
space Natural        = P.MO P.Natural
space Boolean        = P.MO P.Boolean
space Char           = P.Ident "Char"
space String         = P.Ident "String"
space (Vect _)       = error "Vector space not translated"
space Matrix {}      = error "Matrix space not translated"
space (Array _)      = error "Array space not translated"
space (Actor s)      = P.Ident s
space (Set _ )        = error "Set space not translated"
space Void           = error "Void not translated"
space (Reference _)   = error "Reference not translated"
space (Function i t) = P.Row $
  intersperse (P.MO P.Cross) (space <$> toList i) <>  -- AxBxC...xY
  [P.MO P.RArrow, space t]                            -- -> Z
