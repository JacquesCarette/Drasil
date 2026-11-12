module Drasil.Document.Layout.Spec (
  -- * Types
  Spec(..), LinkType(..),
  -- * Type Aliases
  Label, Title
) where

import Drasil.Document.Layout.Expr (Expr)
import Drasil.Document.Layout.Typography (Special)

-- | A Label is just a 'Spec' (sentence).
type Label = Spec
-- | A title is just a sentence ('Spec').
type Title    = Spec

-- | Different types of links for referencing. May be internal, a citation, or
-- external. A citation may also hold additional reference information.
data LinkType = Internal | Cite2 Spec | External

infixr 5 :+:

-- | Redefine the 'Sentence' type from Language.Drasil to be more suitable to printing.
data Spec = E Expr                   -- ^ Holds an expression.
          | S String                 -- ^ Holds a string.
          | Tooltip Spec Spec        -- ^ Tooltip (1) supplements body (2) with optionally displayable content, e.g., on hover for HTML.
          | Spec :+: Spec            -- ^ Concatenation.
          | Sp Special               -- ^ Special characters.
          | Ref LinkType String Spec -- ^ Holds the actual reference of form 'LinkType', reference address, and display name
          | EmptyS                   -- ^ Empty sentence.
          | Quote Spec               -- ^ Quotes are different in different languages.
