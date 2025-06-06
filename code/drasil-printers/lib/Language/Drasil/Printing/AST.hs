-- | Defines types similar to those in "Drasil.Language", but better suited to printing.
module Language.Drasil.Printing.AST where

import Language.Drasil (Special)

-- | Different types of links for referencing. May be internal, a citation, or external.
-- A citation may also hold additional reference information.
data LinkType = Internal | Cite2 Spec | External

-- | Different operators.
data Ops = IsIn | Integer | Real | Rational | Natural | Boolean | Comma | Prime | Log 
  | Ln | Sin | Cos | Tan | Sec | Csc | Cot | Arcsin | Arccos | Arctan | Not
  | Dim | Exp | Neg | Cross | Dot | Scale | Eq | NEq | Lt | Gt | LEq | GEq | Impl | Iff
  | Subt | And | Or | Add | Mul | Summ | Inte | Prod | Point | Perc | LArrow | RArrow | ForAll
  | VAdd | VSub | Partial | SAdd | SRemove | SUnion | SContains deriving Eq

-- | Holds the type of "text fencing" ("(), {}, |, ||").
data Fence = Paren | Curly | Norm | Abs
-- | The "^" symbol.
data OverSymb = Hat
-- | Different font effects (__bold__, /emphasis/).
data Fonts = Bold | Emph
-- | Spacing is Thin.
data Spacing = Thin
-- | A Label is just a 'Spec' (sentence).
type Label = Spec

-- | Redefine the 'Expr' type from Language.Drasil to be more suitable to printing.
data Expr = Dbl    Double
          | Int    Integer
          | Str    String
          | Case   [(Expr, Expr)] -- ^ Case expressions
          | Mtx    [[Expr]] -- ^ Matrix.
          | Set    [Expr]
          | Row    [Expr]
          | Ident  String
          | Label  String
          | Spec   Special -- ^ Special characters.
          | Sub    Expr -- ^ Subscript.
          | Sup    Expr -- ^ Superscript.
          | MO     Ops
          | Over   OverSymb Expr -- ^ Holds an expression that needs a hat symbol "^"
          | Fenced Fence Fence Expr -- ^ Holds an expression that is surrounded with a 'Fence'.
          | Font   Fonts Expr -- ^ Holds an expression with a font.
          | Div    Expr Expr -- ^ Fractions are a layout thing.
          | Sqrt   Expr      -- ^ Roots are also a layout thing. Just sqrt for now.
          | Spc    Spacing -- ^ Holds the 'Spacing'.
          
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
          | HARDNL                   -- Temp fix for multi-line descriptions; 
                                     -- May move to a new LayoutObj, but only exists in TeX
                                     -- so it's not really a big deal ATM.
                                     -- ^ Newline.
-- | A title is just a sentence ('Spec').
type Title    = Spec

-- | Different types of lists that contain an 'ItemType' and may contain a label and a title.
-- May be ordered, unordered, simple, descriptive, or for definitions. More suitable to printing.
data ListType = Ordered     [(ItemType, Maybe Label)]
              | Unordered   [(ItemType, Maybe Label)]
              | Simple      [(Title, ItemType, Maybe Label)]
              | Desc        [(Title, ItemType, Maybe Label)]
              | Definitions [(Title, ItemType, Maybe Label)]

-- | A list may contain an element or another list. More suitable to printing.
data ItemType = Flat Spec
              | Nested Spec ListType
