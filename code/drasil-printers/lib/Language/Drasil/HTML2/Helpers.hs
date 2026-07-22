{-# LANGUAGE OverloadedStrings #-}

-- | Helper functions for creating HTML printers (specifically, HTML tag wrappers).
module Language.Drasil.HTML2.Helpers (
  HTMLRenderOptions (..), BibFormatter (..),
  articleTitle, author, stylesheet,
  specialToString, OpenClose (..), fence, pOps,
  colon, period, comma, vol, pg, pp, no, ed, editedBy
  ) where

import Data.Text (Text)
import Drasil.Data.Formats.HTML (
  Attr (..), HLevel (..), HTMLBody (..), HTMLHead (..)
  )
import qualified Drasil.Data.Formats.HTML as HTML (HTMLBody (Div))
import Language.Drasil (Special (..))
import Language.Drasil.Printing.AST (
  Fence (Abs, Curly, Norm, Paren), Ops (..), Spec (..),
  )

-- | Options for converting layout objects ('LayoutObj's) into HTML AST
data HTMLRenderOptions = HTMLRO
  { -- | Formatting rules for Bib
    bibFmt :: BibFormatter,
    -- | MathJax source URL
    mathJaxSrc :: String
  }

-- | Data type that carries functions that vary
-- for bib printing
data BibFormatter = BibFormatter
  { -- | Emphasis (italics) rendering
    emph :: [HTMLBody] -> [HTMLBody],
    -- | Spec rendering
    spec :: Spec -> [HTMLBody]
  }

specialToString :: Special -> String
specialToString Circle = "°"

-- | Referring to 'fence' (for parenthesis and brackeds). Either opened or closed.
data OpenClose = Open | Close

-- | Allows for open/closed variants of parenthesis, curly brackets, absolute value symbols, and normal symbols.
fence :: OpenClose -> Fence -> Text
fence Open Paren = "("
fence Close Paren = ")"
fence Open Curly = "{"
fence Close Curly = "}"
fence _ Abs = "|"
fence _ Norm = "||"

-- | Converts expression operators into HTML characters (Text format).
pOps :: Ops -> Text
pOps IsIn = " ⋲ "
pOps Integer = "ℤ"
pOps Rational = "ℚ"
pOps Real = "ℝ"
pOps Natural = "ℕ"
pOps Boolean = "𝔹"
pOps Comma = ","
pOps Prime = "′"
pOps Log = "log"
pOps Ln = "ln"
pOps Sin = "sin"
pOps Cos = "cos"
pOps Tan = "tan"
pOps Sec = "sec"
pOps Csc = "csc"
pOps Cot = "cot"
pOps Arcsin = "arcsin"
pOps Arccos = "arccos"
pOps Arctan = "arctan"
pOps Not = "¬"
pOps Dim = "dim"
pOps Exp = "e"
pOps Neg = "−"
pOps Cross = "⨯"
pOps VAdd = "+"
pOps VSub = "−"
pOps Dot = "⋅"
pOps Scale = " " -- same as Mul
pOps Eq = " = " -- with spaces?
pOps NEq = "≠"
pOps Lt = " < " -- thin spaces make these more readable
pOps Gt = " > "
pOps LEq = " ≤ "
pOps GEq = " ≥ "
pOps Impl = " ⇒ "
pOps Iff = " ⇔ "
pOps Subt = "−"
pOps And = " ∧ "
pOps Or = " ∨ "
pOps Add = "+"
pOps Mul = " "
pOps Summ = "∑"
pOps Inte = "∫"
pOps Prod = "∏"
pOps Point = "."
pOps Perc = "%"
pOps LArrow = " ← "
pOps RArrow = " → "
pOps ForAll = " ∀ "
pOps Partial = "∂"
pOps SAdd = " + "
pOps SRemove = " - "
pOps SContains = " in "
pOps SUnion = " and "

colon, period, comma, vol, pg, pp, no, ed, editedBy :: HTMLBody
colon = RawText ": "
period = RawText ". "
comma = RawText ", "
vol = RawText "vol. "
pg = RawText "pg. "
pp = RawText "pp. "
no = RawText "no. "
ed = RawText " ed., "
editedBy = RawText "Edited by "

articleTitle, author :: [HTMLBody] -> HTMLBody
articleTitle t = HTML.Div [Attr "class" "title"] [Heading H1 [] t]
author a       = HTML.Div [Attr "class" "author"] [Heading H2 [] a]

-- | Create the link to the CSS file
stylesheet :: Text -> HTMLHead
stylesheet css = Link "stylesheet" (css <> ".css") [Attr "type" "text/css"]
