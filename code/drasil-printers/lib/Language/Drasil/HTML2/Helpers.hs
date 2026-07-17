{-# LANGUAGE OverloadedStrings #-}

-- | Helper functions for creating HTML printers (specifically, HTML tag wrappers).
module Language.Drasil.HTML2.Helpers (
  printSpec, HTMLRenderOptions (..), BibFormatter (..), htmlBibFormatter,
  specToHTML, exprToHTML, articleTitle, author, foldRaw, stylesheet, renderMath,
  specialToString, OpenClose (..), fence, pOps
  ) where

import Data.Text (Text)
import qualified Data.Text as T (pack)
import Drasil.Data.Formats.HTML (
  Attr (..),
    Format (Emphasis, Span, Subscript, Superscript),
    HLevel (..),
    HTMLBody (..), HTMLHead (..), customTag
  )
import qualified Drasil.Data.Formats.HTML as HTML (Format (Bold), HTMLBody (Div))
import Language.Drasil (Special (..))
import Language.Drasil.Printing.AST
  ( Expr (..),
    Fence (Abs, Curly, Norm, Paren),
    Fonts (Bold, Emph),
    LinkType (Cite2, External, Internal),
    Ops (..),
    OverSymb (Hat),
    Spacing (Thin),
    Spec (E, EmptyS, HARDNL, Quote, Ref, S, Sp, Tooltip, (:+:)),
  )
import Language.Drasil.TeX.Monad (D, MathContext (Math), runPrint, toMath)
import qualified Language.Drasil.TeX.Print as TeX (pExpr)
import Numeric (showEFloat)
import Text.PrettyPrint as PLegacy (Doc, text)

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

-- | HTML specific bib rendering functions
htmlBibFormatter :: BibFormatter
htmlBibFormatter =
  BibFormatter
    { emph = \x -> [TextFormat Emphasis [] x],
      spec = specToHTML
    }

printSpec :: Spec -> Text
printSpec (S s) = T.pack s
printSpec (E e) = T.pack $ show $ renderMath $ TeX.pExpr e
printSpec (a :+: b) = printSpec a <> printSpec b
printSpec HARDNL = " "
printSpec (Sp s) = T.pack $ specialToString s
printSpec (Ref (Cite2 n) _ a) = printSpec a <> " " <> printSpec n
printSpec (Ref _ _ a) = printSpec a
printSpec EmptyS = ""
printSpec (Quote q) = "\"" <> printSpec q <> "\""
printSpec (Tooltip _ s) = printSpec s

-- | Transforms the Sentences ('Spec's) into HTML (called by 'loToHTML').
specToHTML :: Spec -> [HTMLBody]
-- Non-mathjax
specToHTML (E e) = [TextFormat Emphasis [] (foldRaw (exprToHTML e))]
specToHTML (a :+: b) = foldRaw (specToHTML a ++ specToHTML b)
specToHTML (S s) = [RawText (T.pack s)]
specToHTML (Tooltip t s) = [TextFormat Span [Attr "title" (printSpec t)] (specToHTML s)]
specToHTML (Sp s) = [RawText (T.pack $ specialToString s)]
specToHTML HARDNL = [Custom (customTag "br") [] []]
specToHTML (Ref Internal r a) = [Anchor (T.pack $ "#" ++ r) [] (specToHTML a)]
specToHTML (Ref (Cite2 EmptyS) r a) = [Anchor (T.pack $ "#" ++ r) [] (specToHTML a)]
specToHTML (Ref (Cite2 n) r a) = Anchor (T.pack $ "#" ++ r) [] (specToHTML a) : specToHTML n
specToHTML (Ref External r a) = [Anchor (T.pack r) [] (specToHTML a)]
specToHTML EmptyS = []
specToHTML (Quote q) = foldRaw $ [RawText "\""] ++ specToHTML q ++ [RawText "\""]

-- | Renders expressions in the HTML document (called by multiple functions).
exprToHTML :: Expr -> [HTMLBody]
exprToHTML (Dbl d) = [RawText (T.pack $ showEFloat Nothing d "")]
exprToHTML (Int i) = [RawText (T.pack $ show i)]
exprToHTML (Str s) = [RawText $ "\"" <> T.pack s <> "\""]
exprToHTML (Row l) = concatMap exprToHTML l
exprToHTML (Ident s) = [RawText (T.pack s)]
exprToHTML (Label s) = [RawText (T.pack s)]
exprToHTML (Spec s) = [RawText (T.pack $ specialToString s)]
exprToHTML (Sub e) = [TextFormat Subscript [] (exprToHTML e)]
exprToHTML (Sup e) = [TextFormat Superscript [] (exprToHTML e)]
exprToHTML (Over Hat s) = foldRaw (exprToHTML s ++ [RawText "̂"])
exprToHTML (MO o) = [RawText (pOps o)]
exprToHTML (Fenced l r e) =
  foldRaw $ [RawText (fence Open l)] ++ exprToHTML e ++ [RawText (fence Close r)]
exprToHTML (Font Bold e) = [TextFormat HTML.Bold [] (exprToHTML e)]
exprToHTML (Font Emph e) = [TextFormat Emphasis [] (exprToHTML e)]
exprToHTML (Spc Thin) = [RawText " "]
-- Uses TeX for Mathjax for all other exprs
exprToHTML e =
  [RawText $ T.pack $ show $ mjDelimDisp $ renderMath $ TeX.pExpr e]
  where
    mjDelimDisp d = PLegacy.text "\\(" <> d <> PLegacy.text "\\)"

-- | Helper for rendering a 'D' from Latex print.
printMath :: D -> PLegacy.Doc
printMath = (`runPrint` Math)

-- | Helper for converting and rendering math expressions to Latex print.
renderMath :: D -> PLegacy.Doc
renderMath = printMath . toMath

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

articleTitle, author :: [HTMLBody] -> HTMLBody
articleTitle t = HTML.Div [Attr "class" "title"] [Heading H1 [] t]
author a       = HTML.Div [Attr "class" "author"] [Heading H2 [] a]

foldRaw :: [HTMLBody] -> [HTMLBody]
foldRaw [] = []
foldRaw (RawText a : RawText b : rest) = foldRaw (RawText (a <> b) : rest)
foldRaw (x : xs) = x : foldRaw xs

-- | Create the link to the CSS file
stylesheet :: Text -> HTMLHead
stylesheet css = Link "stylesheet" (css <> ".css") [Attr "type" "text/css"]
