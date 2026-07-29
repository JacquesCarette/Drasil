{-# LANGUAGE OverloadedStrings #-}

module Language.Drasil.HTML2.Spec (
  printSpec, specToHTML, exprToHTML
) where

import Text.PrettyPrint as PLegacy (text)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Numeric (showEFloat)

import Language.Drasil (Special (..))
import Language.Drasil.Printing.AST (
  Expr (..), Fonts (Bold, Emph), LinkType (Cite2, External, Internal),
  OverSymb (Hat), Spacing (Thin), Spec
  (E, EmptyS, HARDNL, Quote, Ref, S, Sp, Tooltip, (:+:)),
  Fence (Abs, Curly, Norm, Paren), Ops (..),
  )
import Drasil.Data.Formats.HTML (
  Attr (..), Format (Emphasis, Span, Subscript, Superscript), HTMLBody (..), customTag
  )
import qualified Drasil.Data.Formats.HTML as HTML (Format (Bold))
import qualified Language.Drasil.TeX.Print as TeX (pExpr)
import Language.Drasil.Markdown.Print (printMath)

-- | Transforms the Sentences ('Spec's) into Text
printSpec :: Spec -> Text
printSpec (S s) = T.pack s
printSpec (E e) = T.pack $ show $ printMath $ TeX.pExpr e
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
specToHTML (E e) = [TextFormat Emphasis [] (exprToHTML e)]
specToHTML (a :+: b) = specToHTML a ++ specToHTML b
specToHTML (S s) = [RawText (T.pack s)]
specToHTML (Tooltip t s) = [TextFormat Span [Attr "title" (printSpec t)] (specToHTML s)]
specToHTML (Sp s) = [RawText (T.pack $ specialToString s)]
specToHTML HARDNL = [Custom (customTag "br") [] []]
specToHTML (Ref Internal r a) = [Anchor (T.pack $ "#" ++ r) [] (specToHTML a)]
specToHTML (Ref (Cite2 EmptyS) r a) = [Anchor (T.pack $ "#" ++ r) [] (specToHTML a)]
specToHTML (Ref (Cite2 n) r a) = Anchor (T.pack $ "#" ++ r) [] (specToHTML a) : specToHTML n
specToHTML (Ref External r a) = [Anchor (T.pack r) [] (specToHTML a)]
specToHTML EmptyS = []
specToHTML (Quote q) = [RawText "\""] ++ specToHTML q ++ [RawText "\""]

-- | Generates expressions in the HTML document (called by multiple functions).
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
exprToHTML (Over Hat s) = exprToHTML s ++ [RawText "̂"]
exprToHTML (MO o) = [RawText (pOps o)]
exprToHTML (Fenced l r e) =
  [RawText (fence Open l)] ++ exprToHTML e ++ [RawText (fence Close r)]
exprToHTML (Font Bold e) = [TextFormat HTML.Bold [] (exprToHTML e)]
exprToHTML (Font Emph e) = [TextFormat Emphasis [] (exprToHTML e)]
exprToHTML (Spc Thin) = [RawText " "]
-- Uses TeX for Mathjax for all other exprs
exprToHTML e =
  [RawText $ T.pack $ show $ mjDelimDisp $ printMath $ TeX.pExpr e]
  where
    mjDelimDisp d = PLegacy.text "\\(" <> d <> PLegacy.text "\\)"

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
