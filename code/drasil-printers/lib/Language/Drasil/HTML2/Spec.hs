{-# LANGUAGE OverloadedStrings #-}

module Language.Drasil.HTML2.Spec (
  printSpec, specToHTML, exprToHTML,
  colon, period, comma, vol, pg, pp, no, ed, editedBy,
  articleTitle, author
) where

import Text.PrettyPrint as PLegacy (text)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Numeric (showEFloat)

import Language.Drasil (Special (..))
import qualified Language.Drasil.Printing.AST as AST
import Drasil.Data.Formats.HTML
import qualified Language.Drasil.TeX.Print as TeX (pExpr)
import Language.Drasil.Markdown.Print (printMath)

-- | Transforms the Sentences ('Spec's) into Text
printSpec :: AST.Spec -> Text
printSpec (AST.S s) = T.pack s
printSpec (AST.E e) = T.pack $ show $ printMath $ TeX.pExpr e -- TODO: Remove `show` once LaTeX render is using Prettyprinter
printSpec (a AST.:+: b) = printSpec a <> printSpec b
printSpec AST.HARDNL = " "
printSpec (AST.Sp s) = T.pack $ specialToString s
printSpec (AST.Ref (AST.Cite2 n) _ a) = printSpec a <> " " <> printSpec n
printSpec (AST.Ref _ _ a) = printSpec a
printSpec AST.EmptyS = ""
printSpec (AST.Quote q) = "\"" <> printSpec q <> "\""
printSpec (AST.Tooltip _ s) = printSpec s

-- | Transforms the Sentences ('Spec's) into HTML (called by 'loToHTML').
specToHTML :: AST.Spec -> [HTMLBody]
-- Non-mathjax
specToHTML (AST.E e) = [TextFormat Emphasis [] (exprToHTML e)]
specToHTML (a AST.:+: b) = specToHTML a ++ specToHTML b
specToHTML (AST.S s) = [RawText (T.pack s)]
specToHTML (AST.Tooltip t s) = [TextFormat Span [attr "title" (printSpec t)] (specToHTML s)]
specToHTML (AST.Sp s) = [RawText (T.pack $ specialToString s)]
specToHTML AST.HARDNL = [Custom (customTag "br") [] []]
specToHTML (AST.Ref AST.Internal r a) = [Anchor (T.pack $ "#" ++ r) [] (specToHTML a)]
specToHTML (AST.Ref (AST.Cite2 AST.EmptyS) r a) = [Anchor (T.pack $ "#" ++ r) [] (specToHTML a)]
specToHTML (AST.Ref (AST.Cite2 n) r a) = Anchor (T.pack $ "#" ++ r) [] (specToHTML a) : specToHTML n
specToHTML (AST.Ref AST.External r a) = [Anchor (T.pack r) [] (specToHTML a)]
specToHTML AST.EmptyS = []
specToHTML (AST.Quote q) = ["\""] ++ specToHTML q ++ ["\""]

-- | Generates expressions in the HTML document (called by multiple functions).
exprToHTML :: AST.Expr -> [HTMLBody]
exprToHTML (AST.Dbl d) = [RawText (T.pack $ showEFloat Nothing d "")]
exprToHTML (AST.Int i) = [RawText (T.pack $ show i)]
exprToHTML (AST.Str s) = [RawText $ "\"" <> T.pack s <> "\""]
exprToHTML (AST.Row l) = concatMap exprToHTML l
exprToHTML (AST.Ident s) = [RawText (T.pack s)]
exprToHTML (AST.Label s) = [RawText (T.pack s)]
exprToHTML (AST.Spec s) = [RawText (T.pack $ specialToString s)]
exprToHTML (AST.Sub e) = [TextFormat Subscript [] (exprToHTML e)]
exprToHTML (AST.Sup e) = [TextFormat Superscript [] (exprToHTML e)]
exprToHTML (AST.Over AST.Hat s) = exprToHTML s ++ ["̂"]
exprToHTML (AST.MO o) = [RawText (pOps o)]
exprToHTML (AST.Fenced l r e) =
  [RawText (fence Open l)] ++ exprToHTML e ++ [RawText (fence Close r)]
exprToHTML (AST.Font AST.Bold e) = [TextFormat Bold [] (exprToHTML e)]
exprToHTML (AST.Font AST.Emph e) = [TextFormat Emphasis [] (exprToHTML e)]
exprToHTML (AST.Spc AST.Thin) = [" "]
-- Uses TeX for Mathjax for all other exprs
exprToHTML e =
  [RawText $ T.pack $ show $ mjDelimDisp $ printMath $ TeX.pExpr e]
  where
    mjDelimDisp d = PLegacy.text "\\(" <> d <> PLegacy.text "\\)"

specialToString :: Special -> String
specialToString Circle = "°"

-- | Referring to 'fence' (for parenthesis and brackets). Either opened or closed.
data OpenClose = Open | Close

-- | Allows for open/closed variants of parenthesis, curly brackets, absolute value symbols, and normal symbols.
fence :: OpenClose -> AST.Fence -> Text
fence Open AST.Paren = "("
fence Close AST.Paren = ")"
fence Open AST.Curly = "{"
fence Close AST.Curly = "}"
fence _ AST.Abs = "|"
fence _ AST.Norm = "||"

-- | Converts expression operators into HTML characters (Text format).
pOps :: AST.Ops -> Text
pOps AST.IsIn = " ⋲ "
pOps AST.Integer = "ℤ"
pOps AST.Rational = "ℚ"
pOps AST.Real = "ℝ"
pOps AST.Natural = "ℕ"
pOps AST.Boolean = "𝔹"
pOps AST.Comma = ","
pOps AST.Prime = "′"
pOps AST.Log = "log"
pOps AST.Ln = "ln"
pOps AST.Sin = "sin"
pOps AST.Cos = "cos"
pOps AST.Tan = "tan"
pOps AST.Sec = "sec"
pOps AST.Csc = "csc"
pOps AST.Cot = "cot"
pOps AST.Arcsin = "arcsin"
pOps AST.Arccos = "arccos"
pOps AST.Arctan = "arctan"
pOps AST.Not = "¬"
pOps AST.Dim = "dim"
pOps AST.Exp = "e"
pOps AST.Neg = "−"
pOps AST.Cross = "⨯"
pOps AST.VAdd = "+"
pOps AST.VSub = "−"
pOps AST.Dot = "⋅"
pOps AST.Scale = " " -- same as Mul
pOps AST.Eq = " = " -- with spaces?
pOps AST.NEq = "≠"
pOps AST.Lt = " < " -- thin spaces make these more readable
pOps AST.Gt = " > "
pOps AST.LEq = " ≤ "
pOps AST.GEq = " ≥ "
pOps AST.Impl = " ⇒ "
pOps AST.Iff = " ⇔ "
pOps AST.Subt = "−"
pOps AST.And = " ∧ "
pOps AST.Or = " ∨ "
pOps AST.Add = "+"
pOps AST.Mul = " "
pOps AST.Summ = "∑"
pOps AST.Inte = "∫"
pOps AST.Prod = "∏"
pOps AST.Point = "."
pOps AST.Perc = "%"
pOps AST.LArrow = " ← "
pOps AST.RArrow = " → "
pOps AST.ForAll = " ∀ "
pOps AST.Partial = "∂"
pOps AST.SAdd = " + "
pOps AST.SRemove = " - "
pOps AST.SContains = " in "
pOps AST.SUnion = " and "

colon, period, comma, vol, pg, pp, no, ed, editedBy :: HTMLBody
colon = ": "
period = ". "
comma = ", "
vol = "vol. "
pg = "pg. "
pp = "pp. "
no = "no. "
ed = " ed., "
editedBy = "Edited by "

articleTitle, author :: [HTMLBody] -> HTMLBody
articleTitle t = Div [class_ "title"] [Heading H1 [] t]
author a       = Div [class_ "author"] [Heading H2 [] a]
