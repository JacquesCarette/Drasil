{-# LANGUAGE OverloadedStrings #-}

module Language.Drasil.HTML2.Spec (
  printSpec, specToHTML
) where

import qualified Data.Text as T (pack)
import Data.Text (Text)
import Data.Text.Extras (num2Text)

import Language.Drasil (Special (..))
import qualified Language.Drasil.Printing.AST as AST
import Drasil.Data.Formats.HTML
import Language.Drasil.HTML2.MathJax (inlineEqn)
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
specToHTML (AST.E e) = [emphasis_ (exprToHTML e)]
specToHTML (a AST.:+: b) = specToHTML a ++ specToHTML b
specToHTML (AST.S s) = [RawText (T.pack s)]
specToHTML (AST.Tooltip t s) = [TextFormat Span [attr "title" (printSpec t)] (specToHTML s)]
specToHTML (AST.Sp s) = [RawText (T.pack $ specialToString s)]
specToHTML AST.HARDNL = [Custom brTag [] []]
specToHTML (AST.Ref refType r a) = case refType of
  AST.Internal         -> [internalRef]
  AST.Cite2 AST.EmptyS -> [internalRef]
  AST.Cite2 n          -> internalRef : specToHTML n
  AST.External         -> [Anchor (T.pack r) [] (specToHTML a)]
  where
    internalRef = Anchor (T.pack ('#' : r)) [] (specToHTML a)
specToHTML AST.EmptyS = []
specToHTML (AST.Quote q) = ["\""] ++ specToHTML q ++ ["\""]

-- | Generates expressions in the HTML document (called by multiple functions).
exprToHTML :: AST.Expr -> [HTMLBody]
exprToHTML (AST.Dbl d) = [RawText $ num2Text d]
exprToHTML (AST.Int i) = [RawText $ num2Text i]
exprToHTML (AST.Str s) = [RawText $ "\"" <> T.pack s <> "\""]
exprToHTML (AST.Row l) = concatMap exprToHTML l
exprToHTML (AST.Ident s) = [rawText' s]
exprToHTML (AST.Label s) = [rawText' s]
exprToHTML (AST.Spec s) = [RawText (T.pack $ specialToString s)]
exprToHTML (AST.Sub e) = [subscript_ (exprToHTML e)]
exprToHTML (AST.Sup e) = [superscript_ (exprToHTML e)]
exprToHTML (AST.Over AST.Hat s) = exprToHTML s ++ ["̂"]
exprToHTML (AST.MO o) = [RawText (pOps o)]
exprToHTML (AST.Fenced l r e) =
  [RawText (fence Open l)] ++ exprToHTML e ++ [RawText (fence Close r)]
exprToHTML (AST.Font AST.Bold e) = [bold_ (exprToHTML e)]
exprToHTML (AST.Font AST.Emph e) = [emphasis_ (exprToHTML e)]
exprToHTML (AST.Spc AST.Thin) = [" "]
-- Uses TeX for Mathjax for all other exprs
exprToHTML e =
  [RawText $ inlineEqn $ T.pack $ show $ printMath $ TeX.pExpr e]

-- | Internal: Break tag for hard newlines.
brTag :: CustomTag
brTag = customTag "br"

-- | Internal: Converts a 'Special' symbol to its String representation.
specialToString :: Special -> String
specialToString Circle = "°"

-- | Internal: Referring to 'fence' (for parenthesis and brackets). Either opened or closed.
data OpenClose = Open | Close

-- | Internal: Allows for open/closed variants of parenthesis, curly brackets, absolute value symbols, and normal symbols.
fence :: OpenClose -> AST.Fence -> Text
fence Open AST.Paren = "("
fence Close AST.Paren = ")"
fence Open AST.Curly = "{"
fence Close AST.Curly = "}"
fence _ AST.Abs = "|"
fence _ AST.Norm = "||"

-- | Internal: Converts expression operators into HTML characters (Text format).
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
