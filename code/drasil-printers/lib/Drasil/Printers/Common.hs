{-# LANGUAGE OverloadedStrings #-}

module Drasil.Printers.Common (
  wrap, paren, brak, brace, pipes, dpipes, angbrac, dollar, quote, dquote,
  ddollars, underscores, asterisks, dasterisks, fSlashes, CanCarryText(..)
) where

import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter qualified as PNew
import Text.PrettyPrint qualified as PLegacy

class CanCarryText t where
  holdText :: Text -> t

instance CanCarryText String where
  holdText = T.unpack
  {-# INLINE holdText #-}

instance CanCarryText Text where
  holdText = id
  {-# INLINE holdText #-}

instance CanCarryText PLegacy.Doc where
  holdText = PLegacy.text . T.unpack
  {-# INLINE holdText #-}

instance CanCarryText (PNew.Doc ann) where
  holdText = PNew.pretty
  {-# INLINE holdText #-}

type CanTextWrap dt = (Semigroup dt, CanCarryText dt)

wrap :: (CanTextWrap a) => Text -> Text -> a -> a
wrap l r e = holdText l <> e <> holdText r
{-# INLINE wrap #-}

paren, brak, brace, pipes, dpipes, angbrac, dollar, quote, dquote, ddollars,
  underscores, asterisks, dasterisks, fSlashes :: (CanTextWrap a) => a -> a
paren = wrap "(" ")"
brak = wrap "[" "]"
brace = wrap "{" "}"
pipes = wrap "|" "|"
dpipes = pipes . pipes
angbrac = wrap "<" ">"
dollar = wrap "$" "$"
quote = wrap "'" "'"
dquote = wrap "\"" "\""
ddollars = dollar . dollar
underscores = wrap "_" "_"
asterisks = wrap "*" "*"
dasterisks = asterisks . asterisks
fSlashes = wrap "/" "/"
