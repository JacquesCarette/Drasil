-- | Defines types needed for HTML printers.
module Language.Drasil.HTML.Monad where

import Language.Drasil (RenderSpecial(..), Special(..))

-- | Printing "monad".
newtype PrintHTML = PH {unPH :: String}

-- | Special HTML symbols (degree and Greek partial derivative symbol).
instance RenderSpecial PrintHTML where
  special Circle       = PH "&deg;"
