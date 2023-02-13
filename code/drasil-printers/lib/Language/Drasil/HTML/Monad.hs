-- | Defines types needed for HTML printers.
module Language.Drasil.HTML.Monad where

import Language.Drasil

-----------------------------------------------------------------------------
-- | Printing "monad". Doesn't need context, so Identity (under another name)
-- will do just fine.

newtype PrintHTML = PH {unPH :: String}

-- | Special HTML symbols (degree and Greek partial derivative symbol).
instance RenderSpecial PrintHTML where
  special Circle       = PH "&deg;"
  -- special Partial      = PH "&part;"
