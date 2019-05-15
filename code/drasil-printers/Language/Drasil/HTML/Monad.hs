module Language.Drasil.HTML.Monad where

import Language.Drasil

-----------------------------------------------------------------------------
-- | Printing "monad".  Don't need context, so Identity (under another name)
-- will do just fine.

newtype PrintHTML = PH {unPH :: String}

instance RenderSpecial PrintHTML where
  special Circle       = PH "&deg;"
  special Partial      = PH "&part;"
