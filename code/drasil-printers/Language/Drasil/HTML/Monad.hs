module Language.Drasil.HTML.Monad where

import Language.Drasil

--import Language.Drasil.Unicode (RenderSpecial, 
  --Special(SqBrClose, SqBrOpen, CurlyBrOpen, CurlyBrClose, Hash, UScore, Circle, Partial, Percent),
  --special)

-----------------------------------------------------------------------------
-- | Printing "monad".  Don't need context, so Identity (under another name)
-- will do just fine.

newtype PrintHTML = PH {unPH :: String}


 

  
  
instance RenderSpecial PrintHTML where
  special Circle       = PH "&deg;"
  special Partial      = PH "&part;"
  special UScore       = PH "_"
  special Percent      = PH "%"
  special Hash         = PH "#"
  special CurlyBrOpen  = PH "{"
  special CurlyBrClose = PH "}"
  special SqBrOpen     = PH "["
  special SqBrClose    = PH "]"
