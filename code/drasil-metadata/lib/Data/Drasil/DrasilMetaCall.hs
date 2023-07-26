{-#LANGUAGE TemplateHaskell#-}
module Data.Drasil.DrasilMetaCall (drasilMeta, DrasilMeta(..))where

import Data.Drasil.DrasilMeta (drasilMetaCfg, DrasilMeta(..))
import Data.Maybe (fromMaybe)


-- | Reads drasilMeta at compile-time
drasilMeta :: DrasilMeta
drasilMeta = $drasilMetaCfg
