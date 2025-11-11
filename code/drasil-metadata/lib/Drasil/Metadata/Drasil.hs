{-# LANGUAGE TemplateHaskellQuotes, DeriveLift, DeriveGeneric #-}
module Drasil.Metadata.Drasil (DrasilMeta(..), drasilMetaCfg) where

import Data.Maybe (fromMaybe)
import Data.Aeson (decodeFileStrict, FromJSON, ToJSON)
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift, addDependentFile)
import Language.Haskell.TH (Exp, Q, runIO)

{-
  Thank you to the following people for their helpful public resources.
   - 'leftaroundabout': https://stackoverflow.com/a/44369564/16760741
   - Mark Karpov: https://markkarpov.com/tutorial/th.html
-}

-- | Basic Drasil metadata.
newtype DrasilMeta = DrasilMeta { version :: String }
  deriving (Generic, Show, Lift)

instance ToJSON DrasilMeta
instance FromJSON DrasilMeta

-- | Prepare a Template Haskell expression that reads Drasil's metadata from a local JSON file.
drasilMetaCfg :: Q Exp
drasilMetaCfg = do
  let fp = "lib/Drasil/Metadata/Drasil.json"
  maybeDM <- runIO (decodeFileStrict fp :: IO (Maybe DrasilMeta))
  addDependentFile fp
  [| fromMaybe (error "could not read in the drasil metadata file") maybeDM |]
