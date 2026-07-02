module Drasil.Data.Formats.JSON.Core
  ( -- * JSON
    JSON(..),
  )
where

import Data.Text (Text)
import qualified Data.Text as T (pack)
import Data.Scientific (Scientific)
import Data.String (IsString(..))

-- | A JSON representation conformant with
-- [RFC 8259](https://datatracker.ietf.org/doc/html/rfc8259).
data JSON =
    -- | Note that empty and duplicate keys are allowed,
    -- and how they are handled is up to the implementation of the reader.
    JObject [(Text, JSON)]
  | JArray [JSON]
  | JString Text
  | JNumber Scientific
  | JBool Bool
  | JNull
  deriving (Show, Eq)

-- | Using the 'OverloadedStrings' language extension, string literals
-- can be automatically converted to 'JString's.
instance IsString JSON where
  fromString = JString . T.pack
