{-# LANGUAGE DeriveLift #-}

-- | External file 'Asset's.
module Drasil.Assets.Core
  ( -- * Assets
    Asset,

    -- ** Constructors
    mkAsset,

    -- ** Accessors
    content,
    description,

    -- ** File Dumping
    toFile,
  )
where

import Data.ByteString qualified as B (ByteString)
import Data.Text (Text)
import Language.Haskell.TH.Syntax (Lift)
import Prelude hiding (readFile)

import Drasil.FileHandling (FileLayout, PathSegment, exactFile)

-- | An arbitrary file asset; a 'B.ByteString' with a description.
data Asset = Asset Text B.ByteString
  deriving (Lift)

-- | Get the description of an 'Asset'.
description :: Asset -> Text
description (Asset desc _) = desc

-- | Get the raw 'ByteString' content of an 'Asset'.
content :: Asset -> B.ByteString
content (Asset _ c) = c

mkAsset :: Text -> B.ByteString -> Asset
mkAsset = Asset

-- | Convert an 'Asset' to a 'FileLayout' node given a 'PathSegment' (file name)
-- it is to be written to.
toFile :: Asset -> PathSegment -> FileLayout
toFile a = flip exactFile (content a)
