{-# LANGUAGE FlexibleInstances #-}

module Drasil.Build.Artifacts.Render
  ( Renderable (..),
  )
where

import Data.ByteString.Lazy.Char8 qualified as LB
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Prettyprinter qualified as PNew
import Prettyprinter.Render.Text (renderIO)
import System.File.OsPath (withFile)
import System.IO (Handle, IOMode (..), hPutStr)
import System.OsPath (OsPath)
import Text.PrettyPrint qualified as PLegacy
import Prelude hiding (writeFile)

-- | Render a document and write it to a file (with a trailing newline always
-- added).
class Renderable doc where
  renderToFile :: OsPath -> doc -> IO ()

instance Renderable PLegacy.Doc where
  -- Does conversion to `String` and then does plain `String -> IO ()` writing.
  renderToFile fp = writeFileStr fp . PLegacy.render

instance Renderable (PNew.Doc ann) where
  -- `renderIO` skips intermediate representations before writing to disk:
  -- <https://hackage-content.haskell.org/package/prettyprinter-1.7.2/docs/Prettyprinter-Render-Text.html#v:renderIO>
  renderToFile fp d = writeFile fp $ \h ->
    renderIO h (PNew.layoutPretty PNew.defaultLayoutOptions $ d PNew.<> PNew.line)

instance Renderable String where
  renderToFile = writeFileStr

instance Renderable T.Text where
  renderToFile fp t = writeFile fp $ \h -> TIO.hPutStrLn h t

instance Renderable LB.ByteString where
  renderToFile fp bs = writeFile fp $ \h -> LB.hPutStrLn h bs

-- | Write a 'String' to the given 'OsPath' (with a trailing newline always
-- added).
writeFileStr :: OsPath -> String -> IO ()
writeFileStr rp s = withFile rp WriteMode $ \h -> do
  hPutStr h s
  hPutStr h "\n"
{-# INLINE writeFileStr #-}

-- | Write to a given 'OsPath' with arbitrary method.
writeFile :: OsPath -> (Handle -> IO r) -> IO r
writeFile rp = withFile rp WriteMode
{-# INLINE writeFile #-}
