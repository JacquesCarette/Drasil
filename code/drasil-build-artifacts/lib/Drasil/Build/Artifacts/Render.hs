{-# LANGUAGE FlexibleInstances #-}

{- HLINT ignore "Use writeFile" -}

module Drasil.Build.Artifacts.Render
  ( Renderable (..),
    WritePolicy (..),
  )
where

import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy.Char8 qualified as LB
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Prettyprinter qualified as PNew
import Prettyprinter.Render.Text (renderIO)
import System.File.OsPath (withFile)
import System.IO (Handle, IOMode (..), hPutStr, hPutStrLn)
import System.OsPath (OsPath)
import Text.PrettyPrint qualified as PLegacy
import Prelude hiding (writeFile)

-- | How should files be written?
data WritePolicy
  = -- | With a trailing newline?
    AppendNewline
  | -- | Or without one?
    ExactBytes

-- | Render a document and write it to a file (respecting the 'WritePolicy').
class Renderable doc where
  renderToFile :: OsPath -> WritePolicy -> doc -> IO ()

instance Renderable PLegacy.Doc where
  -- Does conversion to `String` and then does plain `String -> IO ()` writing.
  renderToFile fp pol = writeFileStr fp pol . PLegacy.render
  {-# INLINE renderToFile #-}

instance Renderable (PNew.Doc ann) where
  -- `renderIO` skips intermediate representations before writing to disk:
  -- <https://hackage-content.haskell.org/package/prettyprinter-1.7.2/docs/Prettyprinter-Render-Text.html#v:renderIO>
  renderToFile fp pol d = writeFile fp $ \h ->
    renderIO h (PNew.layoutPretty PNew.defaultLayoutOptions d')
    where
      d' = case pol of
        AppendNewline -> d PNew.<> PNew.line
        ExactBytes -> d
  {-# INLINE renderToFile #-}

instance Renderable String where
  renderToFile = writeFileStr
  {-# INLINE renderToFile #-}

instance Renderable T.Text where
  renderToFile fp pol t = writeFile fp (`write` t)
    where
      write = case pol of
        AppendNewline -> TIO.hPutStrLn
        ExactBytes -> TIO.hPutStr
  {-# INLINE renderToFile #-}

instance Renderable B.ByteString where
  renderToFile fp pol bs = writeFile fp (`write` bs)
    where
      write = case pol of
        AppendNewline -> B.hPutStrLn
        ExactBytes -> B.hPut
  {-# INLINE renderToFile #-}

instance Renderable LB.ByteString where
  renderToFile fp pol bs = writeFile fp (`write` bs)
    where
      write = case pol of
        AppendNewline -> LB.hPutStrLn
        ExactBytes -> LB.hPut
  {-# INLINE renderToFile #-}

-- | Write a 'String' to the given 'OsPath' (respecting the write policy).
writeFileStr :: OsPath -> WritePolicy -> String -> IO ()
writeFileStr rp pol s = withFile rp WriteMode (`write` s)
  where
    write = case pol of
      AppendNewline -> hPutStrLn
      ExactBytes -> hPutStr
{-# INLINE writeFileStr #-}

-- | Write to a given 'OsPath' with arbitrary method.
writeFile :: OsPath -> (Handle -> IO r) -> IO r
writeFile rp = withFile rp WriteMode
{-# INLINE writeFile #-}
