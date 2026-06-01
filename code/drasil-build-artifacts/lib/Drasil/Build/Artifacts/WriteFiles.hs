{-# LANGUAGE FlexibleInstances #-}

{- HLINT ignore "Use writeFile" -}

module Drasil.Build.Artifacts.WriteFiles
  ( Writeable (..),
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

-- | Write arbitrary things to a file (respecting a 'WritePolicy').
class Writeable doc where
  writeToFile :: OsPath -> WritePolicy -> doc -> IO ()

-- | Renders the document before writing using plain rendering style.
instance Writeable PLegacy.Doc where
  -- Does conversion to `String` and then does plain `String -> IO ()` writing.
  writeToFile fp pol = writeFileStr fp pol . PLegacy.render
  {-# INLINE writeToFile #-}

-- | Renders the document before writing using plain rendering style.
instance Writeable (PNew.Doc ann) where
  -- `renderIO` skips intermediate representations before writing to disk:
  -- <https://hackage-content.haskell.org/package/prettyprinter-1.7.2/docs/Prettyprinter-Render-Text.html#v:renderIO>
  writeToFile fp pol d = writeFile fp $ \h ->
    renderIO h (PNew.layoutPretty PNew.defaultLayoutOptions d')
    where
      d' = case pol of
        AppendNewline -> d PNew.<> PNew.line
        ExactBytes -> d
  {-# INLINE writeToFile #-}

instance Writeable String where
  writeToFile = writeFileStr
  {-# INLINE writeToFile #-}

instance Writeable T.Text where
  writeToFile fp pol t = writeFile fp (`write` t)
    where
      write = case pol of
        AppendNewline -> TIO.hPutStrLn
        ExactBytes -> TIO.hPutStr
  {-# INLINE writeToFile #-}

instance Writeable B.ByteString where
  writeToFile fp pol bs = writeFile fp (`write` bs)
    where
      write = case pol of
        AppendNewline -> B.hPutStrLn
        ExactBytes -> B.hPut
  {-# INLINE writeToFile #-}

instance Writeable LB.ByteString where
  writeToFile fp pol bs = writeFile fp (`write` bs)
    where
      write = case pol of
        AppendNewline -> LB.hPutStrLn
        ExactBytes -> LB.hPut
  {-# INLINE writeToFile #-}

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
