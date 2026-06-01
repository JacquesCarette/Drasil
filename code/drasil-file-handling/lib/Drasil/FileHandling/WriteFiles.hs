{-# LANGUAGE FlexibleInstances #-}

{- HLINT ignore "Use writeFile" -}

module Drasil.FileHandling.WriteFiles
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
  writeToFile = writeFile withNL write
    where
      write h d = renderIO h (PNew.layoutPretty PNew.defaultLayoutOptions d)
      withNL h d = write h (d PNew.<> PNew.line)
  {-# INLINE writeToFile #-}

instance Writeable String where
  writeToFile = writeFileStr
  {-# INLINE writeToFile #-}

instance Writeable T.Text where
  writeToFile = writeFile TIO.hPutStrLn TIO.hPutStr
  {-# INLINE writeToFile #-}

instance Writeable B.ByteString where
  writeToFile = writeFile B.hPutStrLn B.hPut
  {-# INLINE writeToFile #-}

instance Writeable LB.ByteString where
  writeToFile = writeFile LB.hPutStrLn LB.hPut
  {-# INLINE writeToFile #-}

-- | Internal: Write a 'String' to the given 'OsPath' respecting the
-- 'WritePolicy'.
writeFileStr :: OsPath -> WritePolicy -> String -> IO ()
writeFileStr = writeFile hPutStrLn hPutStr
{-# INLINE writeFileStr #-}

-- | Internal: Write to an 'OsPath' with a 'WritePolicy' appropriate writer.
writeFile ::
  -- | 'AppendNewline' writer.
  (Handle -> a -> IO r) ->
  -- | 'ExactBytes' writer.
  (Handle -> a -> IO r) ->
  -- | The file to be written to.
  OsPath ->
  -- | The 'WritePolicy'.
  WritePolicy ->
  -- | The data to be written.
  a ->
  IO r
writeFile append _     rp AppendNewline = withFile rp WriteMode . flip append
writeFile _      exact rp ExactBytes    = withFile rp WriteMode . flip exact
{-# INLINE writeFile #-}
