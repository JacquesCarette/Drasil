module Drasil.Build.Artifacts.Render
  ( Renderable (..),
  )
where

import Prettyprinter qualified as PNew
import Prettyprinter.Render.Text (renderIO)
import System.IO (IOMode (WriteMode), withFile)
import Text.PrettyPrint qualified as PLegacy

-- | Render a document and write it to a file (with a trailing newline always
-- added).
class Renderable doc where
  renderToFile :: FilePath -> doc -> IO ()

instance Renderable PLegacy.Doc where
  -- Does conversion to `String` and then does plain `String -> IO ()` writing.
  renderToFile fp = writeFile fp . PLegacy.render . (PLegacy.$+$ PLegacy.text "")

instance Renderable (PNew.Doc ann) where
  -- `renderIO` skips intermediate representations before writing to disk:
  -- <https://hackage-content.haskell.org/package/prettyprinter-1.7.2/docs/Prettyprinter-Render-Text.html#v:renderIO>
  renderToFile fp d = withFile fp WriteMode $ \h ->
    renderIO h (PNew.layoutPretty PNew.defaultLayoutOptions $ d PNew.<> PNew.line)
