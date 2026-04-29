module Drasil.Build.Artifacts.Render (
  Renderable(..)
) where

import Text.PrettyPrint qualified as PLegacy
import Prettyprinter qualified as PNew
import Prettyprinter.Render.Text (renderIO)
import System.IO (withFile, IOMode(WriteMode))

-- | Render an abstract document representation efficiently.
class Renderable doc where
  renderToFile :: FilePath -> doc -> IO ()

instance Renderable PLegacy.Doc where
  -- Does conversion to `String` and then does plain `String -> IO ()` writing.
  renderToFile fp = writeFile fp . PLegacy.render

instance Renderable (PNew.Doc ann) where
  -- `renderIO` skips intermediate representation:
  -- <https://hackage-content.haskell.org/package/prettyprinter-1.7.2/docs/Prettyprinter-Render-Text.html#v:renderIO>
  renderToFile fp d = withFile fp WriteMode $ \h ->
    renderIO h (PNew.layoutPretty PNew.defaultLayoutOptions d)
