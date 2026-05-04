module Drasil.Build.Artifacts.Render
  ( Renderable (..),
  )
where

import Drasil.Build.Artifacts.FilePath
import Prettyprinter qualified as PNew
import Prettyprinter.Render.Text (renderIO)
import Text.PrettyPrint qualified as PLegacy
import Prelude hiding (writeFile)

-- | Render a document and write it to a file (with a trailing newline always
-- added).
class Renderable doc where
  renderToFile :: PathComponent -> doc -> IO ()

instance Renderable PLegacy.Doc where
  -- Does conversion to `String` and then does plain `String -> IO ()` writing.
  renderToFile fp = writeFile fp . PLegacy.render . (PLegacy.$+$ PLegacy.text "")

instance Renderable (PNew.Doc ann) where
  -- `renderIO` skips intermediate representations before writing to disk:
  -- <https://hackage-content.haskell.org/package/prettyprinter-1.7.2/docs/Prettyprinter-Render-Text.html#v:renderIO>
  renderToFile fp d = writeFile' fp $ \h ->
    renderIO h (PNew.layoutPretty PNew.defaultLayoutOptions $ d PNew.<> PNew.line)
