module Drasil.Build.Artifacts.Render (
  Renderable(..)
) where

class Renderable doc where
  renderToFile :: FilePath -> doc -> IO ()

-- instance Renderable Pretty.Doc where
-- ...

-- instance Renderable Prettyprinter.Doc where
-- ...
