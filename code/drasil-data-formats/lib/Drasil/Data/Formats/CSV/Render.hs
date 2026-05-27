module Drasil.Data.Formats.CSV.Render
  ( -- * Rendering
    renderCSV,
    DoubleQuotationPolicy (..),
    csvRenderOpts,
    CSVRenderOptions,
  )
where

import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T (any, pack, replace, splitOn)
import Drasil.Data.Formats.CSV.Core (CSV, header, rows)
import Prettyprinter (Doc, Pretty (..), comma, dquotes, hardline, hcat, vcat)

-- | Options for rendering a 'CSV'.
newtype CSVRenderOptions = CSVRO DoubleQuotationPolicy

-- | Cell-wrapping policy: How often should cells be wrapped in double quotes?
data DoubleQuotationPolicy
  = -- | Only when necessary, i.e., a cell contains either double quotes, a comma,
    -- CR, LF, or CRLF.
    Minimal
  | -- | Everywhere.
    Everywhere

-- | Create 'CSVRenderOptions'.
csvRenderOpts :: DoubleQuotationPolicy -> CSVRenderOptions
csvRenderOpts = CSVRO

-- | Render a 'CSV' to a 'Doc' with the given options.
renderCSV :: CSV -> CSVRenderOptions -> Doc ann
renderCSV csv (CSVRO dqp) = vcat $ map renderRow allRs
  where
    rs = rows csv
    allRs = maybe rs (: rs) $ header csv

    esc = escapeCellPolicy dqp
    renderRow = hcat . intersperse comma . map esc

-- | Internal: Escape a cell according to a 'DoubleQuotationPolicy'.
escapeCellPolicy :: DoubleQuotationPolicy -> Text -> Doc ann
escapeCellPolicy Minimal t
  | T.any needsQuote t = quoteAndEscape t
  | otherwise = escHls t
escapeCellPolicy Everywhere t = quoteAndEscape t

-- | Internal: Check if a character appearing in a cell indicates that the cell
-- /must/ be quoted.
needsQuote :: Char -> Bool
needsQuote c = c `elem` ['"', ',', '\n', '\r']

-- | Internal: Replace all double-quotes with double-double-quotes in a cell and
-- wrap the whole cell in double-quotes.
quoteAndEscape :: Text -> Doc ann
quoteAndEscape = dquotes . escHls . T.replace dqs ddqs

-- | Internal: Double quotes as 'Text'.
dqs :: Text
dqs = T.pack "\""

-- | Internal: Double double-qoutes as 'Text'.
ddqs :: Text
ddqs = dqs <> dqs

-- | Internal: `prettyprinter` needs us to manually deal with hard linebreaks.
escHls :: Text -> Doc ann
escHls = hcat . intersperse hardline . map pretty . T.splitOn lf . T.replace crlf lf
  where
    lf = T.pack "\n"
    crlf = T.pack "\r\n"
