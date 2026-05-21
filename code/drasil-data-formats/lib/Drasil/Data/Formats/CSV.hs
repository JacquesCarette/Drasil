module Drasil.Data.Formats.CSV
  ( -- * Type
    CSV,

    -- * Construction
    mkCSV,

    -- * Destructors
    header,
    rows,
    rowCount,

    -- * Rendering
    renderCSV,
    DoubleQuotationPolicy (..),
    csvRenderOpts,
    CSVRenderOptions
  )
where

import Data.List (find, intersperse)
import Data.Text (Text)
import qualified Data.Text as T (any, pack, replace, splitOn)
import Prettyprinter (Doc, Pretty (..), comma, dquotes, hardline, hcat, vcat)

-- | A CSV file representation.
data CSV = CSV (Maybe [Text]) [[Text]]
  deriving (Show, Eq)

-- | Get the header row of a 'CSV'.
header :: CSV -> Maybe [Text]
header (CSV mhr _) = mhr

-- | Get all rows of a 'CSV'.
rows :: CSV -> [[Text]]
rows (CSV _ rs) = rs

-- | Create a 'CSV'. Expects all rows to have the same length as the header (if
-- it exists) or the first row (if the rows are non-empty).
mkCSV :: Maybe [Text] -> [[Text]] -> Either String CSV
mkCSV mhr rs@(fr : rrs) =
  case find (\(_, r) -> length r /= expLen) (zip ([1 ..] :: [Int]) rrs) of
    Nothing -> pure $ CSV mhr rs
    Just (i, r) -> Left $ concat [
        "Row ", show i, " has ", show (length r), " columns, but expected ", show expLen,
        " (based on ", expLenSrc, ")"
      ]
  where
    (expLen, expLenSrc) = maybe (length fr, "first row") (\r -> (length r, "header")) mhr
mkCSV mhr [] = pure $ CSV mhr []

-- | Calculate the number of rows in a 'CSV'.
rowCount :: CSV -> Int
rowCount (CSV _ rs) = length rs

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
renderCSV (CSV mhr rs) (CSVRO dqp) = vcat $ map renderRow allRs
  where
    allRs = maybe rs (: rs) mhr

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
needsQuote c = c == '"' || c == ',' || c == '\n' || c == '\r'

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
