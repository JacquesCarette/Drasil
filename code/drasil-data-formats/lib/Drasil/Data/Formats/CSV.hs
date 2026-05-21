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

header :: CSV -> Maybe [Text]
header (CSV mhr _) = mhr

rows :: CSV -> [[Text]]
rows (CSV _ rs) = rs

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

rowCount :: CSV -> Int
rowCount (CSV _ rs) = length rs

data DoubleQuotationPolicy = Minimal | Everywhere

newtype CSVRenderOptions = CSVRO DoubleQuotationPolicy

csvRenderOpts :: DoubleQuotationPolicy -> CSVRenderOptions
csvRenderOpts = CSVRO

renderCSV :: CSV -> CSVRenderOptions -> Doc ann
renderCSV (CSV mhr rs) (CSVRO dqp) = vcat $ map renderRow allRs
  where
    allRs = maybe rs (: rs) mhr

    esc = escapeCellPolicy dqp
    renderRow = hcat . intersperse comma . map esc

escapeCellPolicy :: DoubleQuotationPolicy -> Text -> Doc ann
escapeCellPolicy Minimal t
  | T.any needsQuote t = quoteAndEscape t
  | otherwise = escHls t
escapeCellPolicy Everywhere t = quoteAndEscape t

needsQuote :: Char -> Bool
needsQuote c = c == '"' || c == ',' || c == '\n' || c == '\r'

quoteAndEscape :: Text -> Doc ann
quoteAndEscape = dquotes . escHls . T.replace dqs ddqs

dqs, ddqs :: Text
dqs = T.pack "\""
ddqs = dqs <> dqs

-- | Internal: `prettyprinter` needs us to manually deal with hard linebreaks.
escHls :: Text -> Doc ann
escHls = hcat . intersperse hardline . map pretty . T.splitOn lf . T.replace crlf lf
  where
    lf = T.pack "\n"
    crlf = T.pack "\r\n"
