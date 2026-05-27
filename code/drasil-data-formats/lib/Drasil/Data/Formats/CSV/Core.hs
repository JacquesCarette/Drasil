module Drasil.Data.Formats.CSV.Core
  ( -- * CSVs
    CSV,
    ColumnCount,
    RowCount,
    header,
    rows,
    columnCount,
    rowCount,

    -- ** Constructors
    mkCSV,
  )
where

import Data.List (find)
import Data.Text (Text)

-- | The number of columns a CSV has.
type ColumnCount = Int

-- | The number of rows a CSV has.
type RowCount = Int

-- | A CSV file representation containing an optional header and a list of rows.
--
-- Caches the column and row counts for future potential reference.
data CSV = CSV (Maybe [Text]) [[Text]] ColumnCount RowCount
  deriving (Show, Eq)

-- | Get the header row of a 'CSV'.
header :: CSV -> Maybe [Text]
header (CSV mhr _ _ _) = mhr

-- | Get all rows of a 'CSV'.
rows :: CSV -> [[Text]]
rows (CSV _ rs _ _) = rs

-- | Get the number of columns in a 'CSV'.
columnCount :: CSV -> ColumnCount
columnCount (CSV _ _ cc _) = cc

-- | Get the number of rows in a 'CSV'.
rowCount :: CSV -> RowCount
rowCount (CSV _ _ _ rc) = rc

-- | Create a 'CSV'. Expects all rows and the header to have the same length. If
-- the expected column count is not provided (the first parameter), then the
-- number of columns in the header is used as the expected column count. If the
-- header does not exist, the length of the first row is used. If the data is
-- also empty, you will have an empty CSV with no columns and no rows.
mkCSV :: Maybe ColumnCount -> Maybe [Text] -> [[Text]] -> Either String CSV
mkCSV mcols mhr rs = maybe (Right $ CSV mhr rs cc rc) Left (saneLengths (cc, src) mhr rs)
  where
    (cc, src) = expectedColumnCount mcols mhr rs
    rc = length rs

-- | Internal: Check if the header and all rows have the same length as the
-- expected number of columns. Returns 'Nothing' if the CSV is valid, or 'Just'
-- the error message otherwise.
saneLengths :: (Int, String) -> Maybe [Text] -> [[Text]] -> Maybe String
saneLengths (expLen, expLenSrc) mhr rs =
  case mhr of
    Just hdr | length hdr /= expLen -> Just $ formatErr "Header" (length hdr)
    _ -> rowLengthErr <$> find (\(_, r) -> length r /= expLen) (zip [1 :: Int ..] rs)
  where
    formatErr target actualLen = concat [
        target, " has ", show actualLen, " columns, but expected ",
        show expLen, " (based on ", expLenSrc, ")"
      ]
    rowLengthErr (i, r) = formatErr ("Row " ++ show i) (length r)

-- | Internal: Find the expected number of columns for a CSV, along with a
-- description of the source of the expectation.
expectedColumnCount :: Maybe ColumnCount -> Maybe [Text] -> [[Text]] -> (Int, String)
expectedColumnCount (Just cols) _ _ = (cols, "expected columns input")
expectedColumnCount _ (Just header') _ = (length header', "header length")
expectedColumnCount _ _ (fr : _) = (length fr, "first row length")
expectedColumnCount _ _ _ = (0, "empty data")
