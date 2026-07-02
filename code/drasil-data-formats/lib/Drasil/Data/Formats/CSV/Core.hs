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
import Numeric.Natural (Natural)

-- | The number of columns a CSV has.
type ColumnCount = Natural

-- | The number of rows a CSV has (excluding its reader).
type RowCount = Natural

-- | A CSV file representation containing an optional header and a list of rows.
--
-- Caches the column and row counts for future potential reference.
data CSV = CSV
  { _header :: Maybe [Text],
    _rows :: [[Text]],
    _columnCount :: ColumnCount,
    _rowCount :: RowCount
  }
  deriving (Show, Eq)

-- | Get the header row of a 'CSV'.
header :: CSV -> Maybe [Text]
header = _header
{-# INLINE header #-}

-- | Get all rows of a 'CSV' (excludes header).
rows :: CSV -> [[Text]]
rows = _rows
{-# INLINE rows #-}

-- | Get the number of columns in a 'CSV'.
columnCount :: CSV -> ColumnCount
columnCount = _columnCount
{-# INLINE columnCount #-}

-- | Get the number of rows in a 'CSV' (excludes header).
rowCount :: CSV -> RowCount
rowCount = _rowCount
{-# INLINE rowCount #-}

-- | Create a 'CSV'. Expects all rows and the header to have the same length. If
-- the expected column count is not provided (the first parameter), then the
-- number of columns in the header is used as the expected column count. If the
-- header does not exist, the length of the first row is used. If the data is
-- also empty, you will have an empty CSV with no columns and no rows.
mkCSV :: Maybe ColumnCount -> Maybe [Text] -> [[Text]] -> Either String CSV
mkCSV mcols mhr rs = maybe (Right $ CSV mhr rs cc (len rs)) Left (saneLengths (cc, src) mhr rs)
  where
    (cc, src) = expectedColumnCount mcols mhr rs

-- | Internal: Check if the header and all rows have the same length as the
-- expected number of columns. Returns 'Nothing' if the CSV is valid, or 'Just'
-- the error message otherwise.
saneLengths :: (Natural, String) -> Maybe [Text] -> [[Text]] -> Maybe String
saneLengths (expLen, expLenSrc) mhr rs =
  case mhr of
    Just hdr | let l = len hdr, l /= expLen -> Just $ formatErr "Header" l
    _ -> format <$> find ((/= expLen) . snd) (zip [1 :: Natural ..] (map len rs))
  where
    formatErr target actualLen = concat [
        target, " has ", show actualLen, " columns, but expected ",
        show expLen, " (based on ", expLenSrc, ")"
      ]
    format (i, l) = formatErr ("Row " ++ show i) l

-- | Internal: Find the expected number of columns for a CSV, along with a
-- description of the source of the expectation.
expectedColumnCount :: Maybe ColumnCount -> Maybe [Text] -> [[Text]] -> (Natural, String)
expectedColumnCount (Just cols) _ _ = (cols, "expected columns input")
expectedColumnCount _ (Just header') _ = (len header', "header length")
expectedColumnCount _ _ (fr : _) = (len fr, "first row length")
expectedColumnCount _ _ _ = (0, "empty data")

-- | Internal: Output polymorphic 'length'.
len :: (Integral n) => [a] -> n
len = fromIntegral . length
{-# INLINE len #-}
