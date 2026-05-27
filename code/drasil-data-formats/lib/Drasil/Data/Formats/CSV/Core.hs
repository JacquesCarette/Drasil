module Drasil.Data.Formats.CSV.Core
  ( -- * Type
    CSV,

    -- * Construction
    mkCSV,

    -- * Destructors
    header,
    rows,
    rowCount,
  )
where

import Data.List (find)
import Data.Text (Text)

-- | A CSV file representation; an optional header and a list of rows.
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
mkCSV mhr rs = maybe (pure $ CSV mhr rs) Left (saneLengths mhr rs)

-- | Internal: Check if all rows have the same length as the header (if it
-- exists) or the first row (if the rows are non-empty). Returns 'Nothing' if
-- the CSV is valid, or 'Just' the error message otherwise.
saneLengths :: Maybe [Text] -> [[Text]] -> Maybe String
saneLengths mhr (fr : rrs) =
  fmap (\(i, r) -> concat [
      "Row ", show i, " has ", show (length r), " columns, but expected ", show expLen,
      " (based on ", expLenSrc, ")"
    ]) (find (\(_, r) -> length r /= expLen) (zip ([1 ..] :: [Int]) rrs))
  where
    (expLen, expLenSrc) = maybe (length fr, "first row") (\r -> (length r, "header")) mhr
saneLengths _ [] = Nothing

-- | Calculate the number of rows in a 'CSV'.
rowCount :: CSV -> Int
rowCount (CSV _ rs) = length rs
