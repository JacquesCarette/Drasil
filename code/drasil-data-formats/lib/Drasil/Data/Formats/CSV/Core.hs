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
