-- | Functions for constructing CSV files as a 'Doc'.
module Utils.Drasil.CSV where

import Text.PrettyPrint (Doc, text, vcat)
import Data.List (intercalate)

-- | Creates a CSV file as a 'Doc' from a 'String' matrix.
makeCSV :: [[String]] -> Doc
makeCSV rows = vcat $ map (text . formatRow) rows
  where
    -- | Seperates each row item with a comma.
    formatRow :: [String] -> String
    formatRow = intercalate "," . map escape

    -- | Adds quotations around the item if it contains ',', '"', \n', or ' '.
    escape :: String -> String
    escape s
      | any (`elem` ",\"\n ") s = "\"" ++ concatMap escapeChar s ++ "\""
      | otherwise = s

    -- | Escapes double quotes.
    escapeChar :: Char -> String
    escapeChar '"' = "\"\""
    escapeChar c   = [c]
