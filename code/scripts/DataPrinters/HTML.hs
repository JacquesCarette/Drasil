module DataPrinters.HTML where

import Data.List.Split (splitOn)

-- | Adds all of the required html syntax to generate the title files.
mkhtmlTitle :: String -> String
mkhtmlTitle xs = "\t\t<thead>\n" ++ concatMap (\y -> "\t\t\t<th>" ++ y ++ "</th>\n") (splitOn "," xs) ++ "\t\t</thead>"

-- | Similar to 'mkhtmlTitle', but the given list of strings must already be split into their respective cells.
mkhtmlHeader :: [String] -> String
mkhtmlHeader xs = "\t\t<thead>\n" ++ concatMap (\y -> "\t\t\t<th>" ++ y ++ "</th>\n") xs ++ "\t\t</thead>"

-- | Fills in the given number of html cells in a row with empty cells.
mkhtmlEmptyCell :: Int -> String
mkhtmlEmptyCell num
  | num <= 0 = ""
  | otherwise = ",\t" ++ mkhtmlEmptyCell (num-1)

-- | Adds all required html syntax for creating a normal html table row.
mkhtmlRow :: [String] -> String
mkhtmlRow [] = []
mkhtmlRow (x:xs) = "\t\t\t<tr>\n" ++ concatMap (\y -> "\t\t\t\t<td>" ++ y ++ "</td>\n") (splitOn "," x) ++ "\t\t\t</tr>\n" ++ mkhtmlRow xs

-- | HTML housekeeping
htmlDataTableTitle, htmlConfig, htmlEnd :: String
htmlDataTableTitle = "<!DOCTYPE html>\n<html>\n\t<title>Auto-Generated Data Table for Drasil</title>"
htmlConfig = "\t<table border=\"1\" cellspacing=\"0\" cellpadding=\"3\" class=\"dataframe\">"
htmlEnd = "\t\t</tbody>\n</html>"
