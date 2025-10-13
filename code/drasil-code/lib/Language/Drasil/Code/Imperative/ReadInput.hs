-- | Defines functions for reading values from a file corresponding to a DataDesc
module Language.Drasil.Code.Imperative.ReadInput (
  sampleInputDD, readWithDataDesc
) where

import Language.Drasil hiding (Data, Matrix, CodeVarChunk)
import Language.Drasil.Space (ClifKind(..))
import Language.Drasil.Code.DataDesc (DataDesc'(..), Data'(..), DataItem'(..),
  Delimiter, dataDesc, junk, list, singleton')
import Language.Drasil.Chunk.Code (CodeVarChunk)
import Language.Drasil.Expr.Development (Expr(Matrix))

import Control.Lens ((^.))
import Data.List (intersperse, isPrefixOf, transpose)
import Data.List.Split (splitOn)
import Data.List.NonEmpty (NonEmpty( (:|) ), toList)

--------------------------------------------------------------------------------
-- | Reads data from a file and converts the values to 'Expr's. 
-- The file must be formatted according to the 'DataDesc'' passed as a parameter.
--------------------------------------------------------------------------------
readWithDataDesc :: FilePath -> DataDesc' -> IO [Expr]
readWithDataDesc fp ddsc = do
  ins <- readFile fp
  let readDD :: DataDesc' -> String -> [Expr]
      readDD (DD ds dlm dd) s = let (dat,rest) = splitAtFirst s dlm in
        readData ds dat ++ readDD dd rest
      readDD (End d) s = readData d s

      readData :: Data' -> String -> [Expr]
      readData Junk _ = []
      readData (Datum d) s = [readDataItem d s]
      readData (Data dis 0 d) s = zipWith readDataItem (toList dis) (splitOn d s)
      readData (Data ((DI c [dlm1]):|_) 1 dlm2) s =
        map (Matrix . (:[]) . map (strAsExpr (getInnerType $ c ^. typ))) $
        transpose $ map (splitOn dlm2) $ splitOn dlm1 s
      readData (Data ((DI c [dlm1, dlm2]):|_) 2 dlm3) s =
        map (Matrix . map (map (strAsExpr (getInnerType $ c ^. typ))) . transpose) $
        transpose $ map (map (splitOn dlm3) . splitOn dlm2) $ splitOn dlm1 s
      readData _ _ = error "Invalid degree of intermixing or >2D lists not supported"

      readDataItem :: DataItem' -> String -> Expr
      readDataItem (DI c []) s = strAsExpr (c ^. typ) s
      readDataItem (DI c [dlm]) s = strListAsExpr (c ^. typ) (splitOn dlm s)
      readDataItem (DI c [dlm1, dlm2]) s = strList2DAsExpr (c ^. typ)
        (map (splitOn dlm2) $ splitOn dlm1 s)
      readDataItem (DI _ _) _ = error "readWithDataDesc does not support >2D lists"

  return $ readDD ddsc ins

--------------------------------------------------------------------------------
-- | Defines the DataDesc for the file containing a sample data set.
-- Vectors are recognized and given appropriate delimiters for dblpend.
--------------------------------------------------------------------------------
sampleInputDD :: [CodeVarChunk] -> DataDesc'
sampleInputDD ds = dataDesc (junk : intersperse junk (map toData ds)) "\n"
  where
    toData d = toData' (d ^. typ) d

    toData' t@(ClifS (Fixed 2) Vector _) d = list d [", ", "; "]  -- 2D vectors
    toData' t@(ClifS _ Vector _) d = list d [", "]                 -- general vector fallback
    toData' _ d = singleton' d                                     -- scalar

--------------------------------------------------------------------------------
-- | Converts a 'String' to an 'Expr' of a given 'Space'.
--------------------------------------------------------------------------------
strAsExpr :: Space -> String -> Expr
strAsExpr Integer  s = int (read s :: Integer)
strAsExpr Natural  s = int (read s :: Integer)
strAsExpr Real     s = dbl (read s :: Double)
strAsExpr Rational s = dbl (read s :: Double)
strAsExpr String   s = str s
strAsExpr _        _ = error "strAsExpr: unsupported space type"

--------------------------------------------------------------------------------
-- | Gets the dimension of a 'Space'.
--------------------------------------------------------------------------------
getDimension :: Space -> Int
getDimension (ClifS (Fixed n) Vector _) = fromIntegral n
getDimension (ClifS _ Vector t) = 1 + getDimension t
getDimension _ = 0

--------------------------------------------------------------------------------
-- | Splits a string at the first occurrence of a delimiter.
-- The delimiter is dropped from the result.
--------------------------------------------------------------------------------
splitAtFirst :: String -> Delimiter -> (String, String)
splitAtFirst = splitAtFirst' []
  where
    splitAtFirst' acc [] _ = (acc, [])
    splitAtFirst' acc s@(h:t) d =
      if d `isPrefixOf` s then (acc, dropDelim d s)
      else splitAtFirst' (acc++[h]) t d
    dropDelim (d:ds) (s:ss) = if d == s then dropDelim ds ss else error "impossible"
    dropDelim [] s = s
    dropDelim _ [] = error "impossible"

--------------------------------------------------------------------------------
-- | Converts a list of 'String's to a 1D vector (Matrix) 'Expr' of a given 'Space'.
--------------------------------------------------------------------------------
strListAsExpr :: Space -> [String] -> Expr
strListAsExpr (ClifS _ Vector t) ss = Matrix [map (strAsExpr t) ss]
strListAsExpr _ _ = error "strListAsExpr: called on non-vector space"

--------------------------------------------------------------------------------
-- | Converts a 2D list of 'String's to a 2D vector (Matrix) 'Expr' of a given 'Space'.
--------------------------------------------------------------------------------
strList2DAsExpr :: Space -> [[String]] -> Expr
strList2DAsExpr (ClifS _ Vector (ClifS _ Vector t)) sss = Matrix $ map (map (strAsExpr t)) sss
strList2DAsExpr _ _ = error "strList2DAsExpr: called on non-2D-vector space"