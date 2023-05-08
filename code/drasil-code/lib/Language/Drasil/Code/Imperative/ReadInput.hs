-- | Defines functions for reading values from a file corresponding to a DataDesc
module Language.Drasil.Code.Imperative.ReadInput (
  sampleInputDD, readWithDataDesc
) where

import Language.Drasil hiding (Data, Matrix, CodeVarChunk)
import Language.Drasil.Code.DataDesc (DataDesc'(..), Data'(..), DataItem'(..), 
  Delimiter, dataDesc, junk, list, singleton')
import Language.Drasil.Chunk.Code (CodeVarChunk)
import Language.Drasil.Expr.Development (Expr(Matrix))

import Control.Lens ((^.))
import Data.List (intersperse, isPrefixOf, transpose)
import Data.List.Split (splitOn)
import Data.List.NonEmpty (NonEmpty(..), toList)

-- | Reads data from a file and converts the values to 'Expr's. The file must be 
-- formatted according to the 'DataDesc'' passed as a parameter.
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
      readData (Data ((DI c [dlm1]):|_) 1 dlm2) s = map ((Matrix . (:[])) .
        map (strAsExpr (getInnerSpace $ c ^. typ))) $ transpose $
        map (splitOn dlm2) $ splitOn dlm1 s
      readData (Data ((DI c [dlm1, dlm3]):|_) 1 dlm2) s = map (Matrix .
        map (map (strAsExpr (getInnerSpace $ c ^. typ)))) $ transpose $
        map (map (splitOn dlm3) . splitOn dlm2) $ splitOn dlm1 s
      readData (Data ((DI c [dlm1, dlm2]):|_) 2 dlm3) s = map (Matrix .
        map (map (strAsExpr (getInnerSpace $ c ^. typ))) . transpose) $ 
        transpose $ map (map (splitOn dlm3) . splitOn dlm2) $ splitOn dlm1 s
      readData _ _ = error "Invalid degree of intermixing in DataDesc or list with more than 2 dimensions (not yet supported)"
      -- Below match is an attempt at a generic match for Data, but it doesn't 
      -- work because the following are needed:
      --   - 1-D Vect Expr constructor
      --   - A map function on Expr Vects (exprVectMap)
      --   - A transpose function on Expr Vects (exprVectTranspose)
      -- readData (Data ((DI c dlms):dis) i dlm2) s = let (ls,rs) = splitAt i 
      --   dlms in transposeData i $ data (ls ++ [dlm] ++ rs) (getInnerType $ c ^. typ) s
      readDataItem :: DataItem' -> String -> Expr
      readDataItem (DI c []) s = strAsExpr (c ^. typ) s
      readDataItem (DI c [dlm]) s = strListAsExpr (c ^. typ) (splitOn dlm s)
      readDataItem (DI c [dlm1, dlm2]) s = strList2DAsExpr (c ^. typ) 
        (map (splitOn dlm2) $ splitOn dlm1 s)
      -- FIXME: Since the representation for vectors in Expr is Matrix, and that constructor accepts a 2-D list, building a 3-D or higher matrix is not straightforward. This would be easier if Expr had a constructor for 1-D vectors, which could be nested to achieve n-dimensional structures.
      readDataItem (DI _ _) _ = error "readWithDataDesc does not yet support lists with 3 or more dimensions"
  return $ readDD ddsc ins

-- data :: [Delimiter] -> Space -> String -> Expr
-- data [] sp s = strAsExpr sp s
-- data (d:ds) = Vect $ map (data ds) (splitOn d s)

-- transposeData :: Integer -> (Expr -> Expr)
-- transposeData 1 = exprVectTranspose
-- transposeData n = exprVectMap exprVectTranspose . transposeData (n-1)

-- | Defines the DataDesc for the file containing a sample data set, which a 
-- user must supply if they want to generate a sample input file.
sampleInputDD :: [CodeVarChunk] -> DataDesc'
sampleInputDD ds = dataDesc (junk : intersperse junk (map toData ds)) "\n"
  where toData d = toData' (d ^. typ) d
        toData' t@(Vect _) d = list d 
          (take (getDimension t) ([", ", "; "] ++ iterate (':':) ":"))
        toData' _ d = singleton' d

-- helpers

-- | Converts a 'String' to an 'Expr' of a given 'Space'.
strAsExpr :: Space -> String -> Expr
strAsExpr Integer  s = int (read s :: Integer)
strAsExpr Natural  s = int (read s :: Integer)
strAsExpr Real     s = dbl (read s :: Double)
strAsExpr Rational s = dbl (read s :: Double)
strAsExpr String   s = str s
strAsExpr _        _ = error "strAsExpr should only be numeric space or string"

-- | Gets the dimension of a 'Space'.
getDimension :: Space -> Int
getDimension (Vect t) = 1 + getDimension t
getDimension _ = 0

-- | Splits a string at the first (and only the first) occurrence of a delimiter.
-- The delimiter is dropped from the result.
splitAtFirst :: String -> Delimiter -> (String, String)
splitAtFirst = splitAtFirst' []
  where splitAtFirst' acc [] _ = (acc, [])
        splitAtFirst' acc s@(h:t) d = if d `isPrefixOf` s then 
          (acc, dropDelim d s) else splitAtFirst' (acc++[h]) t d
        dropDelim (d:ds) (s:ss) = if d == s then dropDelim ds ss 
          else error "impossible"
        dropDelim [] s = s
        dropDelim _ [] = error "impossible"

-- | Converts a list of 'String's to a Matrix 'Expr' of a given 'Space'.
strListAsExpr :: Space -> [String] -> Expr
strListAsExpr (Vect t) ss = Matrix [map (strAsExpr t) ss]
strListAsExpr _ _ = error "strListsAsExpr called on non-vector space"

-- | Converts a 2D list of 'String's to a Matrix 'Expr' of a given 'Space'.
strList2DAsExpr :: Space -> [[String]] -> Expr
strList2DAsExpr (Vect (Vect t)) sss = Matrix $ map (map (strAsExpr t)) sss
strList2DAsExpr _ _ = error "strLists2DAsExprs called on non-2D-vector space"
