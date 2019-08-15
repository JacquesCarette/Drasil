module Language.Drasil.Code.DataDesc where

import Language.Drasil.Chunk.Code (CodeChunk)

import Data.List (nub)

type DataDesc = [Data]
type DataItem = CodeChunk
           
type Delim = Char  -- delimiter
  
data Data = Singleton DataItem
          | JunkData
          | Line LinePattern Delim
          | Lines LinePattern (Maybe Integer) Delim -- multi-line data
                                                -- (Maybe Int) = number of lines, Nothing = unknown so go to end of file  
          
data LinePattern = Straight [DataItem] -- line of data with no pattern
                 | Repeat [DataItem]   -- line of data with repeated pattern       

singleton :: DataItem -> Data
singleton = Singleton

junkLine :: Data
junkLine = JunkData

singleLine :: LinePattern -> Delim -> Data
singleLine = Line 

multiLine :: LinePattern -> Delim -> Data
multiLine l = Lines l Nothing

multiLine' :: LinePattern -> Integer -> Delim -> Data
multiLine' l i = Lines l (Just i)

straight :: [DataItem] -> LinePattern
straight = Straight

repeated :: [DataItem] -> LinePattern
repeated = Repeat

isJunk :: Data -> Bool
isJunk JunkData = True
isJunk _ = False

isLine :: Data -> Bool
isLine Line{} = True
isLine _ = False

isLines :: Data -> Bool
isLines Lines{} = True
isLines _ = False

getInputs :: DataDesc -> [DataItem]
getInputs d = nub $ concatMap getDataInputs d

getDataInputs :: Data -> [DataItem]
getDataInputs (Singleton v) = [v]
getDataInputs (Line lp _) = getPatternInputs lp
getDataInputs (Lines lp _ _) = getPatternInputs lp
getDataInputs JunkData = []

getPatternInputs :: LinePattern -> [DataItem]
getPatternInputs (Straight vs) = vs
getPatternInputs (Repeat vs) = vs