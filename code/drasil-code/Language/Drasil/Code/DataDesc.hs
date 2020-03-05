module Language.Drasil.Code.DataDesc where

import Language.Drasil.Chunk.Code (CodeVarChunk)

import Data.List (nub)

type DataItem = CodeVarChunk

-- New DataDesc

data DataDesc' = DD Data' Delimiter DataDesc' | End Data'

data Data' = Data Integer [DataItem] [Delimiter] | Junk 

type Delimiter = String

-- New constructors

dataDesc :: [Data'] -> Delimiter -> DataDesc'
dataDesc [d] _ = End d
dataDesc (d:ds) dlm = DD d dlm (dataDesc ds dlm) 
dataDesc [] _ = error "DataDesc must have at least one data item"

singleton' :: DataItem -> Data'
singleton' d = Data 0 [d] []

list :: DataItem -> Delimiter -> Data
list d dlm = Data 0 [d] [dlm]

interwovenLists :: [DataItem] -> [Delimiter] -> Data'
interwovenLists = Data (length d)

-- Old DataDesc

type DataDesc = [Data]
           
type Delim = Char  -- delimiter
  
data Data = Singleton DataItem
          | JunkData
          | Line LinePattern Delim
          | Lines LinePattern (Maybe Integer) Delim -- multi-line data
                                                -- (Maybe Int) = number of lines, Nothing = unknown so go to end of file  
          
data LinePattern = Straight [DataItem] -- line of data with no pattern
                 | Repeat [DataItem]   -- line of data with repeated pattern       

-- Old constructors/helpers

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