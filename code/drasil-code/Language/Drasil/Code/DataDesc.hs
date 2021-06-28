module Language.Drasil.Code.DataDesc where

import Language.Drasil.Chunk.Code (CodeVarChunk)

import Data.List (nub)
import Data.List.NonEmpty (NonEmpty(..), fromList)

-- | A 'DataItem' is just a 'CodeVarChunk' (a piece of data).
type DataItem = CodeVarChunk

-- New DataDesc
-- | A data description either has data connected to other pieces of data, or a single piece of data.
data DataDesc' = DD Data' Delimiter DataDesc' | End Data'

-- Data can either contain a single 'DataItem'', 'Data', or 'Junk'. See source for more details.
data Data' = Datum DataItem' -- ^ Single data item.
  -- | To be used in cases where multiple list-type data have their 
  -- elements intermixed, and thus need to be described together.
  | Data 
    (NonEmpty DataItem') -- The DataItems being simultaneously described. 
                -- The intra-list delimiters for any shared dimensions should 
                -- be the same between the DataItems. For example, if mixing 2 
                -- lists of same dimension, the intra-list delimiters must be 
                -- the same between the two lists. If mixing a 1-D list with a 
                -- 2-D list, the first delimiter must be the same, but the 2nd 
                -- delimiter for the 2-D list is not constrained because there 
                -- is no corresponding delimiter for the 1-D list.
    Integer -- Degree of intermixing
            -- 0 <= degree of intermixing <= minimum dimension of the DataItems
            -- Ex. 2 2-D lists with 1 degree of intermixing:
              -- x11 x12, y11 y12; x21 x22, y21 y22
            -- Ex. 2 2-D lists with 2 degrees of intermixing:
              -- x11, y11 x12, y12; x21, y21 x22, y22
    Delimiter -- Delimiter between elements from different lists
  | Junk -- ^ Data that can be ignored/skipped over.

-- | A piece of data that contains the datum described and delimeters between elements. 
-- The size of the list of delimiters should be equal to the dimension of datum.
data DataItem' = DI 
  CodeVarChunk -- The datum being described
  [Delimiter] -- Delimiters between list elements. 
              -- Size of list should equal dimension of datum 
              -- Ex. a 1-D list needs 1 delimiter, a 2-D list needs 2 delimiters
              -- Outermost delimiter first, innermost last

type Delimiter = String

-- New constructors

dataDesc :: [Data'] -> Delimiter -> DataDesc'
dataDesc [d] _ = End d
dataDesc (d:ds) dlm = DD d dlm (dataDesc ds dlm) 
dataDesc [] _ = error "DataDesc must have at least one data item"

singleton' :: CodeVarChunk -> Data'
singleton' d = Datum $ DI d []

list :: CodeVarChunk -> [Delimiter] -> Data'
list d dlms = Datum $ DI d dlms

interwovenLists :: [DataItem'] -> Integer -> Delimiter -> Data'
interwovenLists [] _ _ = error "interwovenLists must be passed a non-empty list"
interwovenLists ds i dl = Data (fromList ds) i dl

junk :: Data'
junk = Junk

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