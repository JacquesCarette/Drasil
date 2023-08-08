module Language.Drasil.Code.DataDesc where

import Language.Drasil.Chunk.Code (CodeVarChunk)

import Data.List (nub)
import Data.List.NonEmpty (NonEmpty(..), fromList)

-- | A 'DataItem' is just a 'CodeVarChunk' (a piece of data).
type DataItem = CodeVarChunk

-- New DataDesc
-- | A data description either has data connected to other pieces of data, or a single piece of data.
data DataDesc' = DD Data' Delimiter DataDesc' | End Data'

-- Data can either contain a single 'DataItem'', 'Data', or 'Junk'. See @source@ for more details on each constructor.
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

-- | Delimiter between elements of data.
type Delimiter = String

-- New constructors

-- | Organize a list of data with a given 'Delimiter' into a 'DataDesc''.
dataDesc :: [Data'] -> Delimiter -> DataDesc'
dataDesc [d] _ = End d
dataDesc (d:ds) dlm = DD d dlm (dataDesc ds dlm)
dataDesc [] _ = error "DataDesc must have at least one data item"

-- | Constructor for creating a single datum.
singleton' :: CodeVarChunk -> Data'
singleton' d = Datum $ DI d []

-- | Constructor for creating data from a description of the data and a list of delimiters with a size equal to the dimension.
list :: CodeVarChunk -> [Delimiter] -> Data'
list d dlms = Datum $ DI d dlms

-- | Weaves elements of data together given a list of data items, a degree of intermixing, and a delimiter for the data.
interwovenLists :: [DataItem'] -> Integer -> Delimiter -> Data'
interwovenLists [] _ _ = error "interwovenLists must be passed a non-empty list"
interwovenLists ds i dl = Data (fromList ds) i dl

-- | Constructor for data that should be skipped.
junk :: Data'
junk = Junk

-- Old DataDesc
-- | Older version of 'DataDesc''. Holds a list of 'Data'.
type DataDesc = [Data]

-- | Older version of 'Delimiter'. Holds a 'Char'.
type Delim = Char  -- delimiter

-- | Older version of 'Data''.
data Data = Singleton DataItem                      -- ^ Single datum.
          | JunkData                                -- ^ Junk data (can be skipped).
          | Line LinePattern Delim                  -- ^ Single-line pattern of data with a delimiter.
          | Lines LinePattern (Maybe Integer) Delim -- ^ Multi-line data.
                                                    -- @Maybe Int@ determines the number of lines.
                                                    -- If it is Nothing, then it is unknown so go to end of file.
-- | Determines the pattern of data.
data LinePattern = Straight [DataItem] -- ^ Line of data with no pattern.
                 | Repeat [DataItem]   -- ^ Line of data with repeated pattern.

-- Old constructors/helpers

-- | Constructor for a single datum.
singleton :: DataItem -> Data
singleton = Singleton

-- | Constructor for junk data.
junkLine :: Data
junkLine = JunkData

-- | Constructor for a single line of data.
singleLine :: LinePattern -> Delim -> Data
singleLine = Line

-- | Constructor for an unknown amount of lines of data.
multiLine :: LinePattern -> Delim -> Data
multiLine l = Lines l Nothing

-- | Constructor for multi-line data with a specified number of lines.
multiLine' :: LinePattern -> Integer -> Delim -> Data
multiLine' l i = Lines l (Just i)

-- | Constructor for data with no pattern.
straight :: [DataItem] -> LinePattern
straight = Straight

-- | Constructor for data with a repeated pattern.
repeated :: [DataItem] -> LinePattern
repeated = Repeat

-- | Checks if a piece of 'Data' is 'JunkData'.
isJunk :: Data -> Bool
isJunk JunkData = True
isJunk _ = False

-- | Checks if a piece of 'Data' is only a single line.
isLine :: Data -> Bool
isLine Line{} = True
isLine _ = False

-- | Checks if a piece of 'Data' is multi-line.
isLines :: Data -> Bool
isLines Lines{} = True
isLines _ = False

-- | Gets the data inputs from a 'DataDesc'.
getInputs :: DataDesc -> [DataItem]
getInputs d = nub $ concatMap getDataInputs d

-- | Helper that gets data inputs from 'Data'.
getDataInputs :: Data -> [DataItem]
getDataInputs (Singleton v) = [v]
getDataInputs (Line lp _) = getPatternInputs lp
getDataInputs (Lines lp _ _) = getPatternInputs lp
getDataInputs JunkData = []

-- | Helper that gets the data inputs from single-line or multi-line data.
getPatternInputs :: LinePattern -> [DataItem]
getPatternInputs (Straight vs) = vs
getPatternInputs (Repeat vs) = vs
