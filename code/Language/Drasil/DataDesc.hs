{-# LANGUAGE GADTs #-}

module Language.Drasil.DataDesc where

import Language.Drasil.Chunk.Code
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.SymbolForm

type DataDesc = [Data]

type DataItem = CodeChunk

data Entry = Entry DataItem             -- regular entry (float, int, bool, etc)
           | ListEntry [Ind] DataItem   -- index to insert into list
           | JunkEntry                  -- junk should be skipped in input file

data Ind = Explicit Int   -- explicit index
         | WithPattern    -- use current repetition number in repeated pattern
         | WithLine       -- use current line number in multi-line data
           
type Delim = Char  -- delimiter
  
data Data = Singleton DataItem
          | JunkData
          | Line LinePattern Delim
          | Lines LinePattern (Maybe Int) Delim -- multi-line data
                                                -- (Maybe Int) = number of lines, Nothing = unknown so go to end of file  
          
data LinePattern = Straight [Entry]             -- line of data with no pattern
                 | Repeat [Entry] (Maybe Int)   -- line of data with repeated pattern
                                                -- (Maybe Int) = number of repetitions, Nothing = unknown so go to end of line          

                                            

entry :: (Quantity c, SymbolForm c) => c -> Entry
entry c = Entry $ codevar c

listEntry :: (Quantity c, SymbolForm c) => [Ind] -> c -> Entry
listEntry i c = ListEntry i $ codevar c

junk :: Entry
junk = JunkEntry

singleton :: (Quantity c, SymbolForm c) => c -> Data
singleton c = Singleton $ codevar c

junkLine :: Data
junkLine = JunkData

singleLine :: LinePattern -> Delim -> Data
singleLine = Line

multiLine :: LinePattern -> Delim -> Data
multiLine l d = Lines l Nothing d 

multiLine' :: LinePattern -> Int -> Delim -> Data
multiLine' l i d = Lines l (Just i) d 

straight :: [Entry] -> LinePattern
straight = Straight

repeated :: [Entry] -> LinePattern
repeated e = Repeat e Nothing

repeated' :: [Entry] -> Int -> LinePattern
repeated' e i = Repeat e (Just i)


-- for glassbr read_table:

--c_z_array = contents z_array
--c_x_array = contents x_array
--c_y_array = contents y_array

--DataDesc [c_x_array, c_y_array, c_z_array] [
--    Line (Repeat [Junk, ListEntry [WithLine] z_array] Nothing) ',',
--    Lines (Repeat [ListEntry [WithPattern, WithLine] x_array, ListEntry [WithPattern, WithLine] y_array] Nothing) Nothing ','
--  ]
-- ^ pattern evident from TSD.txt

-----

--for glassbr defaultInput:

{-
glassInputData :: 
glassInputData = 
  [ Singleton Junk,
    Singleton c_a, Singleton c_b, Singleton c_t,
    Singleton Junk,
    Singleton c_gt, 
    Singleton Junk,
    Singleton c_w, 
    Singleton Junk, 
    Singleton c_tnt, 
    Singleton Junk,
    Singleton c_sdx, Singleton c_sdy, Singleton c_sdz,
    Singleton Junk,
    Singleton c_pbtol
  ]

--FIXME: replace "Singleton _" with "Line Straight [_]" ?

-}